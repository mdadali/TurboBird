unit DBReaderSqlite;

(*
Sqlite database file reader

Author: Sergey Bodrov, 2025 Minsk
License: MIT

https://sqlite.org/fileformat.html

*)

interface

uses
  SysUtils, Classes, Types, Variants, DB, DBReaderBase, RFUtils;

type
  TSqliteFieldDef = record
    FieldName: string;
    FieldSize: Byte;
    FieldType: Byte;
    TypeName: string;
    IsPrimaryKey: Boolean;
  end;

  TSqliteTableInfo = class(TDbRowsList)
  public
    RootPageNum: Cardinal;  // 1-based
    RowCount: Integer;
    ColCount: Integer;
    PageNumArr: array of Integer;
    PageNumCount: Integer;
    FieldInfoArr: array of TSqliteFieldDef;
    TableSQL: string;
    // contain no rows
    function IsEmpty(): Boolean; override;
    // predefined table
    function IsSystem(): Boolean; override;
    // not defined in metadata
    function IsGhost(): Boolean; override;

    // set table metadata from SQL string
    procedure SetSQL(ASql: string);
    // AType - "TypeName(Size)"
    procedure AddFieldDef(AName, AType: string);
    // Note: adds extra empty numbers (grow)
    procedure AddPageNum(ANum: Cardinal);
  end;

  TSqliteTableInfoList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function GetItem(AIndex: Integer): TSqliteTableInfo;
    function GetByName(AName: string): TSqliteTableInfo;
    function GetByPageNum(ANum: Cardinal): TSqliteTableInfo;
    procedure SortByName();
  end;

  TDBReaderSqlite = class(TDBReader)
  private
    FTableList: TSqliteTableInfoList;
    //FDbVersion: Integer;
    FPageSize: Integer;
    FPageEnd: Integer; // PageSize - UnusedSize
    FIsMetadataLoaded: Boolean;
    FSchemaInfo: TSqliteTableInfo;
    FPagesInfo: TSqliteTableInfo;

    // fill schema tables from initial tables
    procedure FillTablesList();

    function ReadVarInt(var rdr: TRawDataReader): Int64;

    function ReadDataPage(const APageBuf: TByteDynArray; APagePos: Int64;
      ATableInfo: TSqliteTableInfo; AList: TDbRowsList): Boolean;
    function ReadRowData(const APageBuf: TByteDynArray; ARowOffs, ARowSize: Integer;
      ATableInfo: TSqliteTableInfo; AList: TDbRowsList): Boolean;

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

    property TableList: TSqliteTableInfoList read FTableList;
  end;


implementation

type
  TSqliteFileHeader = packed record
    HeaderStr: array [0..15] of AnsiChar;  // The header string: "SQLite format 3\000"
    PageSize: Word;                        // The database page size in bytes. Must be a power of two between 512 and 32768 inclusive, or the value 1 representing a page size of 65536.
    FileFormatW: Byte;                     // File format write version. 1 for legacy; 2 for WAL.
    FileFormatR: Byte;                     // File format read version. 1 for legacy; 2 for WAL.
    UnusedSize: Byte;                      // Bytes of unused "reserved" space at the end of each page. Usually 0.
    MaxPayloadFrac: Byte;                  // Maximum embedded payload fraction. Must be 64.
    MinPayloadFrac: Byte;                  // Minimum embedded payload fraction. Must be 32.
    LeafPayloadFrac: Byte;                 // Leaf payload fraction. Must be 32.
    FileChangeCount: Cardinal;             // File change counter.
    PagesCount: Cardinal;                  // Size of the database file in pages. The "in-header database size".
    FirstFreePage: Cardinal;               // Page number of the first freelist trunk page.
    FreePageCount: Cardinal;               // Total number of freelist pages.
    SchemaCookie: Cardinal;                // The schema cookie.
    SchemaFormst: Cardinal;                // The schema format number. Supported schema formats are 1, 2, 3, and 4.
    PageCacheSize: Cardinal;               // Default page cache size.
    VacuumPage: Cardinal;                  // The page number of the largest root b-tree page when in auto-vacuum or incremental-vacuum modes, or zero otherwise.
    TextEncoding: Cardinal;                // The database text encoding. A value of 1 means UTF-8. A value of 2 means UTF-16le. A value of 3 means UTF-16be.
    UserVersion: Cardinal;                 // The "user version" as read and set by the user_version pragma.
    VacuumMode: Cardinal;                  // True (non-zero) for incremental-vacuum mode. False (zero) otherwise.
    ApplicationID: Cardinal;               // The "Application ID" set by PRAGMA application_id.
    Reserved: array[0..19] of Byte;        // Reserved for expansion. Must be zero.
    VersionValidFor: Cardinal;             // The version-valid-for number.
    SqliteVersion: Cardinal;               // SQLITE_VERSION_NUMBER
    // 100 bytes
  end;

  TSqlitePageHead = packed record
    PageType: Byte;       // b-tree page type.
    FreeBlockOffs: Word;  // start of the first freeblock on the page, or is zero if there are no freeblocks.
    RecCount: Word;       // number of cells on the page.
    ContentOffs: Word;    // start of the cell content area. A zero value for this integer is interpreted as 65536.
    FragSize: Byte;       // number of fragmented free bytes within the cell content area.
    LastPageNum: Cardinal;    // right-most pointer. This value appears in the header of interior b-tree pages only and is omitted from all other pages.
  end;

const
  { Sqlite record format codes }
  rfNull        = $00; // NULL
  rfInt8        = $01; // 8-bit twos-complement integer.
  rfInt16       = $02; // big-endian 16-bit twos-complement integer.
  rfInt24       = $03; // big-endian 24-bit twos-complement integer.
  rfInt32       = $04; // big-endian 32-bit twos-complement integer.
  rfInt48       = $05; // big-endian 48-bit twos-complement integer.
  rfInt64       = $06; // big-endian 64-bit twos-complement integer.
  rfFloat       = $07; // big-endian IEEE 754-2008 64-bit floating point number.
  rf0           = $08; // integer 0. (Only available for schema format 4 and higher.)
  rf1           = $09; // integer 1. (Only available for schema format 4 and higher.)
  rfBlob        = $0C; // blob
  rfText        = $0D; // text

  DB3_PAGE_TYPE_INDEX_TREE  = $02;  // interior index b-tree page
  DB3_PAGE_TYPE_TABLE_TREE  = $05;  // interior table b-tree page
  DB3_PAGE_TYPE_INDEX_LEAF  = $0A;  // leaf index b-tree page
  DB3_PAGE_TYPE_TABLE_LEAF  = $0D;  // leaf table b-tree page

function SqliteColTypeToDbFieldType(AType: string): TFieldType;
var
  s: string;
  n: Integer;
begin
  //s := ExtractFirstWord(AType);
  n := 1;
  while n <= Length(AType) do
  begin
    if AType[n] in [' ', '('] then
      Break;
    Inc(n);
  end;
  s := Copy(AType, 1, n-1);

  s := UpperCase(s);
  if s = 'NULL' then Result := ftUnknown
  else if s = 'INTEGER' then Result := ftLargeint
  else if s = 'INT' then Result := ftInteger
  else if s = 'INT2' then Result := ftInteger
  else if s = 'INT8' then Result := ftInteger
  else if s = 'TINYINT' then Result := ftInteger
  else if s = 'SMALLINT' then Result := ftSmallint
  else if s = 'MEDIUMINT' then Result := ftInteger
  else if s = 'BIGINT' then Result := ftLargeint
  else if s = 'TEXT' then Result := ftString
  else if s = 'VARCHAR' then Result := ftString
  else if s = 'NVARCHAR' then Result := ftString
  else if s = 'CHAR' then Result := ftString
  else if s = 'NCHAR' then Result := ftString
  else if s = 'CHARACTER' then Result := ftString
  else if s = 'CLOB' then Result := ftMemo
  else if s = 'BLOB' then Result := ftBlob
  else if s = 'REAL' then Result := ftFloat
  else if s = 'DOUBLE' then Result := ftFloat
  else if s = 'FLOAT' then Result := ftFloat
  else if s = 'NUMERIC' then Result := ftString
  else if s = 'DATE' then Result := ftDate
  else if s = 'DATETIME' then Result := ftDateTime
  else if s = 'BOOLEAN' then Result := ftBoolean
  else Result := ftUnknown;
end;

{ TDBReaderSqlite }

procedure TDBReaderSqlite.AfterConstruction;
var
  TmpTable: TSqliteTableInfo;
begin
  inherited;
  FIsSingleTable := False;
  FTableList := TSqliteTableInfoList.Create();

  // sqlite_schema
  TmpTable := TSqliteTableInfo.Create();
  TmpTable.RootPageNum := 1;
  TmpTable.SetSQL('CREATE TABLE sqlite_schema(type text, name text, tbl_name text, rootpage integer, sql text, rowid integer PRIMARY KEY);');
  TableList.Add(TmpTable);
  FSchemaInfo := TmpTable;

  // sqlite_pages
  TmpTable := TSqliteTableInfo.Create();
  TmpTable.RootPageNum := 0;
  TmpTable.SetSQL('CREATE TABLE sqlite_pages(PageNum INTEGER, PageType INTEGER, PagePos INTEGER,'
    + ' FreeBlockOffs INTEGER, RecCount INTEGER, ContentOffs INTEGER, FragSize INTEGER, LastPageNum INTEGER,'
    + ' Comment TEXT);');
  TableList.Add(TmpTable);
  FPagesInfo := TmpTable;
end;

procedure TDBReaderSqlite.BeforeDestruction;
begin
  FreeAndNil(FTableList);
  inherited;
end;

function TDBReaderSqlite.FillTableInfoText(ATableName: string; ALines: TStrings): Boolean;
var
  i: Integer;
  TmpTable: TSqliteTableInfo;
  s: string;
begin
  Result := False;
  TmpTable := TableList.GetByName(ATableName);
  if not Assigned(TmpTable) then
    Exit;

  ALines.Add(Format('== Table Name=%s  RowCount=%d', [TmpTable.TableName, TmpTable.RowCount]));
  ALines.Add(Format('RootPageNum=%x', [TmpTable.RootPageNum]));
  ALines.Add(Format('ColCount=%d', [TmpTable.ColCount]));
  // PageNum array
  s := '';
  for i := Low(TmpTable.PageNumArr) to High(TmpTable.PageNumArr) do
    s := s + Format('%x,', [TmpTable.PageNumArr[i]]);
  //Delete(s, Length(s)-1, 1);
  ALines.Add(Format('PageNum[%d]=(%s)', [TmpTable.PageNumCount, s]));
  // SQL can be multiline
  ALines.Add(Format('TableSQL=%s', [TmpTable.TableSQL]));

  ALines.Add(Format('== FieldInfo  Count=%d', [Length(TmpTable.FieldInfoArr)]));
  for i := Low(TmpTable.FieldInfoArr) to High(TmpTable.FieldInfoArr) do
  begin
    s := Format('%.2d Name=%s  Type=%s  Size=%d',
      [i,
        TmpTable.FieldInfoArr[i].FieldName,
        TmpTable.FieldInfoArr[i].TypeName,
        TmpTable.FieldInfoArr[i].FieldSize
      ]);
    ALines.Add(s);
  end;
  Result := True;
end;

procedure TDBReaderSqlite.FillTablesList;
var
  TmpTable, TmpTable2: TSqliteTableInfo;
  TmpRow: TDbRowItem;
  i, ii, iii: Integer;
  RootPageNum: Cardinal;
begin
  FIsMetadataLoaded := True;
  // normalize page num arrays
  for i := 0 to TableList.Count - 1 do
  begin
    TmpTable := TableList.GetItem(i);
    SetLength(TmpTable.PageNumArr, TmpTable.PageNumCount);
  end;

  // read schema
  ReadTable(FSchemaInfo.TableName, MaxInt, nil);

  // fill tables from schema
  for i := 0 to FSchemaInfo.Count - 1 do
  begin
    TmpRow := FSchemaInfo.GetItem(i);
    if VarToStrDef(TmpRow.Values[0], '') <> 'table' then
      Continue;
    RootPageNum := VarToInt(TmpRow.Values[3]); // rootpage
    TmpTable := TableList.GetByPageNum(RootPageNum);
    if not Assigned(TmpTable) then
    begin
      TmpTable := TSqliteTableInfo.Create();
      TableList.Add(TmpTable);
    end;
    TmpTable.TableName := VarToStrDef(TmpRow.Values[1], ''); // name
    TmpTable.RootPageNum := RootPageNum;
    TmpTable.SetSQL(VarToStrDef(TmpRow.Values[4], 'sql'));
  end;

  // find and merge orphan tables to defined tables
  for i := 0 to TableList.Count-1 do
  begin
    TmpTable := TableList.GetItem(i);
    if TmpTable.TableSQL = '' then
      Continue;

    for ii := Low(TmpTable.PageNumArr) to High(TmpTable.PageNumArr) do
    begin
      TmpTable2 := TableList.GetByPageNum(TmpTable.PageNumArr[ii]);
      if Assigned(TmpTable2) and (TmpTable2 <> TmpTable) then
      begin
        TmpTable.RowCount := -1;
        // copy orphan table page nums
        for iii := Low(TmpTable2.PageNumArr) to High(TmpTable2.PageNumArr) do
          TmpTable.AddPageNum(TmpTable2.PageNumArr[iii]);
        SetLength(TmpTable.PageNumArr, TmpTable.PageNumCount); // normalize page num array
        // mark orphan table as deleted
        TmpTable2.RowCount := -2;
      end;
    end;
    // for single-page tables, add RootPageNum to PageNumArr
    if (Length(TmpTable.PageNumArr) = 0) and (TmpTable.RootPageNum <> 0) then
    begin
      TmpTable.AddPageNum(TmpTable.RootPageNum);
      SetLength(TmpTable.PageNumArr, TmpTable.PageNumCount); // normalize page num array
      if TmpTable.RowCount = 0 then
        TmpTable.RowCount := -1;
    end;
  end;
  // delete marked tables
  for i := TableList.Count-1 downto 0 do
  begin
    if TableList.GetItem(i).RowCount = -2 then
      TableList.Delete(i);
  end;

end;

function TDBReaderSqlite.GetTableByIndex(AIndex: Integer): TDbRowsList;
begin
  Result := TableList[AIndex];
end;

function TDBReaderSqlite.GetTablesCount: Integer;
begin
  Result := TableList.Count;
end;

function TDBReaderSqlite.OpenFile(AFileName: string; AStream: TStream): Boolean;
var
  PageBuf: TByteDynArray;
  FileHead: TSqliteFileHeader;
  rdr: TRawDataReader;
  iPagePos: Int64;
  iCurPageID: Cardinal;
begin
  Result := inherited OpenFile(AFileName, AStream);
  if not Result then Exit;
  Result := False;

  // read header
  FFile.ReadBuffer(FileHead, SizeOf(FileHead));
  ReverseBytes(FileHead.PageSize, 2);
  // validate page size
  case FileHead.PageSize of
    $1, $200, $400, $800, $1000, $2000, $4000, $8000: FPageSize := FileHead.PageSize;
  else
    Assert(False, 'Page size incorrect');
  end;
  if FPageSize = 1 then
    FPageSize := 65536;
  PageBuf := [];
  SetLength(PageBuf, FPageSize);
  FPageEnd := FPageSize - FileHead.UnusedSize;

  // scan pages
  iCurPageID := 0;
  iPagePos := 0;
  while iPagePos + FPageSize <= FFile.Size do
  begin
    FFile.Position := iPagePos;
    FFile.Read(PageBuf[0], FPageSize);
    rdr.Init(PageBuf[0], True);

    ReadDataPage(PageBuf, iPagePos, nil, nil);

    Inc(iCurPageID);
    iPagePos := iCurPageID * FPageSize;

    if Assigned(OnPageReaded) then
      OnPageReaded(Self);
  end;

  SetLength(PageBuf, 0);

  FillTablesList();

end;

function TDBReaderSqlite.ReadDataPage(const APageBuf: TByteDynArray; APagePos: Int64;
  ATableInfo: TSqliteTableInfo; AList: TDbRowsList): Boolean;
var
  rdr: TRawDataReader;
  PageHead: TSqlitePageHead;
  RecOffsArr: array of Word;
  i, iRowOffs, iRowSize: Integer;
  CurPageNum, LeftPageNum: Cardinal;
  TmpRow: TDbRowItem;
  TmpRowId: Int64;
  s: string;
begin
(*
A b-tree page is divided into regions in the following order:
* The 100-byte database file header (found on page 1 only)
* The 8 or 12 byte b-tree page header
* The cell pointer array
* Unallocated space
* The cell content area
* The reserved region
*)

  Result := False;
  CurPageNum := (APagePos div FPageSize) + 1;
  rdr.Init(APageBuf[0], True);
  // skip file header
  if APagePos = 0 then
    rdr.SetPosition(SizeOf(TSqliteFileHeader));

  // page header
  PageHead.PageType := rdr.ReadUInt8;
  PageHead.FreeBlockOffs := rdr.ReadUInt16;
  PageHead.RecCount := rdr.ReadUInt16;
  PageHead.ContentOffs := rdr.ReadUInt16;
  PageHead.FragSize := rdr.ReadUInt8;
  PageHead.LastPageNum := 0;
  if PageHead.PageType in [DB3_PAGE_TYPE_INDEX_TREE, DB3_PAGE_TYPE_TABLE_TREE] then   // interior b-tree
    PageHead.LastPageNum := rdr.ReadUInt32;

  TmpRow := nil;
  if IsDebugPages and (not FIsMetadataLoaded) then
  begin
    TmpRow := TDbRowItem.Create(FPagesInfo);
    SetLength(TmpRow.Values, Length(FPagesInfo.FieldsDef));
    FPagesInfo.Add(TmpRow);

    TmpRow.Values[0] := (APagePos div FPageSize) + 1;
    TmpRow.Values[1] := PageHead.PageType;
    TmpRow.Values[2] := APagePos;
    TmpRow.Values[3] := PageHead.FreeBlockOffs;
    TmpRow.Values[4] := PageHead.RecCount;
    TmpRow.Values[5] := PageHead.ContentOffs;
    TmpRow.Values[6] := PageHead.FragSize;
    TmpRow.Values[7] := PageHead.LastPageNum;
    TmpRow.Values[8] := '';
  end;

  if (PageHead.PageType <> DB3_PAGE_TYPE_TABLE_LEAF)
  and (PageHead.PageType <> DB3_PAGE_TYPE_TABLE_TREE) then
    Exit;

  // check page
  if (not FIsMetadataLoaded)
  and ( (PageHead.PageType = DB3_PAGE_TYPE_TABLE_TREE) or (CurPageNum = 1) ) then
  begin
    if Assigned(ATableInfo) and (ATableInfo.RootPageNum <> CurPageNum) then
      Exit;
    // find/add table for root page
    if not Assigned(ATableInfo) then
      ATableInfo := TableList.GetByPageNum(CurPageNum);
    if not Assigned(ATableInfo) then
    begin
      ATableInfo := TSqliteTableInfo.Create();
      ATableInfo.RootPageNum := CurPageNum;
      ATableInfo.TableName := Format('Page_%x', [CurPageNum]);
      ATableInfo.RowCount := -1; // not empty
      TableList.Add(ATableInfo);
    end;

    if (CurPageNum = 1) and (PageHead.PageType = DB3_PAGE_TYPE_TABLE_LEAF) then
    begin
      // store PageID
      ATableInfo.AddPageNum(CurPageNum);
    end;
  end;

  // rec offs list
  RecOffsArr := [];
  SetLength(RecOffsArr, PageHead.RecCount);
  if PageHead.RecCount > 0 then
  begin
    rdr.ReadToBuffer(RecOffsArr[0], PageHead.RecCount * SizeOf(Word)); // big-endian!

    for i := Low(RecOffsArr) to High(RecOffsArr) do
    begin
      iRowOffs := Swap(RecOffsArr[i]);  // swap big-endian bytes
      Assert(iRowOffs < FPageSize);
      rdr.SetPosition(iRowOffs);

      if PageHead.PageType = DB3_PAGE_TYPE_TABLE_LEAF then
      begin
        iRowSize := ReadVarInt(rdr);

        if FIsMetadataLoaded then
          ReadRowData(APageBuf, iRowOffs, iRowSize, ATableInfo, AList);
      end
      else
      if PageHead.PageType = DB3_PAGE_TYPE_TABLE_TREE then
      begin
        // read child page numbers
        LeftPageNum := rdr.ReadUInt32;
        //TmpRowId := ReadVarInt(rdr);
        if not FIsMetadataLoaded then
        begin
          // store PageID
          ATableInfo.AddPageNum(LeftPageNum);
        end;
      end;
    end;
  end;

  // last page
  if (not FIsMetadataLoaded)
  and (PageHead.PageType = DB3_PAGE_TYPE_TABLE_TREE)
  and (PageHead.LastPageNum <> 0) then
  begin
    // store PageID
    ATableInfo.AddPageNum(PageHead.LastPageNum);
    if IsDebugPages then
    begin
      SetLength(ATableInfo.PageNumArr, ATableInfo.PageNumCount);
      s := '';
      for i := Low(ATableInfo.PageNumArr) to High(ATableInfo.PageNumArr) do
        s := s + Format('%x', [ATableInfo.PageNumArr[i]]) + ',';
      TmpRow.Values[8] := s;
    end;
  end;

  Result := True;
end;

function TDBReaderSqlite.ReadRowData(const APageBuf: TByteDynArray; ARowOffs, ARowSize: Integer;
  ATableInfo: TSqliteTableInfo; AList: TDbRowsList): Boolean;
var
  rdr: TRawDataReader;
  TmpRow: TDbRowItem;
  PayloadSize, RowID, HeadStart, HeadSize, SerialType, iVarSize: Int64;
  iCol, iColCount, PayloadPartSize, PayloadMinSize, PayloadBufPos: Integer;
  SerTypeArr: array of Int64;
  PayloadBuf: array of Byte;
  OverPageNum: Cardinal;
  btSerType: Byte;
  v: Variant;
begin
  // == record format:
  // PayloadSize   VarInt
  // RowID         VarInt
  // Payload
  // OverflowPage  UInt32

  // == payload format:
  // HeadSize      VarInt     Includes HeadSize
  // SerialType    VarInt     Field type and size, for each column
  // Body                     Fields content, exept zero-size fields

  rdr.Init(APageBuf[0], True);
  rdr.SetPosition(ARowOffs);
  PayloadSize := ReadVarInt(rdr);
  RowID := ReadVarInt(rdr);
  HeadStart := rdr.GetPosition;
  if PayloadSize > (FPageEnd - 35) then  // > max payload size
  begin
    // payload from multiple pages
    PayloadBufPos := 0;
    PayloadBuf := [];
    SetLength(PayloadBuf, PayloadSize);
    // payload first part size computation
    PayloadMinSize := (((FPageEnd - 12) * 32) div 255) - 23;
    PayloadPartSize := PayloadMinSize + ((PayloadSize - PayloadMinSize) mod (FPageEnd - 4));
    if PayloadPartSize > (FPageEnd - 35) then
      PayloadPartSize := PayloadMinSize;

    rdr.ReadToBuffer(PayloadBuf[PayloadBufPos], PayloadPartSize);
    Inc(PayloadBufPos, PayloadPartSize);
    OverPageNum := rdr.ReadUInt32;
    while OverPageNum > 0 do
    begin
      FFile.Position := (OverPageNum-1) * FPageSize;
      FFile.Read(OverPageNum, SizeOf(OverPageNum));
      // big-endian
      ReverseBytes(OverPageNum, SizeOf(OverPageNum));
      // remaining payload part
      PayloadPartSize := PayloadSize - PayloadBufPos;
      if PayloadPartSize > (FPageEnd - 4) then
        PayloadPartSize := (FPageEnd - 4);
      // read payload part
      FFile.Read(PayloadBuf[PayloadBufPos], PayloadPartSize);
      Inc(PayloadBufPos, PayloadPartSize);
    end;
    if PayloadBufPos < PayloadSize then
      LogInfo('!payload not readed!');

    rdr.Init(PayloadBuf[0], True);
    HeadStart := 0;
  end;
  // Payload Header
  HeadSize := ReadVarInt(rdr);
  iCol := 0;
  SerTypeArr := [];
  SetLength(SerTypeArr, ATableInfo.ColCount);
  while (rdr.GetPosition < (HeadStart + HeadSize)) do
  begin
    // Row can have more columns, than defined in scheme. Unused columns have default values.
    //Assert(iCol < ATableInfo.ColCount, Format('Table %s head read >%d columns!', [ATableInfo.TableName, ATableInfo.ColCount]));
    SerialType := ReadVarInt(rdr);
    if iCol < ATableInfo.ColCount then
    begin
      SerTypeArr[iCol] := SerialType;
      Inc(iCol);
    end;
  end;

  if not Assigned(AList) then
    AList := ATableInfo;
  TmpRow := TDbRowItem.Create(AList);
  AList.Add(TmpRow);
  SetLength(TmpRow.Values, Length(AList.FieldsDef));

  // Payload Body
  iColCount := iCol;
  iCol := 0;
  while (iCol < iColCount) and (rdr.GetPosition <= (HeadStart + PayloadSize)) do
  begin
    Assert(iCol < ATableInfo.ColCount, Format('Table %s body read >%d columns!', [ATableInfo.TableName, ATableInfo.ColCount]));
    SerialType := SerTypeArr[iCol];
    btSerType := Byte(SerialType);
    v := Null;
    case btSerType of
      rfNull:  v := Null;
      rfInt8:  v := rdr.ReadInt8;
      rfInt16: v := rdr.ReadInt16;
      rfInt24: v := rdr.ReadInt24;
      rfInt32: v := rdr.ReadInt32;
      rfInt48: v := rdr.ReadInt48;
      rfInt64: v := rdr.ReadInt64;
      rfFloat: v := rdr.ReadDouble;
      rf0:     v := 0;
      rf1:     v := 1;
    else
      if (SerialType >= rfBlob) and ((SerialType mod 2) = 0) then
      begin
        // BLOB
        iVarSize := (SerialType - rfBlob) div 2;
        Assert(rdr.GetPosition + iVarSize <= (HeadStart + PayloadSize));
        v := rdr.ReadBytes(iVarSize);
      end
      else if (SerialType >= rfText) and ((SerialType mod 2) = 1) then
      begin
        // TEXT
        iVarSize := (SerialType - rfText) div 2;
        Assert(rdr.GetPosition + iVarSize <= (HeadStart + PayloadSize));
        // todo: check encoding
        v := UTF8Decode(rdr.ReadBytes(iVarSize));
      end;
    end;
    //if VarIsNull(v) and ATableInfo.FieldInfoArr[iCol].IsPrimaryKey then
    //  v := RowID;

    TmpRow.Values[iCol] := v;
    Inc(iCol);
  end;

  // primary key
  for iCol := 0 to ATableInfo.ColCount-1 do
  begin
    if ATableInfo.FieldInfoArr[iCol].IsPrimaryKey then
      TmpRow.Values[iCol] := RowID;
  end;

  Result := True;
end;

procedure TDBReaderSqlite.ReadTable(AName: string; ACount: Int64; AList: TDbRowsList);
var
  TmpTable: TSqliteTableInfo;
  TmpRow: TDbRowItem;
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
    AList := TmpTable
  else
  begin
    AList.Clear();
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
      Exit;
    end;
  end;

  PageBuf := [];
  SetLength(PageBuf, FPageSize);
  // read pages
  for i := Low(TmpTable.PageNumArr) to High(TmpTable.PageNumArr) do
  begin
    iPagePos := (TmpTable.PageNumArr[i] - 1) * FPageSize;
    if iPagePos + FPageSize <= FFile.Size then
    begin
      FFile.Position := iPagePos;
      FFile.Read(PageBuf[0], FPageSize);

      ReadDataPage(PageBuf, iPagePos, TmpTable, AList);
    end
    else
      Assert(False, 'Page out of file: ' + IntToStr(TmpTable.PageNumArr[i]));

    if Assigned(OnPageReaded) then
      OnPageReaded(Self);
  end;
end;

function TDBReaderSqlite.ReadVarInt(var rdr: TRawDataReader): Int64;
var
  bt, n: Byte;
begin
  Result := 0;
  bt := $FF;
  n := 0;
  while ((bt and $80) <> 0) do
  begin
    Inc(n);
    bt := rdr.ReadUInt8;
    if n < 9 then
    begin
      Result := Result shl 7;
      Result := Result or (bt and $7F);
    end
    else
    begin
      Result := Result shl 8;
      Result := Result or bt;
      Break;
    end;
  end;

end;

{ TSqliteTableInfo }

procedure TSqliteTableInfo.AddFieldDef(AName, AType: string);
var
  n, nTypeSize: Integer;
  sTypeName: string;
begin
  n := Pos('(', AType);
  nTypeSize := 0;
  if n > 0 then
  begin
    sTypeName := Copy(AType, 1, n-1);
    //nTypeSize := StrToIntDef()
  end
  else
    sTypeName := AType;

  n := Length(FieldInfoArr);

  SetLength(FieldInfoArr, n+1);
  FieldInfoArr[n].FieldName := AName;
  FieldInfoArr[n].TypeName := AType;
  FieldInfoArr[n].IsPrimaryKey := (Pos('PRIMARY KEY', AType) > 0);

  SetLength(FieldsDef, n+1);
  FieldsDef[n].Name := AName;
  FieldsDef[n].TypeName := AType;
  FieldsDef[n].FieldType := SqliteColTypeToDbFieldType(sTypeName);
  FieldsDef[n].Size := 0;

  Inc(ColCount);
end;

procedure TSqliteTableInfo.AddPageNum(ANum: Cardinal);
begin
  // store PageID
  Inc(PageNumCount);
  if PageNumCount >= Length(PageNumArr) then
    SetLength(PageNumArr, Length(PageNumArr) + 32);
  PageNumArr[PageNumCount-1] := ANum;
end;

function TSqliteTableInfo.IsEmpty: Boolean;
begin
  Result := (RowCount = 0);
end;

function TSqliteTableInfo.IsGhost: Boolean;
begin
  Result := (TableSQL = '');
end;

function TSqliteTableInfo.IsSystem: Boolean;
begin
  Result := (TableName = 'sqlite_schema')
         or (TableName = 'sqlite_pages');
end;

procedure TSqliteTableInfo.SetSQL(ASql: string);
var
  s, ss, sField: string;
  i, n: Integer;
begin
  TableSQL := ASql;
  ss := ASql;
  s := ExtractFirstWord(ss);
  if s <> 'CREATE' then Exit;
  s := ExtractFirstWord(ss);
  if s <> 'TABLE' then Exit;

  s := Trim(ExtractFirstWord(ss, '('));
  s := RemoveQuotes(s, '''''');
  s := RemoveQuotes(s, '""');
  s := RemoveQuotes(s, '[]');
  TableName := s;

  // find unmatched ")"
  i := 1;
  n := 0;
  while i < Length(ss) do
  begin
    if Copy(ss, i, 1) = '(' then
      Inc(n)
    else
    if Copy(ss, i, 1) = ')' then
      Dec(n);
    if n < 0 then
      Break;
    Inc(i);
  end;
  ss := Copy(ss, 1, i-1);

  // fields
  while ss <> '' do
  begin
    //sField := Trim(ExtractFirstWord(ss, ','));
    // find unbracketed ','
    i := 1;
    n := 0;
    while i <= Length(ss) do
    begin
      if Copy(ss, i, 1) = '(' then
        Inc(n)
      else
      if Copy(ss, i, 1) = ')' then
        Dec(n)
      else
      if (Copy(ss, i, 1) = ',') and (n = 0) then
        Break;
      Inc(i);
    end;
    sField := Trim(Copy(ss, 1, i-1));
    ss := Copy(ss, i+1, MaxInt);

    s := ExtractFirstWord(sField);  // name
    if (UpperCase(s) = 'FOREIGN') then
      Continue;
    if (UpperCase(s) = 'CONSTRAINT') then
    begin
      if Pos('PRIMARY KEY', UpperCase(sField)) > 0 then
      begin
        // CONSTRAINT pk_name PRIMARY KEY(field_name)
        s := Trim( Copy(sField, Pos('(', sField)+1, MaxInt) );
        s := Trim( Copy(s, 1, Pos(')', s)-1) ); // s = field_name
        s := RemoveQuotes(s, '''''');
        s := RemoveQuotes(s, '""');
        s := RemoveQuotes(s, '[]');
        for i := 0 to Length(FieldInfoArr) - 1 do
        begin
          if AnsiSameText(FieldInfoArr[i].FieldName, s) then
            FieldInfoArr[i].IsPrimaryKey := True;
        end;
      end;
      Continue;
    end;
    s := RemoveQuotes(s, '''''');
    s := RemoveQuotes(s, '""');
    s := RemoveQuotes(s, '[]');
    AddFieldDef(s, Trim(sField));
  end;
end;

{ TSqliteTableInfoList }

function TSqliteTableInfoList.GetByName(AName: string): TSqliteTableInfo;
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

function TSqliteTableInfoList.GetByPageNum(ANum: Cardinal): TSqliteTableInfo;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := GetItem(i);
    if Result.RootPageNum = ANum then
      Exit;
  end;
  Result := nil;
end;

function TSqliteTableInfoList.GetItem(AIndex: Integer): TSqliteTableInfo;
begin
  Result := TSqliteTableInfo(Get(AIndex));
end;

procedure TSqliteTableInfoList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Action = lnDeleted then
    TSqliteTableInfo(Ptr).Free;
end;

procedure TSqliteTableInfoList.SortByName;

  function _DoSortByName(Item1, Item2: Pointer): Integer;
  var
    TmpItem1, TmpItem2: TSqliteTableInfo;
  begin
    TmpItem1 := TSqliteTableInfo(Item1);
    TmpItem2 := TSqliteTableInfo(Item2);
    Result := AnsiCompareStr(TmpItem1.TableName, TmpItem2.TableName);
  end;

begin
  Sort(@_DoSortByName);
end;

end.
