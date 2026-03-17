unit DBReaderInno;

(*
MY SQL InnoDB Database File reader

Author: Sergey Bodrov, 2025 Minsk
License: MIT

https://blog.jcole.us/2013/01/07/the-physical-structure-of-innodb-index-pages/
https://blog.jcole.us/2013/01/10/the-physical-structure-of-records-in-innodb/

https://medium.com/@r844312/mysql-storage-structure-abbd4846e47b
https://programmersought.com/article/203010545066/
https://programmersought.com/article/62621523528/

Index Page structure:
* File header (36 bytes)
* Page header (56 bytes)
* Infimum + Supremum (26 bytes)
* User records
* Free space
* Page directory
* File trailer (8 bytes)

Record structure:
* Variable field lengths (if high bit set ($80), then read also next byte, bit ($40) is blob_ref flag)
* Null bitmap (optional, 1 bit per nullable field)
* Record header (5 bytes)
* Start of record (right after header) - offset from other record point here
* Primary index columns (RowID and others) NOT NULL
* System columns (Transaction ID, Roll Pointer) NOT NULL - if their position last?
* Columns values (only not-null), variable lenghts look up above
*)


interface

uses
  SysUtils, Classes, Variants, DBReaderBase, DB, Types;

type
  TInnoFieldDefRec = record
    Pos: Integer;     // column ordinal number
    MType: Integer;   // MySQL data type MYSQL_TYPE_
    PrType: Integer;  // DATA_TYPE_
    Len: Integer;
    IsVarLen: Boolean;    // length stored in record header
    IsNullable: Boolean;  // can be NULL
    ColKey: Integer;  // column_key  enum('','PRI','UNI','MUL')
    RawOrd: Integer;  // ordinal number in raw data
    Name: AnsiString;
    TypeName: string;
  end;
  TInnoFieldDefRecArr = array of TInnoFieldDefRec;

  TInnoTableInfo = class(TDbRowsList)
  public
    FieldInfoArr: TInnoFieldDefRecArr;
    VarLenCount: Integer;
    FixLenSize: Integer;
    NullBitmapSize: Integer;
    RecCount: Integer;
    IndexID: Int64;  // PageIndexId
    TableID: Int64;  // 6 bytes
    FileName: string;  // if stored in separate file
    IndexName: string;
    IndexType: Integer;
    IsPrimary: Boolean;
    PageIdArr: array of Integer;
    PageIdCount: Integer;
    // contain no rows
    function IsEmpty(): Boolean; override;
    // predefined table
    function IsSystem(): Boolean; override;
    // not defined in metadata
    function IsGhost(): Boolean; override;

    procedure AddFieldDef(AName: string; AType: Word;
      ALength: Integer = 0);

    procedure AddColDef(AName: string; AType: Word;
      ALength: Integer = 0;
      ANotNull: Boolean = False;
      AUnsigned: Boolean = False);
  end;

  TInnoTableInfoList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function GetItem(AIndex: Integer): TInnoTableInfo;
    function GetByName(AName: string): TInnoTableInfo;
    function GetByTableID(ATableID: Integer): TInnoTableInfo;
    function GetByIndexID(AIndexID: Int64): TInnoTableInfo;
    procedure SortByName();
  end;

  TDBReaderInnoDB = class(TDBReader)
  private
    FTableList: TInnoTableInfoList;
    //FFileInfo: TEdbFileInfo;
    FPagesInfo: TInnoTableInfo;

    FPageSize: Integer;
    FIsMetadataLoaded: Boolean;

    // raw page for blob reader
    FBlobRawPage: TByteDynArray;
    FBlobRawPageID: Integer;

    FDebugRowCount: Integer;

    function ReadDataPage(const APageBuf: TByteDynArray; APagePos: Int64;
      ATableInfo: TInnoTableInfo; AList: TDbRowsList): Boolean;

    function ReadDataRecord(const APageBuf: TByteDynArray; ARecOffs, ARecSize: Integer;
      ATableInfo: TInnoTableInfo; AList: TDbRowsList; var ANextRecOffs: Integer): Boolean;

    // define initial system tables structure
    procedure InitSystemTables();
    // fill schema tables from initial tables
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

    property TableList: TInnoTableInfoList read FTableList;
    //property FileInfo: TEdbFileInfo read FFileInfo;
  end;


implementation

type
  TInnoPageHead = record
    // file header (38 bytes)
    Checksum: Cardinal;    // Page space or checksum
    PageOffset: Cardinal;  // Page number
    PagePrev: Cardinal;    // Page number of the last page
    PageNext: Cardinal;    // The page number of the next page
    PageLsn: Int64;        // Page last modified log sequence location
    PageType: Word;        // Type of this page
    PageFileFlushLsn: Int64; // Independent table space is 0
    PageSpaceID: Cardinal; // Which table space belongs to the page
    // page header (56 bytes)
    PageNDirSlots: Word;   // The size of the page directory in “slots”, which are each 16-bit byte offsets.
    PageHeapTop: Word;     // minimum address of the space is not used
    PageNHeap: Word;       // The format of the records in this page, stored in the high bit (0x8000) of the “Number of Heap Records” field.
    PageFree: Word;        // garbage collection points correspond to the offset in the page
    PageGarbage: Word;
    PageLastInsert: Word;  // last inserted position
    PageDirection: Word;   // indicating the insertion direction of the last record
    PageNDirection: Word;  //
    PageNRecs: Word;       // number of non-deleted record in this page
    PageMaxTrxId: Int64;   // max transaction ID
    PageLevel: Word;
    PageIndexId: Int64;    // The ID of the index this page belongs to
    PageBtrSegLeaf: array [0..9] of Byte;
    PageBtrSegTop: array [0..9] of Byte;
    // Infimum + Supremum  (first and last records)
  end;

  TInnoVarDataRec = record
    Offs: Integer;
    Size: Integer;
    Data: AnsiString;
    //IntValue: Integer;  // length for VARCHAR(N)
  end;

  TInnoRowRec = record
    VarData: array of TInnoVarDataRec;
    NullBitmap: TByteDynArray;
    // header  (5 bytes)
    // 1 + 1 bits not used
    DeletedFlag: Boolean; // 1 bit   is deleted
    MinRecFlag: Boolean;  // 1 bit   last record in BTree
    NOwned: Byte;         // 4 bit   parent record ID
    HeapNo: Word;         // 13 bit  offset to heap
    RecordType: Byte;     // 3 bit   (0-normal, 1-BTree branch, 2-infimum, 3-supremum)
    NextRecord: Integer;  // 16 bit  (signed) offset to next record
    // hidden columns
    RowID: AnsiString;    // 6 bytes Row ID, unique
    TrxID: AnsiString;    // 6 bytes Transaction ID
    RollPtr: AnsiString;  // 7 bytes Rollback pointer
  end;

const
  PAGE_TYPE_ALLOCATED          = $00;  // Freshly allocated
  PAGE_TYPE_UNUSED             = $01;  // Unused page
  PAGE_TYPE_UNDO_LOG           = $02;
  PAGE_TYPE_INODE              = $03;  // Index Node
  PAGE_TYPE_IBUF_FREE_LIST     = $04;  // Insert Buffer list
  PAGE_TYPE_IBUF_BITMAP        = $05;  // Insert Buffer bitmap
  PAGE_TYPE_SYS                = $06;  // System page
  PAGE_TYPE_TRX_SYS            = $07;  // Transaction system data
  PAGE_TYPE_FSP_HDR            = $08;  // File space header (first page)
  PAGE_TYPE_XDES               = $09;  // Extent descriptor
  PAGE_TYPE_BLOB               = $0A;  // Uncompressed blob
  PAGE_TYPE_ZBLOB              = $0B;  // Compressed blob (first)
  PAGE_TYPE_ZBLOB2             = $0C;  // Compressed blob (next)
  PAGE_TYPE_UNKNOWN            = $0D;  // Garbage
  PAGE_TYPE_COMPRESSED         = $0E;  // Compressed
  PAGE_TYPE_ENCRYPTED          = $0F;  // Encrypted
  PAGE_TYPE_COMP_ENCR          = $10;  // Compressed + encrypted
  PAGE_TYPE_ENCR_RTREE         = $11;  // Encrypted RTree
  PAGE_TYPE_SDI_BLOB           = $12;  // SDI blob
  PAGE_TYPE_SDI_ZBLOB          = $13;  // SDI compressed blob
  PAGE_TYPE_SDI                = $45BD;  // Tablespace SDI index page
  PAGE_TYPE_RTREE              = $45BE;  // RTree node
  PAGE_TYPE_INDEX              = $45BF;  // BTree node

  // MySQL data types (from column.type enum)
  MYSQL_TYPE_DECIMAL           =  1;
  MYSQL_TYPE_TINY              =  2; // INT8  (BOOL $80 or $81)
  MYSQL_TYPE_SHORT             =  3; // INT16
  MYSQL_TYPE_LONG              =  4; // INT32
  MYSQL_TYPE_FLOAT             =  5; // FLOAT32
  MYSQL_TYPE_DOUBLE            =  6; // FLOAT64
  MYSQL_TYPE_NULL              =  7; // NULL
  MYSQL_TYPE_TIMESTAMP         =  8; //
  MYSQL_TYPE_LONGLONG          =  9; // INT64
  MYSQL_TYPE_INT24             = 10; // INT48
  MYSQL_TYPE_DATE              = 11; // DOUBLE
  MYSQL_TYPE_TIME              = 12; //
  MYSQL_TYPE_DATETIME          = 13; //
  MYSQL_TYPE_YEAR              = 14; //
  MYSQL_TYPE_NEWDATE           = 15; // not used
  MYSQL_TYPE_VARCHAR           = 16; // VARCHAR
  MYSQL_TYPE_BIT               = 17; //
  MYSQL_TYPE_TIMESTAMP2        = 18; //
  MYSQL_TYPE_DATETIME2         = 19; // not used
  MYSQL_TYPE_TIME2             = 20; // not used
  MYSQL_TYPE_NEWDECIMAL        = 21; //
  MYSQL_TYPE_ENUM              = 22; // ENUM INT8
  MYSQL_TYPE_SET               = 23; // INT64
  MYSQL_TYPE_TINY_BLOB         = 24;
  MYSQL_TYPE_MEDIUMBLOB        = 25; // MEDIUM BLOB/TEXT 11 bytes
  MYSQL_TYPE_LONGBLOB          = 26; // LONG BLOB/TEXT  12 bytes
  MYSQL_TYPE_BLOB              = 27; // BLOB/TEXT    10 bytes
  MYSQL_TYPE_VAR_STRING        = 28;
  MYSQL_TYPE_STRING            = 29;
  MYSQL_TYPE_GEOMETRY          = 30;
  MYSQL_TYPE_JSON              = 31; //
  // virtual types
  MYSQL_TYPE_BOOL              = $FC; // INT8 $80 or $81
  MYSQL_TYPE_FIXED             = $FD; // fixed length raw data (for debug)
  MYSQL_TYPE_DEBUG             = $FE; // debug string

  DATA_LEN_TINY    = $FF;
  DATA_LEN_NORMAL  = $FFFF;
  DATA_LEN_MEDIUM  = $FFFFFF;
  DATA_LEN_LONG    = MaxInt; // $FFFFFFFF;

  // todo: remove because not used       size (bytes), 0-VarLen, N-FixLen
  DATA_TYPE_VARCHAR             =  1; // 0
  DATA_TYPE_CHAR                =  2; // N  padded to the right
  DATA_TYPE_FIXBINARY           =  3; // N
  DATA_TYPE_BINARY              =  4; // 0
  DATA_TYPE_BLOB                =  5; // 9-TINYBLOB, 10-BLOB, 11-MEDIUMBLOB, 12-LONGBLOB
  DATA_TYPE_INT                 =  6; // 1-TINYINT, 2-SMALLINT, 3-MEDIUMINT, 4-INT, 8-BIGINT
  DATA_TYPE_SYS_CHILD           =  7; // ? address of the child page in node pointer
  DATA_TYPE_SYS                 =  8; // 8-DATA_ROW_ID, 6-DATA_TRX_ID, 7-DATA_ROLL_PTR
  DATA_TYPE_FLOAT               =  9; // 4
  DATA_TYPE_DOUBLE              = 10; // 8
  DATA_TYPE_DECIMAL             = 11; // 0 stored as ASCII string
  DATA_TYPE_VARMYSQL            = 12; // 0  any charset
  DATA_TYPE_MYSQL               = 13; // N  any charset
  DATA_TYPE_GEOMETRY            = 14; // 0  geometry data types
  DATA_TYPE_DEBUG               = $FE; // N  (virtual) bytes before record start
  DATA_TYPE_MASK                = $FF;

  DATA_TYPE_ROW_ID             = $08;  // (6 bytes) 48-bit integer
  DATA_TYPE_TRX_ID             = $18;  // (6 bytes) transaction ID
  DATA_TYPE_ROLL_PTR           = $28;  // (7 bytes) rollback data pointer
  DATA_TYPE_FTS_DOC_ID         = $38;  //

  DATA_FLAG_NOT_NULL           = $0100; // NOT NULL column
  DATA_FLAG_UNSIGNED           = $0200; // unsigned integer
  DATA_FLAG_BINARY_TYPE        = $0400; // binary character string
  DATA_FLAG_GIS_MBR            = $0800; // GIS MBR column
  DATA_FLAG_LONG_TRUE_VARCHAR  = $1000; // VARCHAR with 2 bytes length, else 1 byte
  DATA_FLAG_VIRTUAL            = $2000; // virtual column
  DATA_FLAG_MULTI_VALUE        = $4000; // multi-value virtual column
  // synthetic types
  DATA_TYPE_UINT               = DATA_TYPE_INT or DATA_FLAG_UNSIGNED;

  INFIMUM_OFFS                 = $63; // offset to infimum

  REC_TYPE_NORMAL              = 0;
  REC_TYPE_BTREE               = 1;
  REC_TYPE_INFIMUM             = 2;
  REC_TYPE_SUPREMUM            = 3;

  COL_KEY_PRI  = 2;    // primary key
  COL_KEY_UNI  = 3;    // unique key
  COL_KEY_MUL  = 4;    // multiple allowed

  INDEX_ID_COLUMNS     = $16;
  INDEX_ID_INDEXES     = $2F;
  INDEX_ID_TABLES      = $4E;
  INDEX_ID_TABLESPACES = $5A;


function InnoColTypeToStr(AVal, ALen: Integer): string;
var
  sLen: string;
begin
  sLen := IntToStr(ALen);
  if ALen = DATA_LEN_NORMAL then
    sLen := '64K'
  else
  if ALen = DATA_LEN_MEDIUM then
    sLen := '16M'
  else
  if ALen = DATA_LEN_LONG then
    sLen := '4GB';

  case (AVal and DATA_TYPE_MASK) of
    DATA_TYPE_VARCHAR:   Result := Format('VARCHAR(%s)', [sLen]);
    DATA_TYPE_CHAR:      Result := Format('CHAR(%s)', [sLen]);
    DATA_TYPE_FIXBINARY: Result := Format('FIXBINARY(%s)', [sLen]);
    DATA_TYPE_BINARY:    Result := 'BINARY';
    DATA_TYPE_BLOB:      Result := Format('BLOB(%s)', [sLen]);
    DATA_TYPE_INT:       Result := Format('INT(%s)', [sLen]);
    DATA_TYPE_SYS_CHILD: Result := 'SYS_CHILD';
    DATA_TYPE_SYS:       Result := Format('SYS(%s)', [sLen]);
    DATA_TYPE_FLOAT:     Result := 'FLOAT';
    DATA_TYPE_DOUBLE:    Result := 'DOUBLE';
    DATA_TYPE_DECIMAL:   Result := 'DECIMAL';
    DATA_TYPE_VARMYSQL:  Result := 'VARMYSQL';
    DATA_TYPE_MYSQL:     Result := Format('MYSQL(%s)', [sLen]);
    DATA_TYPE_GEOMETRY:  Result := 'GEOMETRY';
    DATA_TYPE_DEBUG:     Result := 'DEBUG';
  else
    Result := 'UNKNOWN_' + IntToHex(AVal, 4);
  end;
end;

function InnoColTypeToDbFieldType(AVal: Word): TFieldType;
begin
  case (AVal and DATA_TYPE_MASK) of
    DATA_TYPE_VARCHAR:   Result := ftString;
    DATA_TYPE_CHAR:      Result := ftString;
    DATA_TYPE_FIXBINARY: Result := ftBytes;
    DATA_TYPE_BINARY:    Result := ftBytes;
    DATA_TYPE_BLOB:      Result := ftBlob;
    DATA_TYPE_INT:       Result := ftInteger;
    DATA_TYPE_SYS_CHILD: Result := ftString;
    DATA_TYPE_SYS:       Result := ftString;
    DATA_TYPE_FLOAT:     Result := ftFloat;
    DATA_TYPE_DOUBLE:    Result := ftFloat;
    DATA_TYPE_DECIMAL:   Result := ftString;
    DATA_TYPE_VARMYSQL:  Result := ftString;
    DATA_TYPE_MYSQL:     Result := ftString;
    DATA_TYPE_GEOMETRY:  Result := ftString;
    DATA_TYPE_DEBUG:     Result := ftString;
  else
    Result := ftUnknown;
  end;
end;

function MySqlColTypeToDbFieldType(AVal: Word): TFieldType;
begin
  case (AVal and DATA_TYPE_MASK) of
    MYSQL_TYPE_DECIMAL:   Result := ftString;
    MYSQL_TYPE_TINY,
    MYSQL_TYPE_SHORT,
    MYSQL_TYPE_LONG,
    MYSQL_TYPE_LONGLONG,
    MYSQL_TYPE_INT24,
    MYSQL_TYPE_ENUM,
    MYSQL_TYPE_SET:         Result := ftInteger;

    MYSQL_TYPE_FLOAT,
    MYSQL_TYPE_DOUBLE:      Result := ftFloat;
    MYSQL_TYPE_NULL:        Result := ftUnknown;
    MYSQL_TYPE_TIMESTAMP,
    MYSQL_TYPE_TIMESTAMP2:  Result := ftDateTime;
    MYSQL_TYPE_DATE,
    MYSQL_TYPE_TIME,
    MYSQL_TYPE_DATETIME:    Result := ftDateTime;
    MYSQL_TYPE_VARCHAR:     Result := ftString;
    MYSQL_TYPE_BIT,
    MYSQL_TYPE_BOOL:        Result := ftBoolean;
    MYSQL_TYPE_TINY_BLOB,
    MYSQL_TYPE_MEDIUMBLOB,
    MYSQL_TYPE_LONGBLOB,
    MYSQL_TYPE_BLOB:        Result := ftString; //ftBytes;
    MYSQL_TYPE_VAR_STRING,
    MYSQL_TYPE_STRING,
    MYSQL_TYPE_JSON:        Result := ftString;
    MYSQL_TYPE_DEBUG:       Result := ftString;
    MYSQL_TYPE_FIXED:       Result := ftBytes;
  else
    Result := ftUnknown;
  end;
end;

function ReadPageHeader(const APageBuf: TByteDynArray; var APageHead: TInnoPageHead): Boolean;
var
  rdr: TRawDataReader;
begin
  rdr.Init(APageBuf[0]);
  rdr.IsBigEndian := True;

  with APageHead do
  begin
    // file header (38 bytes)
    Checksum := rdr.ReadUInt32;
    PageOffset := rdr.ReadUInt32;
    PagePrev := rdr.ReadUInt32;
    PageNext := rdr.ReadUInt32;
    PageLsn := rdr.ReadInt64;
    PageType := rdr.ReadUInt16;
    PageFileFlushLsn := rdr.ReadInt64;
    PageSpaceID := rdr.ReadUInt32;
    // page header (56 bytes)
    PageNDirSlots := rdr.ReadUInt16;
    PageHeapTop := rdr.ReadUInt16;
    PageNHeap := rdr.ReadUInt16;
    PageFree := rdr.ReadUInt16;
    PageGarbage := rdr.ReadUInt16;
    PageLastInsert := rdr.ReadUInt16;
    PageDirection := rdr.ReadUInt16;
    PageNDirection := rdr.ReadUInt16;
    PageNRecs := rdr.ReadUInt16;
    PageMaxTrxId := rdr.ReadInt64;
    PageLevel := rdr.ReadUInt16;
    PageIndexId := rdr.ReadInt64;
    rdr.ReadToBuffer(PageBtrSegLeaf, SizeOf(PageBtrSegLeaf));
    rdr.ReadToBuffer(PageBtrSegTop, SizeOf(PageBtrSegTop));
  end;
  Result := True;
end;

function IsNullValue(const ANullBmp: TByteDynArray; AIndex: Integer): Boolean;
var
  NullFlagIndex, NullFlagOffs: Integer;
begin
  NullFlagIndex := Length(ANullBmp) - (AIndex div 8) - 1;
  NullFlagOffs := AIndex mod 8;
  if (NullFlagIndex < Length(ANullBmp)) and (NullFlagIndex >= 0) then
    Result := (ANullBmp[NullFlagIndex] and (Byte(1) shl NullFlagOffs)) <> 0
  else
    Result := False;
end;

{ TDBReaderInnoDB }

procedure TDBReaderInnoDB.AfterConstruction;
begin
  inherited;
  FTableList := TInnoTableInfoList.Create();
  InitSystemTables();
end;

procedure TDBReaderInnoDB.BeforeDestruction;
begin
  FreeAndNil(FTableList);
  inherited;
end;

function TDBReaderInnoDB.FillTableInfoText(ATableName: string; ALines: TStrings): Boolean;
var
  i: Integer;
  TmpTable: TInnoTableInfo;
  s, sFlags: string;
begin
  Result := False;
  TmpTable := FTableList.GetByName(ATableName);
  if not Assigned(TmpTable) then
    Exit;

  ALines.Add(Format('== Table Name=%s  RowCount=%d  IdxName=%s', [TmpTable.TableName, TmpTable.RecCount, TmpTable.IndexName]));
  ALines.Add(Format('IndexID=%x', [TmpTable.IndexID]));
  ALines.Add(Format('TableID=%x', [TmpTable.TableID]));
  ALines.Add(Format('VarLenCount=%d', [TmpTable.VarLenCount]));
  ALines.Add(Format('FixLenSize=%d', [TmpTable.FixLenSize]));
  ALines.Add(Format('NullBitmapSize=%d', [TmpTable.NullBitmapSize]));
  ALines.Add(Format('FileName=%s', [TmpTable.FileName]));
  // PageID array
  s := '';
  for i := Low(TmpTable.PageIdArr) to High(TmpTable.PageIdArr) do
    s := s + Format('%x,', [TmpTable.PageIdArr[i]]);
  //Delete(s, Length(s)-1, 1);
  ALines.Add(Format('PageID[%d]=(%s)', [TmpTable.PageIdCount, s]));

  {ALines.Add(Format('== Fields  Count=%d', [Length(TmpTable.FieldsDef)]));
  for i := Low(TmpTable.FieldsDef) to High(TmpTable.FieldsDef) do
  begin
    s := Format('%.2d Name=%s  Type=%s', [i, TmpTable.FieldsDef[i].Name, TmpTable.FieldsDef[i].TypeName]);
    ALines.Add(s);
  end;  }

  ALines.Add(Format('== FieldInfo  Count=%d', [Length(TmpTable.FieldInfoArr)]));
  for i := Low(TmpTable.FieldInfoArr) to High(TmpTable.FieldInfoArr) do
  begin
    sFlags := '';
    if (TmpTable.FieldInfoArr[i].PrType and DATA_FLAG_UNSIGNED) <> 0 then
      sFlags := sFlags + 'UNSIGNED ';
    //if (TmpTable.FieldInfoArr[i].PrType and DATA_FLAG_NOT_NULL) = 0 then
    if TmpTable.FieldInfoArr[i].IsNullable then
      sFlags := sFlags + 'NULLABLE ';
    if (TmpTable.FieldInfoArr[i].PrType and DATA_FLAG_BINARY_TYPE) <> 0 then
      sFlags := sFlags + 'BINARY ';
    case TmpTable.FieldInfoArr[i].ColKey of
      2: sFlags := sFlags + 'PRI ';
      3: sFlags := sFlags + 'UNI ';
      4: sFlags := sFlags + 'MUL ';
    end;

    s := Format('%.2d Name=%s  Type=%s  Len=%d  Pos=%d  MType=%d  PrType=$%x %s (%s)',
      [i,
        TmpTable.FieldInfoArr[i].Name,
        InnoColTypeToStr(TmpTable.FieldInfoArr[i].PrType, TmpTable.FieldInfoArr[i].Len),
        TmpTable.FieldInfoArr[i].Len,
        TmpTable.FieldInfoArr[i].Pos,
        TmpTable.FieldInfoArr[i].MType,
        TmpTable.FieldInfoArr[i].PrType,
        sFlags,
        TmpTable.FieldInfoArr[i].TypeName
      ]);
    ALines.Add(s);
  end;
end;

procedure TDBReaderInnoDB.FillTablesList;
var
  i, ii: Integer;
  TmpTable, TabTable, IdxTable, ColTable, TspTable: TInnoTableInfo;
  TabRow, IdxRow, ColRow, TspRow: TDbRowItem;
  TabID, IdxID, TspId, ColLen: Int64;
  ColPos, ColType, ColKey, IdxType, TabType, nNullCount, nVarLenCount: Integer;
  ColNotNull, ColUnsigned, IsPrimary: Boolean;
  sName, sEngine, sIdxName: string;
begin
  TabTable := TableList.GetByIndexID(INDEX_ID_TABLES);
  IdxTable := TableList.GetByIndexID(INDEX_ID_INDEXES);
  TspTable := TableList.GetByIndexID(INDEX_ID_TABLESPACES);
  if (not Assigned(TabTable)) or (not Assigned(IdxTable)) then
    Exit;

  // == set TableID from IndexID for readed tables
  for i := 0 to IdxTable.Count - 1 do
  begin
    IdxRow := IdxTable.GetItem(i);
    IdxID := IdxRow.Values[0]; // id
    TabID := IdxRow.Values[3]; // table_id
    sIdxName := IdxRow.Values[4]; // name
    IdxType := IdxRow.Values[5]; // type
    IsPrimary := (IdxType = 1); // type = PRIMARY

    // find table info by IndexID
    if IdxID = 0 then
      Continue;
    TmpTable := TableList.GetByIndexID(IdxID);
    if Assigned(TmpTable) then
    begin
      TmpTable.TableID := TabID;
      TmpTable.IndexName := sIdxName;
      TmpTable.IndexType := IdxType;
      TmpTable.IsPrimary := (IdxType = 1); // type = PRIMARY
    end;
  end;

  // == set readed tables info, create non-readed tables
  for i := 0 to TabTable.Count - 1 do
  begin
    TabRow := TabTable.GetItem(i);

    TabID := TabRow.Values[0];
    sName := TabRow.Values[4]; // name
    TabType := VarToInt(TabRow.Values[5]); // type enum('BASE TABLE','VIEW','SYSTEM VIEW')
    sEngine := TabRow.Values[6];

    if (TabID = 0) or (TabType <> 1) {or (sEngine <> 'InnoDB')} then
      Continue;

    for ii := 0 to TableList.Count - 1 do
    begin
      TmpTable := TableList.GetItem(ii);
      if TmpTable.TableID <> TabID then
        Continue;
      if (TmpTable.IsPrimary) and (sEngine = 'InnoDB') then
        TmpTable.TableName := sName
      else
      if (TmpTable.IsPrimary) then
        TmpTable.TableName := sEngine + ':' + sName
      else
        TmpTable.TableName := sName + ':' + TmpTable.IndexName;
    end;

    // create primary table info if not exists
    TmpTable := TableList.GetByTableID(TabID);
    if not Assigned(TmpTable) then
    begin
      TmpTable := TInnoTableInfo.Create();
      TmpTable.TableID := TabID;
      TmpTable.TableName := sName;
      TmpTable.IsPrimary := True;
      TableList.Add(TmpTable);
    end;
  end;


  ColTable := TableList.GetByIndexID(INDEX_ID_COLUMNS);
  if not Assigned(ColTable) then
    Exit;

  for i := 0 to TableList.Count - 1 do
  begin
    TmpTable := TableList.GetItem(i);
    // -- pages list
    SetLength(TmpTable.PageIdArr, TmpTable.PageIdCount);

    // == columns definitions
    if TmpTable.TableID <> 0 then
    begin
      // skip predefined tables
      if (TmpTable.IsPrimary) and (Length(TmpTable.FieldInfoArr) <> 0) then
        Continue;

      // primary tables has PriKeys+TrxID+Ptr columns first

      // -- primary fields
      for ii := 0 to ColTable.Count - 1 do
      begin
        ColRow := ColTable.GetItem(ii);

        TabID := VarToInt64(ColRow.Values[3]);
        sName := ColRow.Values[4];
        ColPos := VarToInt(ColRow.Values[5]);
        ColType := VarToInt(ColRow.Values[6]);
        ColLen := VarToInt64(ColRow.Values[10]); // char_length
        ColKey := VarToInt(ColRow.Values[28]); // column_key  enum('','PRI','UNI','MUL')
        //ColNotNull := False;
        //if not VarIsNull(ColRow.Values[7]) then
        //  ColNotNull := not ColRow.Values[7];
        ColUnsigned := False;
        if not VarIsNull(ColRow.Values[9]) then
          ColUnsigned := ColRow.Values[9];
        sIdxName := VarToStrDef(ColRow.Values[29], ''); // type name

        // sys columns always primary
        if (ColType = MYSQL_TYPE_INT24) and (ColLen = 6) and (sName = 'DB_TRX_ID') then
          ColKey := COL_KEY_PRI;
        if (ColType = MYSQL_TYPE_LONGLONG) and (ColLen = 7) and (sName = 'DB_ROLL_PTR') then
          ColKey := COL_KEY_PRI;
        //  ColKey := 1;
        if ColLen > MaxInt then
          ColLen := MaxInt;

        if ColType = MYSQL_TYPE_TIMESTAMP2 then
          ColLen := VarToInt(ColRow.Values[13]); // datetime_precision

        if (TabID <> TmpTable.TableID) or (ColKey <> COL_KEY_PRI) then
          Continue;

        ColNotNull := True;

        TmpTable.AddColDef(sName, ColType, ColLen, ColNotNull, ColUnsigned);
        TmpTable.FieldInfoArr[High(TmpTable.FieldInfoArr)].Pos := ColPos;
        TmpTable.FieldInfoArr[High(TmpTable.FieldInfoArr)].ColKey := ColKey;
        TmpTable.FieldInfoArr[High(TmpTable.FieldInfoArr)].TypeName := sIdxName;
      end;

      // -- normal fields
      for ii := 0 to ColTable.Count - 1 do
      begin
        ColRow := ColTable.GetItem(ii);

        TabID := VarToInt64(ColRow.Values[3]);
        sName := ColRow.Values[4];
        ColPos := VarToInt(ColRow.Values[5]);
        ColType := VarToInt(ColRow.Values[6]);
        //ColLen := VarToInt(ColRow.Values[11]);  // numeric_length
        ColLen := VarToInt64(ColRow.Values[10]); // char_length
        ColKey := VarToInt(ColRow.Values[28]); // column_key
        ColNotNull := False;
        if not VarIsNull(ColRow.Values[7]) then  // nullable
          ColNotNull := not ColRow.Values[7];
        ColUnsigned := False;
        if not VarIsNull(ColRow.Values[9]) then
          ColUnsigned := ColRow.Values[9];

        if (TmpTable.TableID = 4) and (TabID = 4) then  // !!!
          beep;
        if (TabID <> TmpTable.TableID) or (ColKey = COL_KEY_PRI) then
          Continue;
        // skip sys columns
        if (ColType = MYSQL_TYPE_INT24) and (ColLen = 6) and (sName = 'DB_TRX_ID') then
          Continue;
        if (ColType = MYSQL_TYPE_LONGLONG) and (ColLen = 7) and (sName = 'DB_ROLL_PTR') then
          Continue;

        if ColKey = COL_KEY_PRI then   // keys not nullable
          ColNotNull := True;
        if ColLen > MaxInt then
          ColLen := MaxInt;

        if ColType = MYSQL_TYPE_TIMESTAMP2 then
          ColLen := VarToInt(ColRow.Values[13]); // datetime_precision

        TmpTable.AddColDef(sName, ColType, ColLen, ColNotNull, ColUnsigned);
        TmpTable.FieldInfoArr[High(TmpTable.FieldInfoArr)].Pos := ColPos;
        TmpTable.FieldInfoArr[High(TmpTable.FieldInfoArr)].ColKey := ColKey;
        TmpTable.FieldInfoArr[High(TmpTable.FieldInfoArr)].TypeName := VarToStrDef(ColRow.Values[29], '');
      end;
    end;

    // -- Null bitmap size and VarLen count
    nNullCount := 0;
    nVarLenCount := 0;
    for ii := 0 to Length(TmpTable.FieldInfoArr) - 1 do
    begin
      if TmpTable.FieldInfoArr[ii].IsNullable then
        Inc(nNullCount);

      if TmpTable.FieldInfoArr[ii].IsVarLen then
        Inc(nVarLenCount);
    end;
    if TmpTable.NullBitmapSize = 0 then
      TmpTable.NullBitmapSize := ((nNullCount + 7) div 8); // bytes count
    if TmpTable.VarLenCount = 0 then
      TmpTable.VarLenCount := nVarLenCount;

    // debug column
    TmpTable.AddColDef('DEBUG', MYSQL_TYPE_DEBUG, 5 + TmpTable.NullBitmapSize + (TmpTable.VarLenCount*2));
  end;

  // == tables in separate files (tablespaces)
  // indexes.table_id [3]
  // indexes.type [5] = 1
  // indexes.tablespace_id [15]
  // tablespaces.name [3] = file name
  if Assigned(TspTable) then
  begin
    for i := 0 to TspTable.Count - 1 do
    begin
      TspRow := TspTable.GetItem(i);
      TspId := TspRow.Values[0];
      sName := TspRow.Values[3];
      // find primary indexes with tablespace_id
      for ii := 0 to IdxTable.Count - 1 do
      begin
        IdxRow := IdxTable.GetItem(ii);
        IdxType := IdxRow.Values[5]; // type
        if (IdxRow.Values[15] <> TspId) or (IdxType <> 1) then
          Continue;

        IdxID := IdxRow.Values[0]; // id
        TabID := IdxRow.Values[3]; // table_id
        TmpTable := TableList.GetByTableID(TabID);
        if Assigned(TmpTable) then
        begin
          TmpTable.FileName := sName;
          if TmpTable.IndexID = 0 then
            TmpTable.IndexID := IdxID;
        end;
      end;
    end;
  end;

  FIsMetadataLoaded := True;
end;

function TDBReaderInnoDB.GetTableByIndex(AIndex: Integer): TDbRowsList;
begin
  Result := TableList.GetItem(AIndex);
end;

function TDBReaderInnoDB.GetTablesCount: Integer;
begin
  Result := TableList.Count;
end;

procedure TDBReaderInnoDB.InitSystemTables;
var
  TabInfo: TInnoTableInfo;
begin

  //if IsDebugPages then
  begin
    TabInfo := TInnoTableInfo.Create();
    TableList.Add(TabInfo);
    FPagesInfo := TabInfo;
    with TabInfo do
    begin
      TableName := 'SYS_PagesInfo';
      //IsSystem := True;
      AddColDef('ID', MYSQL_TYPE_LONGLONG, 8);
      AddColDef('TypeName', MYSQL_TYPE_VARCHAR, DATA_LEN_NORMAL);

      AddColDef('FileOffset', MYSQL_TYPE_LONGLONG, 8, True, True);
      AddColDef('Checksum', MYSQL_TYPE_LONG, 4, True, True);
      AddColDef('Offset', MYSQL_TYPE_LONG, 4, True, True);
      AddColDef('PagePrev', MYSQL_TYPE_LONG, 4, True, True);
      AddColDef('PageNext', MYSQL_TYPE_LONG, 4, True, True);
      AddColDef('PageLsn', MYSQL_TYPE_LONGLONG, 8, True, True);
      AddColDef('PageType', MYSQL_TYPE_SHORT, 2, True, True);
      AddColDef('PageFileFlushLsn', MYSQL_TYPE_LONGLONG, 8, True, True);
      AddColDef('PageSpaceID', MYSQL_TYPE_LONG, 4, True, True);

      AddColDef('PageNDirSlots', MYSQL_TYPE_SHORT, 2, True, True);
      AddColDef('PageHeapTop', MYSQL_TYPE_SHORT, 2, True, True);
      AddColDef('PageNHeap', MYSQL_TYPE_SHORT, 2, True, True);
      AddColDef('PageFree', MYSQL_TYPE_SHORT, 2, True, True);
      AddColDef('PageGarbage', MYSQL_TYPE_SHORT, 2, True, True);
      AddColDef('PageLastInsert', MYSQL_TYPE_SHORT, 2, True);
      AddColDef('PageDirection', MYSQL_TYPE_SHORT, 2, True);
      AddColDef('PageNDirection', MYSQL_TYPE_SHORT, 2, True);
      AddColDef('PageNRecs', MYSQL_TYPE_SHORT, 2, True);
      AddColDef('PageMaxTrxId', MYSQL_TYPE_LONGLONG, 8, True, True);
      AddColDef('PageLevel', MYSQL_TYPE_SHORT, 2, True);
      AddColDef('PageIndexId', MYSQL_TYPE_LONGLONG, 8, True, True);
      //AddColDef('PageBtrSegLeaf', MYSQL_TYPE_FIXED, 10);
      //AddColDef('PageBtrSegTop', MYSQL_TYPE_FIXED, 10);
    end;
  end;

  {TabInfo := TInnoTableInfo.Create();
  TableList.Add(TabInfo);
  with TabInfo do
  begin
    TableName := 'SYS_Inode';
    IndexID := -4294901761; // $FFFFFFFF0000FFFF
    //IsSystem := True;
    AddColDef('DATA', MYSQL_TYPE_FIXED, 41);
    AddColDef('DEBUG', MYSQL_TYPE_DEBUG, 15);
  end; }

  // == mysql.ibd metadata

  TabInfo := TInnoTableInfo.Create();
  TableList.Add(TabInfo);
  with TabInfo do
  begin
    TableName := 'SYS_Tablespaces';
    IndexID := INDEX_ID_TABLESPACES;
    NullBitmapSize := 1;
    AddColDef('ID', MYSQL_TYPE_LONGLONG, 8, True, True); // BIGINT UNSIGNED NOT NULL AUTO_INCREMENT
    AddColDef('DB_TRX_ID', MYSQL_TYPE_INT24, 6, True); // INT24(6) NOT NULL
    AddColDef('DB_ROLL_PTR', MYSQL_TYPE_LONGLONG, 7, True); // BIGINT(7) NOT NULL
    AddColDef('name', MYSQL_TYPE_VARCHAR, 268*3, True);  // VARCHAR(268) NOT NULL
    AddColDef('options', MYSQL_TYPE_VARCHAR, DATA_LEN_MEDIUM); // MEDIUMTEXT
    AddColDef('se_private_data', MYSQL_TYPE_VARCHAR, DATA_LEN_MEDIUM); // MEDIUMTEXT
    AddColDef('comment', MYSQL_TYPE_VARCHAR, 2048*3, True);  // VARCHAR(2048) NOT NULL
    AddColDef('engine', MYSQL_TYPE_VARCHAR, 64*3, True);  // VARCHAR(64) NOT NULL
    AddColDef('engine_attribute', MYSQL_TYPE_JSON, DATA_LEN_MEDIUM); // JSON
    AddColDef('DEBUG', MYSQL_TYPE_DEBUG, 20);
  end;

  TabInfo := TInnoTableInfo.Create();
  TableList.Add(TabInfo);
  with TabInfo do
  begin
    TableName := 'SYS_indexes';
    IndexID := INDEX_ID_INDEXES;
    NullBitmapSize := 1;
    AddColDef('ID', MYSQL_TYPE_LONGLONG, 8, True, True); // BIGINT UNSIGNED NOT NULL AUTO_INCREMENT
    AddColDef('DB_TRX_ID', MYSQL_TYPE_INT24, 6, True); // INT24(6) NOT NULL
    AddColDef('DB_ROLL_PTR', MYSQL_TYPE_LONGLONG, 7, True); // BIGINT(7) NOT NULL
    AddColDef('table_id', MYSQL_TYPE_LONGLONG, 0, True, True); // BIGINT UNSIGNED NOT NULL
    AddColDef('name', MYSQL_TYPE_VARCHAR, 64, True);  // VARCHAR(64) NOT NULL
    AddColDef('type', MYSQL_TYPE_ENUM, 0, True);  // enum('PRIMARY','UNIQUE','MULTIPLE','FULLTEXT','SPATIAL') NOT NULL
    AddColDef('algorithm', MYSQL_TYPE_ENUM, 0, True);  // enum('SE_SPECIFIC','BTREE','RTREE','HASH','FULLTEXT') NOT NULL
    AddColDef('is_algorithm_explicit', MYSQL_TYPE_BOOL, 0, True); // BOOL NOT NULL
    AddColDef('is_visible', MYSQL_TYPE_BOOL, 0, True); // BOOL NOT NULL
    AddColDef('is_generated', MYSQL_TYPE_BOOL, 0, True); // BOOL NOT NULL
    AddColDef('hidden', MYSQL_TYPE_BOOL, 0, True); // BOOL NOT NULL
    AddColDef('ordinal_position', MYSQL_TYPE_LONG, 0, True, True); // INT UNSIGNED NOT NULL
    AddColDef('comment', MYSQL_TYPE_VARCHAR, 2048, True);  // VARCHAR(2048) NOT NULL
    AddColDef('options', MYSQL_TYPE_VARCHAR, DATA_LEN_MEDIUM); // MEDIUMTEXT
    AddColDef('se_private_data', MYSQL_TYPE_VARCHAR, DATA_LEN_MEDIUM); // MEDIUMTEXT
    AddColDef('tablespace_id', MYSQL_TYPE_LONGLONG, 0, False, True); // BIGINT UNSIGNED
    AddColDef('engine', MYSQL_TYPE_VARCHAR, 64, True);  // VARCHAR(64) NOT NULL
    AddColDef('engine_attribute', MYSQL_TYPE_JSON, DATA_LEN_MEDIUM); // JSON
    AddColDef('secondary_engine_attribute', MYSQL_TYPE_JSON, DATA_LEN_MEDIUM); // JSON
    AddColDef('DEBUG', MYSQL_TYPE_DEBUG, 20);
  end;

  TabInfo := TInnoTableInfo.Create();
  TableList.Add(TabInfo);
  with TabInfo do
  begin
    TableName := 'SYS_columns';
    IndexID := INDEX_ID_COLUMNS; // $16
    NullBitmapSize := 3;
    AddColDef('ID', MYSQL_TYPE_LONGLONG, 8, True, True); // BIGINT UNSIGNED NOT NULL
    AddColDef('DB_TRX_ID', MYSQL_TYPE_INT24, 6, True); // INT24(6) NOT NULL
    AddColDef('DB_ROLL_PTR', MYSQL_TYPE_LONGLONG, 7, True); // BIGINT(7) NOT NULL
    AddColDef('table_id', MYSQL_TYPE_LONGLONG, 0, True, True); // BIGINT UNSIGNED NOT NULL
    AddColDef('name', MYSQL_TYPE_VARCHAR, 64, True); // VARCHAR(64) NOT NULL COLLATE
    AddColDef('ordinal_position', MYSQL_TYPE_LONG, 0, True); // INT UNSIGNED NOT NULL
    AddColDef('type', MYSQL_TYPE_ENUM, 0, True); // ENUM NOT NULL
    AddColDef('is_nullable', MYSQL_TYPE_BOOL, 0, True); // BOOL NOT NULL
    AddColDef('is_zerofill', MYSQL_TYPE_BOOL); // BOOL
    AddColDef('is_unsigned', MYSQL_TYPE_BOOL); // BOOL
    AddColDef('char_length', MYSQL_TYPE_LONG, 0, False, True); // INT UNSIGNED
    AddColDef('numeric_precision', MYSQL_TYPE_LONG, 0, False, True); // INT UNSIGNED
    AddColDef('numeric_scale', MYSQL_TYPE_LONG, 0, False, True); // INT UNSIGNED
    AddColDef('datetime_precision', MYSQL_TYPE_LONG, 0, False, True); // INT UNSIGNED
    AddColDef('collation_id', MYSQL_TYPE_LONGLONG, 0, False, True); // BIGINT UNSIGNED
    AddColDef('has_no_default', MYSQL_TYPE_BOOL);       // BOOL
    AddColDef('default_value', MYSQL_TYPE_BLOB, DATA_LEN_NORMAL);        // BLOB
    AddColDef('default_value_utf8', MYSQL_TYPE_VARCHAR, DATA_LEN_NORMAL);   // TEXT
    AddColDef('default_option', MYSQL_TYPE_VARCHAR, DATA_LEN_NORMAL);       // BLOB
    AddColDef('update_option', MYSQL_TYPE_VARCHAR, 32);   // VARCHAR(32)
    AddColDef('is_autoincrement', MYSQL_TYPE_BOOL); // BOOL
    AddColDef('is_virtual', MYSQL_TYPE_BOOL); // BOOL
    AddColDef('generation_expression', MYSQL_TYPE_VARCHAR, DATA_LEN_LONG); // LONGBLOB
    AddColDef('generation_expression_utf8', MYSQL_TYPE_VARCHAR, DATA_LEN_LONG); // LONGTEXT
    AddColDef('comment', MYSQL_TYPE_VARCHAR, 2048, True);  // VARCHAR(2048) NOT NULL
    AddColDef('hidden', MYSQL_TYPE_ENUM, 0, True);  // ENUM NOT NULL
    AddColDef('options', MYSQL_TYPE_VARCHAR, DATA_LEN_MEDIUM); // MEDIUMTEXT
    AddColDef('se_private_data', MYSQL_TYPE_VARCHAR, DATA_LEN_MEDIUM); // MEDIUMTEXT
    AddColDef('column_key', MYSQL_TYPE_ENUM, 0, True); // ENUM NOT NULL
    AddColDef('column_type_utf8', MYSQL_TYPE_VARCHAR, DATA_LEN_MEDIUM, True); // MEDIUMTEXT NOT NULL
    AddColDef('srs_id', MYSQL_TYPE_LONG, 0, False, True); // INT UNSIGNED DEFAULT NULL
    AddColDef('is_explicit_collation', MYSQL_TYPE_BOOL); // BOOL
    AddColDef('engine_attribute', MYSQL_TYPE_JSON, DATA_LEN_MEDIUM); // JSON MEDIUM
    AddColDef('secondary_engine_attribute', MYSQL_TYPE_JSON, DATA_LEN_MEDIUM); // JSON MEDIUM
    AddColDef('DEBUG', MYSQL_TYPE_DEBUG, 20);
  end;

  TabInfo := TInnoTableInfo.Create();
  TableList.Add(TabInfo);
  with TabInfo do
  begin
    TableName := 'SYS_tables';
    IndexID := INDEX_ID_TABLES; // $4E
    NullBitmapSize := 4;
    AddColDef('ID', MYSQL_TYPE_LONGLONG, 8, True, True); // BIGINT UNSIGNED NOT NULL
    AddColDef('DB_TRX_ID', MYSQL_TYPE_INT24, 6, True); // INT24(6) NOT NULL
    AddColDef('DB_ROLL_PTR', MYSQL_TYPE_LONGLONG, 7, True); // BIGINT(7) NOT NULL
    AddColDef('schema_id', MYSQL_TYPE_LONGLONG, 0, True, True); // BIGINT UNSIGNED NOT NULL
    AddColDef('name', MYSQL_TYPE_VARCHAR, 64, True); // VARCHAR(64) NOT NULL
    AddColDef('type', MYSQL_TYPE_ENUM, 0, True);  // enum('BASE TABLE','VIEW','SYSTEM VIEW') NOT NULL
    AddColDef('engine', MYSQL_TYPE_VARCHAR, 64, True);  // VARCHAR(64) NOT NULL
    AddColDef('mysql_version_id', MYSQL_TYPE_LONG, 0, True, True); // INT UNSIGNED NOT NULL
    AddColDef('row_format', MYSQL_TYPE_ENUM);  // enum('Fixed','Dynamic','Compressed','Redundant','Compact','Paged')
    AddColDef('collation_id', MYSQL_TYPE_LONGLONG, 0, False, True);  // BIGINT UNSIGNED
    AddColDef('comment', MYSQL_TYPE_VARCHAR, 2048, True);  // VARCHAR(2048) NOT NULL
    AddColDef('hidden', MYSQL_TYPE_ENUM, 0, True);  // enum('Visible','System','SE','DDL') NOT NULL
    AddColDef('options', MYSQL_TYPE_VARCHAR, DATA_LEN_MEDIUM); // MEDIUMTEXT
    AddColDef('se_private_data', MYSQL_TYPE_VARCHAR, DATA_LEN_MEDIUM); // MEDIUMTEXT
    AddColDef('se_private_id', MYSQL_TYPE_LONGLONG, 0, False, True); // BIGINT UNSIGNED
    AddColDef('tablespace_id', MYSQL_TYPE_LONGLONG, 0, False, True); // BIGINT UNSIGNED
    AddColDef('partition_type', MYSQL_TYPE_ENUM);  // enum('HASH','KEY_51','KEY_55','LINEAR_HASH','LINEAR_KEY_51','LINEAR_KEY_55','RANGE','LIST','RANGE_COLUMNS','LIST_COLUMNS','AUTO','AUTO_LINEAR')
    AddColDef('partition_expression', MYSQL_TYPE_VARCHAR, 2048);  // VARCHAR(2048)
    AddColDef('partition_expression_utf8', MYSQL_TYPE_VARCHAR, 2048);  // VARCHAR(2048)
    AddColDef('default_partitioning', MYSQL_TYPE_ENUM);  // ENUM
    AddColDef('subpartition_type', MYSQL_TYPE_ENUM);  // ENUM
    AddColDef('subpartition_expression', MYSQL_TYPE_VARCHAR, 2048);  // VARCHAR(2048)
    AddColDef('subpartition_expression_utf8', MYSQL_TYPE_VARCHAR, 2048);  // VARCHAR(2048)
    AddColDef('default_subpartitioning', MYSQL_TYPE_ENUM);  // ENUM
    AddColDef('created', MYSQL_TYPE_TIMESTAMP, 0, True);  // TIMESTAMP NOT NULL
    AddColDef('last_altered', MYSQL_TYPE_TIMESTAMP, 0, True);  // TIMESTAMP NOT NULL
    AddColDef('view_definition', MYSQL_TYPE_VARCHAR, DATA_LEN_LONG); // LONGBLOB
    AddColDef('view_definition_utf8', MYSQL_TYPE_VARCHAR, DATA_LEN_LONG); // LONGTEXT
    AddColDef('view_check_option', MYSQL_TYPE_ENUM);  // ENUM
    AddColDef('view_is_updatable', MYSQL_TYPE_ENUM);  // ENUM
    AddColDef('view_algorithm', MYSQL_TYPE_ENUM);  // ENUM
    AddColDef('view_security_type', MYSQL_TYPE_ENUM);  // ENUM
    AddColDef('view_definer', MYSQL_TYPE_VARCHAR, 288);  // VARCHAR(288)
    AddColDef('view_client_collation_id', MYSQL_TYPE_LONGLONG, 0, False, True);  // BIGINT UNSIGNED
    AddColDef('view_connection_collation_id', MYSQL_TYPE_LONGLONG, 0, False, True);  // BIGINT UNSIGNED
    AddColDef('view_column_names', MYSQL_TYPE_VARCHAR, DATA_LEN_LONG); // LONGTEXT
    AddColDef('last_checked_for_upgrade_version_id', MYSQL_TYPE_LONG, 0, True, True); // INT UNSIGNED NOT NULL
    AddColDef('engine_attribute', MYSQL_TYPE_JSON, DATA_LEN_MEDIUM); // JSON
    AddColDef('secondary_engine_attribute', MYSQL_TYPE_JSON, DATA_LEN_MEDIUM); // JSON
    AddColDef('DEBUG', MYSQL_TYPE_DEBUG, 20);
  end;

end;

function TDBReaderInnoDB.OpenFile(AFileName: string; AStream: TStream): Boolean;
var
  PageBuf: TByteDynArray;
  //rdr: TRawDataReader;
  iPagePos: Int64;
  iCurPageID: Cardinal;
  PageHead: TInnoPageHead;
begin
  Result := inherited OpenFile(AFileName, AStream);
  if not Result then Exit;

  FIsMetadataLoaded := False;
  FBlobRawPageID := 0;
  FDebugRowCount := 5;
  //TableList.Clear();

  // read file info
  FPageSize := $4000; // 16K
  SetLength(PageBuf, FPageSize);
  FFIle.Read(PageBuf[0], FPageSize);

  ReadPageHeader(PageBuf, PageHead);
  // todo: validate file

  // scan pages
  iCurPageID := 0;
  iPagePos := 0;
  while iPagePos + FPageSize <= FFile.Size do
  begin
    FFile.Position := iPagePos;
    FFile.Read(PageBuf[0], FPageSize);
    ReadDataPage(PageBuf, iPagePos, nil, nil);

    Inc(iCurPageID);
    iPagePos := iCurPageID * FPageSize;

    if Assigned(OnPageReaded) then
      OnPageReaded(Self);
  end;

  SetLength(PageBuf, 0);

  FillTablesList();

end;

function TDBReaderInnoDB.ReadDataPage(const APageBuf: TByteDynArray; APagePos: Int64;
  ATableInfo: TInnoTableInfo; AList: TDbRowsList): Boolean;
var
  //rdr: TRawDataReader;
  iCurPageID: Cardinal;
  PageHead: TInnoPageHead;
  //TableInfo: TEdbTableInfo;
  s, sPageType: string;
  TmpRow: TDbRowItem;
  TableInfo: TInnoTableInfo;
  iRecPos, iNextRecOffs, nRecs: Integer;
begin
  Result := False;
  iCurPageID := APagePos div FPageSize;
  //FFile.Position := APagePos;
  //FFile.Read(APageBuf[0], FPageSize);

  ReadPageHeader(APageBuf, PageHead);

  if IsDebugPages and (not FIsMetadataLoaded) then
  begin
    case PageHead.PageType of
      PAGE_TYPE_ALLOCATED: sPageType := 'ALLOCATED';
      PAGE_TYPE_UNUSED: sPageType := 'UNUSED';
      PAGE_TYPE_UNDO_LOG: sPageType := 'UNDO_LOG';
      PAGE_TYPE_INODE: sPageType := 'INODE';
      PAGE_TYPE_IBUF_FREE_LIST: sPageType := 'IBUF_FREE_LIST';
      PAGE_TYPE_IBUF_BITMAP: sPageType := 'IBUF_BITMAP';
      PAGE_TYPE_SYS: sPageType := 'SYS';
      PAGE_TYPE_TRX_SYS: sPageType := 'TRX_SYS';
      PAGE_TYPE_FSP_HDR: sPageType := 'FSP HDR';
      PAGE_TYPE_XDES: sPageType := 'XDES';
      PAGE_TYPE_BLOB: sPageType := 'BLOB';
      PAGE_TYPE_ZBLOB: sPageType := 'ZBLOB';
      PAGE_TYPE_ZBLOB2: sPageType := 'ZBLOB2';
      PAGE_TYPE_UNKNOWN: sPageType := 'UNKNOWN';
      PAGE_TYPE_SDI: sPageType := 'SDI';
      PAGE_TYPE_RTREE: sPageType := 'RTREE';
      PAGE_TYPE_INDEX: sPageType := 'INDEX';
    else
      sPageType := 'Unknown_$' + IntToHex(PageHead.PageType, 4);
    end;

    if False then
    begin
      s := Format('Page_%x (num=$%x pos=%x) Type=%s Direction=%d'
         + '    NDirSlots=%d NHeap=%d NDirection=%d NRecs=%d Level=%d'
         + '    HeapTop=%d Free=%d Garbage=%d'
         + '    Checksum=%x',
        [iCurPageID, PageHead.PageOffset, APagePos, sPageType, PageHead.PageDirection,
         PageHead.PageNDirSlots, PageHead.PageNHeap, PageHead.PageNDirection, PageHead.PageNRecs, PageHead.PageLevel,
         PageHead.PageHeapTop, PageHead.PageFree, PageHead.PageGarbage,
         PageHead.Checksum]);

      LogInfo(s);
    end;

    TmpRow := TDbRowItem.Create(FPagesInfo);
    SetLength(TmpRow.Values, Length(FPagesInfo.FieldsDef));
    FPagesInfo.Add(TmpRow);

    TmpRow.Values[0] := iCurPageID;
    TmpRow.Values[1] := sPageType;
    TmpRow.Values[2] := APagePos;

    TmpRow.Values[3] := PageHead.Checksum;
    TmpRow.Values[4] := PageHead.PageOffset;
    TmpRow.Values[5] := PageHead.PagePrev;
    TmpRow.Values[6] := PageHead.PageNext;
    TmpRow.Values[7] := PageHead.PageLsn;
    TmpRow.Values[8] := PageHead.PageType;
    TmpRow.Values[9] := PageHead.PageFileFlushLsn;
    TmpRow.Values[10] := PageHead.PageSpaceID;

    TmpRow.Values[11] := PageHead.PageNDirSlots;
    TmpRow.Values[12] := PageHead.PageHeapTop;
    TmpRow.Values[13] := PageHead.PageNHeap;
    TmpRow.Values[14] := PageHead.PageFree;
    TmpRow.Values[15] := PageHead.PageGarbage;
    TmpRow.Values[16] := PageHead.PageLastInsert;
    TmpRow.Values[17] := PageHead.PageDirection;
    TmpRow.Values[18] := PageHead.PageNDirection;
    TmpRow.Values[19] := PageHead.PageNRecs;
    TmpRow.Values[20] := PageHead.PageMaxTrxId;
    TmpRow.Values[21] := PageHead.PageLevel;
    TmpRow.Values[22] := PageHead.PageIndexId;
  end;

  if (PageHead.PageType = PAGE_TYPE_INDEX)
  and (PageHead.PageNRecs <> 0)              // has non-deleted records
  and ((PageHead.PageNHeap and $8000) <> 0)  // COMPRESSED records type
  then
  begin
    //rdr.Init(APageBuf[0]);
    //rdr.IsBigEndian := True;
    //rdr.SetPosition(0);
    //TmpRow.RawData := rdr.ReadBytes(FPageSize);
    //BufToFile(TmpRow.RawData[$50], $2000, 'inf_sup.data');

    TableInfo := ATableInfo;
    if not Assigned(TableInfo) then
      TableInfo := TableList.GetByIndexID(PageHead.PageIndexId)
    else if TableInfo.IndexID <> PageHead.PageIndexId then
      Exit;   

    if not FIsMetadataLoaded then
    begin
      if not Assigned(TableInfo) then
      begin
        TableInfo := TInnoTableInfo.Create();
        TableInfo.IndexID := PageHead.PageIndexId;
        TableInfo.TableName := Format('Table_%x', [TableInfo.IndexID]);
        TableList.Add(TableInfo);
      end;

      TableInfo.RecCount := TableInfo.RecCount + PageHead.PageNRecs;
      // store PageID
      Inc(TableInfo.PageIdCount);
      if TableInfo.PageIdCount >= Length(TableInfo.PageIdArr) then
        SetLength(TableInfo.PageIdArr, Length(TableInfo.PageIdArr) + 32);
      TableInfo.PageIdArr[TableInfo.PageIdCount-1] := iCurPageID;
    end;


    if Assigned(TableInfo) and (Length(TableInfo.FieldInfoArr) <> 0) then
    begin
      iRecPos := INFIMUM_OFFS; // 5e + 5
      nRecs := PageHead.PageNRecs;
      ReadDataRecord(APageBuf, iRecPos, 13, TableInfo, AList, iNextRecOffs);     // infimum
      while (iNextRecOffs <> 0) and (nRecs > 0) do
      begin
        Dec(nRecs);
        //Inc(iRecPos, iNextRecOffs);
        iRecPos := iRecPos + iNextRecOffs;
        Assert(iRecPos < FPageSize);
        if iRecPos > FPageSize then
          Break;
        ReadDataRecord(APageBuf, iRecPos, iNextRecOffs, TableInfo, AList, iNextRecOffs);
      end;
      //ReadDataRecord(APageBuf, INFIMUM_OFFS+13, 13, nil, nil);  // supremum
      Result := True;
    end;
  end;
end;

function TDBReaderInnoDB.ReadDataRecord(const APageBuf: TByteDynArray; ARecOffs, ARecSize: Integer;
  ATableInfo: TInnoTableInfo; AList: TDbRowsList; var ANextRecOffs: Integer): Boolean;
var
  rdr: TRawDataReader;
  rec: TInnoRowRec;
  bt: Byte;
  i, ii, VarSizePos, VarPos, nVarLen, nColID: Integer;
  w: Word;
  s, ss, sInfo: string;
  NullBmp: TByteDynArray;
  TmpRow: TDbRowItem;
  sData: AnsiString;
  mType, PrType, mSize: Integer;
  i64: Int64;
  dt: TDateTime;

  function _GetValLen(AIndex, ASize: Integer): Integer;
  var
    PrevPos: Integer;
  begin
    Result := 0;
    PrevPos := rdr.GetPosition();
    if (AIndex < ATableInfo.VarLenCount) then
    begin
      rdr.SetPosition(VarSizePos);
      Result := rdr.ReadUInt8;
      Dec(VarSizePos);

      if (Result >= $80) and (ASize > 255) then
      begin
        Result := (Result and $3F) shl 8;
        rdr.SetPosition(VarSizePos);
        Result := Result or rdr.ReadUInt8;
        Dec(VarSizePos);
      end;
    end;
    rdr.SetPosition(PrevPos);
  end;

begin
  rdr.Init(APageBuf[0]);
  rdr.IsBigEndian := True;
  rdr.SetPosition(ARecOffs-5);

  // read flags
  bt := rdr.ReadUInt8;
  rec.DeletedFlag := (bt and $20) <> 0;
  rec.MinRecFlag := (bt and $10) <> 0;
  rec.NOwned := (bt and $0F);
  w := rdr.ReadUInt16;
  rec.HeapNo := w shr 3;
  rec.RecordType := w and $7;
  //w := rdr.ReadUInt16;
  rec.NextRecord := rdr.ReadInt16;;
  rec.RowID := rdr.ReadBytes(8);

  ANextRecOffs := rec.NextRecord;
  Result := True;

  // !!!
  if False then
  begin
    case rec.RecordType of
      REC_TYPE_NORMAL:   s := 'NORM';
      REC_TYPE_BTREE:    s := 'BTRE';
      REC_TYPE_INFIMUM:  s := 'INFI';
      REC_TYPE_SUPREMUM: s := 'SUPR';
    else
      s := 'UNKN_' + IntToStr(rec.RecordType);
    end;

    if rec.DeletedFlag then
      s := s + ' DEL';
    if rec.MinRecFlag then
      s := s + ' MIN';

    sInfo := Format('Rec %s NOwned=%d HeapNo=%d NextRec=%x RowID=%s',
     [s, rec.NOwned, rec.HeapNo, rec.NextRecord, rec.RowID]);
    LogInfo(sInfo);
  end;

  if not Assigned(ATableInfo) then Exit;

  // var data
  if (rec.RecordType = REC_TYPE_NORMAL) then
  begin
    //if IsDebugRows then
    if False then  // !!!
    begin
      Dec(FDebugRowCount);
      if FDebugRowCount < 0 then
        IsDebugRows := False;
      ss := '';
      ss := ss + sInfo + sLineBreak;
      ss := ss + 'ARecOffs=' + IntToStr(ARecOffs) + sLineBreak;

      rdr.SetPosition(ARecOffs-15);
      s := rdr.ReadBytes(10);
      ss := ss + 'var_part=' + BufferToHex(s[1], Length(s)) + sLineBreak;
      ss := ss + 'var_part=' + DataAsStr(s[1], Length(s)) + sLineBreak;
      rdr.SetPosition(ARecOffs-5);
      s := rdr.ReadBytes(5);
      ss := ss + 'header=' + BufferToHex(s[1], Length(s)) + sLineBreak;
      ss := ss + 'NextRecord=' + IntToStr(rec.NextRecord) + sLineBreak;
      rdr.SetPosition(ARecOffs);
      //if rec.NextRecord > 5 then
      begin
        s := rdr.ReadBytes(64);
        ss := ss + 'data=' + BufferToHex(s[1], Length(s)) + sLineBreak;
        ss := ss + 'data=' + DataAsStr(s[1], Length(s)) + sLineBreak;

        // next data
        {s := rdr.ReadBytes(64);
        ss := ss + 'next_data=' + BufferToHex(s[1], Length(s)) + sLineBreak;
        ss := ss + 'next_data=' + DataAsStr(s[1], Length(s)) + sLineBreak;  }
        ss := ss + 'next_offs=' + Format('rec_%x.data', [ARecOffs + rec.NextRecord])
      end;


      BufToFile(ss[1], Length(ss), Format('rec_%x.data', [ARecOffs]));
    end;

    if ATableInfo.IndexID = INDEX_ID_COLUMNS then  // !!!
      Beep;

    // Null bitmap
    //iBitmapLen := ((LastFixColID + 7) div 8); // bytes count
    SetLength(NullBmp, ATableInfo.NullBitmapSize);
    if ATableInfo.NullBitmapSize > 0 then
    begin
      rdr.SetPosition(ARecOffs - 5 - ATableInfo.NullBitmapSize);
      rdr.ReadToBuffer(NullBmp[0], ATableInfo.NullBitmapSize);
    end;

    VarSizePos := ARecOffs-6; // byte before header
    Dec(VarSizePos, ATableInfo.NullBitmapSize);

    if not Assigned(AList) then
      AList := ATableInfo;

    TmpRow := TDbRowItem.Create(AList);
    AList.Add(TmpRow);
    SetLength(TmpRow.Values, Length(ATableInfo.FieldsDef));
    if IsDebugRows then
      SetLength(TmpRow.RawOffs, Length(ATableInfo.FieldsDef));

    // read fixed size data
    rdr.SetPosition(ARecOffs);
    nVarLen := 0;
    nColID := -1;
    for i := 0 to Length(ATableInfo.FieldsDef) - 1 do
    begin
      // !!! debug
      if (ATableInfo.IndexID = INDEX_ID_COLUMNS) and (rec.NextRecord = $B4) and (i = 17) then
        Beep;

      mType := ATableInfo.FieldInfoArr[i].MType;
      PrType := ATableInfo.FieldInfoArr[i].PrType;
      mSize := ATableInfo.FieldInfoArr[i].Len;

      if IsDebugRows then
        TmpRow.RawOffs[i] := rdr.GetPosition() - ARecOffs;

      // debug column - [VarLen][NullBitmap][Header]
      if PrType = DATA_TYPE_DEBUG then
      begin
        VarPos := rdr.GetPosition();
        if mSize < 5 then
          mSize := 5;
        rdr.SetPosition(ARecOffs - mSize);
        sData := rdr.ReadBytes(mSize);
        sInfo := BufferToHex(sData[mSize-5+1], 5); // header
        if (ATableInfo.NullBitmapSize > 0) and (mSize >= (ATableInfo.NullBitmapSize + 5)) then
          sInfo := BufferToHex(sData[mSize-5-ATableInfo.NullBitmapSize+1], ATableInfo.NullBitmapSize) + sInfo;
        if mSize > ATableInfo.NullBitmapSize + 5 then
          sInfo := BufferToHex(sData[1], mSize-ATableInfo.NullBitmapSize-5) + sInfo;
        TmpRow.Values[i] := sInfo;
        rdr.SetPosition(VarPos);
        Continue;
      end;

      // only nullable columns in bitmap
      if ((ATableInfo.FieldInfoArr[i].PrType and DATA_FLAG_NOT_NULL) = 0) then
      begin
        Inc(nColID);
        if IsNullValue(NullBmp, nColID) {and (not ATableInfo.FieldInfoArr[i].IsPrimaryKey)} then
        begin
          TmpRow.Values[i] := Null;
          Continue;
        end;
      end;

      if ATableInfo.FieldInfoArr[i].IsVarLen then  // VarLen
      begin
        //mSize := rec.VarData[nVarLen].Size;
        mSize := _GetValLen(nVarLen, mSize);
        sData := '';
        if (mSize > 0) then
          sData := rdr.ReadBytes(mSize);
        TmpRow.Values[i] := sData;
        Inc(nVarLen);
      end
      else
      begin
        // fixed size
        if (ATableInfo.FieldInfoArr[i].PrType and DATA_FLAG_BINARY_TYPE) > 0 then
        begin
          // fixed with VarLen
          //mSize := rec.VarData[nVarLen].Size;
          //rec.VarData[nVarLen].Offs := rdr.GetPosition;
          mSize := _GetValLen(nVarLen, mSize);
          Inc(nVarLen);
        end;

        sData := rdr.ReadBytes(mSize);
        case PrType and DATA_TYPE_MASK of
          DATA_TYPE_INT:
          begin
            i64 := 0;
            for ii := 1 to mSize do
            begin
              i64 := i64 shl 8;
              i64 := i64 or Ord(sData[ii]);
            end;
            
            if (mType in [MYSQL_TYPE_TIMESTAMP, MYSQL_TYPE_TIMESTAMP2]) then // unix timestamp
            begin
              dt := i64 / SecsPerDay + UnixDateDelta;
              TmpRow.Values[i] := dt;
            end
            else
            if (mType in [MYSQL_TYPE_BOOL, MYSQL_TYPE_TINY]) and (i64 = 128) then
              TmpRow.Values[i] := False
            else
            if (mType in [MYSQL_TYPE_BOOL, MYSQL_TYPE_TINY]) and (i64 = 129) then
              TmpRow.Values[i] := True
            else
              TmpRow.Values[i] := i64;
          end;
        else
          TmpRow.Values[i] := sData;
        end;
      end;
    end;
    if IsDebugRows then
    begin
      VarPos := rdr.GetPosition();
      rdr.SetPosition(ARecOffs);
      TmpRow.RawData := rdr.ReadBytes(VarPos - ARecOffs);
    end;
  end;
end;

procedure TDBReaderInnoDB.ReadTable(AName: string; ACount: Int64; AList: TDbRowsList);
var
  TmpTable: TInnoTableInfo;
  i: Integer;
  PageBuf: TByteDynArray;
  iPagePos: Int64;
  iCurPageID: Cardinal;
  TmpRow: TDbRowItem;
  TmpFile: TStream;
  TmpFilename: string;
begin
  TmpTable := TableList.GetByName(AName);

  if not Assigned(TmpTable) then
  begin
    //Assert(False, 'Table not found: ' + AName);
    LogInfo('!Table not found: ' + AName);
    Exit;
  end;
  if not Assigned(AList) then
    AList := TmpTable;
  AList.Clear();  // ???
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
  TmpFile := FFile;
  if (TmpTable.FileName <> '') and (TmpTable.FileName <> 'mysql') then
  begin
    TmpFilename := IncludeTrailingPathDelimiter(ExtractFilePath(Self.FileName)) + TmpTable.FileName + '.ibd';
    if FileExists(TmpFilename) then
      TmpFile := TFileStream.Create(TmpFilename, fmOpenRead + fmShareDenyNone);
  end;

  SetLength(PageBuf, FPageSize);
  if Length(TmpTable.PageIdArr) = 0 then
  begin
    // scan pages
    iCurPageID := 0;
    iPagePos := 0;
    while iPagePos + FPageSize <= TmpFile.Size do
    begin
      TmpFile.Position := iPagePos;
      TmpFile.Read(PageBuf[0], FPageSize);
      ReadDataPage(PageBuf, iPagePos, TmpTable, AList);

      Inc(iCurPageID);
      iPagePos := iCurPageID * FPageSize;

      if Assigned(OnPageReaded) then
        OnPageReaded(Self);
    end;
  end
  else
  begin
    // read pages
    for i := Low(TmpTable.PageIdArr) to High(TmpTable.PageIdArr) do
    begin
      iPagePos := TmpTable.PageIdArr[i] * FPageSize;
      if iPagePos + FPageSize <= TmpFile.Size then
      begin
        TmpFile.Position := iPagePos;
        TmpFile.Read(PageBuf[0], FPageSize);

        ReadDataPage(PageBuf, iPagePos, TmpTable, AList);
      end
      else
        Assert(False, 'Page out of file: ' + IntToStr(TmpTable.PageIdArr[i]));

      if Assigned(OnPageReaded) then
        OnPageReaded(Self);
    end;
  end;

  if TmpFile <> FFile then
    TmpFile.Free();
  
end;

{ TEdbTableInfo }

procedure TInnoTableInfo.AddColDef(AName: string; AType: Word; ALength: Integer;
  ANotNull: Boolean; AUnsigned: Boolean);
var
  n, PrLen: Integer;
  PrType: Word;
begin
  if (AType = MYSQL_TYPE_LONGLONG) and (ALength > 8) then
    ALength := 8;

  if (AType = MYSQL_TYPE_LONG) and (ALength > 4) then
    ALength := 4;

  PrLen := ALength;
  case AType of
    MYSQL_TYPE_TINY:     PrLen := 1;
    MYSQL_TYPE_SHORT:    PrLen := 2;
    MYSQL_TYPE_LONG:     PrLen := 4;
    MYSQL_TYPE_INT24:    PrLen := 6;
    MYSQL_TYPE_LONGLONG: PrLen := 8;
    MYSQL_TYPE_VARCHAR:  PrLen := 255;
    MYSQL_TYPE_BOOL:     PrLen := 1;
    MYSQL_TYPE_ENUM:     PrLen := 1;
    MYSQL_TYPE_SET:      PrLen := 8;
    MYSQL_TYPE_TIMESTAMP,
    MYSQL_TYPE_TIMESTAMP2: PrLen := 4;
    //COL_FLOAT: ALength := 8;
    MYSQL_TYPE_DOUBLE:   PrLen := 8;
  end;

  if (ALength > 0) and ((ALength < PrLen) or (AType = MYSQL_TYPE_VARCHAR)) then
    PrLen := ALength;

  if (AType = MYSQL_TYPE_TIMESTAMP2) and (ALength > 4) then
    PrLen := ALength;

  PrType := 0;
  case AType of
    MYSQL_TYPE_TINY,
    MYSQL_TYPE_SHORT,
    MYSQL_TYPE_LONG,
    MYSQL_TYPE_LONGLONG,
    MYSQL_TYPE_INT24,
    MYSQL_TYPE_ENUM,
    MYSQL_TYPE_SET,
    MYSQL_TYPE_BOOL: PrType := DATA_TYPE_INT;
    MYSQL_TYPE_VARCHAR: PrType := DATA_TYPE_VARCHAR;
    MYSQL_TYPE_MEDIUMBLOB,
    MYSQL_TYPE_LONGBLOB,
    MYSQL_TYPE_BLOB,
    MYSQL_TYPE_JSON: PrType := DATA_TYPE_VARCHAR;
    MYSQL_TYPE_TIMESTAMP,
    MYSQL_TYPE_TIMESTAMP2: PrType := DATA_TYPE_INT;
    //COL_FLOAT: ALength := 8;
    //COL_DOUBLE: ALength := 8;
    MYSQL_TYPE_DEBUG: PrType := DATA_TYPE_DEBUG;
  end;

  if ANotNull then
    PrType := PrType or DATA_FLAG_NOT_NULL;
  if AUnsigned then
    PrType := PrType or DATA_FLAG_UNSIGNED;

  AddFieldDef(AName, PrType, PrLen);
  n := Length(FieldInfoArr)-1;
  FieldInfoArr[n].MType := AType;
  FieldsDef[n].FieldType := MySqlColTypeToDbFieldType(AType);
end;

procedure TInnoTableInfo.AddFieldDef(AName: string; AType: Word; ALength: Integer);
var
  n: Integer;
  MType: Word;
begin
  MType := AType and $00FF;
  if ALength = 0 then
  begin
    case MType of
      DATA_TYPE_INT: ALength := 4;
      DATA_TYPE_FLOAT: ALength := 8;
      DATA_TYPE_DOUBLE: ALength := 8;
    end;
  end;

  if ALength = 0 then
    Inc(VarLenCount)
  else if MType in [DATA_TYPE_VARCHAR, DATA_TYPE_BINARY] then
    Inc(VarLenCount)
  else if (MType <> DATA_TYPE_DEBUG) then
    Inc(FixLenSize, ALength);
  if (AType and DATA_FLAG_BINARY_TYPE) <> 0 then
    Inc(VarLenCount);


  SetLength(FieldInfoArr, Length(FieldInfoArr)+1);
  n := Length(FieldsDef);
  SetLength(FieldsDef, Length(FieldsDef)+1);

  FieldInfoArr[n].RawOrd := n;
  FieldInfoArr[n].Name := AName;
  //FieldInfoArr[n].MType := MType;
  FieldInfoArr[n].PrType := AType;
  FieldInfoArr[n].Len := ALength;
  FieldInfoArr[n].IsVarLen := (MType in [DATA_TYPE_VARCHAR, DATA_TYPE_BINARY]) or (ALength = 0);
  FieldInfoArr[n].IsNullable := ((AType and DATA_FLAG_NOT_NULL) = 0) and (MType <> DATA_TYPE_DEBUG);

  FieldsDef[n].Name := AName;
  FieldsDef[n].TypeName := Format('%s(%d)', [InnoColTypeToStr(MType, ALength), ALength]);
  FieldsDef[n].FieldType := InnoColTypeToDbFieldType(MType);
  FieldsDef[n].Size := ALength;
  FieldsDef[n].RawOffset := 0;
end;

function TInnoTableInfo.IsEmpty: Boolean;
begin
  Result := (RecCount = 0) and (Count = 0);
end;

function TInnoTableInfo.IsGhost: Boolean;
begin
  Result := (Length(FieldInfoArr) = 0);
end;

function TInnoTableInfo.IsSystem: Boolean;
begin
  Result := not IsPrimary;
end;

{ TInnoTableInfoList }

function TInnoTableInfoList.GetByIndexID(AIndexID: Int64): TInnoTableInfo;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := GetItem(i);
    if Result.IndexID = AIndexID then
      Exit;
  end;
  Result := nil;
end;

function TInnoTableInfoList.GetByTableID(ATableID: Integer): TInnoTableInfo;
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

function TInnoTableInfoList.GetByName(AName: string): TInnoTableInfo;
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

function TInnoTableInfoList.GetItem(AIndex: Integer): TInnoTableInfo;
begin
  Result := TInnoTableInfo(Get(AIndex));
end;

procedure TInnoTableInfoList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Action = lnDeleted then
    TInnoTableInfo(Ptr).Free;
end;

procedure TInnoTableInfoList.SortByName;
begin

end;

end.
