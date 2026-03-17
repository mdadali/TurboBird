unit DBReaderMdf;

(*
MS SQL Master Data File (.mdf) reader

Author: Sergey Bodrov, 2024 Minsk
License: MIT

https://learn.microsoft.com/en-us/sql/relational-databases/pages-and-extents-architecture-guide?view=sql-server-ver15
https://anatoliyon.wordpress.com/2017/10/03/data-file-structure-and-pages/
https://anatoliyon.wordpress.com/2017/12/17/data-file-page-types-gam-sgam-pfs-and-iam/
https://techcommunity.microsoft.com/t5/sql-server-support-blog/sql-server-extents-pfs-gam-sgam-and-iam-and-related-corruptions/ba-p/1606011
https://techcommunity.microsoft.com/t5/sql-server-support-blog/sql-server-iam-page/ba-p/1637065
http://improve.dk/category/SQL%20Server%20-%20Internals/
https://infostart.ru/1c/articles/709159/
*)

interface

uses
  SysUtils, Classes, Variants, DBReaderBase, DB;

const
  MDF_PAGE_SIZE = $2000;

type
  TMdfPageBuffer = array[0..MDF_PAGE_SIZE-1] of Byte;

  TMdfFieldDefRec = record
    ID: LongWord;
    Number: Word;
    ColID: LongWord;
    Name: string;
    XType: Byte;
    UType: LongWord;
    Length: Word;
    Prec: Byte;
    Scale: Byte;
    CollationID: LongWord;
    Status: LongWord;
  end;
  TMdfFieldDefRecArr = array of TMdfFieldDefRec;

  TMdfTableList = class;

  TMdfTable = class(TDbRowsList)
  public
    ObjectID: LongWord;   // for system tables
    AllocID: Int64;       // for all tables
    FieldDefArr: TMdfFieldDefRecArr;
    RowCount: Integer;
    TableType: AnsiChar;  // S or U
    //PKFieldName: string;  // primary key field
    PageIdArr: array of LongWord;
    PageIdCount: Integer;

    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;
    // contain no rows
    function IsEmpty(): Boolean; override;
    // predefined table
    function IsSystem(): Boolean; override;
    // not defined in metadata
    function IsGhost(): Boolean; override;

    procedure AddFieldDef(AName: string; AType: Byte;
      ALength: Integer = 0;
      APrec: Byte = 0;
      AScale: Byte = 0;
      AColID: Integer = -1);
  end;

  TMdfTableList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function GetItem(AIndex: Integer): TMdfTable;
    function GetByName(AName: string): TMdfTable;
    function GetByID(AObjID: LongWord): TMdfTable;
    function GetByAllocID(AAllocID: Int64): TMdfTable;
    procedure SortByName();
  end;



  TDBReaderMdf = class(TDBReader)
  private
    FTableList: TMdfTableList;
    // current page for ReadNextXXX methods
    FPageBuf: TMdfPageBuffer;
    FPagePos: Integer;  // offset inside page
    // current page for ReadBlobDate method
    FBlobPageBuf: TMdfPageBuffer;
    FBlobPagePos: Int64;
    // current table
    FCurTable: TMdfTable;

    FTextPageIdArr: array of LongWord;
    FTextPagePosArr: array of Int64;
    FTextPageCount: Integer;

    FIsMetadataLoaded: Boolean;

    function ReadNextByte(): Byte;
    function ReadNextWord(): Word;
    function ReadNextDWord(): LongWord;
    function ReadNextQWord(): Int64;
    function ReadNextDate(ASize: Byte): TDateTime;
    function ReadNextDateTime(): TDateTime;
    function ReadNextFloat(): Real;
    function ReadNextCurrency(): Currency;
    function ReadNextChar(ACount: Word): AnsiString;
    function ReadNextBinary(ASize: Word): AnsiString;
    function ReadNextNumeric(ASize, APrec, AScale: Byte): string;

    procedure ReadNextDataRec(AMinLen, ARecId: Integer; AList: TDbRowsList = nil);

    function ReadByte(const APageBuf: TMdfPageBuffer; AOffset: Integer): Byte;
    function ReadWord(const APageBuf: TMdfPageBuffer; AOffset: Integer): Word;
    function ReadLongWord(const APageBuf: TMdfPageBuffer; AOffset: Integer): LongWord;
    function ReadBlobAddr(ABlobAddr: AnsiString): AnsiString;
    function ReadBlobData(APageID: LongWord; AFileID, ASlotID: Word): AnsiString;

    function ReadDataPage(const APageBuf: TMdfPageBuffer; APagePos: Int64;
      ATable: TMdfTable = nil; AList: TDbRowsList = nil): Boolean;
    // seek and read page by PID/FID
    function FindPage(APageID: LongWord; AFileID: Word; var APageBuf: TMdfPageBuffer): Boolean;

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

    property TableList: TMdfTableList read FTableList;
  end;

implementation

type
  TMdfBootPageHeaderRec = packed record
    Version: Word;
    CreateVer: Word;
    Unknown4: array [4..31] of Byte;
    Status: LongInt;
    NextId: LongInt;
    Unknown40: array [40..47] of Byte;
    DBName: array [0..127] of WideChar;
    Unknown304: array [304..307] of Byte;
    DBID: Word;
    Unknown310: array [310..311] of Byte;
    MaxDbTimestamp: Int64;
    Unknown320: array [320..383] of Byte;
    CollationID: LongWord;
    Unknown389: array [389..511] of Byte;
    IndexPageID: LongWord;
    IndexFileID: Word;
    Unknown518: array [518..1440] of Byte;
  end;

  TMdfPageHeaderRec = packed record
    HeaderVer: Byte;
    PageType: Byte;             // MDF_PAGE_TYPE_
    TypeFlagBits: Byte;
    Level: Byte;
    FlagBits: Word;
    IndexID: Word;
    PrevPageID: LongWord;
    PrevFileID: Word;
    PMinLen: Word;
    NextPageID: LongWord;
    NextFileID: Word;
    SlotCnt: Word;
    ObjectID: LongWord;
    FreeCnt: Word;
    FreeData: Word;
    PageID: LongWord;
    FileID: Word;
    ReservedCnt: Word;
    LSN1: LongWord;
    LSN2: LongWord;
    LSN3: Word;
    XActReserved: Word;
    XDesIDPart2: LongWord;
    XDesIDPart1: Word;
    GhostRecCnt: Word;
    Checksum: LongWord;
    Unknown64: array [64..95] of Byte;
  end;

  TMdfRowOffsetArray = array of Word;

  TMdfRowRec = record
    Attrs: Byte;
    RowOffs: Word;
    RawDataLen: Integer;    // fixed part size (offset to ColCount)
    VarDataOffs: Word;      // offset to variable data
    NullBitmapPos: Integer; // from start of block
    NullBitmapLen: Integer;
    ColCount: Integer;      // total column count (includes var columns)
    VarCount: Integer;      // variable length columns count

    NullBitmap: array of Byte;
  end;

  TMdfBlobLinkRec = record
    Size: LongWord;
    PageID: LongWord;
    FileID: Word;
    SlotID: Word;
  end;

const
  MDF_PAGE_TYPE_DATA      = 1;
  MDF_PAGE_TYPE_INDEX     = 2;
  MDF_PAGE_TYPE_TEXT_MIX  = 3;
  MDF_PAGE_TYPE_TEXT_TREE = 4;
  MDF_PAGE_TYPE_SORT      = 7;
  MDF_PAGE_TYPE_GAM       = 8;   // Global Allocation Map
  MDF_PAGE_TYPE_SGAM      = 9;   // Shared Global Allocation Map
  MDF_PAGE_TYPE_IAM       = 10;  // Index Allocation Map
  MDF_PAGE_TYPE_PFS       = 11;  // Page Free Space
  MDF_PAGE_TYPE_BOOT      = 13;
  MDF_PAGE_TYPE_FILE_HEAD = 15;
  MDF_PAGE_TYPE_DIFF_MAP  = 16;  // Differential Changed Map
  MDF_PAGE_TYPE_ML_MAP    = 17;  // Bulk Changed Map

  // Record types
  MDF_REC_TYPE_MASK        = $07;  // mask for attribute flags
  MDF_REC_TYPE_PRIMARY     = 0;
  MDF_REC_TYPE_FORWARDED   = 1;
  MDF_REC_TYPE_FORWARDING  = 2;
  MDF_REC_TYPE_INDEX       = 3;
  MDF_REC_TYPE_BLOB_FRAG   = 4;
  MDF_REC_TYPE_GHOST_IDX   = 5;
  MDF_REC_TYPE_GHOST_DATA  = 6;
  MDF_REC_TYPE_GHOST_VER   = 7;

  // Record attribute flags
  MDF_REC_ATTR_NULL_BITMAP = $10;  // has NULL flags bitmap
  MDF_REC_ATTR_VAR_COLUMNS = $20;  // has variable langth columns
  MDF_REC_ATTR_VER_TAG     = $40;  // has versioning tag

  MDF_BLOB_TYPE_SMALL_ROOT = 0;  // inline data
  MDF_BLOB_TYPE_LARGE_ROOT = 1;
  MDF_BLOB_TYPE_INTERNAL   = 2;  // INTERNAL, pointers to DATA
  MDF_BLOB_TYPE_DATA       = 3;  // DATA, part of blob data
  MDF_BLOB_TYPE_ROOT_YUKON = 5;  // LARGE_ROOT_YUKON, pointers to INTERNAL

  // System tables
  MDF_SYS_RSCOLS          = 3;
  MDF_SYS_ROWSETS         = 5;   // RowsetID for objects
  MDF_SYS_ALLOCUNITS      = 7;   // AUID for RowsetID
  MDF_SYS_FILES1          = 8;
  MDF_SYS_PRIORITIES      = 17;
  MDF_SYS_FGFRAG          = 19;
  MDF_SYS_PHFG            = 23;
  MDF_SYS_PRUFILES        = 24;
  MDF_SYS_FTINDS          = 25;
  MDF_SYS_OWNERS          = 27;
  MDF_SYS_SCHOBJS         = 34;  // schema objects (tables, views, triggers, etc..)
  MDF_SYS_COLPARS         = 41;  // column parameters
  MDF_SYS_IDXSTATS        = 54;
  MDF_SYS_CLSOBJS         = 64;
  MDF_SYS_SINGLEOBJREFS   = 74;

  // Column types
  MDF_COL_IMAGE           = 34;
  MDF_COL_TEXT            = 35;
  MDF_COL_UID             = 36;  // UniqueIdentifier
  MDF_COL_DATE            = 40;
  MDF_COL_TIME            = 41;
  MDF_COL_DATETIME2       = 42;
  MDF_COL_DATETIMEOFFSET  = 43;
  MDF_COL_TINYINT         = 48;
  MDF_COL_SMALLINT        = 52;
  MDF_COL_INT             = 56;
  MDF_COL_SMALLDATETIME   = 58;
  MDF_COL_REAL            = 59;
  MDF_COL_MONEY           = 60;
  MDF_COL_DATETIME        = 61;
  MDF_COL_FLOAT           = 62;
  MDF_COL_VARIANT         = 98;
  MDF_COL_NTEXT           = 99;
  MDF_COL_BIT             = 104;
  MDF_COL_DECIMAL         = 106;
  MDF_COL_NUMERIC         = 108;
  MDF_COL_SMALLMONEY      = 122;
  MDF_COL_BIGINT          = 127;
  MDF_COL_VARBINARY       = 165;
  MDF_COL_VARCHAR         = 167;
  MDF_COL_BINARY          = 173;
  MDF_COL_CHAR            = 175;
  MDF_COL_TIMESTAMP       = 189;
  MDF_COL_NVARCHAR        = 231;
  MDF_COL_SYSNAME         = 231;
  MDF_COL_NCHAR           = 239;
  MDF_COL_HIERARCHYID     = 240; // HierarchyId
  MDF_COL_GEOGRAPHY       = 240;
  MDF_COL_GEOMETRY        = 240;
  MDF_COL_XML             = 241;
  //MDF_COL_RID = 15;
  //MDF_COL_UNIQUIFIER = 22;
  //MDF_COL_COMPUTED = 34;

  //MDF_PID_TABLE = 8277;
  //MDF_PID_VIEW  = 8278;


function MdfPageTypeToStr(AVal: Byte): string;
begin
  case AVal of
    MDF_PAGE_TYPE_DATA:      Result := 'DATA';
    MDF_PAGE_TYPE_INDEX:     Result := 'INDEX';
    MDF_PAGE_TYPE_TEXT_MIX:  Result := 'TEXT_MIX';
    MDF_PAGE_TYPE_TEXT_TREE: Result := 'TEXT_TREE';
    MDF_PAGE_TYPE_SORT:      Result := 'SORT';
    MDF_PAGE_TYPE_GAM:       Result := 'GAM';
    MDF_PAGE_TYPE_SGAM:      Result := 'SGAM';
    MDF_PAGE_TYPE_IAM:       Result := 'IAM';
    MDF_PAGE_TYPE_PFS:       Result := 'PFS';
    MDF_PAGE_TYPE_BOOT:      Result := 'BOOT';
    MDF_PAGE_TYPE_FILE_HEAD: Result := 'FILE_HEAD';
    MDF_PAGE_TYPE_DIFF_MAP:  Result := 'DIFF_MAP';
    MDF_PAGE_TYPE_ML_MAP:    Result := 'ML_MAP';
  else
    Result := 'PAGE_$' + IntToHex(AVal, 2);
  end;
end;

function MdfColTypeToStr(AVal: Byte): string;
begin
  case AVal of
    MDF_COL_BIGINT:      Result := 'BigInt';
    MDF_COL_BINARY:      Result := 'Binary';
    MDF_COL_BIT:         Result := 'Bit';
    MDF_COL_CHAR:        Result := 'Char';
    MDF_COL_DATE:        Result := 'Date';
    MDF_COL_DATETIME:    Result := 'DateTime';
    MDF_COL_DECIMAL:     Result := 'Decimal';
    MDF_COL_IMAGE:       Result := 'Image';
    MDF_COL_INT:         Result := 'Int';
    MDF_COL_MONEY:       Result := 'Money';
    MDF_COL_NCHAR:       Result := 'NChar';
    MDF_COL_NUMERIC:     Result := 'Numeric';
    MDF_COL_NTEXT:       Result := 'NText';
    MDF_COL_NVARCHAR:    Result := 'NVarChar';
    //MDF_COL_RID:         Result := 'RID';
    MDF_COL_SMALLDATETIME: Result := 'SmallDateTime';
    MDF_COL_SMALLINT:    Result := 'SmallInt';
    MDF_COL_SMALLMONEY:  Result := 'SmallMoney';
    MDF_COL_TEXT:        Result := 'Text';
    MDF_COL_TINYINT:     Result := 'TinyInt';
    MDF_COL_UID:         Result := 'UniqueIdentifier';
    //MDF_COL_UNIQUIFIER:  Result := 'Uniquifier';
    MDF_COL_VARBINARY:   Result := 'VarBinary';
    MDF_COL_VARCHAR:     Result := 'VarChar';
    MDF_COL_VARIANT:     Result := 'Variant';
    MDF_COL_HIERARCHYID: Result := 'HierarchyID';
    MDF_COL_XML:         Result := 'XML';
    MDF_COL_TIMESTAMP:   Result := 'Timestamp';
    //MDF_COL_GEOGRAPHY:   Result := 'Geography';
    //MDF_COL_GEOMETRY:    Result := 'Geometry';
    MDF_COL_TIME:        Result := 'Time';
    MDF_COL_DATETIME2:   Result := 'DateTime2';
    MDF_COL_DATETIMEOFFSET: Result := 'DateTimeOffset';
    //MDF_COL_COMPUTED:    Result := 'Computed';
    MDF_COL_FLOAT:       Result := 'Float';
    //MDF_COL_SYSNAME:     Result := 'SysName';
    MDF_COL_REAL:        Result := 'Real';
  else
    Result := 'Unknown_' + IntToStr(AVal);
  end;
end;

function MdfColTypeToDbFieldType(AVal: Byte): TFieldType;
begin
  case AVal of
    MDF_COL_BIGINT:      Result := ftLargeint;
    MDF_COL_BINARY:      Result := ftBytes;
    MDF_COL_BIT:         Result := ftBoolean;
    MDF_COL_CHAR:        Result := ftFixedChar;
    MDF_COL_DATE:        Result := ftDate;
    MDF_COL_DATETIME:    Result := ftDateTime;
    MDF_COL_DECIMAL:     Result := ftBCD;
    MDF_COL_IMAGE:       Result := ftBlob;
    MDF_COL_INT:         Result := ftInteger;
    MDF_COL_MONEY:       Result := ftCurrency;
    MDF_COL_NCHAR:       Result := ftString;
    MDF_COL_NUMERIC:     Result := ftFloat;
    MDF_COL_NTEXT:       Result := ftMemo;
    MDF_COL_NVARCHAR:    Result := ftString;
    //MDF_COL_RID:         Result := ftString;
    MDF_COL_SMALLDATETIME: Result := ftDateTime;
    MDF_COL_SMALLINT:    Result := ftSmallint;
    MDF_COL_SMALLMONEY:  Result := ftCurrency;
    MDF_COL_TEXT:        Result := ftMemo;
    MDF_COL_TINYINT:     Result := ftSmallint;
    MDF_COL_UID:         Result := ftGuid;
    //MDF_COL_UNIQUIFIER:  Result := ftString;
    MDF_COL_VARBINARY:   Result := ftVarBytes;
    MDF_COL_VARCHAR:     Result := ftString;
    MDF_COL_VARIANT:     Result := ftVariant;
    MDF_COL_HIERARCHYID: Result := ftString;
    MDF_COL_XML:         Result := ftMemo;
    MDF_COL_TIMESTAMP:   Result := ftTimeStamp;
    //MDF_COL_GEOGRAPHY:   Result := ftUnknown;
    //MDF_COL_GEOMETRY:    Result := ftUnknown;
    MDF_COL_TIME:        Result := ftTime;
    MDF_COL_DATETIME2:   Result := ftDateTime;
    MDF_COL_DATETIMEOFFSET: Result := ftFloat;
    //MDF_COL_COMPUTED:    Result := ftUnknown;
    MDF_COL_FLOAT:       Result := ftFloat;
    //MDF_COL_SYSNAME:     Result := ftString; // WideString
    MDF_COL_REAL:        Result := ftFloat;
  else
    Result := ftUnknown;
  end;
end;

{ TDBReaderMdf }

procedure TDBReaderMdf.AfterConstruction;
begin
  inherited;
  FTableList := TMdfTableList.Create();
  // init system tables
  InitSystemTables();
end;

procedure TDBReaderMdf.BeforeDestruction;
begin
  FreeAndNil(FTableList);
  inherited;
end;

function TDBReaderMdf.FillTableInfoText(ATableName: string; ALines: TStrings): Boolean;
var
  i: Integer;
  TmpTable: TMdfTable;
  s: string;
begin
  Result := False;
  TmpTable := FTableList.GetByName(ATableName);
  if not Assigned(TmpTable) then
    Exit;

  ALines.Add(Format('== Table Name=%s  RowCount=%d', [TmpTable.TableName, TmpTable.RowCount]));
  ALines.Add(Format('AllocID=%s', [IntToHex(TmpTable.AllocID, 16)]));
  ALines.Add(Format('ObjectID=%s', [IntToHex(TmpTable.ObjectID, 8)]));
  ALines.Add(Format('== Fields  Count=%d', [Length(TmpTable.FieldsDef)]));
  for i := Low(TmpTable.FieldsDef) to High(TmpTable.FieldsDef) do
  begin
    s := Format('%.2d Name=%s  Type=%s', [i, TmpTable.FieldsDef[i].Name, TmpTable.FieldsDef[i].TypeName]);
    ALines.Add(s);
  end;

  ALines.Add(Format('== FieldDefs  Count=%d', [Length(TmpTable.FieldDefArr)]));
  for i := Low(TmpTable.FieldDefArr) to High(TmpTable.FieldDefArr) do
  begin
    s := Format('%.2d Name=%s  Type=%s  Length=%d  Prec=%d',
      [i,
        TmpTable.FieldDefArr[i].Name,
        MdfColTypeToStr(TmpTable.FieldDefArr[i].XType),
        TmpTable.FieldDefArr[i].Length,
        TmpTable.FieldDefArr[i].Prec
      ]);
    ALines.Add(s);
  end;
end;

procedure TDBReaderMdf.FillTablesList;
var
  i, ii: Integer;
  ObjTab, ColTab, RowTab, AloTab: TMdfTable;
  ObjRow, ColRow, RowRow, AloRow: TDbRowItem;
  cObjType: Char;
  nTabID, nPrevTabID: LongWord;
  nRowsetID, nAllocID: Int64;
  nColLen, nColID, nColCount, nColPos: Integer;
  sColName, sTabType: string;
  nColType, nColPrec, nColScale: Byte;
  TmpTab: TMdfTable;
begin
  nPrevTabID := 0;
  TmpTab := nil;
  // schobj  Schema objects
  AloTab := FTableList.GetByID(MDF_SYS_ALLOCUNITS);
  RowTab := FTableList.GetByID(MDF_SYS_ROWSETS);
  ObjTab := FTableList.GetByID(MDF_SYS_SCHOBJS);
  ColTab := FTableList.GetByID(MDF_SYS_COLPARS);
  if not Assigned(AloTab) then Exit;
  if not Assigned(RowTab) then Exit;
  if not Assigned(ObjTab) then Exit;
  if not Assigned(ColTab) then Exit;

  for i := 0 to ObjTab.Count - 1 do
  begin
    ObjRow := ObjTab.GetItem(i);
    sTabType := VarToStr(ObjRow.Values[5]);
    cObjType := sTabType[1]; // type
    if not (cObjType in ['U', 'S']) then
      Continue;

    nTabID := ObjRow.Values[0]; // id

    // find RowsetID
    nRowsetID := 0;
    for ii := 0 to RowTab.Count - 1 do
    begin
      RowRow := RowTab.GetItem(ii);
      if RowRow.Values[2] = nTabID then  // [2] IdMajor
      begin
        nRowsetID := RowRow.Values[0]; // [0] RowsetID
        Break;
      end;
    end;

    // find AllocID
    nAllocID := 0;
    if nRowsetID <> 0 then
    begin
      for ii := 0 to AloTab.Count - 1 do
      begin
        AloRow := AloTab.GetItem(ii);
        if AloRow.Values[2] = nRowsetID then  // [2] OwnerID
        begin
          nAllocID := AloRow.Values[0]; // [0] AUID
          Break;
        end;
      end;
    end;

    if nPrevTabID <> nTabID then
    begin
      nPrevTabID := nTabID;
      if nAllocID <> 0 then
        TmpTab := FTableList.GetByAllocID(nAllocID);
      if not Assigned(TmpTab) then
        TmpTab := FTableList.GetByID(nTabID);

      if not Assigned(TmpTab) then
      begin
        TmpTab := TMdfTable.Create();
        //TmpTab.ObjectID := nTabID;
        //TmpTab.RowsetID := nRowsetID;
        //TmpTab.RowCount := 0;  // was no pages for this table
        TmpTab.TableType := cObjType;
        // [9] intprop - ColCount
        FTableList.Add(TmpTab);
      end;
      TmpTab.ObjectID := nTabID;
      if nAllocID <> 0 then
        TmpTab.AllocID := nAllocID;
      TmpTab.TableName := ObjRow.Values[1]; // name
    end;

    // find ColRow by TabID
    for ii := 0 to ColTab.Count - 1 do
    begin
      ColRow := ColTab.GetItem(ii);
      if ColRow.Values[0] = nTabID then
      begin
        nColID := ColRow.Values[2]; // colid
        sColName := ColRow.Values[3]; // name
        nColType := ColRow.Values[4]; // xtype
        nColLen :=  ColRow.Values[6]; // length
        nColPrec := ColRow.Values[7]; // prec
        nColScale := ColRow.Values[8]; // scale
        TmpTab.AddFieldDef(sColName, nColType, nColLen, nColPrec, nColScale, nColID);
      end;
    end;

    if IsUseRowRawData then
    begin
      // set raw data position (for debug)
      nColPos := 0; // 0-based
      for ii := Low(TmpTab.FieldsDef) to High(TmpTab.FieldsDef) do
      begin
        nColType := TmpTab.FieldDefArr[ii].XType;
        nColLen := 0;
        case nColType of
          MDF_COL_INT:        nColLen := 4;
          MDF_COL_BIGINT:     nColLen := 8;
          MDF_COL_SMALLINT:   nColLen := 2;
          MDF_COL_TINYINT:    nColLen := 1;
          MDF_COL_DATE:       nColLen := TmpTab.FieldDefArr[ii].Length; // 3
          MDF_COL_DATETIME:   nColLen := 8;
          MDF_COL_FLOAT:      nColLen := 8;
          MDF_COL_MONEY:      nColLen := 8;
          MDF_COL_NUMERIC:    nColLen := TmpTab.FieldDefArr[ii].Length;
          MDF_COL_BIT:        nColLen := 1; // ??
          MDF_COL_CHAR,
          MDF_COL_NCHAR,
          MDF_COL_NTEXT,
          MDF_COL_BINARY:     nColLen := TmpTab.FieldDefArr[ii].Length;
        end;
        if nColLen > 0 then
          TmpTab.FieldsDef[ii].RawOffset := nColPos;
        Inc(nColPos, nColLen);
      end;
    end;
  end;

  // ??? fill from ObjectID
  if ObjTab.Count = 0 then
  begin
    // find ColRow by TabID
    for ii := 0 to ColTab.Count - 1 do
    begin
      ColRow := ColTab.GetItem(ii);
      nTabID := ColRow.Values[0];

      if nPrevTabID <> nTabID then
      begin
        nPrevTabID := nTabID;
        TmpTab := FTableList.GetByID(nTabID);

        if not Assigned(TmpTab) then
        begin
          TmpTab := TMdfTable.Create();
          TmpTab.TableName := 'oid_' + IntToStr(nTabID); // name
          TmpTab.ObjectID := nTabID;
          // [9] intprop - ColCount
          FTableList.Add(TmpTab);
        end;
      end;

      if ColRow.Values[0] = nTabID then
      begin
        nColID := ColRow.Values[2]; // colid
        sColName := ColRow.Values[3]; // name
        nColType := ColRow.Values[4]; // xtype
        nColLen :=  ColRow.Values[6]; // length
        nColPrec := ColRow.Values[7]; // prec
        nColScale := ColRow.Values[8]; // scale
        TmpTab.AddFieldDef(sColName, nColType, nColLen, nColPrec, nColScale, nColID);
      end;
    end;

  end;


  // remove deleted columns
  for i := 0 to FTableList.Count - 1 do
  begin
    TmpTab := FTableList.GetItem(i);
    SetLength(TmpTab.PageIdArr, TmpTab.PageIdCount);
    if (TmpTab.RowCount <> 0) and (TmpTab.PageIdCount = 0) then
      TmpTab.RowCount := 0;

    SetLength(TmpTab.FieldsDef, Length(TmpTab.FieldDefArr));
    nColCount := 0;
    for ii := 0 to Length(TmpTab.FieldDefArr) - 1 do
    begin
      if TmpTab.FieldDefArr[ii].XType = 0 then
        Continue;
      with TmpTab.FieldsDef[nColCount] do
      begin
        Name := TmpTab.FieldDefArr[ii].Name;
        FieldType := MdfColTypeToDbFieldType(TmpTab.FieldDefArr[ii].XType);
        Size := TmpTab.FieldDefArr[ii].Length;
        TypeName := MdfColTypeToStr(TmpTab.FieldDefArr[ii].XType);
      end;
      Inc(nColCount);
    end;
    SetLength(TmpTab.FieldsDef, nColCount);
  end;

  FTableList.SortByName();
end;

function TDBReaderMdf.FindPage(APageID: LongWord; AFileID: Word;
  var APageBuf: TMdfPageBuffer): Boolean;
var
  nPagePos: Int64;
begin
  Result := False;
  if APageID = 0 then Exit;
  nPagePos := APageID * MDF_PAGE_SIZE;
  if nPagePos + MDF_PAGE_SIZE <= FFile.Size then
  begin
    FFile.Position := nPagePos;
    FFile.Read(FPageBuf, SizeOf(FPageBuf));
    Result := True;
  end;
end;

function TDBReaderMdf.GetTableByIndex(AIndex: Integer): TDbRowsList;
begin
  Result := TableList.GetItem(AIndex);
end;

function TDBReaderMdf.GetTablesCount: Integer;
begin
  Result := TableList.Count;
end;

procedure TDBReaderMdf.InitSystemTables;
var
  TmpTab: TMdfTable;
begin
  // sys_colpas
  TmpTab := TMdfTable.Create();
  FTableList.Add(TmpTab);
  with TmpTab do
  begin
    TableType := 'S';
    ObjectID := MDF_SYS_COLPARS;
    TableName := 'sys_colpar';
    AddFieldDef('ID', MDF_COL_INT);
    AddFieldDef('Number', MDF_COL_SMALLINT);
    AddFieldDef('ColID', MDF_COL_INT);
    AddFieldDef('Name', MDF_COL_SYSNAME);
    AddFieldDef('XType', MDF_COL_TINYINT);
    AddFieldDef('UType', MDF_COL_INT);
    AddFieldDef('Length', MDF_COL_SMALLINT);
    AddFieldDef('Prec', MDF_COL_TINYINT);
    AddFieldDef('Scale', MDF_COL_TINYINT);
    AddFieldDef('CollationID', MDF_COL_INT);
    AddFieldDef('Status', MDF_COL_INT);
    AddFieldDef('MaxInRow', MDF_COL_SMALLINT);
    AddFieldDef('XmlNs', MDF_COL_INT);
    AddFieldDef('dflt', MDF_COL_INT);
    AddFieldDef('chk', MDF_COL_INT);
    AddFieldDef('IdtVal', MDF_COL_VARBINARY);
  end;

  // sys_clsobj
  TmpTab := TMdfTable.Create();
  FTableList.Add(TmpTab);
  with TmpTab do
  begin
    TableType := 'S';
    ObjectID := MDF_SYS_CLSOBJS;
    TableName := 'sys_clsobj';
    AddFieldDef('Class', MDF_COL_TINYINT);
    AddFieldDef('ID', MDF_COL_INT);
    AddFieldDef('Name', MDF_COL_SYSNAME);
    AddFieldDef('Status', MDF_COL_INT);
    AddFieldDef('Type', MDF_COL_CHAR, 2);
    AddFieldDef('IntProp', MDF_COL_INT);
    AddFieldDef('Created', MDF_COL_DATETIME);
    AddFieldDef('Modified', MDF_COL_DATETIME);
  end;

  // sys_owner
  TmpTab := TMdfTable.Create();
  FTableList.Add(TmpTab);
  with TmpTab do
  begin
    TableType := 'S';
    ObjectID := MDF_SYS_OWNERS;
    TableName := 'sys_owner';
    AddFieldDef('ID', MDF_COL_INT);
    AddFieldDef('Name', MDF_COL_SYSNAME);
    AddFieldDef('Type', MDF_COL_CHAR, 1);
    AddFieldDef('SID', MDF_COL_VARBINARY);
    AddFieldDef('Password', MDF_COL_VARBINARY);
    AddFieldDef('DfltSch', MDF_COL_SYSNAME);
    AddFieldDef('Status', MDF_COL_INT);
    AddFieldDef('Created', MDF_COL_DATETIME);
    AddFieldDef('Modified', MDF_COL_DATETIME);
  end;

  // sys_schobj
  TmpTab := TMdfTable.Create();
  FTableList.Add(TmpTab);
  with TmpTab do
  begin
    TableType := 'S';
    ObjectID := MDF_SYS_SCHOBJS;
    TableName := 'sys_schobj';
    AddFieldDef('ID', MDF_COL_INT);
    AddFieldDef('Name', MDF_COL_SYSNAME);
    AddFieldDef('NSID', MDF_COL_INT);
    AddFieldDef('NSClass', MDF_COL_TINYINT);
    AddFieldDef('Status', MDF_COL_INT);
    AddFieldDef('Type', MDF_COL_CHAR, 2);
    AddFieldDef('PID', MDF_COL_INT);
    AddFieldDef('PClass', MDF_COL_TINYINT);
    AddFieldDef('IntProp', MDF_COL_INT);
    AddFieldDef('Created', MDF_COL_DATETIME);
    AddFieldDef('Modified', MDF_COL_DATETIME);
    AddFieldDef('Status2', MDF_COL_INT);
  end;

  // sys_rowset
  TmpTab := TMdfTable.Create();
  FTableList.Add(TmpTab);
  with TmpTab do
  begin
    TableType := 'S';
    ObjectID := MDF_SYS_ROWSETS;
    TableName := 'sys_rowset';
    AddFieldDef('RowsetID', MDF_COL_BIGINT);
    AddFieldDef('OwnerType', MDF_COL_TINYINT);
    AddFieldDef('IdMajor', MDF_COL_INT);
    AddFieldDef('IdMinor', MDF_COL_INT);
    AddFieldDef('NumPart', MDF_COL_INT);
    AddFieldDef('Status', MDF_COL_INT);
    AddFieldDef('fgidfs', MDF_COL_SMALLINT);
    AddFieldDef('RcRows', MDF_COL_BIGINT);
    AddFieldDef('CmprLevel', MDF_COL_TINYINT);
    AddFieldDef('FillFact', MDF_COL_TINYINT);
    AddFieldDef('MaxNullBit', MDF_COL_SMALLINT);
    AddFieldDef('MaxLeaf', MDF_COL_INT);
    AddFieldDef('MaxInt', MDF_COL_SMALLINT);
    AddFieldDef('MinLeaf', MDF_COL_SMALLINT);
    AddFieldDef('MinInt', MDF_COL_SMALLINT);
    AddFieldDef('RsGuid', MDF_COL_VARBINARY);
    AddFieldDef('LockRes', MDF_COL_VARBINARY);
    AddFieldDef('DbFragId', MDF_COL_INT);
  end;

  // sys_rscol
  TmpTab := TMdfTable.Create();
  FTableList.Add(TmpTab);
  with TmpTab do
  begin
    TableType := 'S';
    ObjectID := MDF_SYS_RSCOLS;
    TableName := 'sys_rscol';
    AddFieldDef('RSID', MDF_COL_BIGINT);
    AddFieldDef('RSColID', MDF_COL_INT);
    AddFieldDef('HBColID', MDF_COL_INT);
    AddFieldDef('RcModified', MDF_COL_BIGINT);
    AddFieldDef('TI', MDF_COL_INT);
    AddFieldDef('CID', MDF_COL_INT);
    AddFieldDef('OrdKey', MDF_COL_SMALLINT);
    AddFieldDef('MaxInRowLen', MDF_COL_SMALLINT);
    AddFieldDef('Status', MDF_COL_INT);
    AddFieldDef('Offset', MDF_COL_INT);
    AddFieldDef('NullBit', MDF_COL_INT);
    AddFieldDef('BitPos', MDF_COL_SMALLINT);
    AddFieldDef('ColGuid', MDF_COL_VARBINARY, 16);
    AddFieldDef('DbFragId', MDF_COL_INT);
  end;

  // MDF_SYS_ALLOCUNITS
  TmpTab := TMdfTable.Create();
  FTableList.Add(TmpTab);
  with TmpTab do
  begin
    TableType := 'S';
    ObjectID := MDF_SYS_ALLOCUNITS;
    TableName := 'sys_allocunits';
    AddFieldDef('AUID', MDF_COL_BIGINT);
    AddFieldDef('Type', MDF_COL_TINYINT);
    AddFieldDef('OwnerID', MDF_COL_BIGINT);
    AddFieldDef('Status', MDF_COL_INT);
    AddFieldDef('FGID', MDF_COL_SMALLINT);
    AddFieldDef('PgFirst', MDF_COL_VARBINARY);
    AddFieldDef('PgRoot', MDF_COL_VARBINARY);
    AddFieldDef('PgFirstIAM', MDF_COL_VARBINARY);
    AddFieldDef('PcUsed', MDF_COL_BIGINT);
    AddFieldDef('PcData', MDF_COL_BIGINT);
    AddFieldDef('PcReserved', MDF_COL_BIGINT);
    AddFieldDef('DbFragID', MDF_COL_INT);
  end;

end;

function TDBReaderMdf.OpenFile(AFileName: string; AStream: TStream): Boolean;
var
  hdr: TMdfPageHeaderRec;
  boot_hdr: TMdfBootPageHeaderRec;
  nPage: Integer;
  nPagePos: Int64;
begin
  Result := inherited OpenFile(AFileName, AStream);
  if not Result then Exit;

  // read pages
  nPage := 0;
  while FFile.Position + MDF_PAGE_SIZE <= FFile.Size do
  begin
    nPagePos := FFile.Position;
    FFile.Read(FPageBuf, SizeOf(FPageBuf));
    // header
    System.Move(FPageBuf, hdr, SizeOf(hdr));

    if IsDebugPages then
      LogInfo(Format('== Page %d (%s) pos=$%.8x ObjectID=%d ($%.8x)', [nPage, MdfPageTypeToStr(hdr.PageType), nPagePos, hdr.ObjectID, hdr.ObjectID]));

    if hdr.PageType = MDF_PAGE_TYPE_BOOT then
    begin
      // boot page header
      //BufToFile(page_buf[SizeOf(hdr)], SizeOf(boot_hdr), 'BOOT.data');
      System.Move(FPageBuf[SizeOf(hdr)], boot_hdr, SizeOf(boot_hdr));
      LogInfo('DBName=' + boot_hdr.DBName);
      LogInfo('Version=' + IntToStr(boot_hdr.Version));
      LogInfo('CreateVersion=' + IntToStr(boot_hdr.CreateVer));
    end
    else
    if hdr.PageType = MDF_PAGE_TYPE_DATA then
    begin
      {
      //if (hdr.ObjectID = MDF_SYS_SINGLEOBJREFS)
      if (hdr.ObjectID = MDF_SYS_COLPARS)
      then
      begin
        BufToFile(FPageBuf, SizeOf(FPageBuf), 'DATA_' + IntToStr(nPage) + '.data');
        ReadDataPage(FPageBuf);
        Exit;
      end;
      }

      ReadDataPage(FPageBuf, nPagePos);
    end
    else
    if hdr.PageType = MDF_PAGE_TYPE_TEXT_MIX then
    begin
      // store PageID/PagePos
      Inc(FTextPageCount);
      if FTextPageCount >= Length(FTextPageIdArr) then
      begin
        SetLength(FTextPageIdArr, Length(FTextPageIdArr) + 32);
        SetLength(FTextPagePosArr, Length(FTextPagePosArr) + 32);
      end;
      FTextPageIdArr[FTextPageCount-1] := hdr.PageID; // not trustworthy
      //FTextPageIdArr[FTextPageCount-1] := nPagePos div MDF_PAGE_SIZE;
      FTextPagePosArr[FTextPageCount-1] := nPagePos;
    end;

    Inc(nPage);
    if Assigned(OnPageReaded) then OnPageReaded(Self);
  end;

  SetLength(FTextPageIdArr, FTextPageCount);
  SetLength(FTextPagePosArr, FTextPageCount);

  FillTablesList();
  FIsMetadataLoaded := True;
end;

function TDBReaderMdf.ReadDataPage(const APageBuf: TMdfPageBuffer; APagePos: Int64;
  ATable: TMdfTable; AList: TDbRowsList): Boolean;
var
  hdr: TMdfPageHeaderRec;
  roffs: TMdfRowOffsetArray;
  i, nRowCount: Integer;
  nRowOffs: Word;
  nAllocID: Int64;
begin
  Result := False;
  System.Move(APageBuf, hdr, SizeOf(hdr));

  // allocation unit ID
  nAllocID := hdr.IndexID;
  nAllocID := nAllocID shl 32;
  nAllocID := nAllocID or hdr.ObjectID;
  nAllocID := nAllocID shl 16;

  if (hdr.PageType <> MDF_PAGE_TYPE_DATA)
  or (Assigned(ATable) and (ATable.AllocID <> nAllocID) and (ATable.TableType <> 'S')) then
  begin
    //Assert(False, 'Page AllocID not same!');
    //Exit;
  end;

  if not Assigned(FCurTable) or (FCurTable.ObjectID <> hdr.ObjectID) then
  begin
    if not FIsMetadataLoaded then
      FCurTable := FTableList.GetByID(hdr.ObjectID)
    else
      FCurTable := FTableList.GetByAllocID(nAllocID)
  end;
  if not Assigned(FCurTable) then
  begin
    FCurTable := TMdfTable.Create();
    FCurTable.TableName := Format('i_o_$%.4x_$%.8x_%s', [hdr.IndexID, hdr.ObjectID, IntToHex(nAllocID, 16)]); // name
    FCurTable.ObjectID := hdr.ObjectID;
    FCurTable.AllocID := nAllocID;
    FCurTable.RowCount := -1;
    FTableList.Add(FCurTable);
  end;

  if not FIsMetadataLoaded then
  begin
    Inc(FCurTable.PageIdCount);
    if FCurTable.PageIdCount >= Length(FCurTable.PageIdArr) then
      SetLength(FCurTable.PageIdArr, Length(FCurTable.PageIdArr) + 32);
    //FCurTable.PageIdArr[FCurTable.PageIdCount-1] := hdr.PageID; // not trustworthy
    FCurTable.PageIdArr[FCurTable.PageIdCount-1] := APagePos div MDF_PAGE_SIZE;
  end;

  // skip reading rows from undefined pages
  if Length(FCurTable.FieldDefArr) = 0 then
    Exit;

  if IsDebugPages then
  begin
    LogInfo('HeaderVer=' + IntToStr(hdr.HeaderVer));
    LogInfo('PageType=' + IntToStr(hdr.PageType));
    LogInfo('TypeFlagBits=' + IntToStr(hdr.TypeFlagBits));
    LogInfo('Level=' + IntToStr(hdr.Level));
    LogInfo('FlagBits=' + IntToStr(hdr.FlagBits));
    LogInfo('IndexID=' + IntToStr(hdr.IndexID));
    LogInfo('PrevPageID=$' + IntToHex(hdr.PrevPageID, 8));
    LogInfo('PrevFileID=$' + IntToHex(hdr.PrevFileID, 8));
    LogInfo('PMinLen=' + IntToStr(hdr.PMinLen));
    LogInfo('NextPageID=$' + IntToHex(hdr.NextPageID, 8));
    LogInfo('NextFileID=$' + IntToHex(hdr.NextFileID, 8));
    LogInfo('SlotCnt=' + IntToStr(hdr.SlotCnt));
    LogInfo(Format('ObjectID=%d ($%.8x)', [hdr.ObjectID, hdr.ObjectID]));
    LogInfo('FreeCnt=' + IntToStr(hdr.FreeCnt));
    LogInfo('FreeData=' + IntToStr(hdr.FreeData));
    LogInfo('PageID=$' + IntToHex(hdr.PageID, 8));
    LogInfo('FileID=$' + IntToHex(hdr.FileID, 8));
    LogInfo('ReservedCnt=' + IntToStr(hdr.ReservedCnt));
    LogInfo('LSN1=' + IntToStr(hdr.LSN1));
    LogInfo('LSN2=' + IntToStr(hdr.LSN2));
    LogInfo('LSN3=' + IntToStr(hdr.LSN3));
    LogInfo('XActReserved=' + IntToStr(hdr.XActReserved));
    LogInfo('XDesIDPart2=' + IntToStr(hdr.XDesIDPart2));
    LogInfo('XDesIDPart1=' + IntToStr(hdr.XDesIDPart1));
    LogInfo('GhostRecCnt=' + IntToStr(hdr.GhostRecCnt));
    LogInfo('Checksum=' + IntToStr(hdr.Checksum));
    LogInfo('---');
  end;

  nRowCount := hdr.SlotCnt;
  SetLength(roffs, nRowCount);
  for i := 1 to nRowCount do
  begin
    System.Move(APageBuf[SizeOf(APageBuf)-(i*2)], nRowOffs, SizeOf(nRowOffs));
    roffs[i-1] := nRowOffs;
  end;

  {
  // row offset array (detect size)
  nRowCount := 0;
  SetLength(roffs, 3000);
  for i := 1 to 3000 do
  begin
    System.Move(APageBuf[SizeOf(APageBuf)-(i*2)], nRowOffs, SizeOf(nRowOffs));
    if nRowOffs = 0 then
      Break;
    roffs[nRowCount] := nRowOffs;
    Inc(nRowCount);
  end;
  SetLength(roffs, nRowCount);

  if hdr.SlotCnt <> nRowCount then
  begin
    LogInfo('!! SlotCnt=' + IntToStr(hdr.SlotCnt) + ' nRowCount=' + IntToStr(nRowCount));
  end;
  }

  // records
  for i := Low(roffs) to High(roffs) do
  begin
    FPagePos := roffs[i];
    Assert(FPagePos + hdr.PMinLen < SizeOf(APageBuf), 'FPagePos=' + IntToStr(FPagePos) + 'GhostRecCnt=' + IntToStr(hdr.GhostRecCnt));
    ReadNextDataRec(hdr.PMinLen, i, AList);
  end;
  Result := True;
end;

function TDBReaderMdf.ReadBlobAddr(ABlobAddr: AnsiString): AnsiString;
var
  rdr: TRawDataReader;
  nBlobID: Int64;
  nPageID: LongWord;
  nFileID, nSlotID: Word;
begin
  // == Blob pointer ==
  // BlobID: Int64
  // PageID: LongWord
  // FileID: Word;
  // SlotID: Word;
  Result := '';
  if Length(ABlobAddr) < 16 then Exit;
  rdr.Init(ABlobAddr[1]);
  nBlobID := rdr.ReadInt64;
  nPageID := rdr.ReadUInt32;
  nFileID := rdr.ReadUInt16;
  nSlotID := rdr.ReadUInt16;

  Result := ReadBlobData(nPageID, nFileID, nSlotID);
end;

function TDBReaderMdf.ReadBlobData(APageID: LongWord; AFileID, ASlotID: Word): AnsiString;
var
  rdr: TRawDataReader;
  nOffset, nBlobID: Int64;
  nRecStatus, nFixLen, nBlobType, nCurLinks: Word;
  hdr: TMdfPageHeaderRec;
  roffs: TMdfRowOffsetArray;
  i, nRowCount, nDataOffs, nDataSize: Integer;
  nRowOffs: Word;
  LinkArr: array of TMdfBlobLinkRec;
begin
  Result := '';
  // https://www.kazamiya.net/en/mssql_4n6-04

  //LogInfo(Format('blob addr PageID=$%x FileID=$%x SlotID=$%x', [APageID, FileID, SlotID]));
  // find page offset by PageID
  nOffset := 0;
  for i := 0 to FTextPageCount - 1 do
  begin
    if FTextPageIdArr[i] = APageID then
    begin
      nOffset := FTextPagePosArr[i];
      Break;
    end;
  end;

  if nOffset = 0 then
  begin
    Result := '<blob not found>';
    Exit;
  end;

  if FBlobPagePos <> nOffset then
  begin
    FBlobPagePos := nOffset;
    FFile.Position := FBlobPagePos;
    FFile.Read(FBlobPageBuf, SizeOf(FBlobPageBuf));
  end;

  System.Move(FBlobPageBuf, hdr, SizeOf(hdr));
  if hdr.PageType <> MDF_PAGE_TYPE_TEXT_MIX then
  begin
    Assert(hdr.PageType = MDF_PAGE_TYPE_TEXT_MIX, 'Not TEXT page!');
    Exit;
  end;
  if hdr.PageID <> APageID then
  begin
    Assert(hdr.PageID = APageID, 'Not same PageID!');
    Exit;
  end;

  nRowCount := hdr.SlotCnt;
  SetLength(roffs, nRowCount);
  for i := 1 to nRowCount do
  begin
    System.Move(FBlobPageBuf[SizeOf(FBlobPageBuf)-(i*2)], nRowOffs, SizeOf(nRowOffs));
    roffs[i-1] := nRowOffs;
  end;

  Assert(ASlotID < Length(roffs), 'SlotID > SlotCnt');
  //if ASlotID = 0 then Exit;
  //  nSlotID := 1;
  Assert(ASlotID >= 0, 'SlotID=' + IntToStr(ASlotID));

  nDataOffs := roffs[ASlotID];
  //nDataSize := roffs[nSlotID] - nDataOffs; // 10+;

  //Assert(nDataSize < $2000, 'blob data size too big');

  // == Blob record ==
  // Status: Word
  // FixLen: Word;
  // -- rec fixed data
  // BlobID: Int64
  // BlobType: Word

  rdr.Init(FBlobPageBuf[nDataOffs]);
  // rec header
  nRecStatus := rdr.ReadUInt16(); // Status
  nFixLen := rdr.ReadUInt16(); // FixLen
  // blob header
  nBlobID := rdr.ReadInt64(); // BlobID
  nBlobType := rdr.ReadUInt16(); // BlobType
  nDataOffs := nDataOffs + 14;

  // blob data
  if nBlobType = MDF_BLOB_TYPE_SMALL_ROOT then // SMALL_ROOT
  begin
    // Size: Word;
    // Unused: LongWord
    // Data: array[Size] of Byte
    rdr.Init(FBlobPageBuf[nDataOffs]);
    nDataSize := rdr.ReadUInt16();
    rdr.ReadUInt32();
    Inc(nDataOffs, 6);
    if (nDataSize > 0) and (nDataSize <= nFixLen-16) then
    begin
      SetLength(Result, nDataSize);
      System.Move(FBlobPageBuf[nDataOffs], Result[1], nDataSize);
    end;
  end
  else
  if nBlobType = MDF_BLOB_TYPE_DATA then // DATA
  begin
    nDataSize := nFixLen - 10;
    SetLength(Result, nDataSize);
    System.Move(FBlobPageBuf[nDataOffs], Result[1], nDataSize);
  end
  else
  if nBlobType = MDF_BLOB_TYPE_ROOT_YUKON then // LARGE_ROOT_YUKON
  begin
    // MaxLinks: Word;
    // CurLinks: Word;
    // Level: Word;
    // Unused: LongWord
    // === Link ===
    // Size: LongWord
    // PageID: LongWord;
    // FileID: Word;
    // SlotID: Word;

    rdr.Init(FBlobPageBuf[nDataOffs]);
    rdr.ReadUInt16(); // MaxLinks
    nCurLinks := rdr.ReadUInt16(); // CurLinks
    rdr.ReadUInt16(); // Level
    rdr.ReadUInt32(); // Unused

    SetLength(LinkArr, nCurLinks);
    for i := 0 to nCurLinks - 1 do
    begin
      LinkArr[i].Size := rdr.ReadUInt32();
      LinkArr[i].PageID := rdr.ReadUInt32();
      LinkArr[i].FileID := rdr.ReadUInt16();
      LinkArr[i].SlotID := rdr.ReadUInt16();
    end;

    // fetch data from links (can mess up FBlobPageBuf)
    for i := 0 to nCurLinks - 1 do
    begin
      Result := Result + ReadBlobData(LinkArr[i].PageID, LinkArr[i].FileID, LinkArr[i].SlotID);
    end;
  end
  else
  if nBlobType = MDF_BLOB_TYPE_INTERNAL then // INTERNAL
  begin
    // MaxLinks: Word;
    // CurLinks: Word;
    // Level: Word;
    // === Link ===
    // Size: LongWord
    // Unused: LongWord
    // PageID: LongWord;
    // FileID: Word;
    // SlotID: Word;

    rdr.Init(FBlobPageBuf[nDataOffs]);
    rdr.ReadUInt16(); // MaxLinks
    nCurLinks := rdr.ReadUInt16(); // CurLinks
    rdr.ReadUInt16(); // Level

    SetLength(LinkArr, nCurLinks);
    for i := 0 to nCurLinks - 1 do
    begin
      LinkArr[i].Size := rdr.ReadUInt32();
      rdr.ReadUInt32(); // Unused
      LinkArr[i].PageID := rdr.ReadUInt32();
      LinkArr[i].FileID := rdr.ReadUInt16();
      LinkArr[i].SlotID := rdr.ReadUInt16();
    end;

    // fetch data from links (can mess up FBlobPageBuf)
    for i := 0 to nCurLinks - 1 do
    begin
      Result := Result + ReadBlobData(LinkArr[i].PageID, LinkArr[i].FileID, LinkArr[i].SlotID);
    end;
  end
  else
  begin
    nDataOffs := roffs[ASlotID];
    nDataSize := nFixLen;
    Result := Format('BlobType=%d  FixLen=%d  RawRec=%s', [nBlobType, nFixLen,
      BufToHex(FBlobPageBuf[nDataOffs], nDataSize)]);
  end;
  //Assert(FPagePos + hdr.PMinLen < SizeOf(APageBuf), 'FPagePos=' + IntToStr(FPagePos) + 'GhostRecCnt=' + IntToStr(hdr.GhostRecCnt));
end;

function TDBReaderMdf.ReadByte(const APageBuf: TMdfPageBuffer; AOffset: Integer): Byte;
begin
  Result := 0;
  System.Move(APageBuf[AOffset], Result, SizeOf(Result));
end;

function TDBReaderMdf.ReadWord(const APageBuf: TMdfPageBuffer; AOffset: Integer): Word;
begin
  Result := 0;
  System.Move(APageBuf[AOffset], Result, SizeOf(Result));
end;

function TDBReaderMdf.ReadLongWord(const APageBuf: TMdfPageBuffer; AOffset: Integer): LongWord;
begin
  Result := 0;
  System.Move(APageBuf[AOffset], Result, SizeOf(Result));
end;

function TDBReaderMdf.ReadNextBinary(ASize: Word): AnsiString;
var
  nPageID: LongWord;
  nFileID: Word;
begin
  if ASize >= 6 then
  begin
    // PageID/FileID
    nPageID := ReadNextDWord;
    nFileID := ReadNextWord;
    Inc(FPagePos, ASize-6);
    Result := Format('<blob pid=%d fid=%d>', [nPageID, nFileID]);
  end
  else
  begin
    Result := '<blob ' + BufToHex(FPageBuf[FPagePos], ASize) + '>';
    Inc(FPagePos, ASize);
  end;
end;

function TDBReaderMdf.ReadNextByte: Byte;
begin
  Result := 0;
  System.Move(FPageBuf[FPagePos], Result, SizeOf(Result));
  Inc(FPagePos, SizeOf(Result));
end;

function TDBReaderMdf.ReadNextChar(ACount: Word): AnsiString;
begin
  Result := '';
  if ACount > 0 then
  begin
    SetLength(Result, ACount);
    System.Move(FPageBuf[FPagePos], Result[1], ACount);
    Inc(FPagePos, ACount);
  end;
end;

function TDBReaderMdf.ReadNextCurrency: Currency;
begin
  Result := 0;
  System.Move(FPageBuf[FPagePos], Result, SizeOf(Result));
  Inc(FPagePos, SizeOf(Result));
end;

function TDBReaderMdf.ReadNextWord: Word;
begin
  Result := 0;
  System.Move(FPageBuf[FPagePos], Result, SizeOf(Result));
  Inc(FPagePos, SizeOf(Result));
end;

procedure TDBReaderMdf.ReadNextDataRec(AMinLen, ARecId: Integer; AList: TDbRowsList);
type
  TVarColRec = record
    PosA: Word;
    PosB: Word;
    IsOverflow: Boolean;
    Data: AnsiString;
  end;
var
  i, ii, iCol, iVarCol, nFieldCount: Integer;
  nRowOffs, nVarPos, nPrevVarPos, nVarLen: Word;
  s, sBits: string;
  rr: TMdfRowRec;
  VarCols: array of TVarColRec;
  v: Variant;
  TmpRow: TDbRowItem;
  IsNullValue: Boolean;
  NullFlagIndex, NullFlagOffs: Integer;
  XType: Byte;
begin
  if not Assigned(FCurTable) then Exit;
  nFieldCount := Length(FCurTable.FieldDefArr);

  nRowOffs := FPagePos;
  // === Row structure:
  // [00]           int8    <Attrs>
  // [01]           int8    <??>
  // [02]           int16   <RawDataLen>
  // [04]                   [RawData]
  // [RawDataLen]   int16   <ColCount>
  // [  ]                   [NullBitmap]   // if MDF_REC_ATTR_NULL_BITMAP
  // [RowSize]      int16   [VarCount]     // if MDF_REC_ATTR_VAR_COLUMNS
  // [RowSize+2]    int16   [VarEndOffs0]  // if > $2000 then not offset
  // [RowSize+4]    int16   [VarEndOffs1]
  // [RowSize+2+(VarCount*2)] [VarData0]
  // [VarEndOffs0]          [VarData1]
  // ...

  rr.Attrs := ReadByte(FPageBuf, nRowOffs);
  if rr.Attrs > $70 then
  begin
    LogInfo(Format('![%.4x] row=%d  hdr.Attrs=$%.2x ', [FPagePos, ARecId, rr.Attrs]));
    Exit;
  end;
  if (rr.Attrs and MDF_REC_TYPE_MASK) > 0 then
  begin
    // == Forwarding rec
    // [00]   u8    <Attrs>
    // [01]   u32   <PageID>
    // [05]   u16   <FileID>
    // [07]   u16   <SlotID>

    // == Forwarded rec
    // VarEndOffs with top bit set (+ $8000) contain extra var data:
    // [00]   u16   <ColID> (+ $100)
    // [02]   u32   <PageID>  points to forwarding rec
    // [06]   u16   <FileID>
    // [08]   u16   <SlotID>
  end;

  rr.RawDataLen := ReadWord(FPageBuf, nRowOffs + 2);
  if nRowOffs + rr.RawDataLen > SizeOf(FPageBuf) then
  begin
    LogInfo(Format('![%.4x] row=%d  RawDataLen=%d ', [FPagePos, ARecId, rr.RawDataLen]));
    Exit;
  end;
  if rr.RawDataLen < AMinLen then
  begin
    LogInfo(Format('![%.4x] row=%d  RawDataLen=%d  MinLen=%d', [FPagePos, ARecId, rr.RawDataLen, AMinLen]));
    Exit;
  end;
  rr.ColCount := ReadWord(FPageBuf, nRowOffs + rr.RawDataLen);
  rr.NullBitmapPos := 0;
  rr.NullBitmapLen := 0;
  if ((rr.Attrs and MDF_REC_ATTR_NULL_BITMAP) > 0) then
    rr.NullBitmapLen := ((rr.ColCount div 8) + 1);
  if (rr.ColCount mod 8) = 0 then
    Dec(rr.NullBitmapLen);
  if rr.NullBitmapLen > 0 then
    rr.NullBitmapPos := nRowOffs + rr.RawDataLen + 2;
  //rr.RowSize := AMinLen + 2 + rr.NullBitmapLen;  // fixed part + ColCount + NullBitmap
  rr.VarDataOffs := rr.RawDataLen + 2 + rr.NullBitmapLen;  // fixed part + ColCount + NullBitmap
  rr.RowOffs := nRowOffs;
  SetLength(VarCols, 0);

  if IsDebugRows then
  begin
    LogInfo(Format('= Row %d  offs=%d  size=%d', [ARecId, nRowOffs, rr.VarDataOffs]));
    //LogInfo(Format('= %s', [BufferToHex(FPageBuf[nRowOffs], rr.RowSize)]));
    LogInfo(Format('[00] Attrs $%.2x ', [FPageBuf[nRowOffs + 0]]));
    LogInfo(Format('[02] RawDataLen %d ', [rr.RawDataLen]));
    //LogInfo(Format('[04] RowID %.8x ', [ReadLongWord(APageBuf, nRowOffs + 4)]));
    LogInfo(Format('[%.2d] ColCount %d ', [rr.RawDataLen, rr.ColCount]));
    s := '';
    //rr.NullBitmapLen := rr.RowSize - (rr.RawDataLen + 2);
    if rr.NullBitmapLen > 0 then
      s := BufToHex(FPageBuf[nRowOffs + rr.RawDataLen + 2], rr.NullBitmapLen);
    LogInfo(Format('[%.2d] NullBitmap %s ', [rr.RawDataLen + 2, s]));
  end;
  //Assert(nFieldCount >= rr.ColCount, Format('ColCount=%d FieldCount=%d', [rr.ColCount, nFieldCount]));
  if rr.ColCount > nFieldCount * 2 then
  begin
    LogInfo(Format('! %s row=%d ColCount=%d FieldCount=%d', [FCurTable.TableName, ARecId, rr.ColCount, nFieldCount]));
    Exit;
  end;

  // read veriable size columns data
  // data order may be differ from columns order!
  if (rr.Attrs and MDF_REC_ATTR_VAR_COLUMNS) > 0 then
  begin
    rr.VarCount := ReadWord(FPageBuf, nRowOffs + rr.VarDataOffs);

    if IsDebugRows then
    begin
      if rr.VarCount > rr.ColCount then
        BufToFile(FPageBuf[nRowOffs], rr.VarDataOffs + 2, 'row.data');

      LogInfo(Format('[%.2d] VarCount %d ', [rr.VarDataOffs, rr.VarCount]));
      Assert(rr.VarCount < rr.ColCount, Format('ColCount=%d VarCount=%d', [rr.ColCount, rr.VarCount]));
    end;
    if rr.VarCount > rr.ColCount then
    begin
      LogInfo(Format('! %s row=%d ColCount=%d VarCount=%d($%x)', [FCurTable.TableName, ARecId, rr.ColCount, rr.VarCount, rr.VarCount]));
      //Exit;
      rr.VarCount := (rr.VarCount and $FF);
    end;
    nVarPos := rr.VarDataOffs + 2;
    SetLength(VarCols, rr.VarCount);
    //{!!!!!!}
    //if (rr.VarLenColCount >= 4) and (ARecId > 10) then
    //  BufToFile(FPageBuf[nRowOffs], rr.RowSize + 100, 'row.data');

    for i := 0 to rr.VarCount - 1 do
    begin
      VarCols[i].PosB := ReadWord(FPageBuf, nRowOffs + nVarPos);
      Inc(nVarPos, 2);
      //LogInfo(Format('[%.2d] VarLen=%d VarData=%s ', [nVarPos, nVarLen, ws]));
    end;
    // fill var data
    nPrevVarPos := nVarPos;
    for i := 0 to rr.VarCount - 1 do
    begin
      VarCols[i].IsOverflow := (VarCols[i].PosB and $8000) <> 0;
      if VarCols[i].IsOverflow then
      begin
        // remove var data overflow bit
        VarCols[i].PosB := (VarCols[i].PosB and $7FFF);
      end;
      VarCols[i].PosA := nPrevVarPos;
      nPrevVarPos := VarCols[i].PosB;

      if VarCols[i].PosB < VarCols[i].PosA then
      begin
        //LogInfo(Format('[%.2d] VarCols[%d] PosA=%d PosB=%d', [FPagePos, ii, VarCols[ii].PosA, VarCols[ii].PosB]));
        LogInfo('=== VarLen error');
        for ii := 0 to rr.VarCount - 1 do
          LogInfo(Format('[%.2d] VarCols[%d] PosA=%d PosB=%d', [FPagePos, ii, VarCols[ii].PosA, VarCols[ii].PosB]));
        Exit;
      end;

      nVarLen := VarCols[i].PosB - VarCols[i].PosA;
      if nVarLen > 0 then
      begin
        SetLength(VarCols[i].Data, nVarLen);
        System.Move(FPageBuf[nRowOffs + VarCols[i].PosA], VarCols[i].Data[1], nVarLen);
      end;
    end;
  end;

  if not Assigned(AList) then
    AList := FCurTable;
  TmpRow := TDbRowItem.Create(AList);
  AList.Add(TmpRow);
  SetLength(TmpRow.Values, Length(FCurTable.FieldsDef));

  // read columns data
  if IsUseRowRawData then
  begin
    FPagePos := nRowOffs + 4;
    TmpRow.RawData := ReadNextChar(rr.RawDataLen - 4);
  end;
  FPagePos := nRowOffs + 4;
  iCol := 0;
  iVarCol := 0;
  sBits := '';

  for i := 0 to nFieldCount - 1 do
  begin
    // NULL bitmap
    IsNullValue := False;
    NullFlagIndex := (i div 8);
    NullFlagOffs := i mod 8;
    if (rr.NullBitmapPos > 0) and (NullFlagIndex < rr.NullBitmapLen) and (NullFlagIndex < Length(FPageBuf)) then
      IsNullValue := (Ord(FPageBuf[rr.NullBitmapPos + NullFlagIndex]) and (Byte(1) shl NullFlagOffs)) <> 0;

    v := Null;
    XType := FCurTable.FieldDefArr[i].XType;
    case XType of
      MDF_COL_INT:        v := ReadNextDWord();
      MDF_COL_BIGINT:     v := ReadNextQWord();
      MDF_COL_SMALLINT:   v := ReadNextWord();
      MDF_COL_TINYINT:    v := ReadNextByte();
      MDF_COL_DATE:       v := ReadNextDate(FCurTable.FieldDefArr[i].Length);
      MDF_COL_DATETIME:   v := ReadNextDateTime();
      MDF_COL_TIMESTAMP:  v := ReadNextQWord();
      MDF_COL_FLOAT:      v := ReadNextFloat();
      MDF_COL_MONEY:      v := ReadNextCurrency();
      MDF_COL_NUMERIC:    v := ReadNextNumeric(FCurTable.FieldDefArr[i].Length, FCurTable.FieldDefArr[i].Prec, FCurTable.FieldDefArr[i].Scale);

      MDF_COL_VARCHAR,
      MDF_COL_NVARCHAR,
      MDF_COL_XML:
      begin
        if iVarCol < Length(VarCols) then
        begin
          if XType = MDF_COL_NVARCHAR then
            v := WideDataToStr(VarCols[iVarCol].Data)
          else
            v := VarCols[iVarCol].Data;
        end;
        Inc(iVarCol);
      end;

      MDF_COL_BINARY:
      begin
        v := ReadNextChar(FCurTable.FieldDefArr[i].Length);
      end;

      MDF_COL_VARBINARY,
      MDF_COL_IMAGE,
      MDF_COL_NTEXT:
      begin
        // VarCount can be less than actual, for Null values
        if iVarCol < Length(VarCols) then
        begin
          s := VarCols[iVarCol].Data;
          if VarCols[iVarCol].PosA = 0 then  // blob dummy
            v := s
          else
          if Length(s) > 16 then
            v := Format('<blob %d bytes>', [Length(s)])
          else
          if Length(s) > 0 then
          begin
            //v := BufferToHex(s[1], Length(s));
            s := ReadBlobAddr(VarCols[iVarCol].Data);
            if (XType = MDF_COL_NTEXT) then
              v := WideDataToStr(s)
            else
              v := s;
          end;
        end;

        Inc(iVarCol);
      end;
      MDF_COL_CHAR,
      MDF_COL_NCHAR:
      begin
        s := ReadNextChar(FCurTable.FieldDefArr[i].Length);
        if (XType = MDF_COL_NCHAR) or (XType = MDF_COL_NTEXT) then
          v := WideDataToStr(s)
        else
          v := s;
      end;
      MDF_COL_BIT:
      begin
        if sBits = '' then
          sBits := ReadNextChar(1);
        // todo: read bits
        //v := ReadNextByte();
      end;
    else
      if XType <> 0 then
      begin
        LogInfo(Format('[%.2d] Type=%d Size=%d', [FPagePos-nRowOffs, XType, FCurTable.FieldDefArr[i].Length]));
        v := Format('<Type=%d>', [XType]);
      end;
    end;
    if IsNullValue then
      v := Null;
    if XType <> 0 then
    begin
      TmpRow.Values[iCol] := v;
      Inc(iCol);
    end;
  end;
end;

function TDBReaderMdf.ReadNextDate(ASize: Byte): TDateTime;
var
  dd, bt: LongWord;
  i: Integer;
begin
  dd := 0;
  i := 0;
  while i < ASize do
  begin
    bt := ReadNextByte();
    bt := bt shl (8 * i);
    dd := dd or bt;
    Inc(i);
  end;
  Result := dd + 2;
  if (Result > $200000) or (Result < -$200000) then
    Result := 695400;
  Result := IncMonth(Result, -1899*12);
end;

function TDBReaderMdf.ReadNextDateTime: TDateTime;
var
  dd: LongInt;
  zz: LongInt;
begin
  zz := LongInt(ReadNextDWord());
  dd := LongInt(ReadNextDWord());
  Result := dd + 2;
  // todo: part of day
  if (Result > $200000) or (Result < -$200000) then
    Result := 0;
end;

function TDBReaderMdf.ReadNextDWord: LongWord;
begin
  Result := 0;
  System.Move(FPageBuf[FPagePos], Result, SizeOf(Result));
  Inc(FPagePos, SizeOf(Result));
end;

function TDBReaderMdf.ReadNextFloat: Real;
begin
  Result := 0;
  System.Move(FPageBuf[FPagePos], Result, SizeOf(Result));
  Inc(FPagePos, SizeOf(Result));
end;

function TDBReaderMdf.ReadNextNumeric(ASize, APrec, AScale: Byte): string;
var
  sBuf: AnsiString;
  i: Integer;
  n, nn: Int64;
begin
  Result := '';
  if ASize > 0 then
  begin
    if (ASize > 1) and (ASize <= 17) then
    begin
      nn := 0;
      ReadNextByte();
      for i := 0 to ASize - 2 do
      begin
        n := ReadNextByte();
        n := n shl (i*8);
        nn := nn or n;
      end;
      Result := IntToStr(nn);
      // place decimal dot
      if AScale > 0 then
      begin
        while Length(Result) < AScale+1 do
          Result := Result + '0';
        Result := Copy(Result, 1, Length(Result)-AScale) + '.' + Copy(Result, Length(Result)-AScale+1, AScale);
      end;
    end
    else
    begin
      SetLength(sBuf, ASize);
      System.Move(FPageBuf[FPagePos], sBuf[1], ASize);
      Inc(FPagePos, ASize);

      Result := BufToHex(sBuf[1], ASize);
    end;
  end;
end;

function TDBReaderMdf.ReadNextQWord: Int64;
begin
  Result := 0;
  System.Move(FPageBuf[FPagePos], Result, SizeOf(Result));
  Inc(FPagePos, SizeOf(Result));
end;

procedure TDBReaderMdf.ReadTable(AName: string; ACount: Int64; AList: TDbRowsList);
var
  TmpTable: TMdfTable;
  hdr: TMdfPageHeaderRec;
  nAllocID, nRowsetID, nPagePos: Int64;
  i, ii: Integer;
  ObjTab, RowTab, AloTab: TMdfTable;
  ObjRow, RowRow, AloRow: TDbRowItem;
  ObjID: LongWord;
begin
  TmpTable := FTableList.GetByName(AName);
  if Assigned(TmpTable) then
  begin
    AList.FieldsDef := TmpTable.FieldsDef;
    // read if needed
    //if (TmpTable.RowCount < 0) or (TmpTable.RowCount <> TmpTable.Count) then
    begin
      if Length(TmpTable.FieldDefArr) = 0 then
      begin
        // find RowsetID by AllocID
        nRowsetID := 0;
        if TmpTable.AllocID <> 0 then
        begin
          AloTab := FTableList.GetByID(MDF_SYS_ALLOCUNITS);
          for ii := 0 to AloTab.Count - 1 do
          begin
            AloRow := AloTab.GetItem(ii);
            if TmpTable.AllocID = AloRow.Values[0] then  // [0] AUID
            begin
              nRowsetID := AloRow.Values[2]; // [2] OwnerID
              Break;
            end;
          end;
        end
        else
          LogInfo(Format('Table %s - AllocID=%d', [TmpTable.TableName, TmpTable.AllocID]));

        // find ObjID by RowsetID
        ObjID := 0;
        if nRowsetID <> 0 then
        begin
          RowTab := FTableList.GetByID(MDF_SYS_ROWSETS);
          for ii := 0 to RowTab.Count - 1 do
          begin
            RowRow := RowTab.GetItem(ii);
            if RowRow.Values[0] = nRowsetID then  // [0] RowsetID
            begin
              ObjID := RowRow.Values[2]; // [2] IdMajor
              Break;
            end;
          end;
        end
        else
          LogInfo(Format('Table %s - AllocID=%d  RowsetID=%d', [TmpTable.TableName, TmpTable.AllocID, nRowsetID]));

        if ObjID <> 0 then
          LogInfo(Format('Table %s - ObjID=%d ($%x)', [TmpTable.TableName, ObjID, ObjID]));

        Exit;
      end;

      {if (TmpTable.PageIdCount = 0) then
      begin
        // scan for pages
        FFile.Position := 0;
        while FFile.Position + MDF_PAGE_SIZE <= FFile.Size do
        begin
          nPagePos := FFile.Position;
          FFile.Read(FPageBuf, SizeOf(FPageBuf));
          // header
          System.Move(FPageBuf, hdr, SizeOf(hdr));
          // allocation unit ID
          nAllocID := hdr.IndexID;
          nAllocID := nAllocID shl 32;
          nAllocID := nAllocID or hdr.ObjectID;
          nAllocID := nAllocID shl 16;

          if (hdr.PageType = MDF_PAGE_TYPE_DATA) and (nAllocID = TmpTable.AllocID) then
          begin
            FCurTable := TmpTable;
            if not ReadDataPage(FPageBuf, nPagePos, TmpTable, AList) then
              Break;
          end;
        end;
      end
      else }
      begin
        // read pages
        for i := Low(TmpTable.PageIdArr) to High(TmpTable.PageIdArr) do
        begin
          if FindPage(TmpTable.PageIdArr[i], 1, FPageBuf) then
          begin
            nPagePos := TmpTable.PageIdArr[i] * MDF_PAGE_SIZE;
            FCurTable := TmpTable;
            ReadDataPage(FPageBuf, nPagePos, TmpTable, AList);
            if Assigned(OnPageReaded) then OnPageReaded(Self);
          end
          else
          begin
            Assert(False, 'Page not found: ' + IntToStr(TmpTable.PageIdArr[i]));
          end;
        end;
      end;

      TmpTable.RowCount := AList.Count;
    end;
  end;
end;

{ TMdfTable }

procedure TMdfTable.AddFieldDef(AName: string; AType: Byte;
  ALength: Integer;
  APrec: Byte;
  AScale: Byte;
  AColID: Integer);
var
  i: Integer;
begin
  if ALength = 0 then
  begin
    case AType of
      MDF_COL_BIGINT:   ALength := 8;
      MDF_COL_INT:      ALength := 4;
      MDF_COL_SMALLINT: ALength := 2;
      MDF_COL_TINYINT:  ALength := 1;
      MDF_COL_DATETIME: ALength := 4;
    end;
  end;

  if AColID < 0 then
    i := Length(FieldDefArr)
  else
    i := AColID-1;
  if i >= Length(FieldDefArr) then
    SetLength(FieldDefArr, i+1);
  with FieldDefArr[i] do
  begin
    Name := AName;
    XType := AType;
    Length := ALength;
    Prec := APrec;
    Scale := AScale;
  end;

  // DB.FieldType
  if AColID < 0 then
    i := Length(FieldsDef)
  else
    i := AColID-1;
  if i >= Length(FieldsDef) then
    SetLength(FieldsDef, i+1);
  with FieldsDef[i] do
  begin
    Name := AName;
    FieldType := MdfColTypeToDbFieldType(Atype);
    Size := ALength;
    TypeName := MdfColTypeToStr(AType);
  end;
end;

procedure TMdfTable.AfterConstruction;
begin
  inherited;
  RowCount := -1;
end;

procedure TMdfTable.BeforeDestruction;
begin
  inherited;

end;

function TMdfTable.IsEmpty: Boolean;
begin
  Result := (RowCount = 0);
end;

function TMdfTable.IsGhost: Boolean;
begin
  Result := (Length(FieldsDef) = 0);
end;

function TMdfTable.IsSystem: Boolean;
begin
  Result := (ObjectID <= 100);
end;

{ TMdfTableList }

function TMdfTableList.GetByAllocID(AAllocID: Int64): TMdfTable;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := GetItem(i);
    if Result.AllocID = AAllocID then
      Exit;
  end;
  Result := nil;
end;

function TMdfTableList.GetByID(AObjID: LongWord): TMdfTable;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := GetItem(i);
    if Result.ObjectID = AObjID then
      Exit;
  end;
  Result := nil;
end;

function TMdfTableList.GetByName(AName: string): TMdfTable;
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

function TMdfTableList.GetItem(AIndex: Integer): TMdfTable;
begin
  Result := TMdfTable(Get(AIndex));
end;

procedure TMdfTableList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Action = lnDeleted then
    TMdfTable(Ptr).Free;
end;


procedure TMdfTableList.SortByName;

  function DoSortByName(Item1, Item2: Pointer): Integer;
  var
    TmpItem1, TmpItem2: TMdfTable;
  begin
    TmpItem1 := TMdfTable(Item1);
    TmpItem2 := TMdfTable(Item2);
    Result := AnsiCompareStr(TmpItem1.TableName, TmpItem2.TableName);
  end;

begin
  Sort(@DoSortByName);
end;

end.
