unit DBReaderFirebird;

(*
Interbase/FireBird (GDB, FDB) database file reader

Author: Sergey Bodrov, 2024
License: MIT

Thanks to Norman Dunbar for database structure explanation!
https://www.ibexpert.net/ibe/pmwiki.php?n=Doc.StructureOfADataPage
https://www.ibexpert.net/ibe/pmwiki.php?n=Doc.FirebirdForTheDatabaseExpertEpisode2PageTypes#BLP
https://www.firebirdsql.org/file/documentation/html/en/firebirddocs/firebirdinternals/firebird-internals.html#fbint-introduction


-- tested versions:
InterBase 6  (ODS 10)
InterBase 7  (ODS 11)
FireBird 2   (ODS 11)
FireBird 2.5 (ODS 11.2)
*)

{$A8}  // align fields in records by 8 bytes

interface

uses
  {$ifdef FPC}
  LazUTF8,
  {$endif}
  SysUtils, Classes, Variants, DBReaderBase, DB;

type
  TDBReaderFB = class;

  TRDB_FieldInfoRec = record
    Size: Integer;
    FieldType: Word;     // from FIELDS
    FieldLength: Word;   // data length, not size
    FieldSubType: Integer;
    Name: string;
    DType: Byte;         // from FORMATS
    Scale: Integer;
    Length: Word;        // from FORMATS
    SubType: Word;
    Flags: Cardinal;
    Offset: Cardinal;

    function AsString(): string;
    function GetSize(): Integer;
  end;

  TRDB_Blob = record
    RelID: Integer;
    RowID: Cardinal;
    BlobSubType: Integer;  // -1 = error
    Data: AnsiString;
  end;

  TRDBTable = class(TDbRowsList)
  public
    RelationID: Integer;  // Relation ID
    FieldsInfo: array of TRDB_FieldInfoRec;
    RowCount: Integer;
    PageIdArr: array of LongWord;  // PageNum
    PageIdCount: Integer;
    FormatID: Integer;
    DataPos: Integer; // for AddFieldDef

    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;
    // contain no rows
    function IsEmpty(): Boolean; override;
    // predefined table
    function IsSystem(): Boolean; override;
    // not defined in metadata
    function IsGhost(): Boolean; override;

    function FindRecByValue(AColIndex: Integer; AValue: Variant): TDbRowItem;
    procedure AddFieldDef(AName: string; AType: Integer;
      ALength: Integer = 0;
      ASubType: Integer = 0;
      AScale: Integer = 0;
      AOffset: Integer = 0);
  end;

  TRDBTableList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function GetItem(AIndex: Integer): TRDBTable;
    function GetByName(AName: string): TRDBTable;
    function GetByRelationID(ARelID: Integer): TRDBTable;
    procedure SortByName();
  end;

  TRDBPageItem = class(TObject)
  public
    PageNum: Integer;             // INTEGER
    RelationID: Word;             // SMALLINT
    PageSeq: Integer;             // INTEGER
    PageType: Word;               // SMALLINT
  end;

  TRDBPageItemList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function GetItem(AIndex: Integer): TRDBPageItem;
    procedure SortItems();
    // find page by Type/Rel/Seq. If Result <0 it position for insert
    function FindPageIndex(APageType: Word; ARelationID, ASequence: Cardinal): Integer;
  end;

  TRDB_RowItem = class(TDbRowItem)
  private
    function GetTableInfo: TRDBTable;
  protected
    FDataPos: Integer;

    FReader: TDBReaderFB;
    FRelID: Integer;
    FRowID: Cardinal;    // (PageSeq * FMaxRecPerPage) + RecIndex
    function ReadInt(ABytes: Integer = 4): Integer;
    function ReadInt64(ABytes: Integer = 8): Int64;
    function ReadCurrency(ABytes: Integer = 8): Currency;
    function ReadFloat(ABytes: Integer = 8): Double;
    // AType 0-DateTime 1-Date 2-Time
    function ReadDateTime(AType: Integer = 0; ABytes: Integer = 8): TDateTime;
    function ReadBlob(AType: Integer = 0; ASize: Integer = 0): TRDB_Blob;
    function ReadVarChar(ACount: Integer; ASize: Integer = 0; ASub: Integer = 0): string;
    function ReadChar(ACount: Integer; ASub: Integer = 0): string;
    function IsNullValue(AIndex: Integer): Boolean;
    function IsBlobValue(AIndex: Integer): Boolean;
  public
    FormatID: Integer;
    constructor Create(AOwner: TDbRowsList; AReader: TDBReaderFB; ARelID: Integer; ARowID: Cardinal); reintroduce;
    //procedure ReadRawData(const AData; ALen: Integer);
    procedure ReadData(ATableInfo: TRDBTable; const AData: AnsiString);
    function GetAsText(): string; virtual;
    function GetBlobValue(var AValue: TRDB_Blob): AnsiString;
    function GetFieldAsStr(AFieldIndex: Integer): string; override;
    function GetFieldAsBlob(AFieldIndex: Integer): AnsiString;
    function GetValueByName(AName: string): Variant; virtual;

    property Reader: TDBReaderFB read FReader;
    property RelID: Integer read FRelID;
    property RowID: Cardinal read FRowID;
    property TableInfo: TRDBTable read GetTableInfo;
  end;


  TDBReaderFB = class(TDBReader)
  private
    FTableList: TRDBTableList;
    FPagesList: TRDBPageItemList;
    FPageSize: Integer;
    FOdsVersion: Integer;
    FMaxRecPerPage: Word;              // for calculation of Seq/Rec from RecID
    FIsMetadataReaded: Boolean;        // true after FillTablesList()
    FIsInterBase: Boolean;

    // raw page for blob reader
    FBlobRawPage: TByteArray;
    FBlobRawPageNum: Cardinal;

    FOnLog: TGetStrProc;         // (rel=8)

    function GetRelationName(ARelID: Integer): string;

    function ReadDataPage(const APageBuf: TByteArray; APagePos: Int64;
      ATable: TRDBTable = nil; AList: TDbRowsList = nil): Boolean;
    // set table format from FORMATS.DESCRIPTOR field
    procedure SetRawFormat(ATable: TRDBTable; ARawFormat: AnsiString);

    // find page by Type/Rel/Seq
    function FindPageNum(APageType: Word; ARelationID, ASequence: Integer; out APageNum: Cardinal): Boolean;
    function GetBlobFromPage(const ARawPage: TByteArray; AIndex: Integer; var AData: AnsiString): Boolean;
    // Return blob content for Relation/RowID
    // if False, then AData contain error description
    function GetBlobData(ARelationID, ARowID: Cardinal; var AData: AnsiString): Boolean;
    // Get partial data, recursive
    function GetFragmentData(APageNum, ALineID: Cardinal; ARecursionID: Integer = 0): AnsiString;

    // define initial system tables structure
    procedure InitSystemTables();
    // fill schema tables from initial tables
    procedure FillTablesList();
  public
    IsLogPages: Boolean;
    IsLogBlobs: Boolean;
    IsLogFormats: Boolean;
    IsClearRawData: Boolean;
    DebugRelID: Integer;
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

    property TableList: TRDBTableList read FTableList;
    property OdsVersion: Integer read FOdsVersion;
    property IsInterBase: Boolean read FIsInterBase;  // False if FireBird

    property OnLog: TGetStrProc read FOnLog write FOnLog;
  end;


implementation

type
  // Basic page header
  TFBPageHead = packed record
    PageType: Byte;        // PAGE_TYPE_
    Flags: Byte;           // PAGE_FLAG_
    Reserved: Word;        // for alignment
    Generation: Cardinal;
    Scn: Cardinal;
    PageNo: Cardinal;      // for validation
  end;

  // Uncomplete (!) header
  TFBOdsPageHead = packed record
    PageHead: TFBPageHead;
    PageSize: Word;              // Page size of database
    OdsVersion: Word;            // Version of on-disk structure
    PagesPage: Cardinal;         // Page number of PAGES relation
    NextPage: Cardinal;          // Page number of next hdr page
  end;

  // Basic Data page header
  TFBDataPageHead = packed record
    PageHead: TFBPageHead;
    Sequence: Cardinal;
    RelationID: Word;
    Count: Word;
  end;

  // Data page record position
  TFBDataPageRec = packed record
    Offset: Word;
    Length: Word;
  end;


  TFBDataRecHead = packed record
    transaction: Cardinal;      // transaction id (lowest 32 bits)
    b_page: Cardinal;           // back pointer
    b_line: Word;               // back line
    flags: Word;                // flags, etc
    format: Byte;               // format version
  end;

  TFBDataFragRecHead = packed record
    transaction: Cardinal;      // transaction id (lowest 32 bits)
    b_page: Cardinal;           // back pointer
    b_line: Word;               // back line
    flags: Word;                // flags, etc
    format: Byte;               // format version
    Unused: Byte;
    tra_high: Word;             // higher bits of transaction id
    f_page: Cardinal;           // next fragment page
    f_line: Word;               // next fragment line
    //Unused2: Byte;
  end;

  // Record header for blob
  TFBBlobRecHead = record
    LeadPage: Cardinal;            // First data page number
    MaxSequence: Cardinal;	       // Number of data pages
    MaxSegment: Word;              // Longest segment
    Flags: Word;                   // flags, etc
    Level: Byte;                   // Number of address levels, see blb_level in blb.h
    Count: Cardinal;               // Total number of segments
    Length: Cardinal;              // Total length of data
    SubType: Word;                 // Blob sub-type
    Charset: Byte;                 // Blob charset (since ODS 11.1)
    Unused: Byte;
    //Page: array[0..0] of Cardinal;  // Page vector for blob pages
  end;

  // Format descriptor item (ODS 11+, FB 2.X+)
  TFBFormatRec = packed record
    DType: Byte;        // 1    DTYPE_
    Scale: Shortint;    // 1
    Length: Word;       // 2    Field length in bytes
    SubType: Word;      // 2    For BLOB/TEXT and integers
    Flags: Word;        // 2
    Offset: Cardinal;   // 4    Position from begining of row
  end;

  TFBBlobPageHead = packed record
    PageHead: TFBPageHead;
    LeadPage: Cardinal; // This field holds the page number for the first page for this blob
    Sequence: Cardinal; // The sequence number of this page within the page range for this blob.
    Length: Word;       // The length of the blob data on this page, in bytes.
    Padding: Word;      // Not used for any data, used as padding.
  end;

  // Blob data field record
  // RelID    2  RelationID
  // HiRelID  1  High byte of RelationID
  // HiRowID  1  High byte of RowID
  // RowID    4  RowID
  TBlobFieldRec = packed record
    RelationID: Integer;   // Word
    RowID: Cardinal;
  end;

const
  // Page types
  PAGE_TYPE_UNDEFINED    = 0;
  PAGE_TYPE_HEADER       = 1;   // Database header page
  PAGE_TYPE_PAGES        = 2;   // Page inventory page
  PAGE_TYPE_TRANSACTIONS = 3;   // Transaction inventory page
  PAGE_TYPE_POINTER      = 4;   // Pointer page
  PAGE_TYPE_DATA         = 5;   // Data page
  PAGE_TYPE_ROOT         = 6;   // Index root page
  PAGE_TYPE_INDEX        = 7;   // Index (B-tree) page
  PAGE_TYPE_BLOB         = 8;   // Blob data page
  PAGE_TYPE_IDS          = 9;   // Gen-ids
  PAGE_TYPE_SCNS         = 10;  // SCN's inventory page

  DATA_PAGE_FLAG_ORPHAN   = 1;  // Page is an orphan - it has no entry in the pointer page
  DATA_PAGE_FLAG_FULL     = 2;  // Page is full up
  DATA_PAGE_FLAG_LARGE    = 4;  // Large object is stored on this page

  BLOB_PAGE_FLAG_POINTERS = 1;  // Blob pointer page, not data page

  SYS_REL_PAGES           = 0;  // RDB$PAGES
  SYS_REL_FIELDS          = 2;  // RDB$FIELDS
  SYS_REL_RELATION_FIELDS = 5;  // RDB$RELATION_FIELDS
  SYS_REL_RELATIONS       = 6;  // RDB$RELATIONS
  SYS_REL_FORMATS         = 8;  // RDB$FORMATS

  // Record header flags
  REC_DELETED     = 1;    // record is logically deleted
  REC_CHAIN       = 2;    // record is an old version
  REC_FRAGMENT    = 4;    // record is a fragment
  REC_INCOMPLETE  = 8;    // record is incomplete
  REC_BLOB        = 16;   // isn't a record but a blob
  REC_STREAM_BLOB = 32;   // blob is a stream mode blob
  REC_DELTA       = 32;   // prior version is differences only
  REC_LARGE       = 64;   // object is large
  REC_DAMAGED     = 128;  // object is known to be damaged
  REC_GC_ACTIVE   = 256;  // garbage collecting dead record version
  REC_UK_MODIFIED = 512;  // record key field values are changed
  REC_LONG_TRANUM = 1024; // transaction number is 64-bit

  DTYPE_TEXT      = 1;  // Sub_0, Sub_3 = Len;   Sub_1, Sub_2 = 8 (BLOB)
  DTYPE_CSTRING   = 2;  // Len
  DTYPE_VARYNG    = 3;  // 2 + Len
  DTYPE_PACKED    = 6;
  DTYPE_BYTE      = 7;  // 1  Byte
  DTYPE_SHORT     = 8;  // 2  Word
  DTYPE_LONG      = 9;  // 4  Cardinal
  DTYPE_QUAD      = 10; // 8  DWord
  DTYPE_REAL      = 11; // 4  Single
  DTYPE_DOUBLE    = 12; // 8  Double
  DTYPE_DFLOAT    = 13; //
  DTYPE_SQL_DATE  = 14; // 4  Integer
  DTYPE_SQL_TIME  = 15; // 4  Cardinal
  DTYPE_TIMESTAMP = 16; // 8  DateTime
  DTYPE_BLOB      = 17; // 8  RelID/RowID
  DTYPE_ARRAY     = 18;
  DTYPE_INT64     = 19; // 8 Int64

  FB_EPOCH_DIFF          = 15018;  // Days between Delphi and Firebird epoch

function OdsPageTypeToStr(AValue: Byte): string;
begin
  case AValue of
    PAGE_TYPE_HEADER:       Result := 'Database header page';
    PAGE_TYPE_PAGES:        Result := 'Page inventory page';
    PAGE_TYPE_TRANSACTIONS: Result := 'Transaction inventory page';
    PAGE_TYPE_POINTER:      Result := 'Pointer page';
    PAGE_TYPE_DATA:         Result := 'Data page';
    PAGE_TYPE_ROOT:         Result := 'Index root page';
    PAGE_TYPE_INDEX:        Result := 'Index (B-tree) page';
    PAGE_TYPE_BLOB:         Result := 'Blob data page';
    PAGE_TYPE_IDS:          Result := 'Gen-ids';
    PAGE_TYPE_SCNS:         Result := 'SCN inventory page';
  else
    Result := 'Undefined (' + IntToStr(AValue) + ')';
  end;
end;

function FieldTypeToSize(AType, ALen, ASubType: Word): Integer;
begin
  case AType of
    7: Result := 2; // SMALLINT
    8: Result := 4; // INTEGER
    10: Result := 8; // FLOAT
    14: // CHAR
    begin
      Result := ALen;
      if ASubType = 3 then
        Inc(Result);  // Encoding
    end;
    16: Result := 8; // INT64
    27: Result := 8; // DOUBLE
    35: Result := 8; // TIMESTAMP (DateTime)
    37: Result := ALen + 2; // VARCHAR
    261: // BLOB
    begin
      Result := ALen; // BINARY
      case ASubType of
        1: Result := 14; // TEXT
        2: Result := 4;  // BLR (Binary Language Representation)
      end;
    end;
  else
    Result := 0;
  end;
end;

function FieldTypeToStr(AType, ALen, ASubType: Word): string;
begin
  case AType of
    7: Result := 'SMALLINT';
    8: Result := 'INTEGER';
    //8: Values[i] := ReadInt64(); // INTEGER
    10: Result := 'FLOAT';
    14: // CHAR
    begin
      Result := Format('CHAR(%d)', [ALen]);
      case ASubType of
        3: Result := Result + 'UTF8';
      end;
    end;
    16: // CURRENCY
    begin
      Result := 'INT64';
      if ASubType = 1 then
        Result := 'CURRENCY';  // ???
    end;
    27: Result := 'DOUBLE';  //'INT64';
    35: Result := 'DATETIME';
    37: Result := Format('VARCHAR(%d)', [ALen]);
    261: // BLOB
    begin
      case ASubType of
        0: Result := 'BINARY';
        1: Result := 'TEXT';
        2: Result := 'BLR';
        //3: Result := 'ACL''
        //4: Result := 'RANGES';
        //5: Result := 'SUMMARY';
        //6: Result := 'FORMAT';
        //7: Result := 'TRANSACTION_DESCRIPTION';
        //8: Result := 'EXTERNAL_FILE_DESCRIPTION';
      else
        Result := 'BLOB_' + IntToStr(ASubType);
      end;
      Result := Format('%s(%d)', [Result, ALen]);
    end
  else
    Result := '#'+IntToStr(AType);
  end;
end;

function FieldTypeToDbFieldType(AType, ALen, ASubType: Word): TFieldType;
begin
  case AType of
    7: Result := ftSmallint;
    8: Result := ftInteger;
    //8: Values[i] := ReadInt64(); // INTEGER
    10: Result := ftFloat;
    14: Result := ftString;
    16:
    begin
      Result := ftLargeint;
      if ASubType = 1 then
        Result := ftCurrency;
    end;
    27: Result := ftFloat; // ftLargeint;
    35: Result := ftDateTime;
    37: Result := ftString;
    261: // BLOB
    begin
      case ASubType of
        0: Result := ftBlob;
        1: Result := ftMemo;
        2: Result := ftMemo;
      else
        Result := ftBytes;
      end;
    end
  else
    Result := ftUnknown;
  end;
end;

function DataTypeToStr(AType, ALen, ASubType: Word): string;
begin
  case AType of
    0: Result := 'Unknown';
    1:
    begin
      Result := 'Text';
      case ASubType of
        0: Result := Result + Format(' CHAR(%d)', [ALen]);
        1: Result := Result + '(binary)'; // strings can contain null bytes
        2: Result := Result + '(ASCII)';  // string contains only ASCII characters
        3: Result := Result + '(UTF-8)';  // string represents system metadata
      else
        // sub = codepage
        Result := Result + Format(' CHAR(%d) CP_%d', [ALen, ASubType]);
      end;
    end;
    2: Result := Format('CString(%d)', [ALen]);
    3: Result := Format('Varyng(%d) CP_%d', [ALen, ASubType]);
    // 4, 5 - ?
    6: Result := 'Packed';
    7: Result := 'Byte';
    8: Result := 'Short'; // 'SMALLINT';
    9:
    begin
      Result := 'Long';
      case ASubType of
        0: Result := 'INTEGER';
        1: Result := 'NUMERIC';
        2: Result := 'DECIMAL';
      end;
    end;
    10: Result := 'Quad';
    11: Result := 'Real';
    12: Result := 'Double';
    13: Result := 'DFloat';
    14: Result := 'SqlDate';
    15: Result := 'SqlTime';
    16: Result := 'Timestamp';  // DATETIME
    17: // BLOB
    begin
      case ASubType of
        0: Result := 'BINARY';
        1: Result := 'TEXT';
        2: Result := 'BLR';
      else
        Result := 'BLOB_' + IntToStr(ASubType);
      end;
      Result := 'Blob ' + Result;
    end;
    18: Result := 'Array';
    19: Result := 'Int64';
    //20: Result := '';
  else
    Result := '#'+IntToStr(AType);
  end;

end;


function DataFlagsToStr(AValue: Word): string;
begin
  Result := '[';
  if (AValue and REC_DELETED) > 0 then
    Result := Result + 'del ';
  if (AValue and REC_CHAIN) > 0 then
    Result := Result + 'old ';
  if (AValue and REC_FRAGMENT) > 0 then
    Result := Result + 'frag ';
  if (AValue and REC_INCOMPLETE) > 0 then
    Result := Result + 'incompl ';
  if (AValue and REC_BLOB) > 0 then
    Result := Result + 'blob ';
  if (AValue and REC_STREAM_BLOB) > 0 then
    Result := Result + 'sblob ';
  if (AValue and REC_DELTA) > 0 then
    Result := Result + 'delta ';
  if (AValue and REC_LARGE) > 0 then
    Result := Result + 'large ';
  if (AValue and REC_DAMAGED) > 0 then
    Result := Result + 'damg ';
  if (AValue and REC_GC_ACTIVE) > 0 then
    Result := Result + 'gc ';
  if (AValue and REC_UK_MODIFIED) > 0 then
    Result := Result + 'mod ';
  if (AValue and REC_LONG_TRANUM) > 0 then
    Result := Result + 'tran64 ';
  Result := Trim(Result) + ']';
end;

function RleDecompress(AData: AnsiString; APadding: Boolean = True): AnsiString;
var
  nLen: ShortInt;
  nPos, nDataLen, nResLen: Integer;
begin
  Result := '';
  nDataLen := Length(AData);
  if nDataLen = 0 then Exit;

  nPos := 1;
  nLen := 0;
  while nPos <= nDataLen do
  begin
    Move(AData[nPos], nLen, 1);
    Inc(nPos);
    
    if nLen > 0 then
    begin
      Result := Result + Copy(AData, nPos, nLen);
      Inc(nPos, nLen);
    end
    else if nLen < 0 then
    begin
      nResLen := Length(Result);
      SetLength(Result, nResLen + Abs(nLen));
      FillChar(Result[nResLen + 1], Abs(nLen), AData[nPos]);
      Inc(nPos);
    end
    else
      Break;
  end;
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

{ TDBReaderFB }

procedure TDBReaderFB.AfterConstruction;
begin
  inherited;
  FTableList := TRDBTableList.Create;
  FPagesList := TRDBPageItemList.Create;

  DebugRelID := -1;
  FMaxRecPerPage := 239; // ??
  IsLogFormats := True;

  FBlobRawPageNum := High(Cardinal); // MAXDWORD
end;

procedure TDBReaderFB.BeforeDestruction;
begin
  FreeAndNil(FPagesList);
  FreeAndNil(FTableList);
  inherited;
end;

procedure TDBReaderFB.FillTablesList;
var
  i, ii, iRecField, FieldID, nSize, nOffset: Integer;
  TabRelations: TRDBTable;
  TabFields: TRDBTable;
  TabRelFields: TRDBTable;
  TabFormats: TRDBTable;
  TmpTable: TRDBTable;
  RecRel, RecRelField, RecField, RecFmt: TDbRowItem;
  nRelID: Integer;
  ws: WideString;
begin
  // normalize PageId arrays
  for i := 0 to TableList.Count - 1 do
  begin
    TmpTable := TableList.GetItem(i);
    SetLength(TmpTable.PageIdArr, TmpTable.PageIdCount);
  end;

  TabRelations := TableList.GetByRelationID(SYS_REL_RELATIONS);
  TabFields := TableList.GetByRelationID(SYS_REL_FIELDS);
  TabRelFields := TableList.GetByRelationID(SYS_REL_RELATION_FIELDS);
  TabFormats := TableList.GetByRelationID(SYS_REL_FORMATS);
  if not Assigned(TabRelations)
  or not Assigned(TabFields)
  or not Assigned(TabRelFields)
  or not Assigned(TabFormats)
  then
    Exit;

  TmpTable := nil;
  RecRel := nil;
  for iRecField := 0 to TabRelFields.Count-1 do
  begin
    RecRelField := TabRelFields.GetItem(iRecField);
    // find RecField[0]=FIELD_NAME by RecRelField[2]=FIELD_SOURCE
    RecField := TabFields.FindRecByValue(0, RecRelField.Values[2]);
    if not Assigned(RecField) then
    begin
      LogInfo('! FIELDS.FIELD_NAME not found: ' + RecRelField.Values[2]);
      Continue;
    end;

    // find  RecRel[8]RELATION_NAME by RecRelField[1]RELATION_NAME
    // same RELATION_NAME?
    ws := RecRelField.Values[1];
    if ws = '' then
      ws := RecRelField.Values[2];  // FIELD_SOURCE
    if (not Assigned(RecRel)) or (RecRel.Values[8] <> ws) then
    begin
      RecRel := TabRelations.FindRecByValue(8, ws);
      if not Assigned(RecRel) then
      begin
        LogInfo('! RELATIONS.RELATION_NAME not found: ' + ws);
        Continue;
      end;
    end;

    nRelID := RecRel.Values[3]; // RELATION_ID
    if (not Assigned(TmpTable) or (TmpTable.RelationID <> nRelID)) then
    begin
      TmpTable := TableList.GetByRelationID(nRelID);
      if not Assigned(TmpTable) then
      begin
        //LogInfo('! TmpTable not found');
        //Continue;
        // empty table
        TmpTable := TRDBTable.Create();
        TmpTable.RelationID := nRelID;
        TableList.Add(TmpTable);
      end;
      TmpTable.TableName := RecRel.Values[8]; // [8]RELATION_NAME
      TmpTable.FormatID := RecRel.Values[6]; // [6]FORMAT
    end;

    // set field definitions
    FieldID := RecRelField.Values[9]; // [9]FIELD_ID
    //if IsInterBase then
    //  FieldID := RecRelField.Values[6]; // [6]FIELD_POSITION
    if Length(TmpTable.FieldsInfo) < FieldID+1 then
      SetLength(TmpTable.FieldsInfo, FieldID+1);
    TmpTable.FieldsInfo[FieldID].Name         := RecRelField.Values[0];  // FIELD_NAME
    TmpTable.FieldsInfo[FieldID].FieldLength  := VarToInt(RecField.Values[8]);
    TmpTable.FieldsInfo[FieldID].Scale        := VarToInt(RecField.Values[9]);
    TmpTable.FieldsInfo[FieldID].FieldType    := VarToInt(RecField.Values[10]);
    TmpTable.FieldsInfo[FieldID].FieldSubType := VarToInt(RecField.Values[11]);
    // non-predefined tables
    if TmpTable.Count = 0 then
    begin
      TmpTable.FieldsInfo[FieldID].DType        := 0;
      TmpTable.FieldsInfo[FieldID].Size         := 0;
      TmpTable.FieldsInfo[FieldID].Offset       := 0;
    end;
  end;

  // set fields format (if exists)
  for i := 0 to TabFormats.Count - 1 do
  begin
    RecFmt := TabFormats.GetItem(i);
    nRelID := RecFmt.Values[0];  // [0]RELATION_ID
    TmpTable := TableList.GetByRelationID(nRelID);
    if not Assigned(TmpTable) then
      Continue;
    // format blobs data can not be retrieved at OpenFile(), try to get value as blob
    if (TmpTable.FormatID = VarToInt(RecFmt.Values[1]))
    and (RecFmt is TRDB_RowItem) then
      SetRawFormat(TmpTable, (RecFmt as TRDB_RowItem).GetFieldAsBlob(2)); // [2]DESCRIPTOR
  end;


  // update dataset fields
  for i := 0 to TableList.Count - 1 do
  begin
    TmpTable := TableList.GetItem(i);
    if TmpTable.PageIdCount = 0 then
      TmpTable.RowCount := 0;
    if Length(TmpTable.FieldsDef) > 0 then
      Continue;
    // NullBitmap - 4 bytes minimum, grow by 4 bytes
    nOffset := (((Length(TmpTable.FieldsInfo) div 32) + 1) * 4);

    if TmpTable.RelationID = DebugRelID then
      asm nop end;  // for breakpoint

    SetLength(TmpTable.FieldsDef, Length(TmpTable.FieldsInfo));
    for ii := 0 to Length(TmpTable.FieldsInfo) - 1 do
    begin
      TmpTable.FieldsDef[ii].Name := TmpTable.FieldsInfo[ii].Name;
      TmpTable.FieldsDef[ii].TypeName := TmpTable.FieldsInfo[ii].AsString;
      TmpTable.FieldsDef[ii].FieldType := FieldTypeToDbFieldType(
        TmpTable.FieldsInfo[ii].FieldType,
        TmpTable.FieldsInfo[ii].FieldLength,
        TmpTable.FieldsInfo[ii].FieldSubType);

      nSize := FieldTypeToSize(
          TmpTable.FieldsInfo[ii].FieldType,
          TmpTable.FieldsInfo[ii].FieldLength,
          TmpTable.FieldsInfo[ii].FieldSubType);
      TmpTable.FieldsDef[ii].Size := TmpTable.FieldsInfo[ii].FieldLength;
      if TmpTable.FieldsDef[ii].Size = 0 then
        TmpTable.FieldsDef[ii].Size := nSize;
      if TmpTable.FieldsDef[ii].Size = 0 then
        TmpTable.FieldsDef[ii].Size := TmpTable.FieldsInfo[ii].GetSize();

      TmpTable.FieldsDef[ii].RawOffset := TmpTable.FieldsInfo[ii].Offset;
      if TmpTable.FieldsDef[ii].RawOffset = 0 then
        TmpTable.FieldsDef[ii].RawOffset := nOffset;

      Inc(nOffset, nSize);
    end;
  end;
  FIsMetadataReaded := True;
end;

function TDBReaderFB.FindPageNum(APageType: Word; ARelationID, ASequence: Integer; out APageNum: Cardinal): Boolean;
var
  i: Integer;
  M: TRDBPageItem;
begin
  Result := False;
  // binary search
  i := FPagesList.FindPageIndex(APageType, ARelationID, ASequence);
  if i >= 0 then
  begin
    M := FPagesList.GetItem(i);
    APageNum := M.PageNum;
    Result := True;
  end;
end;

function TDBReaderFB.GetBlobData(ARelationID, ARowID: Cardinal; var AData: AnsiString): Boolean;
var
  PageSeq, RowIndex: Integer;
  PageNum: Cardinal;
  nPos: Int64;
begin
  Result := False;
  PageSeq := ARowID div FMaxRecPerPage;
  RowIndex := ARowID mod FMaxRecPerPage;
  if FindPageNum(PAGE_TYPE_DATA, ARelationID, PageSeq, PageNum) then
  begin
    if FBlobRawPageNum <> PageNum then
    begin
      // read raw page
      nPos := Int64(PageNum) * FPageSize;
      if nPos < (FFile.Size - FPageSize) then
      begin
        FFile.Position := nPos;
        FFile.Read(FBlobRawPage, FPageSize);
        FBlobRawPageNum := PageNum;
      end
      else
        AData := Format('<blob PageID=%d out of range>', [PageNum]);
    end;

    if FBlobRawPageNum = PageNum then
      Result := GetBlobFromPage(FBlobRawPage, RowIndex, AData);
  end
  else
    AData := Format('<blob page not found! RelID=%d PageSeq=%d >', [ARelationID, PageSeq]);
end;

function TDBReaderFB.GetBlobFromPage(const ARawPage: TByteArray; AIndex: Integer; var AData: AnsiString): Boolean;
var
  DataPageHead: TFBDataPageHead;
  DataPageRecs: array of TFBDataPageRec;
  i, iOffs, iDataHeadLen, iDataOffs, iDataLen: Integer;
  BlobRecHead: TFBBlobRecHead;
begin
  Result := False;
  // Get header
  Move(ARawPage, DataPageHead, SizeOf(DataPageHead));
  //Assert(DataPageHead.PageHead.PageType = PAGE_TYPE_DATA, 'Not data page');
  if DataPageHead.PageHead.PageType <> PAGE_TYPE_DATA then
  begin
    AData := Format('<blob page is not data page>', []);
    Exit;
  end;
  if DataPageHead.Count = 0 then
  begin
    //Assert(DataPageHead.Count > 0, 'No data on page');
    AData := Format('<blob page empty>', []);
    LogInfo(Format('! No data on page for blob[%d]', [AIndex]));
    Exit;
  end;
  if AIndex >= DataPageHead.Count then
  begin
    AData := Format('<blob RecID=%d Count=%d>', [AIndex, DataPageHead.Count]);
    LogInfo(Format('! Blob Rec Index=%d more than Count=%d', [AIndex, DataPageHead.Count]));
    Exit;
  end;
  // read rows list
  DataPageRecs := [];
  SetLength(DataPageRecs, DataPageHead.Count);
  iOffs := SizeOf(DataPageHead);
  Move(ARawPage[iOffs], DataPageRecs[0], DataPageHead.Count * SizeOf(TFBDataPageRec));
  // get row
  i := AIndex;
  Move(ARawPage[DataPageRecs[i].Offset], BlobRecHead, SizeOf(TFBBlobRecHead));
  //Assert((BlobRecHead.Flags and REC_BLOB) <> 0, 'Rec is not blob');
  if (BlobRecHead.Flags and REC_BLOB) = 0 then
  begin
    AData := '<rec is not blob>';
    Exit;
  end;

  iDataHeadLen := SizeOf(BlobRecHead);
  iDataOffs := DataPageRecs[i].Offset + iDataHeadLen;
  iDataLen := DataPageRecs[i].Length - iDataHeadLen;

  // align
  //iDataOffs := iDataOffs + (iDataOffs mod 4);

  // get data
  if iDataLen > 0 then
  begin
    //Inc(iDataOffs, SizeOf(BlobRecHead) - 4);
    if BlobRecHead.SubType = 1 then  // TEXT
    begin
      Inc(iDataOffs, 2);
      Dec(iDataLen, 2);
    end;
    if BlobRecHead.SubType = 2 then  // BLR
    begin
      Inc(iDataOffs, 9);
      Dec(iDataLen, 9);
    end;
    if BlobRecHead.SubType = 3 then
    begin
      Inc(iDataOffs, 6);
      Dec(iDataLen, 6);
    end;
    if BlobRecHead.SubType = 5 then
    begin
      Inc(iDataOffs, 8);
      Dec(iDataLen, 8);
    end;

    if iDataLen > 0 then
    begin
      SetLength(AData, iDataLen);
      Move(ARawPage[iDataOffs], AData[1], iDataLen);
    end;
    Result := True;
  end;
end;

function TDBReaderFB.GetFragmentData(APageNum, ALineID: Cardinal;
  ARecursionID: Integer): AnsiString;
var
  nPos: Int64;
  DataPageHead: TFBDataPageHead;
  DataPageRecs: array of TFBDataPageRec;
  DataRecHead: TFBDataRecHead;
  //DataFragRecHead: TFBDataFragRecHead;
  i, iOffs, iDataHeadLen, iDataOffs, iDataLen: Integer;
  //s: string;
begin
  Result := '';
  if FBlobRawPageNum <> APageNum then
  begin
    // read raw page
    nPos := Int64(APageNum) * FPageSize;
    if nPos < (FFile.Size - FPageSize) then
    begin
      FFile.Position := nPos;
      FFile.Read(FBlobRawPage, FPageSize);
      FBlobRawPageNum := APageNum;
    end;
  end;
  if FBlobRawPageNum <> APageNum then Exit;

  // Get header
  Move(FBlobRawPage, DataPageHead, SizeOf(DataPageHead));
  Assert(DataPageHead.PageHead.PageType = PAGE_TYPE_DATA, 'Not data page');
  if DataPageHead.PageHead.PageType <> PAGE_TYPE_DATA then
    Exit;
  Assert(DataPageHead.Count > 0, 'No data on page');
  if DataPageHead.Count = 0 then
    Exit;
  Assert(ALineID < DataPageHead.Count, 'Rec Index more than Count');
  if ALineID >= DataPageHead.Count then
    Exit;
  // read rows list
  DataPageRecs := [];
  SetLength(DataPageRecs, DataPageHead.Count);
  iOffs := SizeOf(DataPageHead);
  Move(FBlobRawPage[iOffs], DataPageRecs[0], DataPageHead.Count * SizeOf(TFBDataPageRec));
  // get row
  i := ALineID;
  Move(FBlobRawPage[DataPageRecs[i].Offset], DataRecHead, SizeOf(DataRecHead));
  Assert((DataRecHead.Flags and REC_FRAGMENT) <> 0, 'Rec is not fragment');
  if (DataRecHead.Flags and REC_FRAGMENT) = 0 then
    Exit;

  iDataHeadLen := SizeOf(DataRecHead);
  iDataOffs := DataPageRecs[i].Offset + iDataHeadLen;
  iDataLen := DataPageRecs[i].Length - iDataHeadLen;

  if iDataLen <> 0 then
  begin
    SetLength(Result, iDataLen);
    Move(FBlobRawPage[iDataOffs], Result[1], iDataLen);
    Result := RleDecompress(Result);
  end;

  {s := Result;
  if Length(s) > 0 then
    s := DataAsStr(s[1], Length(s));
  LogInfo(Format('FRAG Page=%d  RowID=%d BackPag:Lin=%d:%d Flags=%s Fmt=%d  Raw(%d)=%s Data(%d)=%s%s',
    [APageNum,
     (DataPageHead.Sequence * FMaxRecPerPage) + Cardinal(i),
     DataRecHead.b_page, DataRecHead.b_line,
     DataFlagsToStr(DataRecHead.flags),
     DataRecHead.format,
     iDataLen, BufferToHex(FBlobRawPage[iDataOffs], 4),
     Length(Result), BufferToHex(Result[1], 4), s
    ]));    }
  
end;

function TDBReaderFB.GetTableByIndex(AIndex: Integer): TDbRowsList;
begin
  Result := TableList.GetItem(AIndex);
end;

function TDBReaderFB.GetTablesCount: Integer;
begin
  Result := TableList.Count;
end;

function TDBReaderFB.GetRelationName(ARelID: Integer): string;
var
  i: Integer;
  TmpItem: TRDBTable;
begin
  Result := '';
  for i := 0 to TableList.Count - 1 do
  begin
    TmpItem := TableList.GetItem(i);
    if TmpItem.RelationID = ARelID then
    begin
      Result := TmpItem.TableName;
      Exit;
    end;
  end;
end;

procedure TDBReaderFB.InitSystemTables;
var
  TmpTab: TRDBTable;
begin
  TmpTab := TRDBTable.Create();
  TableList.Add(TmpTab);
  with TmpTab do
  begin
    RelationID := SYS_REL_PAGES;
    TableName := 'RDB$PAGES';
    // fb11
    AddFieldDef('PAGE_NUMBER', DTYPE_LONG, 4, 0);   // INTEGER
    AddFieldDef('RELATION_ID', DTYPE_SHORT, 2, 0);  // SMALLINT
    AddFieldDef('PAGE_SEQUENCE', DTYPE_LONG, 4, 0); // INTEGER
    AddFieldDef('PAGE_TYPE', DTYPE_SHORT, 2, 0);    // SMALLINT
  end;

  TmpTab := TRDBTable.Create();
  TableList.Add(TmpTab);
  with TmpTab do
  begin
    RelationID := SYS_REL_FIELDS;
    TableName := 'RDB$FIELDS';
    if IsInterBase then
    begin
      if OdsVersion = 10 then
      begin
        AddFieldDef('FIELD_NAME', DTYPE_CSTRING, 32, 0);     // CHAR(31)
        AddFieldDef('QUERY_NAME', DTYPE_CSTRING, 32, 0);     // CHAR(31)
      end
      else
      begin
        AddFieldDef('FIELD_NAME', DTYPE_CSTRING, 68, 0);     // CHAR(67)
        AddFieldDef('QUERY_NAME', DTYPE_CSTRING, 68, 0);     // CHAR(67)
      end;
      AddFieldDef('VALIDATION_BLR', DTYPE_BLOB, 8, 2);     // BLOB_2
      AddFieldDef('VALIDATION_SOURCE', DTYPE_TEXT, 8, 1); // BLOB_TEXT   8
      AddFieldDef('COMPUTED_BLR', DTYPE_BLOB, 8, 2);       // BLOB_2
      AddFieldDef('COMPUTED_SOURCE', DTYPE_TEXT, 8, 1);   // BLOB_TEXT   8
      AddFieldDef('DEFAULT_VALUE', DTYPE_BLOB, 8, 2);      // BLOB_2
      AddFieldDef('DEFAULT_SOURCE', DTYPE_TEXT, 8, 1);    // BLOB_TEXT   8
      if OdsVersion = 10 then
        DataPos := 116
      else
        DataPos := 188;  // pos 188
    end
    else // FireBird
    begin
      AddFieldDef('FIELD_NAME', DTYPE_CSTRING, 31, 0);     // CHAR(31)
      AddFieldDef('QUERY_NAME', DTYPE_CSTRING, 31, 0);     // CHAR(31)
      AddFieldDef('VALIDATION_BLR', DTYPE_BLOB, 4, 2);     // BLOB_2
      AddFieldDef('VALIDATION_SOURCE', DTYPE_TEXT, 14, 1); // BLOB_TEXT   14
      AddFieldDef('COMPUTED_BLR', DTYPE_BLOB, 4, 2);       // BLOB_2
      AddFieldDef('COMPUTED_SOURCE', DTYPE_TEXT, 14, 1);   // BLOB_TEXT   14
      AddFieldDef('DEFAULT_VALUE', DTYPE_BLOB, 4, 2);      // BLOB_2
      AddFieldDef('DEFAULT_SOURCE', DTYPE_TEXT, 14, 1);    // BLOB_TEXT    14
    end;

    AddFieldDef('FIELD_LENGHT', DTYPE_SHORT, 2, 0);      // SMALLINT
    AddFieldDef('FIELD_SCALE', DTYPE_SHORT, 2, 0);       // SMALLINT
    AddFieldDef('FIELD_TYPE', DTYPE_SHORT, 2, 0);        // SMALLINT
    AddFieldDef('FIELD_SUB_TYPE', DTYPE_SHORT, 2, 0);    // SMALLINT
    AddFieldDef('MISSING_VALUE', DTYPE_BLOB, 4, 2);      // BLOB_2
    AddFieldDef('MISSING_SOURCE', DTYPE_BLOB, 10, 1);    // BLOB_TEXT  10
    AddFieldDef('DESCRIPTION', DTYPE_BLOB, 10, 1);       // BLOB_TEXT  10
    AddFieldDef('SYSTEM_FLAG', DTYPE_SHORT, 2, 0);       // SMALLINT
    AddFieldDef('QUERY_HEADER', DTYPE_TEXT, 14, 1);      // BLOB_TEXT  14
    AddFieldDef('SEGMENT_LENGTH', DTYPE_SHORT, 2, 0);    // SMALLINT
    AddFieldDef('EDIT_STRING', DTYPE_VARYNG, 126, 0);    // VARCHAR(125) + 3
    AddFieldDef('EXTERNAL_LENGTH', DTYPE_SHORT, 2, 0);   // SMALLINT
    AddFieldDef('EXTERNAL_SCALE', DTYPE_SHORT, 2, 0);    // SMALLINT
    AddFieldDef('EXTERNAL_TYPE', DTYPE_SHORT, 2, 0);     // SMALLINT
    AddFieldDef('DIMENSIONS', DTYPE_SHORT, 2, 0);        // SMALLINT
    AddFieldDef('NULL_FLAG', DTYPE_SHORT, 2, 0);         // SMALLINT
    AddFieldDef('CHARACTER_LENGTH', DTYPE_SHORT, 2, 0);  // SMALLINT
    AddFieldDef('COLLATION_ID', DTYPE_SHORT, 2, 0);      // SMALLINT
    AddFieldDef('CHARACTER_SET_ID', DTYPE_SHORT, 2, 0);  // SMALLINT
    AddFieldDef('FIELD_PRECISION', DTYPE_SHORT, 2, 0);   // SMALLINT
  end;

  TmpTab := TRDBTable.Create();
  TableList.Add(TmpTab);
  with TmpTab do
  begin
    RelationID := SYS_REL_RELATION_FIELDS;
    TableName := 'RDB$RELATION_FIELDS';
    if IsInterBase and (OdsVersion = 11) then
    begin
      AddFieldDef('FIELD_NAME', DTYPE_CSTRING, 67, 0);     // CHAR(67)UTF8
      AddFieldDef('RELATION_NAME', DTYPE_CSTRING, 67, 0);  // CHAR(67)UTF8
      AddFieldDef('FIELD_SOURCE', DTYPE_CSTRING, 67, 0);   // CHAR(67)UTF8
      AddFieldDef('QUERY_NAME', DTYPE_CSTRING, 67, 0);     // CHAR(67)UTF8
      AddFieldDef('BASE_FIELD', DTYPE_CSTRING, 67, 0);     // CHAR(67)UTF8
    end
    else // FireBird
    begin
      AddFieldDef('FIELD_NAME', DTYPE_CSTRING, 31, 0);     // CHAR(31)
      AddFieldDef('RELATION_NAME', DTYPE_CSTRING, 31, 0);  // CHAR(31)
      AddFieldDef('FIELD_SOURCE', DTYPE_CSTRING, 31, 0);   // CHAR(31)
      AddFieldDef('QUERY_NAME', DTYPE_CSTRING, 31, 0);     // CHAR(31)
      AddFieldDef('BASE_FIELD', DTYPE_CSTRING, 31, 0);     // CHAR(31)
    end;
    AddFieldDef('EDIT_STRING', DTYPE_VARYNG, 127, 0);    // VARCHAR(125) + 4
    AddFieldDef('FIELD_POSITION', DTYPE_SHORT, 2, 0);    // SMALLINT
    if IsInterBase and (OdsVersion = 11) then
      AddFieldDef('QUERY_HEADER', DTYPE_TEXT, 10, 1)        // BLOB_TEXT
    else
      AddFieldDef('QUERY_HEADER', DTYPE_TEXT, 14, 1);      // BLOB_TEXT
    AddFieldDef('UPDATE_FLAG', DTYPE_SHORT, 2, 0);       // SMALLINT
    AddFieldDef('FIELD_ID', DTYPE_SHORT, 2, 0);          // SMALLINT     (unique for RelationName)
    AddFieldDef('VIEW_CONTEXT', DTYPE_SHORT, 2, 0);      // SMALLINT
    AddFieldDef('DESCRIPTION', DTYPE_TEXT, 14, 1);       // BLOB_TEXT
    AddFieldDef('DEFAULT_VALUE', DTYPE_BLOB, 4, 2);      // BLOB_BLR
    AddFieldDef('SYSTEM_FLAG', DTYPE_SHORT, 2, 0);       // SMALLINT
    AddFieldDef('SECURITY_CLASS', DTYPE_CSTRING, 31, 0); // CHAR(31)
    AddFieldDef('COMPLEX_NAME', DTYPE_CSTRING, 31, 0);   // CHAR(31)
    AddFieldDef('NULL_FLAG', DTYPE_SHORT, 2, 0);         // SMALLINT
    AddFieldDef('DEFAULT_SOURCE', DTYPE_TEXT, 14, 1);    // BLOB_TEXT
    AddFieldDef('COLLATION_ID', DTYPE_SHORT, 2, 0);      // SMALLINT
  end;

  TmpTab := TRDBTable.Create();
  TableList.Add(TmpTab);
  with TmpTab do
  begin
    RelationID := SYS_REL_RELATIONS;
    TableName := 'RDB$RELATIONS';
    if IsInterBase then
    begin
      AddFieldDef('VIEW_BLR', DTYPE_BLOB, 8, 1);     // BLOB TEXT
      AddFieldDef('VIEW_SOURCE', DTYPE_BLOB, 8, 1);  // BLOB TEXT
      AddFieldDef('DESCRIPTION', DTYPE_BLOB, 8, 1);  // BLOB TEXT
    end
    else // FireBird
    begin
      AddFieldDef('VIEW_BLR', DTYPE_BLOB, 4, 2);     // BLOB BLR
      AddFieldDef('VIEW_SOURCE', DTYPE_BLOB, 8, 1);  // BLOB TEXT
      AddFieldDef('DESCRIPTION', DTYPE_BLOB, 8, 1);  // BLOB TEXT
      Inc(DataPos, 8);               // skip 8
    end;
    AddFieldDef('RELATION_ID', DTYPE_SHORT, 2, 0);
    AddFieldDef('SYSTEM_FLAG', DTYPE_SHORT, 2, 0);
    AddFieldDef('DBKEY_LENGTH', DTYPE_SHORT, 2, 0);
    AddFieldDef('FORMAT', DTYPE_SHORT, 2, 0);
    AddFieldDef('FIELD_ID', DTYPE_SHORT, 2, 0);
    AddFieldDef('RELATION_NAME', DTYPE_CSTRING, 31, 0);
    AddFieldDef('SECURITY_CLASS', DTYPE_CSTRING, 31, 0);
    AddFieldDef('EXTERNAL_FILE', DTYPE_VARYNG, 257, 0); // CHAR(255)
    AddFieldDef('RUNTIME', DTYPE_BLOB, 8, 5);
    AddFieldDef('EXTERNAL_DESCRIPTION', DTYPE_BLOB, 8, 8);
    if not IsInterBase then
      Inc(DataPos, 8);               // skip 8
    AddFieldDef('OWNER_NAME', DTYPE_CSTRING, 31, 0);
    AddFieldDef('DEFAULT_CLASS', DTYPE_CSTRING, 31, 0);
    AddFieldDef('FLAGS', DTYPE_SHORT, 2, 0);
    AddFieldDef('RELATION_TYPE', DTYPE_SHORT, 2, 0);
  end;

  TmpTab := TRDBTable.Create();
  TableList.Add(TmpTab);
  with TmpTab do
  begin
    RelationID := SYS_REL_FORMATS;
    TableName := 'RDB$FORMATS';
    AddFieldDef('RELATION_ID', DTYPE_SHORT, 2, 0); // SMALLINT
    AddFieldDef('FORMAT', DTYPE_SHORT, 2, 0);      // SMALLINT
    AddFieldDef('DESCRIPTOR', DTYPE_BLOB, 8, 6);   // BLOB SUB_TYPE 6
  end;
end;

function TDBReaderFB.FillTableInfoText(ATableName: string; ALines: TStrings): Boolean;
var
  i: Integer;
  TmpTable: TRDBTable;
  TmpField: TRDB_FieldInfoRec;
  s: string;
begin
  Result := False;
  if not Assigned(ALines) then Exit;
  
  // table info
  TmpTable := nil;
  for i := 0 to TableList.Count - 1 do
  begin
    TmpTable := TableList.GetItem(i);
    if TmpTable.TableName = ATableName then
      Break;
    TmpTable := nil;
  end;
  if Assigned(TmpTable) then
  begin
    with ALines do
    begin
      Add(Format('== Table RelationID=%d  Name=%s ', [TmpTable.RelationID, TmpTable.TableName]));
      Add(Format('Format=%d', [TmpTable.FormatID]));
      {Add(Format('RelationType=%d', [TmpTable.RelationType]));
      Add(Format('SystemFlag=%d', [TmpTable.SystemFlag]));
      Add(Format('Flags=%d', [TmpTable.Flags]));
      Add(Format('OwnerName=%s', [TmpTable.OwnerName]));
      Add(Format('Runtime=%s', [TmpTable.Runtime]));
      Add(Format('DefaultClass=%s', [TmpTable.DefaultClass]));
      Add(Format('ExternalFile=%s', [TmpTable.ExternalFile]));
      Add(Format('SecurityClass=%s', [TmpTable.SecurityClass]));
      Add(Format('FieldID=%d', [TmpTable.FieldID]));
      Add(Format('Format=%d', [TmpTable.Format]));
      Add(Format('DBKeyLen=%d', [TmpTable.DBKeyLen]));
      Add(Format('ViewBlr=%s', [TmpTable.GetBlobValue(TmpTable.ViewBlr)]));
      Add(Format('ViewSource=%s', [TmpTable.GetBlobValue(TmpTable.ViewSource)])); }
      Add(Format('== Fields  Count=%d', [Length(TmpTable.FieldsInfo)]));
    end;
    for i := 0 to Length(TmpTable.FieldsInfo) - 1 do
    begin
      TmpField := TmpTable.FieldsInfo[i];
      s := Format('%.2d Name=%s  Type=%s', [i, TmpTable.FieldsInfo[i].Name, TmpTable.FieldsInfo[i].AsString]);
      if TmpTable.FieldsInfo[i].FieldType <> 0 then
      begin
        s := s + Format('  Field=(%s Type=%d.%d Len=%d)',
          [FieldTypeToStr(TmpTable.FieldsInfo[i].FieldType, TmpTable.FieldsInfo[i].FieldLength, TmpTable.FieldsInfo[i].FieldSubType),
           TmpTable.FieldsInfo[i].FieldType,
           TmpTable.FieldsInfo[i].FieldSubType,
           TmpTable.FieldsInfo[i].FieldLength
          ]);
      end;
      if TmpTable.FieldsInfo[i].DType <> 0 then
      begin
        s := s + Format('  Format=(%s  DType=%d.%d  Len=%d  Flags=%d  Offs=%d)',
          [DataTypeToStr(TmpTable.FieldsInfo[i].DType, TmpTable.FieldsInfo[i].Length, TmpTable.FieldsInfo[i].SubType),
           TmpTable.FieldsInfo[i].DType,
           TmpTable.FieldsInfo[i].SubType,
           TmpTable.FieldsInfo[i].Length,
           TmpTable.FieldsInfo[i].Flags,
           TmpTable.FieldsInfo[i].Offset
          ]);
      end;
      ALines.Add(s);
    end;
    {if Assigned(TmpTable._Format) then
      ALines.Add(Format('== Format  Length=%d', [Length(TmpTable._Format.Descriptor.Data)])); }
    Result := True;
  end;
end;

function TDBReaderFB.OpenFile(AFileName: string; AStream: TStream): Boolean;
var
  nPage: Integer;
  nPos: Int64;
  PageHead: TFBPageHead;
  OdsHeader: TFBOdsPageHead;
  //PtrPageHead: pointer_page;
  //PtrPageNumbers: array of Cardinal;
  //BlobPageHead: TFBBlobPageHead;
  RawPage: TByteArray;
  s: string;
begin
  Result := inherited OpenFile(AFileName, AStream);
  if not Result then Exit;
  FPagesList.Clear();
  TableList.Clear();

  // read header page
  FFile.Read(OdsHeader, SizeOf(OdsHeader));
  FPageSize := OdsHeader.PageSize;
  FOdsVersion := (OdsHeader.OdsVersion and $0F);

  FMaxRecPerPage := (FPageSize - SizeOf(TFBDataPageHead)) div (SizeOf(TFBDataPageRec) + SizeOf(TFBDataRecHead));
  {case FPageSize of
    1024: FMaxRecPerPage := 58;
    2048: FMaxRecPerPage := 119;
    4096: FMaxRecPerPage := 239;
    8192: FMaxRecPerPage := 480;
  end;}

  LogInfo(Format('=== %d %s ===', [0, OdsPageTypeToStr(OdsHeader.PageHead.PageType)]));
  LogInfo(Format('PageSize=%d', [OdsHeader.PageSize]));
  s := Format('ODS Version=%d.%d', [(OdsHeader.OdsVersion and $0F), ((OdsHeader.OdsVersion and $7FF0) shr 4)]);
  FIsInterBase := (OdsHeader.OdsVersion and $8000) = 0;
  if FIsInterBase then
    s := s + ' (InterBase)'
  else
    s := s + ' (FireBird)';
  LogInfo(s);
  if IsLogPages then
  begin
    LogInfo(Format('PAGES=%d', [OdsHeader.PagesPage]));
    LogInfo(Format('Next Header=%d', [OdsHeader.NextPage]));
  end;

  InitSystemTables();  // after database version detection

  // pages list
  nPage := 1;
  nPos := nPage * FPageSize;
  while nPos < (FFile.Size - 1024) do
  begin
    FFile.Position := nPos;
    FFile.Read(RawPage, FPageSize);
    Move(RawPage, PageHead, SizeOf(PageHead));
    if IsLogPages then
      LogInfo(Format('=== %d %s ===', [nPage, OdsPageTypeToStr(PageHead.PageType)]));

    case PageHead.PageType of
      {PAGE_TYPE_POINTER:
      begin
        // read database pointer page
        Move(RawPage, PtrPageHead, SizeOf(PtrPageHead));
        if IsLogPages then
        begin
          LogInfo(Format('Relation ID=%d', [PtrPageHead.ppg_relation]));
          LogInfo(Format('Next PTR Page=%d', [PtrPageHead.ppg_next]));
          LogInfo(Format('Count=%d', [PtrPageHead.ppg_count]));
        end;

        if PtrPageHead.ppg_count > 0 then
        begin
          SetLength(PtrPageNumbers, PtrPageHead.ppg_count);
          iOffs := SizeOf(PtrPageHead) - SizeOf(Cardinal);
          Move(RawPage[iOffs], PtrPageNumbers[0], PtrPageHead.ppg_count * SizeOf(Cardinal));

          for i := 0 to PtrPageHead.ppg_count - 1 do
          begin
            if IsLogPages then
              LogInfo(Format('Page Index=%d', [PtrPageNumbers[i]]));
          end;
        end;
      end; }

      PAGE_TYPE_DATA:
      begin
        // read data page
        ReadDataPage(RawPage, nPos);
      end;

      {PAGE_TYPE_BLOB:
      begin
        // read blob page
        Move(RawPage, BlobPageHead, SizeOf(BlobPageHead));
        if BlobPageHead.Length > 0 then
        begin
          if (BlobPageHead.PageHead.Flags and BLOB_PAGE_FLAG_POINTERS) > 0 then
          begin
            // page with pointers to blobs
            SetLength(PtrPageNumbers, BlobPageHead.Length div 4);
            Move(RawPage[SizeOf(BlobPageHead)], PtrPageNumbers[0], BlobPageHead.Length);
            if IsLogBlobs then
              LogInfo(Format('blob lead=%d seq=%d len=%d count=%d',
                [BlobPageHead.LeadPage, BlobPageHead.Sequence, BlobPageHead.Length, Length(PtrPageNumbers)]));
          end
          else
          begin
            // page with blob
            SetLength(s, BlobPageHead.Length);
            Move(RawPage[SizeOf(BlobPageHead)], s[1], BlobPageHead.Length);
            if IsLogBlobs then
              LogInfo(Format('blob lead=%d seq=%d data=%s',
                [BlobPageHead.LeadPage, BlobPageHead.Sequence, DataAsStr(s[1], Length(s))]));
          end;
        end;
        //s := GetRelationName(DataPageHead.dpg_relation);
        //if IsLogPages then
        //  LogInfo(Format('Seq=%d  Rel=%d (%s) Count=%d', [DataPageHead.dpg_sequence, DataPageHead.dpg_relation, DataPageHead.dpg_count]));

      end;  }
    end;


    Inc(nPage);
    nPos := nPage * FPageSize;
    //if nPage > 1000 then Break;

  end;
  FillTablesList();
  TableList.SortByName();
end;

function TDBReaderFB.ReadDataPage(const APageBuf: TByteArray; APagePos: Int64; ATable: TRDBTable;
  AList: TDbRowsList): Boolean;
var
  DataPageHead: TFBDataPageHead;
  DataRecHead: TFBDataRecHead;
  i, nPage, iPageIndex, iOffs: Integer;
  iDataHeadLen, iDataOffs, iDataLen: Integer;
  TmpPageItem: TRDBPageItem;
  TmpTable: TRDBTable;
  s: string;
  DataPageRecs: array of TFBDataPageRec;
  //CurRowID: Cardinal;
  DataFragRecHead: TFBDataFragRecHead;
  //BlobRecHead: TFBBlobRecHead;
  //iLen: Integer;
  TableRowItem: TRDB_RowItem;
begin
  Result := False;
  DataPageRecs := [];
  // read data page
  nPage := APagePos div FPageSize;
  Move(APageBuf, DataPageHead, SizeOf(DataPageHead));
  if DataPageHead.PageHead.PageType <> PAGE_TYPE_DATA then Exit;
  if Assigned(ATable) and (DataPageHead.RelationID <> ATable.RelationID) then Exit;

  if IsLogPages and (DataPageHead.RelationID = DebugRelID) then
  begin
    s := GetRelationName(DataPageHead.RelationID);
    LogInfo(Format('=== #%d Seq=%d  Rel=%d (%s) Count=%d ===', [nPage, DataPageHead.Sequence, DataPageHead.RelationID, s, DataPageHead.Count]));
  end;

  if not FIsMetadataReaded then
  begin
    // add to pages list
    iPageIndex := FPagesList.FindPageIndex(DataPageHead.PageHead.PageType, DataPageHead.RelationID, DataPageHead.Sequence);
    if iPageIndex < 0 then
    begin
      TmpPageItem := TRDBPageItem.Create();
      TmpPageItem.PageType := DataPageHead.PageHead.PageType;
      TmpPageItem.PageNum := nPage;
      TmpPageItem.RelationID := DataPageHead.RelationID;
      TmpPageItem.PageSeq := DataPageHead.Sequence;
      FPagesList.Insert(-iPageIndex-1, TmpPageItem);
    end;

    TmpTable := TableList.GetByRelationID(DataPageHead.RelationID);
    if not Assigned(TmpTable) then
    begin
      TmpTable := TRDBTable.Create();
      TmpTable.RelationID := DataPageHead.RelationID;
      TmpTable.TableName := Format('RelationID=%d', [TmpTable.RelationID]);
      TableList.Add(TmpTable);
    end;
    // store PageNum
    Inc(TmpTable.PageIdCount);
    if TmpTable.PageIdCount >= Length(TmpTable.PageIdArr) then
      SetLength(TmpTable.PageIdArr, Length(TmpTable.PageIdArr) + 32);
    TmpTable.PageIdArr[TmpTable.PageIdCount-1] := nPage;
    // skip reading data non-system data for first time
    //if Length(TmpTable.FieldsInfo) = 0 then
    if DataPageHead.RelationID >= 40 then
    begin
      Result := True;
      Exit;
    end;
    ATable := TmpTable;
    AList := TmpTable;
  end;

  if DataPageHead.Count > 0 then
  begin
    SetLength(DataPageRecs, DataPageHead.Count);
    iOffs := SizeOf(DataPageHead);
    Move(APageBuf[iOffs], DataPageRecs[0], DataPageHead.Count * SizeOf(TFBDataPageRec));

    for i := 0 to DataPageHead.Count - 1 do
    begin
      if DataPageRecs[i].Length = 0 then
        Continue;
      //CurRowID := (DataPageHead.Sequence * FMaxRecPerPage) + Cardinal(i);  // ??

      Move(APageBuf[DataPageRecs[i].Offset], DataRecHead, SizeOf(DataRecHead));
      iDataHeadLen := SizeOf(DataRecHead);
      iDataOffs := DataPageRecs[i].Offset + iDataHeadLen;
      iDataLen := DataPageRecs[i].Length - iDataHeadLen;
      s := '';
      if (iDataLen > 0) and (DataRecHead.flags = 0) then
      begin
        SetLength(s, iDataLen);
        Move(APageBuf[iDataOffs], s[1], iDataLen);
        s := RleDecompress(s);

        if Assigned(ATable) then
        begin
          TableRowItem := TRDB_RowItem.Create(AList, Self, DataPageHead.RelationID, (DataPageHead.Sequence * FMaxRecPerPage) + Cardinal(i));
          TableRowItem.FormatID := DataRecHead.format;
          //TableRowItem._RowID := (DataPageHead.Sequence * FMaxRecPerPage) + i;
          //TableRowItem.ReadRawData(RawPage[DataPageRecs[i].Offset], DataPageRecs[i].Length);
          if (DataRecHead.flags = 0) then
          begin
            // data record
            try
              TableRowItem.ReadData(ATable, s);
            except
              on E: Exception do
                LogInfo('! row read error: ' + E.Message);
            end;
          end;

          if Assigned(AList) then
            AList.Add(TableRowItem)
          else
          begin
            //LogInfo(TableRowItem.GetAsText);
            TableRowItem.Free();
          end;
        end;
      end
      else
      if (DataRecHead.flags and REC_INCOMPLETE) > 0 then
      begin
        // incomplete
        Move(APageBuf[DataPageRecs[i].Offset], DataFragRecHead, SizeOf(DataFragRecHead));
        iDataHeadLen := SizeOf(DataFragRecHead);
        iDataOffs := DataPageRecs[i].Offset + iDataHeadLen;
        iDataLen := DataPageRecs[i].Length - iDataHeadLen;
        if iDataLen > 0 then
        begin
          SetLength(s, iDataLen);
          Move(APageBuf[iDataOffs], s[1], iDataLen);
          s := RleDecompress(s);
        end;
        {LogInfo(Format('INCOMPLETE Page=%d  RowID=%d  Flags=%s Fmt=%d  FragPage:Line=%d:%d  Unused=%d  Raw(%d)=%s Data(%d)=%s%s',
          [nPage,
           (DataPageHead.Sequence * FMaxRecPerPage) + Cardinal(i),
           DataFlagsToStr(DataFragRecHead.flags),
           DataFragRecHead.format,
           DataFragRecHead.f_page, DataFragRecHead.f_line,
           DataFragRecHead.Unused,
           iDataLen, BufferToHex(RawPage[iDataOffs], 4),
           Length(s), BufferToHex(s[1], 4), DataAsStr(s[1], Length(s))
          ]));
        LogInfo(Format('INCOMPLETE Raw(%d)=%s', [iDataLen, BufferToHex(RawPage[iDataOffs], iDataLen)]));  }
        s := s + GetFragmentData(DataFragRecHead.f_page, DataFragRecHead.f_line);

        if Assigned(ATable) then
        begin
          // read data to item
          TableRowItem := TRDB_RowItem.Create(AList, Self, DataPageHead.RelationID, (DataPageHead.Sequence * FMaxRecPerPage) + Cardinal(i));
          TableRowItem.ReadData(ATable, s);
          if Assigned(AList) then
            AList.Add(TableRowItem)
          else
          begin
            LogInfo(TableRowItem.GetAsText);
            TableRowItem.Free();
          end;
        end;
      end
      else if (DataRecHead.flags and REC_FRAGMENT) > 0 then
        // ok
      else if (DataRecHead.flags and REC_BLOB) > 0 then
        // ok
      else
      begin
        if iDataLen > 0 then
        begin
          s := DataAsStr(APageBuf[iDataOffs], iDataLen);
        end;
        LogInfo(Format('Page:Line=%d:%d  Flags=%s  Data(%d)=%s',
          [nPage, i,
           DataFlagsToStr(DataRecHead.flags),
           iDataLen,
           s
          ]));

      end;

      // == BLOB record
      (*
      if (iDataLen > 0) and ((DataRecHead.flags and REC_BLOB) > 0) then
      begin
        // blob
        Move(APageBuf[DataPageRecs[i].Offset], BlobRecHead, SizeOf(BlobRecHead));
        iDataHeadLen := SizeOf(BlobRecHead);
        iDataOffs := DataPageRecs[i].Offset + iDataHeadLen;
        iDataLen := DataPageRecs[i].Length - iDataHeadLen;

        //Move(RawPage[iDataOffs], BlobRecHead, SizeOf(BlobRecHead));

        if IsLogBlobs and (DataPageHead.RelationID = DebugRelID) then
        begin
          s := BufferToHex(APageBuf[iDataOffs], 10);
          LogInfo(Format('DATA_REC_BLOB page=%d lead=%d  max_seq=%d  max_seg=%d  flags=%d  level=%d  count=%d  len=%d  sub_type=%d  blh_charset=%d %s',
            [nPage,
             BlobRecHead.LeadPage,
             BlobRecHead.MaxSequence,
             BlobRecHead.MaxSegment,
             BlobRecHead.Flags,
             BlobRecHead.Level,
             BlobRecHead.Count,
             BlobRecHead.Length,
             BlobRecHead.SubType,
             BlobRecHead.Charset,
             s]));
        end;

        //iLen := iDataLen - SizeOf(BlobRecHead) - 4;
        iLen := iDataLen;
        //iLen := 0;
        if iLen > 0 then
        begin
          //Inc(iDataOffs, SizeOf(BlobRecHead) - 4);
          if BlobRecHead.SubType = 1 then  // TEXT
          begin
            Inc(iDataOffs, 2);
            Dec(iLen, 2);
          end;
          if BlobRecHead.SubType = 2 then  // BLR
          begin
            Inc(iDataOffs, 9);
            Dec(iLen, 9);
          end;
          if BlobRecHead.SubType = 3 then
          begin
            Inc(iDataOffs, 6);
            Dec(iLen, 6);
          end;
          if BlobRecHead.SubType = 5 then
          begin
            Inc(iDataOffs, 8);
            Dec(iLen, 8);
          end;

          if iLen > 0 then
          begin
            SetLength(s, iLen);
            Move(APageBuf[iDataOffs], s[1], iLen);
          end;
          //s := RleDecompress(s);

          // == System tables (blobs)
          {if DataPageHead.RelationID < 40 then
            ReadSystemTable(DataPageHead.RelationID, CurRowID, s, True); }

          if Length(s) > 0 then
            s := DataAsStr(s[1], Length(s));

        end
        else
        begin
          SetLength(s, iDataLen);
          Move(APageBuf[iDataOffs], s[1], iDataLen);
          //s := RleDecompress(s);

          if Length(s) > 0 then
            s := DataAsStr(s[1], Length(s));
        end;

        if IsLogBlobs and (DataPageHead.RelationID = DebugRelID) then
          LogInfo(Format('DATA_REC_blob Rel=%d seq=%d flags=%s n=%d size=%d data=%s',
            [DataPageHead.RelationID, DataPageHead.Sequence, DataFlagsToStr(DataRecHead.flags), i, iDataLen, s]));
      end;
      *)

      if IsLogPages and (DataPageHead.RelationID = DebugRelID) then
      begin
        LogInfo(Format('i=%d off=%d len=%d TID=%d flags=%s data=%s',
          [i, DataPageRecs[i].Offset, DataPageRecs[i].Length, DataRecHead.transaction,
           DataFlagsToStr(DataRecHead.flags), s]));
      end;
    end;
  end;
  if Assigned(OnPageReaded) then OnPageReaded(Self);
  Result := True;
end;

procedure TDBReaderFB.ReadTable(AName: string; ACount: Int64; AList: TDbRowsList);
var
  i, nPage: Integer;
  nPos: Int64;
  RawPage: TByteArray;
  TmpTable: TRDBTable;
  TmpRow: TDbRowItem;
begin
  if not Assigned(AList) then Exit;
  AList.Clear();

  TmpTable := TableList.GetByName(AName);
  if not Assigned(TmpTable) then Exit;

  // Show table info
  LogInfo(Format('== TableName=%s  RelationID=%d', [TmpTable.TableName, TmpTable.RelationID]));
  AList.TableName := TmpTable.TableName;
  AList.FieldsDef := TmpTable.FieldsDef;
  //LogInfo('==');

  if (not FIsMetadataReaded) and (TmpTable.Count > 0) then
  begin
    // predefined table
    for i := 0 to TmpTable.Count-1 do
    begin
      TmpRow := TDbRowItem.Create(AList);
      TmpRow.Assign(TmpTable.GetItem(i));
      AList.Add(TmpRow);
    end;
    Exit;
  end;

  if ACount = 0 then
    Exit;
  // find pages for RelationID
  {for i := 0 to FPagesList.Count - 1 do
  begin
    PageItem := FPagesList.GetItem(i) as TRDB_PagesItem;
    if (PageItem.RelationID <> RelID) and (PageItem.PageType <> 4) then
      Continue;

  end;  }

  // find pages by PageID
  if Assigned(TmpTable) and (TmpTable.PageIdCount > 0) then
  begin
    for i := Low(TmpTable.PageIdArr) to High(TmpTable.PageIdArr) do
    begin
      nPos := TmpTable.PageIdArr[i] * FPageSize;
      FFile.Position := nPos;
      FFile.Read(RawPage, FPageSize);
      ReadDataPage(RawPage, nPos, TmpTable, AList);
    end;
  end
  else
  begin
    // scan pages
    nPage := 1;
    nPos := nPage * FPageSize;
    while nPos < (FFile.Size - FPageSize) do
    begin
      FFile.Position := nPos;
      FFile.Read(RawPage, FPageSize);
      ReadDataPage(RawPage, nPos, TmpTable, AList);
      Inc(nPage);
      nPos := nPage * FPageSize;
    end;
  end;
  TmpTable.RowCount := AList.Count;
end;

procedure TDBReaderFB.SetRawFormat(ATable: TRDBTable; ARawFormat: AnsiString);
var
  i, nPre, iPos, iRecSize, n: Integer;
  //FormatRec: TFBFormatRec;
  rdr: TRawDataReader;
begin
  nPre := 2;
  iRecSize := 12;  // SizeOf(TFBFormatRec)
  if IsInterBase and (OdsVersion = 11) then
  begin
    iRecSize := 16;
  end
  else
  if (OdsVersion = 11) then
    nPre := 2
  else
  if OdsVersion = 12 then
    nPre := 4;

  if IsLogFormats then
  begin
    //StrToFile(ARawFormat, 'RelID_160_format.data');
    LogInfo(Format('RelID_%d_format=%s', [ATable.RelationID, BufferToHex(ARawFormat[1], Length(ARawFormat))]));
    n := (Length(ARawFormat) - nPre) div iRecSize;
    LogInfo(Format('Rec count (Size div RecSize)=%d', [n]));
  end;
  if (Copy(ARawFormat, 1, 1) = '<') and (Copy(ARawFormat, Length(ARawFormat), 1) = '>') then
  begin
    LogInfo(Format('RelID_%d_format=%s', [ATable.RelationID, ARawFormat]));
    Exit;
  end;

  for i := 0 to Length(ATable.FieldsInfo) - 1 do
  begin

    iPos := nPre + (i * iRecSize);
    if iPos + iRecSize > Length(ARawFormat) then
      Exit;
    //Move(sData[iPos+1], FormatRec, SizeOf(FormatRec));
    //StrToFile(sData, 'test_s1.data');
    rdr.InitBuf(ARawFormat, iPos+1);

    // Format rec
    {AFieldInfo.DType := FormatRec.DType;
    AFieldInfo.Scale := FormatRec.Scale;
    AFieldInfo.Length := FormatRec.Length;
    AFieldInfo.SubType := FormatRec.SubType;
    AFieldInfo.Flags := FormatRec.Flags;
    AFieldInfo.Offset := FormatRec.Offset;  }
    //
    if IsInterBase and (OdsVersion = 11) then
    begin
      n := rdr.ReadUInt8; // unknown, 0
      ATable.FieldsInfo[i].DType := rdr.ReadUInt8;    // can not be same as field
      ATable.FieldsInfo[i].Scale := rdr.ReadInt16;
      ATable.FieldsInfo[i].Length := rdr.ReadUInt16;  // can not be same as field
      ATable.FieldsInfo[i].SubType := rdr.ReadUInt16;
      ATable.FieldsInfo[i].Flags := rdr.ReadUInt32;
      ATable.FieldsInfo[i].Offset := rdr.ReadUInt16;  // ok
      n := rdr.ReadUInt16; // unknown, 0
    end
    else // FireBird
    begin
      { DType: Byte;
        Scale: Shortint;
        Length: Word;
        SubType: Word;  (codepage for CHAR) For BLOB/TEXT and integers
        Flags: Word;
        Address: DWORD_PTR; Position from begining of row }
      ATable.FieldsInfo[i].DType := rdr.ReadUInt8;
      ATable.FieldsInfo[i].Scale := rdr.ReadInt8;
      ATable.FieldsInfo[i].Length := rdr.ReadUInt16;
      ATable.FieldsInfo[i].SubType := rdr.ReadUInt16;
      ATable.FieldsInfo[i].Flags := rdr.ReadUInt16;
      ATable.FieldsInfo[i].Offset := rdr.ReadUInt32;
    end;

    //ATable.FieldsInfo[i].Size := FormatRec.Length;
    //ATable.FieldsInfo[i].FieldType := FormatRec.DType;
    //ATable.FieldsInfo[i].FieldSubType := FormatRec.SubType;
    //ATable.FieldsInfo[i].FieldLength := FormatRec.Length;

    if IsLogFormats then
    begin
      LogInfo(SysUtils.Format('FORMAT raw=%s  DType=%d(%d) Scale=%d Len=%d(%d) SubType=%d(%d) Flags=%d Offs=%d',
        [BufferToHex(ARawFormat[iPos+1], iRecSize),
        ATable.FieldsInfo[i].DType, ATable.FieldsInfo[i].FieldType,
        ATable.FieldsInfo[i].Scale,
        ATable.FieldsInfo[i].Length, ATable.FieldsInfo[i].FieldLength,
        ATable.FieldsInfo[i].SubType, ATable.FieldsInfo[i].FieldSubType,
        ATable.FieldsInfo[i].Flags,
        ATable.FieldsInfo[i].Offset]
      ));
    end;
  end;
end;


{ TRDB_TableItem }

constructor TRDB_RowItem.Create(AOwner: TDbRowsList; AReader: TDBReaderFB; ARelID: Integer; ARowID: Cardinal);
begin
  inherited Create(AOwner);
  FReader := AReader;
  FRelID := ARelID;
  FRowID := ARowID;
end;

function TRDB_RowItem.GetAsText: string;
begin
  Result := '';
end;

function TRDB_RowItem.GetBlobValue(var AValue: TRDB_Blob): AnsiString;
begin
  if AValue.Data = '' then
    Reader.GetBlobData(AValue.RelID, AValue.RowID, AValue.Data);
  Result := AValue.Data;
end;

function TRDB_RowItem.GetFieldAsBlob(AFieldIndex: Integer): AnsiString;
begin
  Result := '';
  if IsBlobValue(AFieldIndex) then
  begin
    if VarIsOrdinal(Values[AFieldIndex]) and IsBlobValue(AFieldIndex) then
    begin
      // Blob Value is RowID - get blob data
      try
        Reader.GetBlobData(RelID, Values[AFieldIndex], Result);
      except on E: Exception do
        //Result := E.Message;
      end;
    end
    else
      Result := Values[AFieldIndex];
  end;
end;

function TRDB_RowItem.GetFieldAsStr(AFieldIndex: Integer): string;
var
  d: Double;
begin
  if (AFieldIndex >= 0) and (AFieldIndex < Length(Values)) then
  begin
    if VarIsNull(Values[AFieldIndex]) then
      Result := '<null>'
    else
    if VarIsFloat(Values[AFieldIndex]) then
    begin
      // float point in short form
      d := Values[AFieldIndex];
      Result := Format('%.6g', [d]);
    end
    else
    if VarIsOrdinal(Values[AFieldIndex]) and IsBlobValue(AFieldIndex) then
    begin
      // Blob Value is RowID - get blob data
      try
        Reader.GetBlobData(RelID, Values[AFieldIndex], Result);
      except on E: Exception do
        Result := E.Message;
      end;
      //Values[AFieldIndex] := Result;
    end
    else
    if VarIsStr(Values[AFieldIndex]) then
    begin
      Result := Values[AFieldIndex];
      if Length(Result) > 0 then
        Result := DataAsStr(Result[1], Length(Result));
    end
    else
    if VarIsType(Values[AFieldIndex], varDate) then
    begin
      d := Values[AFieldIndex];
      if (d > -500000) and (d < 500000) then
        Result := VarToStrDef(Values[AFieldIndex], '')
      else
        Result := Format('<date %.6g>', [d]);
    end
    else
      Result := VarToStrDef(Values[AFieldIndex], '');
  end
  else
    Result := '<out of index>'
end;

function TRDB_RowItem.GetTableInfo: TRDBTable;
begin
  if (Owner is TRDBTable) then
    Result := Owner as TRDBTable
  else
    Result := nil;
end;

function TRDB_RowItem.GetValueByName(AName: string): Variant;
var
  i: Integer;
begin
  Result := Null;
  if Assigned(TableInfo) then
  begin
    AName := UpperCase(AName);
    for i := Low(TableInfo.FieldsInfo) to High(TableInfo.FieldsInfo) do
    begin
      if UpperCase(TableInfo.FieldsInfo[i].Name) = AName then
      begin
        Result := Values[i];
        Exit;
      end;
    end;
  end;
end;

function TRDB_RowItem.IsBlobValue(AIndex: Integer): Boolean;
begin
  Result := False;
  if Assigned(TableInfo) and (AIndex <= High(TableInfo.FieldsInfo)) then
  begin
    if (TableInfo.FieldsInfo[AIndex].DType = DTYPE_BLOB)
    or ((TableInfo.FieldsInfo[AIndex].DType = DTYPE_TEXT) and (TableInfo.FieldsInfo[AIndex].SubType = 1))
    or (TableInfo.FieldsInfo[AIndex].FieldType = 261)
    then
      Result := True;
  end;
end;

function TRDB_RowItem.IsNullValue(AIndex: Integer): Boolean;
var
  FlagIndex, FlagOffs: Integer;
begin
  Result := False;
  FlagIndex := AIndex div 8;
  FlagOffs := AIndex mod 8;

  if FlagIndex < Length(RawData) then
    Result := (Ord(RawData[FlagIndex+1]) and (Byte(1) shl FlagOffs)) <> 0;
end;

// Type 0 = 8 bytes
// Type 1 = 10 bytes
// Type 2 = 8 bytes
function TRDB_RowItem.ReadBlob(AType, ASize: Integer): TRDB_Blob;
var
  nMaxLen, nPos: Integer;
  wRow, wRel: Word;
  rec: TBlobFieldRec;  // RelID(32bit), RowID(32bit)
begin
  Result.RelID := 0;
  Result.RowID := 0;
  Result.BlobSubType := AType;
  Result.Data := '';
  if ASize = 0 then
  begin
    case AType of
      0: nMaxLen := 8;  // BIN
      1: nMaxLen := 8;  // TEXT
      2: nMaxLen := 4;  // BLR ??
      5: nMaxLen := 10; // ??
      6: nMaxLen := 6;  // ??
      8: nMaxLen := 9;  // ??
    else
      nMaxLen := 8;
    end;
  end
  else
    nMaxLen := ASize;

  // Align
  //nPos := FDataPos + ((FDataPos-1) mod 4);
  nPos := FDataPos;

  FillChar(rec, SizeOf(rec), 0);
  if AType in [0, 1, 5, 6, 8] then
  begin
    if nPos-1 + SizeOf(rec) <= Length(RawData) then
    begin
      Move(RawData[nPos], rec, SizeOf(rec));
      //Result := Format('%d:%d', [rec.RelationID, rec.RecID]);
      Result.RelID := rec.RelationID;
      Result.RowID := rec.RowID;
      if (rec.RelationID <> 0) then
        if not Reader.GetBlobData(RelID, rec.RowID, Result.Data) then
          Result.BlobSubType := -1;
    end;

    if Reader.IsLogBlobs and ((rec.RelationID <> 0) or (rec.RowID <> 0)) then
    begin
      Reader.LogInfo( Format('BLOB_FIELD RelID=%d RowID=%d : type=%d rel=%d rec=%d raw(%d)=%s dump(%d)=%s data=%s',
        [RelID, RowID,
        AType, rec.RelationID, rec.RowID,
        nPos, BufferToHex(RawData[nPos], SizeOf(rec)),
        FDataPos, BufferToHex(RawData[FDataPos], nMaxLen),
        Result.Data]) );
    end;
  end
  else
  if AType = 2 then
  begin
    if FDataPos-1 + SizeOf(rec) <= Length(RawData) then
    begin
      Move(RawData[FDataPos], wRow, SizeOf(wRow));
      Move(RawData[FDataPos+2], wRel, SizeOf(wRel));
      rec.RowID := wRow;
      rec.RelationID := wRel;
      //Result := Format(':%d', [rec.RecID]);
      if rec.RelationID = Self.RelID then
      begin
        Result.RowID := Self.RowID-1;
        Result.RelID := Self.RelID;
        //Result.Data := Reader.GetBlobData(RelID, rec.RowID);
        Result.Data := Format('!BLOB_BLR row=%d rel=%d', [rec.RowID, rec.RelationID]);
        Result.BlobSubType := -1;
      end
      else
      begin
        Result.RowID := rec.RowID;
        //Result.Data := Reader.GetBlobData(RelID, rec.RowID);
        Result.Data := Format('!BLOB_BLR row=%d rel=%d %s', [rec.RowID, rec.RelationID, BufferToHex(RawData[FDataPos], 4)]);
        Result.BlobSubType := -1;
      end;
    end;
  end
  else
    Result.Data := BufferToHex(RawData[FDataPos], nMaxLen);

  Inc(FDataPos, nMaxLen);
end;

function TRDB_RowItem.ReadChar(ACount: Integer; ASub: Integer = 0): string;
var
  nPos: Integer;
begin
  Result := '';

  nPos := FDataPos;
  while ((nPos - FDataPos) < ACount) and (nPos <= Length(RawData)) do
  begin
    if RawData[nPos] = #0 then
      Break;
    Inc(nPos);
  end;
  if nPos > FDataPos then
    Result := Trim(Copy(RawData, FDataPos, nPos-FDataPos));
  {$ifdef FPC}
  if ASub <> 3 then
    Result := WinCPToUTF8(Result);
  {$endif}

  Inc(FDataPos, ACount);
end;

function TRDB_RowItem.ReadCurrency(ABytes: Integer): Currency;
begin
  Result := 0;
  if (ABytes > 0) and (Length(RawData) >= (FDataPos + ABytes)) then
    Move(RawData[FDataPos], Result, 8);
  Inc(FDataPos, ABytes);
end;

procedure TRDB_RowItem.ReadData(ATableInfo: TRDBTable; const AData: AnsiString);
var
  i, nSize, nLen, nSub: Integer;
  dType: Word;
  BlobRec: TRDB_Blob;
  TmpTable: TRDBTable;
begin
  //StrToFile(AData, 'test_s1.data');
  RawData := AData;
  TmpTable := ATableInfo;
  if not Assigned(TmpTable) then Exit;

  // row data
  // NullBitmap - 4 bytes minimum, grow by 4 bytes
  // ColumnData
  FDataPos := (Length(TmpTable.FieldsInfo) div 32);
  FDataPos := ((FDataPos + 1) * 4) + 1;   // 1-based

  SetLength(Values, Length(TmpTable.FieldsInfo));
  for i := Low(TmpTable.FieldsInfo) to High(TmpTable.FieldsInfo) do
  begin
    if (not Reader.IsInterBase) and IsNullValue(i) then
    begin
      Values[i] := Null;
      // Interbase keep null values
      Continue;
    end;

    dType := TmpTable.FieldsInfo[i].DType;
    if dType = 0 then
    begin
      // no format descriptor, use field definition
      dType := TmpTable.FieldsInfo[i].FieldType;
      nSize := TmpTable.FieldsInfo[i].Size;
      nLen := TmpTable.FieldsInfo[i].FieldLength;
      nSub := TmpTable.FieldsInfo[i].FieldSubType;
      if (nSize = 0) then
        nSize := FieldTypeToSize(dType, nLen, nSub);
      //if TmpTable.FieldsInfo[i].Offset <> 0 then
      //  FDataPos := TmpTable.FieldsInfo[i].Offset + 1;

      if (nSize = 0) then
      begin
        Values[i] := Null;
        Continue;
      end;

      case dType of
        7: Values[i] := ReadInt(2); // SMALLINT
        8: Values[i] := ReadInt(4); // INTEGER
        //8: Values[i] := ReadInt64(); // INTEGER
        10: Values[i] := ReadFloat(); // FLOAT
        14: Values[i] := ReadChar(nLen, nSub); // CHAR
        16:
        begin
          if nSub = 1 then
            Values[i] := ReadCurrency // CURRENCY
          else
            Values[i] := ReadInt64; // INT64
        end;
        27: Values[i] := ReadFloat(); // DOUBLE
        35: Values[i] := ReadDateTime(0, nSize); // DATETIME
        37: Values[i] := ReadVarChar(nLen, nSize, nSub); // VARCHAR
        261:
        begin
          BlobRec := ReadBlob(nSub, nLen); // BLOB
          if (BlobRec.Data <> '') and (BlobRec.BlobSubType <> -1) then
            Values[i] := BlobRec.Data
          else
          if (BlobRec.RelID <> 0) then
            Values[i] := BlobRec.RowID;
        end;
      else
        Values[i] := Null;
      end;
    end
    else
    begin
      // use format descriptor
      //nSize := TmpTable.FieldsInfo[i].GetSize();
      nLen := TmpTable.FieldsInfo[i].Length;
      nSub := TmpTable.FieldsInfo[i].FieldSubType;

      FDataPos := TmpTable.FieldsInfo[i].Offset + 1;
      case dType of
        DTYPE_TEXT:
        begin
          // encoding?
          if (nSub = 0) or (nSub = 3) then
            Values[i] := ReadChar(nLen, nSub)
          else
          begin
            BlobRec := ReadBlob(nSub);
            if (BlobRec.Data <> '') and (BlobRec.BlobSubType <> -1) then
            begin
              {$ifdef FPC}
              Values[i] := WinCPToUTF8(BlobRec.Data);
              {$else}
              Values[i] := BlobRec.Data;
              {$endif}
            end
            else
            if (BlobRec.RelID <> 0) then
              Values[i] := BlobRec.RowID;
          end;
        end;
        DTYPE_CSTRING:   Values[i] := ReadChar(nLen, nSub);
        DTYPE_VARYNG:    Values[i] := ReadVarChar(nLen, 0, nSub);
        //DTYPE_PACKED:    = 6;
        DTYPE_BYTE:      Values[i] := ReadInt(1);  // 1  Byte
        DTYPE_SHORT:     Values[i] := ReadInt(2);  // 2  Word
        DTYPE_LONG:      Values[i] := ReadInt(4);  // 4  Cardinal
        DTYPE_QUAD:      Values[i] := ReadInt64;  // 8  DWord
        DTYPE_REAL:      Values[i] := ReadFloat(4); // 4  Single
        DTYPE_DOUBLE:    Values[i] := ReadFloat(); // 8  Double
        DTYPE_DFLOAT:    Values[i] := ReadFloat(); //
        DTYPE_SQL_DATE:  Values[i] := ReadDateTime(1, 4); // 4  Integer
        DTYPE_SQL_TIME:  Values[i] := ReadDateTime(2, 4); // 4  Cardinal
        DTYPE_TIMESTAMP: Values[i] := ReadDateTime(0, 8); // 8  DateTime
        DTYPE_BLOB:
        begin
          BlobRec := ReadBlob(nSub, nLen);
          if (BlobRec.Data <> '') and (BlobRec.BlobSubType <> -1) then
          begin
            if Reader.IsInterBase then
            begin
              case nSub of
                1:
                begin
                  {$ifdef FPC}
                  Values[i] := WinCPToUTF8(Copy(BlobRec.Data, 3, MaxInt)); // TEXT  skip first 2 bytes
                  {$else}
                  Values[i] := Copy(BlobRec.Data, 3, MaxInt); // TEXT  skip first 2 bytes
                  {$endif}
                end;
              else
                Values[i] := BlobRec.Data
              end;
            end
            else
            begin
              {$ifdef FPC}
              if nSub = 1 then // TEXT
                Values[i] := WinCPToUTF8(BlobRec.Data)
              else
                Values[i] := BlobRec.Data;
              {$else}
              Values[i] := BlobRec.Data;
              {$endif}
            end;
          end
          else
          if (BlobRec.RelID <> 0) then
            Values[i] := BlobRec.RowID;
        end;
        DTYPE_ARRAY:     Values[i] := '?ARRAY';
        DTYPE_INT64:
        begin
          if nSub = 1 then
            Values[i] := ReadCurrency // 8 Currency
          else
            Values[i] := ReadInt64; // 8 Int64
        end;
      else
        Values[i] := Null;
      end;
    end;

    if Reader.IsInterBase and IsNullValue(i) then
      Values[i] := Null;
  end;

  // leave only null flags at beginning of row
  //SetLength(FRawData, ((Length(TmpTable.FieldsInfo]) div 32) + 1) * 4);
  if Reader.IsClearRawData then
    RawData := '';
end;

function TRDB_RowItem.ReadDateTime(AType, ABytes: Integer): TDateTime;
var
  Days, Part, nPos: Integer;
  //yy, mm, dd: Word;
  //dt: TDateTime;
  //s: string;
begin
  Result := 0;
  if FDataPos >= Length(RawData) then
    Exit;
  // alignment
  //nPos := FDataPos + 4 - ((FDataPos-1) mod 4);
  nPos := FDataPos;
  Part := 0;
  // AType 0-DateTime 1-Date 2-Time
  if AType < 2 then
  begin
    // days from epoch begining
    Move(RawData[nPos], Days, 4);
    if Days = 0 then
      Result := 0
    else
      Result := Days - FB_EPOCH_DIFF;
    if AType = 0 then
      Move(RawData[nPos+4], Part, 4);
  end
  else
  begin
    // part of day
    Move(RawData[nPos], Part, 4);
  end;
  // part of day
  Result := Result + (1 / (24*60*60*10000) * Part);

{$ifdef DEBUG}
  // not needed
  if Result < -(366 * 3000) then
    Result := EncodeDate(0001, 01, 01)
  else if Result > (366 * 3000) then
    Result := EncodeDate(3000, 01, 01);

  {if RelID = DebugRelID then
    _Log( IntToHex(FDataPos-1, 4)
       + ' ' + BufferToHex(FRawData[FDataPos], 12)
       + ' ' + IntToStr(nPos-FDataPos)
       + ' ' + BufferToHex(FRawData[nPos], 4) + '=' + IntToStr(Days)
       //+ ' ' + BufferToHex(Days2, 4) + '=' + IntToStr(Days2)
       + ' = ' + FormatDateTime('YYYY-MM-DD HH:NN:SS', Result)
    );  }
{$endif}
  Inc(FDataPos, ABytes);
end;

function TRDB_RowItem.ReadFloat(ABytes: Integer): Double;
var
  fSingle: Single;
begin
  if (FDataPos+ABytes) <= Length(RawData) then
  begin
    if ABytes = 4 then
    begin
      Move(RawData[FDataPos], fSingle, SizeOf(fSingle));
      Result := fSingle;
    end
    else
      Move(RawData[FDataPos], Result, SizeOf(Result))
  end
  else
    Result := 0;
  Inc(FDataPos, ABytes);
end;

function TRDB_RowItem.ReadInt(ABytes: Integer): Integer;
begin
  Result := 0;
  if (ABytes > 0) and (Length(RawData) >= (FDataPos + ABytes)) then
    Move(RawData[FDataPos], Result, ABytes);
  Inc(FDataPos, ABytes);
end;

function TRDB_RowItem.ReadInt64(ABytes: Integer): Int64;
begin
  Result := 0;
  if (ABytes > 0) and (Length(RawData) >= (FDataPos + ABytes)) then
    Move(RawData[FDataPos], Result, 8);
  Inc(FDataPos, ABytes);
end;

{procedure TRDB_RowItem.ReadRawData(const AData; ALen: Integer);
var
  sData: AnsiString;
  ptr: PByte;
  DataRecHead: TFBDataRecHead;
  iDataLen: Integer;
begin
  if ALen = 0 then Exit;
  ptr := Addr(AData);
  Move(AData, DataRecHead, SizeOf(DataRecHead));
  iDataLen := ALen - SizeOf(DataRecHead);
  if (iDataLen > 0) and (DataRecHead.flags = 0) then
  begin
    SetLength(sData, iDataLen);
    Inc(ptr, SizeOf(DataRecHead));
    Move(ptr^, sData[1], iDataLen);
    sData := RleDecompress(sData);
    ReadData(sData);
  end;
end;  }

function TRDB_RowItem.ReadVarChar(ACount: Integer; ASize: Integer = 0;
  ASub: Integer = 0): string;
var
  nPos: Integer;
  nLen: Word;
begin
  Result := '';
  if ASize = 0 then
  begin
    ASize := ACount + 2 + 4;  // 2 bytes length
    // align
    nPos := FDataPos + ASize;
    nPos := nPos + ((nPos-1) mod 2);
    ASize := nPos - FDataPos;
  end;

  nPos := FDataPos;
  if nPos >= Length(RawData)-SizeOf(nLen) then
  begin
    Result := Format('<offs=%d size=%d>', [nPos, Length(RawData)]);
    Exit;
  end;

  // read length SMALLINT
  Move(RawData[nPos], nLen, SizeOf(nLen));
  Inc(nPos, SizeOf(nLen));
  if nLen > ACount then
    nLen := ACount;

  if (nPos + nLen <= Length(RawData)) then
    Result := Copy(RawData, nPos, nLen);
  {$ifdef FPC}
  if ASub <> 3 then
    Result := WinCPToUTF8(Result);
  {$endif}

  Inc(FDataPos, ASize);
end;

{ TRDBTable }

procedure TRDBTable.AddFieldDef(AName: string; AType: Integer;
      ALength: Integer;
      ASubType: Integer;
      AScale: Integer;
      AOffset: Integer);
var
  i: Integer;
begin
  i := Length(FieldsInfo);
  if (AOffset = 0) then
  begin
    if (DataPos > 0) then
      AOffset := DataPos
    else
    if i = 0 then
      // NullBitmap - 4 bytes minimum, grow by 4 bytes
      AOffset := (((Length(FieldsInfo) div 32) + 1) * 4)
    else
    begin
      AOffset := FieldsInfo[i-1].Offset;
      AOffset := AOffset + FieldsInfo[i-1].GetSize();
    end;
  end;

  SetLength(FieldsInfo, i+1);
  FieldsInfo[i].Name := AName;
  FieldsInfo[i].DType := AType;
  FieldsInfo[i].Length := ALength;
  FieldsInfo[i].SubType := ASubType;
  FieldsInfo[i].Scale := AScale;
  FieldsInfo[i].Offset := AOffset;
  // next offser
  DataPos := AOffset + FieldsInfo[i].GetSize();

  SetLength(FieldsDef, i+1);
  FieldsDef[i].Name := AName;
  FieldsDef[i].FieldType := ftString; // just to show value
  FieldsDef[i].RawOffset := AOffset;
  FieldsDef[i].Size := FieldsInfo[i].GetSize();
  FieldsDef[i].TypeName := DataTypeToStr(AType, ALength, ASubType);
end;

procedure TRDBTable.AfterConstruction;
begin
  inherited;
  RowCount := -1;
end;

procedure TRDBTable.BeforeDestruction;
begin
  inherited;

end;

function TRDBTable.FindRecByValue(AColIndex: Integer; AValue: Variant): TDbRowItem;
var
  i: Integer;
  ws1, ws2: WideString;
begin
  ws2 := Trim(AValue);
  for i := 0 to Count - 1 do
  begin
    Result := GetItem(i);
    ws1 := Trim(Result.Values[AColIndex]);
    if WideSameText(ws1, ws2) then
      Exit;
  end;
  Result := nil;
end;

function TRDBTable.IsEmpty: Boolean;
begin
  Result := (RowCount = 0);
end;

function TRDBTable.IsGhost: Boolean;
begin
  Result := (Length(FieldsDef) = 0);
end;

function TRDBTable.IsSystem: Boolean;
begin
  Result := (RelationID <= 50);
end;

{ TRDBTableList }

function TRDBTableList.GetByName(AName: string): TRDBTable;
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

function TRDBTableList.GetByRelationID(ARelID: Integer): TRDBTable;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := GetItem(i);
    if Result.RelationID = ARelID then
      Exit;
  end;
  Result := nil;
end;

function TRDBTableList.GetItem(AIndex: Integer): TRDBTable;
begin
  Result := TRDBTable(Get(AIndex));
end;

procedure TRDBTableList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Action = lnDeleted then
    TRDBTable(Ptr).Free;
end;

function DoSortByName(Item1, Item2: Pointer): Integer;
var
  TmpItem1, TmpItem2: TRDBTable;
begin
  TmpItem1 := TRDBTable(Item1);
  TmpItem2 := TRDBTable(Item2);
  Result := AnsiCompareStr(TmpItem1.TableName, TmpItem2.TableName);
end;

procedure TRDBTableList.SortByName;
  // Warning! Nested sort function receive bad pointers!
  //function DoSortByName(Item1, Item2: Pointer): Integer;
begin
  Sort(@DoSortByName);
end;

{ TRDB_FieldInfoRec }

function TRDB_FieldInfoRec.AsString: string;
begin
  if DType <> 0 then
    Result := DataTypeToStr(DType, Length, SubType)
  else
    Result := FieldTypeToStr(FieldType, FieldLength, FieldSubType);
end;

function TRDB_FieldInfoRec.GetSize: Integer;
var
  nLen: Integer;
begin
  if Length > 0 then
    nLen := Length
  else
    nLen := FieldLength;
  Result := Length;
  case DType of
    DTYPE_TEXT:
    begin
      case SubType of
        0, 3: Result := nLen; // CHAR(Len)
        1, 2: if nLen = 0 then Result := 8;  // BLOB
      end;
    end;
    DTYPE_CSTRING:   Result := nLen;  // Len
    DTYPE_VARYNG:    Result := 2 + nLen;  // 2 + Len
    DTYPE_PACKED:    Result := 4;
    DTYPE_BYTE:      Result := 1; // 1  Byte
    DTYPE_SHORT:     Result := 2; // 2  Word
    DTYPE_LONG:      Result := 4; // 4  Cardinal
    DTYPE_QUAD:      Result := 8; // 8  DWord
    DTYPE_REAL:      Result := 4; // 4  Single
    DTYPE_DOUBLE:    Result := 8; // 8  Double
    DTYPE_DFLOAT:    Result := 8; //
    DTYPE_SQL_DATE:  Result := 4; // 4  Integer
    DTYPE_SQL_TIME:  Result := 4; // 4  Cardinal
    DTYPE_TIMESTAMP: Result := 8; // 8  DateTime
    DTYPE_BLOB:
    begin
      if nLen = 0 then
      case SubType of
        0, 1: Result := 8; // BLOB (8)  RelID/RowID
        2: Result := 4; // BLR (4)
      else
        Result := 8;
      end;
    end;
    DTYPE_ARRAY:     Result := 8;
    DTYPE_INT64:     Result := 8; // 8 Int64
  else
    Result := nLen;
  end;
end;

function TRDBPageItemList.FindPageIndex(APageType: Word; ARelationID, ASequence: Cardinal): Integer;
var
  iL, iR, iM, iCmpRes: Integer;
  M: TRDBPageItem;
begin
  Result := -1;
  // binary search
  iL := 0;
  iR := Count-1;
  while iL <= iR do
  begin
    iM := (iL + iR) div 2;
    M := TRDBPageItem(Get(iM));
    // compare
    iCmpRes := 0;
    if APageType > M.PageType then
      iCmpRes := 1
    else
    if APageType < M.PageType then
      iCmpRes := -1
    else
    begin
      if ARelationID > M.RelationID then
        iCmpRes := 1
      else
      if ARelationID < M.RelationID then
        iCmpRes := -1
      else
      begin
        if ASequence > M.PageSeq then
          iCmpRes := 1
        else
        if ASequence < M.PageSeq then
          iCmpRes := -1
      end;
    end;
    // get next mid
    if iCmpRes > 0 then
    begin
      iL := iM + 1;
      Result := -(iM + 2);
    end
    else
    if iCmpRes < 0 then
    begin
      iR := iM - 1;
      Result := -(iM + 1);
    end
    else
    begin
      Result := iM;
      Break;
    end;
  end;
end;

function TRDBPageItemList.GetItem(AIndex: Integer): TRDBPageItem;
begin
  Result := TRDBPageItem(Get(AIndex));
end;

procedure TRDBPageItemList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Action = lnDeleted then
    TRDBPageItem(Ptr).Free;
end;

function ComparePageItems(Item1, Item2: Pointer): Integer;
var
  A, B: TRDBPageItem;
begin
  Result := 0;
  A := TRDBPageItem(Item1);
  B := TRDBPageItem(Item2);
  if A.PageType > B.PageType then
    Result := 1
  else
  if A.PageType < B.PageType then
    Result := -1
  else
  begin
    if A.RelationID > B.RelationID then
      Result := 1
    else
    if A.RelationID < B.RelationID then
      Result := -1
    else
    begin
      if A.PageSeq > B.PageSeq then
        Result := 1
      else
      if A.PageSeq < B.PageSeq then
        Result := -1;
    end;
  end;
end;

procedure TRDBPageItemList.SortItems;
begin
  Sort(ComparePageItems);
end;

end.
