unit DBReaderEdb;

(*
Extensible Storage Engine (ESE) Database File (EDB) reader

Author: Sergey Bodrov, 2025 Minsk
License: MIT

todo: read columns SLV, GUID, compression

https://github.com/libyal/libesedb/blob/main/documentation/Extensible%20Storage%20Engine%20(ESE)%20Database%20File%20(EDB)%20format.asciidoc
*)

interface

uses
  SysUtils, Classes, Variants, DBReaderBase, DB, Types;

type
  TEdbFileInfo = record
    Checksum: Cardinal;
    Signature: Cardinal; // $89ABCDEF
    FileFmtVer: Integer;
    FileType: Integer;  // 0-database, 1-streaming file
    DBTime: TDateTime;
    DBSignature: array [0..27] of Byte;
    DBState: Integer;
    LogPosition: Int64;
    ConsistTime: TDateTime;
    AttachTime: TDateTime;
    AttachPosition: Int64;
    DetachTime: TDateTime;
    DetachPosition: Int64;
    // DBID: Cardinal;
    LogSignature: array [0..27] of Byte;
    PrevFullBackup: array [0..23] of Byte;
    PrevIncBackup: array [0..23] of Byte;
    CurFullBackup: array [0..23] of Byte;
    ShadowingDisabled: Integer;
    LastObjId: Cardinal;
    MajorVer: Integer;
    MinorVer: Integer;
    BuildNum: Integer;
    SPackNum: Integer; // Service pack number
    FileFmtRev: Integer;
    PageSize: Integer;
    RepairCount: Integer;
    RepairTime: TDateTime;
    // Unknown 28 bytes

  end;

  TEdbFieldDefRec = record
    // fixed size columns
    ObjIdTable: Cardinal;      // Long   Object or table identifier
    CatType: Word;             // Short  Catalog type
    ColID: Integer;            // Long   Identifier
    ColTypOrPgNoFdp: Integer;  // Long   Column type COL_TYPE_ or FDP page number
    SpaceUsage: Integer;       // Long   How many bytes for fixed columns
    Flags: Cardinal;           // Long   Column flags COL_FLAG_
    PagesOrLocale: Integer;    // Long   Number of pages or codepage
    //RootFlag: Boolean;         // Bit
    //RecordOffset: Word;        // Short
    //LCMapFlag: Cardinal;       // Long   Flags for the LCMapString function
    //KeyMost: Word;             // Short  rev_$0C
    //LVChunkMax: Cardinal;      // Long   rev_$14
    // variable size
    Name: AnsiString;
    //Stats: AnsiString;
    //TemplateTable: AnsiString;
    //DefaultValue: AnsiString;
    //KeyFldIDs: AnsiString;
    //VarSegMac: AnsiString;
    //ConditionalColumns: AnsiString;
    //TupleLimits: AnsiString;
    //Version: AnsiString;       // rev_0C
    //SortID: AnsiString;        // rev_E6
    // tagged data
    //CallbackData: AnsiString;
    //CallbackDependencies: AnsiString;
    //SeparateLV:
  end;
  TEdbFieldDefRecArr = array of TEdbFieldDefRec;

  TEdbTableInfo = class(TDbRowsList)
  public
    TableID: Integer;  // ObjD
    RowCount: Integer;
    VarColCount: Integer;
    TaggedCount: Integer;
    PageIdArr: array of Integer;
    PageIdCount: Integer;
    FieldInfoArr: TEdbFieldDefRecArr;
    //TableType: AnsiChar; // N ок S
    // Add field definition, if not exist. Ordered by ColID
    // contain no rows
    function IsEmpty(): Boolean; override;
    // predefined table
    function IsSystem(): Boolean; override;
    // not defined in metadata
    function IsGhost(): Boolean; override;

    procedure AddFieldDef(AName: string; AType: Word;
      ALength: Integer = 0;
      AColID: Integer = -1;
      AColFlags: Integer = 0;
      ALocale: Integer = 0);
  end;

  TEdbTableInfoList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function GetItem(AIndex: Integer): TEdbTableInfo;
    function GetByName(AName: string): TEdbTableInfo;
    function GetByID(ATableID: Integer): TEdbTableInfo;
    procedure SortByName();
  end;

  TEdbPageHeadRec = record
    Checksum: Int64;
    PageNumber: Int64;
    ModificationTime: TDateTime;
    PrevPageNum: Cardinal;
    NextPageNum: Cardinal;
    ObjID: Cardinal;
    AvailSize: Word;
    UncommittedSize: Word;
    AvailOffs: Word;
    AvailPageTag: Word;
    PageFlags: Cardinal;
  end;

  TDBReaderEdb = class(TDBReader)
  private
    FTableList: TEdbTableInfoList;
    FFileInfo: TEdbFileInfo;
    FPageSize: Integer;
    FPageHeadSize: Integer;
    FIsMetadataLoaded: Boolean;

    // raw page for blob reader
    FBlobRawPage: TByteDynArray;
    FBlobRawPageID: Integer;

    function ReadPageHeader(const APageBuf: TByteDynArray; var APageHead: TEdbPageHeadRec): Boolean;

    function ReadDataPage(const APageBuf: TByteDynArray; APagePos: Int64;
      ATableInfo: TEdbTableInfo; AList: TDbRowsList): Boolean;

    function ReadDataRecord(const APageBuf: TByteDynArray; ARecOffs, ARecSize: Integer;
      ATableInfo: TEdbTableInfo; AList: TDbRowsList): Boolean;

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

    property TableList: TEdbTableInfoList read FTableList;
    property FileInfo: TEdbFileInfo read FFileInfo;
  end;


implementation

type
  TEdbPageTagRec = record
    ValueOffset: Word;
    Flags: Byte;
    ValueSize: Word;
    KeySize: Word;
  end;

  TEdbVarLenRec = record
    ColID: Integer;
    Offs: Integer;
    Size: Integer;
    Data: AnsiString;
  end;

const
  PAGE_FLAG_ROOT           = $0001; // root page
  PAGE_FLAG_LEAF           = $0002; // leaf page
  PAGE_FLAG_BRANCH         = $0004; // parent (branch) page
  PAGE_FLAG_EMPTY          = $0008; // empty page
  PAGE_FLAG_SPACE_TREE     = $0020; // space tree page
  PAGE_FLAG_INDEX          = $0040; // index page
  PAGE_FLAG_LONG_VALUE     = $0080; // long value page
  PAGE_FLAG_NEW_FORMAT     = $2000; // new record format
  PAGE_FLAG_IS_SCRUBBED    = $4000; // is scrubbed (was zero-ed)

  PAGE_MASK_DATA           = $2003; // data page (no other flags)

  TAG_SIZE = 4;  // 4 byte
  TAG_FLAG_V   = $01;  // variable sized?
  TAG_FLAG_D   = $02;  // deleted, data not used
  TAG_FLAG_C   = $04;  // common page key (size)

  SYS_OBJ_TABLE_ID         = 2;  // MSysObjects TableID

  CAT_TYPE_TABLE           = 1;  // Table
  CAT_TYPE_COLUMN          = 2;  // Column
  CAT_TYPE_INDEX           = 3;  // Index
  CAT_TYPE_LONG_VALUE      = 4;  // Long value
  CAT_TYPE_CALLBACK        = 5;  // Callback

  COL_TYPE_NIL             = 0;  // Invalid column type.
  COL_TYPE_BIT             = 1;  // Boolean 8-bit
  COL_TYPE_BYTE            = 2;  // Byte
  COL_TYPE_SHORT           = 3;  // Int16
  COL_TYPE_LONG            = 4;  // Int32
  COL_TYPE_CURRENCY        = 5;  // Currency 64-bit
  COL_TYPE_SINGLE          = 6;  // Float32
  COL_TYPE_DOUBLE          = 7;  // Float64
  COL_TYPE_DATETIME        = 8;  // Filetime 64-bit
  COL_TYPE_BINARY          = 9;  // Binary data up to 255 bytes
  COL_TYPE_TEXT            = 10; // Text up to 255 bytes (127 Unicode chars)
  COL_TYPE_LONG_BINARY     = 11; // Binary data up to Int32 bytes size
  COL_TYPE_LONG_TEXT       = 12; // Text up to Int32 bytes
  COL_TYPE_SLV             = 13; // Super long value (obsolete)
  COL_TYPE_ULONG           = 14; // UInt32;
  COL_TYPE_LONG_LONG       = 15; // Int64
  COL_TYPE_GUID            = 16; // GUID 128-bit
  COL_TYPE_USHORT          = 17; // UInt16

  COL_FLAG_FIXED           = $0001; // fixed size
  COL_FLAG_TAGGED          = $0002; // take up no space if empty
  COL_FLAG_NOT_NULL        = $0004; // not allowed to be empty
  COL_FLAG_VERSION         = $0008; // version of the row
  COL_FLAG_AUTOINCREMENT   = $0010; // autoincremented number
  COL_FLAG_UPDATABLE       = $0020; //
  COL_FLAG_TT_KEY          = $0040; //
  COL_FLAG_TT_DECENDING    = $0080; //
  COL_FLAG_MULTI_VALUED    = $0400; // tagged
  COL_FLAG_ESCROW_UPDATE   = $0800; // can be updated concurrently
  COL_FLAG_UNVERSIONED     = $1000; // no version info, cannot be used in transaction
  COL_FLAG_DELETE_ON_ZERO  = $2000; // tagged, escrow
  COL_FLAG_FINALIZE        = $4000; // same as DeleteOnZero, with callback
  COL_FLAG_USER_DEF        = $8000; // user defined default value (from callcack)

function EdbColTypeToStr(AVal: Word): string;
begin
  case AVal of
    COL_TYPE_BIT:      Result := 'Boolean';
    COL_TYPE_BYTE:     Result := 'Byte';
    COL_TYPE_SHORT:    Result := 'Short';
    COL_TYPE_LONG:     Result := 'Long';
    COL_TYPE_CURRENCY: Result := 'Currency';
    COL_TYPE_SINGLE:   Result := 'Single';
    COL_TYPE_DOUBLE:   Result := 'Double';
    COL_TYPE_DATETIME: Result := 'DateTime';
    COL_TYPE_BINARY:   Result := 'Binary';
    COL_TYPE_TEXT:     Result := 'Text';
    COL_TYPE_LONG_BINARY: Result := 'LongBinary';
    COL_TYPE_LONG_TEXT: Result := 'LongText';
    COL_TYPE_SLV:      Result := 'SuperLongValue';
    COL_TYPE_ULONG:    Result := 'ULong';
    COL_TYPE_LONG_LONG: Result := 'LongLong';
    COL_TYPE_GUID:     Result := 'Guid';
    COL_TYPE_USHORT:   Result := 'UShort';
  else
    Result := 'Unknown_' + IntToHex(AVal, 4);
  end;
end;

function EdbColTypeToDbFieldType(AVal: Word): TFieldType;
begin
  case AVal of
    COL_TYPE_BIT:      Result := ftBoolean;
    COL_TYPE_BYTE:     Result := ftSmallint;
    COL_TYPE_SHORT:    Result := ftInteger;
    COL_TYPE_LONG:     Result := ftInteger;
    COL_TYPE_CURRENCY: Result := ftCurrency;
    COL_TYPE_SINGLE:   Result := ftFloat;
    COL_TYPE_DOUBLE:   Result := ftFloat;
    COL_TYPE_DATETIME: Result := ftDateTime;
    COL_TYPE_BINARY:   Result := ftBytes;
    COL_TYPE_TEXT:     Result := ftString;
    COL_TYPE_LONG_BINARY: Result := ftBlob;
    COL_TYPE_LONG_TEXT: Result := ftMemo;
    COL_TYPE_SLV:      Result := ftBlob;
    COL_TYPE_ULONG:    Result := ftInteger;
    COL_TYPE_LONG_LONG: Result := ftLargeint;
    COL_TYPE_GUID:     Result := ftGuid;
    COL_TYPE_USHORT:   Result := ftInteger;
  else
    Result := ftUnknown;
  end;
end;

function IsNullValue(const ANullBmp: TByteDynArray; AIndex: Integer): Boolean;
var
  NullFlagIndex, NullFlagOffs: Integer;
begin
  NullFlagIndex := (AIndex div 8);
  NullFlagOffs := AIndex mod 8;
  if NullFlagIndex < Length(ANullBmp) then
    Result := (ANullBmp[NullFlagIndex] and (Byte(1) shl NullFlagOffs)) <> 0
  else
    Result := False;
end;

{ TDBReaderMdb }

procedure TDBReaderEdb.AfterConstruction;
begin
  inherited;
  FTableList := TEdbTableInfoList.Create();
  // init system tables
  InitSystemTables();
end;

procedure TDBReaderEdb.BeforeDestruction;
begin
  FreeAndNil(FTableList);
  inherited;
end;

function TDBReaderEdb.FillTableInfoText(ATableName: string; ALines: TStrings): Boolean;
var
  i: Integer;
  TmpTable: TEdbTableInfo;
  s: string;
begin
  Result := False;
  TmpTable := FTableList.GetByName(ATableName);
  if not Assigned(TmpTable) then
    Exit;

  ALines.Add(Format('== Table Name=%s  RowCount=%d', [TmpTable.TableName, TmpTable.RowCount]));
  ALines.Add(Format('ObjID=%x', [TmpTable.TableID]));
  ALines.Add(Format('VarColCount=%d', [TmpTable.VarColCount]));
  ALines.Add(Format('TaggedCount=%d', [TmpTable.TaggedCount]));
  ALines.Add(Format('== Fields  Count=%d', [Length(TmpTable.FieldsDef)]));
  for i := Low(TmpTable.FieldsDef) to High(TmpTable.FieldsDef) do
  begin
    s := Format('%.2d Name=%s  Type=%s', [i, TmpTable.FieldsDef[i].Name, TmpTable.FieldsDef[i].TypeName]);
    ALines.Add(s);
  end;

  ALines.Add(Format('== FieldInfo  Count=%d', [Length(TmpTable.FieldInfoArr)]));
  for i := Low(TmpTable.FieldInfoArr) to High(TmpTable.FieldInfoArr) do
  begin
    s := Format('%.2d Name=%s  Type=%s  Length=%d  ID=%d  Flags=$%x  Locale=%d',
      [i,
        TmpTable.FieldInfoArr[i].Name,
        EdbColTypeToStr(TmpTable.FieldInfoArr[i].ColTypOrPgNoFdp),
        TmpTable.FieldInfoArr[i].SpaceUsage,
        TmpTable.FieldInfoArr[i].ColID,
        TmpTable.FieldInfoArr[i].Flags,
        TmpTable.FieldInfoArr[i].PagesOrLocale
      ]);
    ALines.Add(s);
  end;
end;

procedure TDBReaderEdb.FillTablesList;
var
  i, ii, iOffs: Integer;
  TmpTable, SysObjTable: TEdbTableInfo;
  TmpRow: TDbRowItem;
  ObjID, CatType, ColID, ColType, ColSize, ColFlags, ColLocale: Integer;
  sName: string;
begin
  SysObjTable := TableList.GetByID(SYS_OBJ_TABLE_ID);
  if not Assigned(SysObjTable) then
    Exit;

  for i := 0 to SysObjTable.Count-1 do
  begin
    TmpRow := SysObjTable.GetItem(i);
    ObjID    := VarToInt(TmpRow.Values[0]); // ObjidTable
    CatType  := VarToInt(TmpRow.Values[1]); // Type
    ColID    := VarToInt(TmpRow.Values[2]); // Id
    ColType  := VarToInt(TmpRow.Values[3]); // ColtypOrPgnoFDP
    ColSize  := VarToInt(TmpRow.Values[4]); // SpaceUsage
    ColFlags := VarToInt(TmpRow.Values[5]); // Flags
    ColLocale := VarToInt(TmpRow.Values[6]); // PagesOrLocale
    sName    := VarToStrDef(TmpRow.Values[7], ''); // Name

    TmpTable := TableList.GetByID(ObjID);
    if not Assigned(TmpTable) then
    begin
      TmpTable := TEdbTableInfo.Create();
      TmpTable.TableID := ObjID;
      TmpTable.TableName := Format('TabID_%x', [ObjID]);
      TableList.Add(TmpTable);
    end;
    case CatType of
      CAT_TYPE_TABLE:
        TmpTable.TableName := sName;
      CAT_TYPE_COLUMN:
      begin
        TmpTable.AddFieldDef(sName, ColType, ColSize, ColID, ColFlags, ColLocale);
      end;
      CAT_TYPE_INDEX:
      begin
        // not needed
      end;
      CAT_TYPE_LONG_VALUE:
      begin
        // todo: implement
      end;
    end;
  end;

  for i := 0 to TableList.Count - 1 do
  begin
    TmpTable := TableList.GetItem(i);

    // -- pages list
    SetLength(TmpTable.PageIdArr, TmpTable.PageIdCount);
    // -- fields defs
    //SetLength(TmpTable.FieldsDef, Length(TmpTable.FieldInfoArr));
    iOffs := 4; // skip row header
    for ii := 0 to Length(TmpTable.FieldInfoArr) - 1 do
    begin
      // calculate offset for fixed columns
      if TmpTable.FieldInfoArr[ii].ColID < $80 then
      begin
        TmpTable.FieldsDef[ii].RawOffset := iOffs;
        Inc(iOffs, TmpTable.FieldInfoArr[ii].SpaceUsage);
      end;

      {// order by id
      id := TmpTable.FieldInfoArr[ii].ColID;
      if (id >= Low(TmpTable.FieldsDef)) and (id <= High(TmpTable.FieldsDef)) then
      begin
        TmpTable.FieldsDef[id].Name := TmpTable.FieldInfoArr[ii].Name;
        TmpTable.FieldsDef[id].TypeName := EdbColTypeToStr(TmpTable.FieldInfoArr[ii].ColType);
        TmpTable.FieldsDef[id].FieldType := EdbColTypeToDbFieldType(TmpTable.FieldInfoArr[ii].ColType);
        TmpTable.FieldsDef[id].Size := TmpTable.FieldInfoArr[ii].SpaceUsage;
        //TmpTable.FieldsDef[id].RawOffset := TmpTable.FieldInfoArr[ii].Offset;
      end; }
    end;
  end;

  FIsMetadataLoaded := True; // allow reading tables data

  // read MSysObjects table
  //ReadTable(SysObjTable.TableName, MaxInt, SysObjTable);

  // fill tables names
  {for i := 0 to SysObjTable.Count - 1 do
  begin
    TmpRow := SysObjTable.GetItem(i);
    if VarIsOrdinal(TmpRow.Values[0]) then
      TmpTable := TableList.GetByID(TmpRow.Values[0]);
    if Assigned(TmpTable) then
      TmpTable.TableName := VarToStrDef(TmpRow.Values[2], TmpTable.TableName);
  end;  }
end;

function TDBReaderEdb.GetTableByIndex(AIndex: Integer): TDbRowsList;
begin
  Result := TableList.GetItem(AIndex);
end;

function TDBReaderEdb.GetTablesCount: Integer;
begin
  Result := TableList.Count;
end;

procedure TDBReaderEdb.InitSystemTables;
var
  TmpTab: TEdbTableInfo;
begin
  TmpTab := TEdbTableInfo.Create();
  FTableList.Add(TmpTab);
  with TmpTab do
  begin
    //TableType := 'S';
    TableID := SYS_OBJ_TABLE_ID;
    TableName := 'MSysObjects';
    AddFieldDef('ObjidTable', COL_TYPE_LONG);
    AddFieldDef('Type', COL_TYPE_SHORT);
    AddFieldDef('Id', COL_TYPE_LONG);
    AddFieldDef('ColtypOrPgnoFDP', COL_TYPE_LONG);
    AddFieldDef('SpaceUsage', COL_TYPE_LONG);
    AddFieldDef('Flags', COL_TYPE_LONG);
    AddFieldDef('PagesOrLocale', COL_TYPE_LONG);
    //AddFieldDef('RootFlag', COL_TYPE_BIT);
    //AddFieldDef('RecordOffset', COL_TYPE_SHORT);
    //AddFieldDef('LCMapFlags', COL_TYPE_LONG);

    AddFieldDef('Name', COL_TYPE_TEXT, 0, $80);
    //AddFieldDef('Stats', COL_TYPE_BINARY);
    //AddFieldDef('TemplateTable', COL_TYPE_TEXT);
    //AddFieldDef('DefaultValue', COL_TYPE_BINARY);
    //AddFieldDef('KeyFldIDs', COL_TYPE_BINARY);
  end;

end;

function TDBReaderEdb.OpenFile(AFileName: string; AStream: TStream): Boolean;
var
  PageBuf: TByteDynArray;
  rdr: TRawDataReader;
  iPagePos: Int64;
  iCurPageID: Cardinal;
  PageHead: TEdbPageHeadRec;
  TableInfo: TEdbTableInfo;
  s: string;
begin
  Result := inherited OpenFile(AFileName, AStream);
  if not Result then Exit;

  FIsMetadataLoaded := False;
  FBlobRawPageID := 0;
  //TableList.Clear();

  // read file info
  FPageSize := $400; // 1K
  SetLength(PageBuf, FPageSize);
  FFIle.Read(PageBuf[0], FPageSize);
  rdr.Init(PageBuf[0]);

  with FFileInfo do
  begin
    Checksum := rdr.ReadUInt32;
    Signature := rdr.ReadUInt32; // $89ABCDEF
    FileFmtVer := rdr.ReadInt32;
    FileType := rdr.ReadInt32;  // 0-database, 1-streaming file
    DBTime := rdr.ReadDouble;
    rdr.ReadToBuffer(DBSignature, SizeOf(DBSignature));
    DBState := rdr.ReadInt32;
    LogPosition := rdr.ReadInt64;
    ConsistTime := rdr.ReadDouble;
    AttachTime := rdr.ReadDouble;
    AttachPosition := rdr.ReadInt64;
    DetachTime := rdr.ReadDouble;
    DetachPosition := rdr.ReadInt64;
    rdr.ReadUInt32; // DBID: Cardinal;
    rdr.ReadToBuffer(LogSignature, SizeOf(LogSignature));
    rdr.ReadToBuffer(PrevFullBackup, SizeOf(PrevFullBackup));
    rdr.ReadToBuffer(PrevIncBackup, SizeOf(PrevIncBackup));
    rdr.ReadToBuffer(CurFullBackup, SizeOf(CurFullBackup));
    ShadowingDisabled := rdr.ReadInt32;
    LastObjId := rdr.ReadUInt32;
    MajorVer := rdr.ReadInt32;
    MinorVer := rdr.ReadInt32;
    BuildNum := rdr.ReadInt32;
    SPackNum := rdr.ReadInt32; // Service pack number
    FileFmtRev := rdr.ReadInt32;
    PageSize := rdr.ReadInt32;
    RepairCount := rdr.ReadInt32;
    RepairTime := rdr.ReadDouble;

  end;

  if FFileInfo.Signature <> $89ABCDEF then
  begin
    LogInfo('!Wrong signature for EDB file!');
    Result := False;
    Exit;
  end;

  FPageSize := FFileInfo.PageSize;
  FPageHeadSize := 40;
  if FFileInfo.FileFmtRev >= $11 then
    FPageHeadSize := 80;
  SetLength(PageBuf, FPageSize);

  // version info
  s := Format('%x.%x', [FFileInfo.FileFmtVer, FFileInfo.FileFmtRev]);
  if FFileInfo.FileFmtVer = $620 then
  begin
    case FFileInfo.FileFmtRev of
      $09: s := 'Used in Windows XP SP3';
      $0B: s := 'Used in Exchange';
      $0C: s := 'Used in Windows Vista';
      $11: s := 'Used in Windows 7';
      $14: s := 'Used in Exchange 2013 and Active Directory 2016';
      $6E: s := 'Used in Windows 10';
      $C8: s := 'Used in Windows 11 (21H2)';
      $E6: s := 'Used in Windows 11';
    end;
  end;
  LogInfo('File Version: ' + s);

  // scan pages
  iCurPageID := 1;
  iPagePos := FPageSize;
  while iPagePos + FPageSize <= FFile.Size do
  begin
    FFile.Position := iPagePos;
    FFile.Read(PageBuf[0], FPageSize);
    ReadPageHeader(PageBuf, PageHead);

    if IsDebugPages then
    begin
      s := Format('Page_%x (num=$%x pos=%x) ObjID=%x  offs=%d  tag=%d  flags=%x',
        [iCurPageID, PageHead.PageNumber, iPagePos, PageHead.ObjID, PageHead.AvailOffs, PageHead.AvailPageTag, PageHead.PageFlags]);
      if (PageHead.PageFlags and PAGE_FLAG_ROOT) <> 0 then
        s := s + ' ROOT';
      if (PageHead.PageFlags and PAGE_FLAG_LEAF) <> 0 then
        s := s + ' LEAF';
      if (PageHead.PageFlags and PAGE_FLAG_BRANCH) <> 0 then
        s := s + ' BRANCH';
      if (PageHead.PageFlags and PAGE_FLAG_EMPTY) <> 0 then
        s := s + ' EMPTY';
      if (PageHead.PageFlags and PAGE_FLAG_SPACE_TREE) <> 0 then
        s := s + ' SPACE_TREE';
      if (PageHead.PageFlags and PAGE_FLAG_INDEX) <> 0 then
        s := s + ' INDEX';
      if (PageHead.PageFlags and PAGE_FLAG_LONG_VALUE) <> 0 then
        s := s + ' LONG_VALUE';
      if (PageHead.PageFlags and PAGE_FLAG_NEW_FORMAT) <> 0 then
        s := s + ' NEW_FORMAT';
      if (PageHead.PageFlags and PAGE_FLAG_IS_SCRUBBED) <> 0 then
        s := s + ' IS_SCRUBBED';
      LogInfo(s);
    end;

    // read page data
    if ((PageHead.PageFlags and PAGE_FLAG_BRANCH) > 0)
    or ((PageHead.PageFlags and PAGE_FLAG_EMPTY) > 0)
    or ((PageHead.PageFlags and PAGE_FLAG_SPACE_TREE) > 0)
    or ((PageHead.PageFlags and PAGE_FLAG_INDEX) > 0)
    or ((PageHead.PageFlags and PAGE_FLAG_LONG_VALUE) > 0)
    or ((PageHead.PageFlags and PAGE_FLAG_IS_SCRUBBED) > 0) then
      // skip
    else
    if ((PageHead.PageFlags and PAGE_MASK_DATA) > 0) then
    begin
      TableInfo := TableList.GetByID(PageHead.ObjID);
      if not Assigned(TableInfo) then
      begin
        TableInfo := TEdbTableInfo.Create();
        TableInfo.TableID := PageHead.ObjID;
        TableInfo.TableName := Format('TabID_%x', [PageHead.ObjID]);
        TableInfo.RowCount := -1; // not empty
        TableList.Add(TableInfo);
      end;

      // store PageID
      Inc(TableInfo.PageIdCount);
      if TableInfo.PageIdCount >= Length(TableInfo.PageIdArr) then
        SetLength(TableInfo.PageIdArr, Length(TableInfo.PageIdArr) + 32);
      TableInfo.PageIdArr[TableInfo.PageIdCount-1] := iCurPageID;

      ReadDataPage(PageBuf, iPagePos, nil, nil);
    end;

    Inc(iCurPageID);
    iPagePos := iCurPageID * FPageSize;

    if Assigned(OnPageReaded) then
      OnPageReaded(Self);
  end;

  SetLength(PageBuf, 0);

  FillTablesList();
end;

function TDBReaderEdb.ReadDataPage(const APageBuf: TByteDynArray; APagePos: Int64;
  ATableInfo: TEdbTableInfo; AList: TDbRowsList): Boolean;
var
  rdr: TRawDataReader;
  iCurPageID, nTag, nFatherID: Cardinal;
  PageHead: TEdbPageHeadRec;
  PageTag: TEdbPageTagRec;
  i, iRecOffs, iLeafHeadSize: Integer;
  w: Word;
begin
  Result := False;
  iCurPageID := APagePos div FPageSize;
  ReadPageHeader(APageBuf, PageHead);
  rdr.Init(APageBuf[0]);

  // read tags
  i := 1;
  iRecOffs := FPageSize - (i * TAG_SIZE);
  //while iRecOffs > PageHead.AvailOffs do
  while i <= PageHead.AvailPageTag do
  begin
    rdr.SetPosition(iRecOffs);
    nTag := rdr.ReadUInt32;
    PageTag.Flags := 0;
    if FFileInfo.FileFmtRev >= $11 then  // Win7+
    begin
      PageTag.ValueOffset := Word(nTag shr 16) and $7FFF;
      PageTag.ValueSize := Word(nTag) and $7FFF;
    end
    else // old rec format
    begin
      PageTag.ValueOffset := Word(nTag shr 16) and $1FFF;
      PageTag.Flags := (Word(nTag shr 29) and $7);
      PageTag.ValueSize := Word(nTag) and $1FFF;
    end;

    // read tag value
    if PageTag.ValueSize > 0 then
    begin
      //if (PageHead.ObjID = 2) and (i = 2) then
      //  BufToFile(APageBuf[FPageHeadSize + PageTag.ValueOffset], PageTag.ValueSize, 'Tag_' + IntToStr(i) + '.data');
      if IsDebugRows then
        LogInfo(Format('- Tag_%x offs=%x len=%x  data=%s',
          [i, PageTag.ValueOffset, PageTag.ValueSize, DataAsStr(APageBuf[FPageHeadSize + PageTag.ValueOffset], PageTag.ValueSize)]));

      rdr.SetPosition(FPageHeadSize + PageTag.ValueOffset);
      if (i = 1) and ((PageHead.PageFlags and PAGE_FLAG_ROOT) <> 0) then
      begin
        // first ROOT page tag is root page header
        if PageTag.ValueSize >= 16 then
        begin
          rdr.ReadUInt32;  // initial pages count
          if PageTag.ValueSize >= 24 then
            rdr.ReadUInt8; // unknown
          nFatherID := rdr.ReadUInt32;  // Parent Father Data Page (FDP) number
          rdr.ReadUInt32;  // Extent space, 0-single, 1-multi
          rdr.ReadUInt32;  // space tree page number, 0 if not set
          if PageTag.ValueSize >= 24 then
            rdr.ReadUInt64; // unknown
        end;

        //if Assigned(ATableInfo) and (ATableInfo.TableID <> nFatherID) then
        //  Exit;

      end
      else
      if (i = 1) and ((PageHead.PageFlags and PAGE_FLAG_LEAF) <> 0) then
      begin
        // first NON-ROOT LEAF page tag is LEAF page header
        if PageTag.ValueSize > 0 then
        begin
          // common page key
        end;

      end
      else
      if ((PageHead.PageFlags and PAGE_FLAG_LEAF) <> 0) then
      begin
        // LEAF page entry
        // 0D         - size?
        // 20
        // 7F 80      - separator?
        // 00 00 03   - TableID
        // 7F 80
        // 02         - CAT_TYPE_
        // 7F 80
        // 00

        // 13  2   == data/type ID (01..7F fixed, 80..FF variable, 100..FFFF tagged)

        w := rdr.ReadUInt16;
        if FFileInfo.FileFmtRev >= $11 then
        begin
          // <3bit flags> <13bit key size>
          PageTag.Flags := ((w shr 13) and $7);
        end;
        PageTag.KeySize := w and $1FFF;

        if ((PageTag.Flags and 4) <> 0) then
          PageTag.KeySize := rdr.ReadUInt16 + 4  //  <CommonKeySize><KeySize><Key>
        else
          PageTag.KeySize := PageTag.KeySize + 2;  // <KeySize><Key>
        iLeafHeadSize := PageTag.KeySize;
        //iLeafHeadSize := 15;
        if IsDebugRows then
          LogInfo(Format('- Tag_%x flags=%x  head=%s',
            [i, PageTag.Flags, BufferToHex(APageBuf[FPageHeadSize + PageTag.ValueOffset], iLeafHeadSize + 10)]));

        if not Assigned(ATableInfo) then
          ATableInfo := TableList.GetByID(PageHead.ObjID);
        if Assigned(ATableInfo) then
          ReadDataRecord(APageBuf, FPageHeadSize + PageTag.ValueOffset + iLeafHeadSize, PageTag.ValueSize - iLeafHeadSize, ATableInfo, AList);
      end
      else
      begin
        //???
      end;
    end;

    // next tag
    Inc(i);
    iRecOffs := FPageSize - (i * TAG_SIZE);
  end;

end;

function TDBReaderEdb.ReadDataRecord(const APageBuf: TByteDynArray; ARecOffs, ARecSize: Integer;
  ATableInfo: TEdbTableInfo; AList: TDbRowsList): Boolean;
var
  rdr: TRawDataReader;
  i, ii, iOffs, iPrevOffs, iVarOffs, iVarCol, iVarStart, iFixPos, iBitmapLen, iTotalSize: Integer;
  LastFixColID, LastVarColID: Integer;
  ColType, ColSize, ColID: Word;
  NullBmp: TByteDynArray;
  v: Variant;
  TmpRow: TDbRowItem;
  VarLenArr: array of TEdbVarLenRec;
  TagLenArr: array of TEdbVarLenRec;
  NeedSkip: Boolean;
  sData: AnsiString;
begin
  // [0]   4   == Data definition header
  //       1   Last Fix ColID
  //       1   Last Var ColID
  //       2   Fixed size data offset [v]
  // [4]   x   == Fixed size columns
  //           == Null bitmap, size depend on fixed col count
  // [v]       == VarData offset array, 2 bytes per item. Same as cumulative size (4,4,8 is sizes 4,0,4)
  //           -- VarData data array
  //           == Tagged data
  Result := False;
  if not Assigned(ATableInfo) then Exit;
  if not Assigned(AList) then
    AList := ATableInfo;
  rdr.Init(APageBuf[ARecOffs]);
  //rdr.SetPosition(ARecOffs);
  // Data definition header
  LastFixColID := rdr.ReadUInt8;
  LastVarColID := rdr.ReadUInt8;
  iVarOffs := rdr.ReadUInt16;
  SetLength(VarLenArr, ATableInfo.VarColCount);
  SetLength(TagLenArr, ATableInfo.TaggedCount);
  if LastVarColID >= $80 then
    SetLength(VarLenArr, LastVarColID - $7F);

  // Null bitmap
  iBitmapLen := ((LastFixColID + 7) div 8); // bytes count
  SetLength(NullBmp, iBitmapLen);
  if iBitmapLen > 0 then
  begin
    rdr.SetPosition(iVarOffs - iBitmapLen);
    rdr.ReadToBuffer(NullBmp[0], iBitmapLen);
  end;

  // == read VarLen data
  rdr.SetPosition(iVarOffs);
  if ATableInfo.VarColCount > 0 then
  begin
    iVarStart := rdr.GetPosition;
    iTotalSize := 0;
    iPrevOffs := 0;
    // sizes
    for i := 0 to Length(VarLenArr) - 1 do
    begin
      iOffs := rdr.ReadUInt16;
      Inc(iVarStart, 2);
      if (iOffs and ($8000) <> 0) then
      begin
        VarLenArr[i].Size := -1;  // Null
      end
      else
      begin
        VarLenArr[i].Size := iOffs - iPrevOffs;
        Inc(iTotalSize, VarLenArr[i].Size);
        iPrevOffs := iOffs;
      end;

      // do not go outside record
      if iVarStart + iTotalSize > ARecSize then
      begin
        VarLenArr[i].Size := -1;
        if iTotalSize > ARecSize then
          Dec(iVarStart, 2);
        Break;
      end;
    end;
    // values
    rdr.SetPosition(iVarStart);
    for i := 0 to Length(VarLenArr) - 1 do
    begin
      if (VarLenArr[i].Size > 0) and (rdr.GetPosition + VarLenArr[i].Size <= ARecSize) then
      begin
        //Assert(rdr.GetPosition + VarLenArr[i].Size <= ARecSize,
        //  Format('!RecSize=%d  VarPos=%d  VarSize=%d', [ARecSize, rdr.GetPosition, VarLenArr[i].Size]));
        VarLenArr[i].Offs := rdr.GetPosition;
        VarLenArr[i].Data := rdr.ReadBytes(VarLenArr[i].Size);
      end
      else
      begin
        VarLenArr[i].Size := 0;
        VarLenArr[i].Data := '';
      end;
    end;
  end;

  // == read Tagged data
  if (ATableInfo.TaggedCount > 0) and (FileInfo.FileFmtRev >= $09) then
  begin
    iVarStart := rdr.GetPosition;
    if iVarStart >= ARecSize then
      SetLength(TagLenArr, 0);
    // sizes
    for i := 0 to Length(TagLenArr) - 1 do
    begin
      //Inc(iVarStart, 4);

      TagLenArr[i].ColID := rdr.ReadUInt16;
      if (TagLenArr[i].ColID < $100) then
      begin
        LogInfo('Wrong tagged value ColID!');
        TagLenArr[i].Size := -1;
        Break;
      end;
      iOffs := rdr.ReadUInt16;
      TagLenArr[i].Offs := iVarStart + (iOffs and $3FFF) + 1;  // strip flags
      TagLenArr[i].Size := ARecSize - TagLenArr[i].Offs;
      if i > 0 then
        TagLenArr[i-1].Size := TagLenArr[i].Offs - TagLenArr[i-1].Offs;

      // do not go outside record
      if TagLenArr[i].Offs + TagLenArr[i].Size > ARecSize  then
      begin
        TagLenArr[i].Size := 0;
        Break;
      end;
    end;
    // values
    for i := 0 to Length(TagLenArr) - 1 do
    begin
      if (TagLenArr[i].Size > 0) and (TagLenArr[i].Offs + TagLenArr[i].Size <= ARecSize) then
      begin
        rdr.SetPosition(TagLenArr[i].Offs);
        //Assert(rdr.GetPosition + TagLenArr[i].Size <= ARecSize,
        //  Format('!RecSize=%d  VarPos=%d  VarSize=%d', [ARecSize, rdr.GetPosition, TagLenArr[i].Size]));
        TagLenArr[i].Data := rdr.ReadBytes(TagLenArr[i].Size);
      end
      else
        TagLenArr[i].Data := '';
    end;

  end;

  TmpRow := TDbRowItem.Create(AList);
  AList.Add(TmpRow);
  SetLength(TmpRow.Values, Length(ATableInfo.FieldInfoArr));
  if IsDebugRows then
  begin
    rdr.SetPosition(0);
    TmpRow.RawData := rdr.ReadBytes(ARecSize);
    //if Pos('tbUpdates', TmpRow.RawData) > 0 then
    //  BufToFile(TmpRow.RawData[1], ARecSize, 'tbUpdates.data');

  end;

  // fields
  iVarCol := 0;
  iFixPos := 4; // after header
  for i := 0 to Length(ATableInfo.FieldInfoArr) - 1 do
  begin
    ColID := ATableInfo.FieldInfoArr[i].ColID;
    ColType := ATableInfo.FieldInfoArr[i].ColTypOrPgNoFdp;
    ColSize := ATableInfo.FieldInfoArr[i].SpaceUsage;
    NeedSkip := True;
    if (ColID >= $100) then
    begin
      // tagged data
      for ii := 0 to Length(TagLenArr) - 1 do
      begin
        if (TagLenArr[ii].ColID = ColID) and (TagLenArr[ii].Size >= ColSize) then
        begin
          rdr.SetPosition(TagLenArr[ii].Offs);
          ColSize := TagLenArr[ii].Size;
          NeedSkip := False;
          Break;
        end;
      end;
    end
    else
    if (ColID < $80) then
    begin
      // fixed size and position
      if (ColID <= LastFixColID) then
      begin
        rdr.SetPosition(iFixPos);
        Inc(iFixPos, ColSize);
        if not IsNullValue(NullBmp, ColId) then
          NeedSkip := False;
      end;
    end
    else if (ColID <= LastVarColID) then
    begin
      // variable size
      case ColType of
        COL_TYPE_BINARY,
        COL_TYPE_TEXT:
        begin
          if (VarLenArr[iVarCol].Size > 0) then
          begin
            rdr.SetPosition(VarLenArr[iVarCol].Offs);
            ColSize := VarLenArr[iVarCol].Size;
            NeedSkip := False;
          end;
          Inc(iVarCol);
        end
      else
        Assert(False, 'VarLen wrong type=' + IntToStr(ColType));
      end;
    end;

    if NeedSkip then
    begin
      TmpRow.Values[i] := Null;
      Continue;
    end;

    v := Null;
    case ColType of
      COL_TYPE_NIL:       rdr.ReadUInt8;
      COL_TYPE_BIT:       v := (rdr.ReadUInt8 <> 0);
      COL_TYPE_BYTE:      v := rdr.ReadUInt8;
      COL_TYPE_SHORT:     v := rdr.ReadInt16;
      COL_TYPE_LONG:      v := rdr.ReadInt32;
      COL_TYPE_CURRENCY:  v := rdr.ReadCurrency;
      COL_TYPE_SINGLE:    v := rdr.ReadSingle;
      COL_TYPE_DOUBLE:    v := rdr.ReadDouble;
      COL_TYPE_DATETIME:  v := TDateTime(rdr.ReadDouble);
      COL_TYPE_ULONG:     v := rdr.ReadUInt32;
      COL_TYPE_LONG_LONG: v := rdr.ReadInt64;
      COL_TYPE_USHORT:    v := rdr.ReadUInt16;

      COL_TYPE_GUID:      v := '<GUID>';

      // variable size fields
      COL_TYPE_BINARY,
      COL_TYPE_TEXT:
      begin
        sData := rdr.ReadBytes(ColSize);
        if ATableInfo.FieldInfoArr[i].PagesOrLocale = 1200 then  // UTF-16
          sData := WideDataToStr(sData);
        v := sData;
      end;
      COL_TYPE_LONG_BINARY,
      COL_TYPE_LONG_TEXT:
      begin
        sData := rdr.ReadBytes(ColSize);
        if ATableInfo.FieldInfoArr[i].PagesOrLocale = 1200 then  // UTF-16
          sData := WideDataToStr(sData);
        v := sData;
      end;
      COL_TYPE_SLV:
      begin
        v := '<SLV>';
      end
    else
      Assert(False, 'Unknown column type: ' + IntToStr(ColType));
    end;
    TmpRow.Values[i] := v;
  end;

end;

function TDBReaderEdb.ReadPageHeader(const APageBuf: TByteDynArray;
  var APageHead: TEdbPageHeadRec): Boolean;
var
  rdr: TRawDataReader;
begin
  rdr.Init(APageBuf[0]);
  // header
  if FFileInfo.FileFmtRev >= $11 then  // Win7+
  begin
    APageHead.Checksum := rdr.ReadInt64;
  end
  else
  if FFileInfo.FileFmtRev >= $0B then  // WinVista
  begin
    APageHead.Checksum := rdr.ReadInt64;
  end
  else
  begin
    APageHead.Checksum := rdr.ReadUInt32;
    APageHead.PageNumber := rdr.ReadUInt32;
  end;
  APageHead.ModificationTime := rdr.ReadDouble;
  APageHead.PrevPageNum := rdr.ReadUInt32;
  APageHead.NextPageNum := rdr.ReadUInt32;
  APageHead.ObjID := rdr.ReadUInt32;
  APageHead.AvailSize := rdr.ReadUInt16;
  APageHead.UncommittedSize := rdr.ReadUInt16;
  APageHead.AvailOffs := rdr.ReadUInt16;
  APageHead.AvailPageTag := rdr.ReadUInt16;
  APageHead.PageFlags := rdr.ReadUInt32;
  if FFileInfo.FileFmtRev >= $11 then  // Win7+
  begin
    rdr.ReadInt64; // CRC 1
    rdr.ReadInt64; // CRC 2
    rdr.ReadInt64; // CRC 3
    APageHead.PageNumber := rdr.ReadInt64;
    rdr.ReadInt64; // empty
  end;
  Result := True;
end;

procedure TDBReaderEdb.ReadTable(AName: string; ACount: Int64; AList: TDbRowsList);
var
  TmpTable: TEdbTableInfo;
  i: Integer;
  PageBuf: TByteDynArray;
  iPagePos: Int64;
  //TmpRow: TDbRowItem;
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
  AList.Clear();
  AList.FieldsDef := TmpTable.FieldsDef;
  // !! copy data
  {for i := 0 to TmpTable.Count - 1 do
  begin
    TmpRow := TDbRowItem.Create(AList);
    AList.Add(TmpRow);

    TmpRow.Values := TmpTable.GetItem(i).Values;
    TmpRow.RawData := TmpTable.GetItem(i).RawData;
  end; }

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

{ TEdbTableInfoList }

function TEdbTableInfoList.GetByID(ATableID: Integer): TEdbTableInfo;
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

function TEdbTableInfoList.GetByName(AName: string): TEdbTableInfo;
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

function TEdbTableInfoList.GetItem(AIndex: Integer): TEdbTableInfo;
begin
  Result := TEdbTableInfo(Get(AIndex));
end;

procedure TEdbTableInfoList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Action = lnDeleted then
    TEdbTableInfo(Ptr).Free;
end;

procedure TEdbTableInfoList.SortByName;
begin

end;

{ TEdbTableInfo }

procedure TEdbTableInfo.AddFieldDef(AName: string; AType: Word; ALength, AColID, AColFlags, ALocale: Integer);
var
  i, n: Integer;
begin
  if AColID < 0 then
    AColID := Length(FieldInfoArr) + 1;

  if ALength = 0 then
  begin
    case AType of
      COL_TYPE_NIL: ALength := 1;
      COL_TYPE_BIT: ALength := 1;
      COL_TYPE_BYTE: ALength := 1;
      COL_TYPE_SHORT: ALength := 2;
      COL_TYPE_LONG: ALength := 4;
      COL_TYPE_CURRENCY: ALength := 8;
      COL_TYPE_SINGLE: ALength := 4;
      COL_TYPE_DOUBLE: ALength := 8;
      COL_TYPE_DATETIME: ALength := 8;
      COL_TYPE_BINARY: ALength := 255;
      COL_TYPE_TEXT: ALength := 255;
      COL_TYPE_ULONG: ALength := 4;
      COL_TYPE_LONG_LONG: ALength := 8;
      COL_TYPE_USHORT: ALength := 2;
      COL_TYPE_GUID: ALength := 16;
    end;
  end;

  // get array index for ColID
  n := 0;
  for i := 0 to Length(FieldInfoArr) - 1 do
  begin
    if FieldInfoArr[n].ColID = AColID then
      Exit
    else
    if FieldInfoArr[n].ColID < AColID then
     n := i + 1;
  end;

  SetLength(FieldInfoArr, Length(FieldInfoArr)+1);
  SetLength(FieldsDef, Length(FieldsDef)+1);
  // if not last item, shift items
  if (n+1) < Length(FieldInfoArr) then
  begin
    for i := Length(FieldInfoArr)-1 downto n+1 do
    begin
      FieldInfoArr[i] := FieldInfoArr[i-1];
      FieldsDef[i] := FieldsDef[i-1];
    end;
  end;

  FieldInfoArr[n].ColTypOrPgNoFdp := AType;
  FieldInfoArr[n].ColID := Cardinal(AColID);
  FieldInfoArr[n].SpaceUsage := Cardinal(ALength);
  FieldInfoArr[n].Flags := Cardinal(AColFlags);
  FieldInfoArr[n].PagesOrLocale := ALocale;
  FieldInfoArr[n].Name := AName;

  FieldsDef[n].Name := AName;
  FieldsDef[n].TypeName := Format('%s(%d)', [EdbColTypeToStr(AType), ALength]);
  FieldsDef[n].FieldType := EdbColTypeToDbFieldType(AType);
  FieldsDef[n].Size := ALength;
  FieldsDef[n].RawOffset := 0;

  if (AColID >= $80) and (AColID < $100) then
    Inc(VarColCount);

  if (AColID >= $100)then
    Inc(TaggedCount);
end;

function TEdbTableInfo.IsEmpty: Boolean;
begin
  Result := (RowCount = 0);
end;

function TEdbTableInfo.IsGhost: Boolean;
begin
  Result := (Length(FieldsDef) = 0);
end;

function TEdbTableInfo.IsSystem: Boolean;
begin
  Result := (Copy(TableName, 1, 4) = 'MSys');
end;

end.
