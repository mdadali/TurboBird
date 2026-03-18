unit DBReaderSybase;

(*
Sybase/Watcom database file reader

Author: Sergey Bodrov, 2025 Minsk
License: MIT

Data format guessed from sample databases
*)

interface

uses
  SysUtils, Classes, Types, Variants, DB, DBReaderBase, RFUtils;

type
  TSybaseFieldDef = record
    FieldName: string;
    FieldSize: Integer;
    FieldType: Integer;
    TypeName: string;
    IsPrimaryKey: Boolean;
    IsNullable: Boolean;
  end;

  TSybaseTableInfo = class(TDbRowsList)
  public
    TableID: Integer;
    //RootPageID: Cardinal;  // 0-based
    RowCount: Integer;
    ColCount: Integer;
    NullCount: Integer;  // nullable fields count
    BoolCount: Integer;  // boolean fields count
    PageIdArr: array of Integer;
    PageIdCount: Integer;
    FieldInfoArr: array of TSybaseFieldDef;
    TableSQL: string;
    RawHead: string; // !!!
    TableType: Integer;
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
    procedure AddPageID(APageID: Cardinal);
  end;

  TSybaseTableInfoList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function GetItem(AIndex: Integer): TSybaseTableInfo;
    function GetByName(AName: string): TSybaseTableInfo;
    function GetByTableID(ATabID: Cardinal): TSybaseTableInfo;
    //function GetByPageID(ANum: Cardinal): TSybaseTableInfo;
    procedure SortByName();
  end;

  TDBReaderSybase = class(TDBReader)
  private
    FTableList: TSybaseTableInfoList;
    FDbVersion: Integer;
    FPageSize: Integer;
    FIsMetadataLoaded: Boolean;
    FPagesInfo: TSybaseTableInfo;

    // fill schema tables from initial tables
    procedure FillTablesList();

    function ReadDataPage(var APageBuf: TByteDynArray; APagePos: Int64;
      ATableInfo: TSybaseTableInfo; AList: TDbRowsList): Boolean;
    function ReadRowData(const APageBuf: TByteDynArray; ARowOffs, ARowSize: Integer;
      ATableInfo: TSybaseTableInfo; AList: TDbRowsList): Boolean;
    function ReadRowDataRaw(const APageBuf: TByteDynArray; ARowOffs, ARowSize: Integer;
      ATableInfo: TSybaseTableInfo; AList: TDbRowsList): Boolean;

    function ReadNumeric(var rdr: TRawDataReader): string;

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

    property TableList: TSybaseTableInfoList read FTableList;
  end;


implementation

const
  SY_TABLE_TYPE_SYS      = $01;

  SY_PAGE_TYPE_PAGEMAP  = $02;
  SY_PAGE_TYPE_FILEHEAD = $41;  // file header
  SY_PAGE_TYPE_DATA     = $42;  // table rows
  SY_PAGE_TYPE_DATA2    = $53;  //
  SY_PAGE_TYPE_DATA3    = $73;  // table pagemap?

  SY_PAGE_HEAD_SIZE = 20;

  SY_COL_TYPE_UNKNOWN  = 0;
  SY_COL_TYPE_SMALLINT = 1;   // 2
  SY_COL_TYPE_INT      = 2;   // 4
  SY_COL_TYPE_NUMERIC  = 3;   // 20   salary
  SY_COL_TYPE_INT2     = 4;   // 4  avg_num_rows, avg_cost
  SY_COL_TYPE_DATE     = 6;   // 4  date
  SY_COL_TYPE_CHAR     = 8;
  SY_COL_TYPE_CHAR2    = 9;   // 128  remote_name
  SY_COL_TYPE_TEXT     = 10;
  SY_COL_TYPE_TEXT2    = 11;  // 1280 collation_order
  SY_COL_TYPE_DATETIME = 13;  // 8    date
  SY_COL_TYPE_BOOLEAN  = 19;  // 1    flag  (bitmap for next bool columns)
  SY_COL_TYPE_UINT64   = 20;  // 8    max_identity
  SY_COL_TYPE_UINT32   = 21;  // 4    table_id, first_page
  SY_COL_TYPE_INT64    = 23;  // 8    creator


type
  TSybaseFileHeader = record
    //HeaderStr: array [0..$150] of AnsiChar;  //
    PageSize: Integer;                        // The database page size in bytes.
    // 150 bytes
  end;

  TSybasePageHead = record
    PageType: Word;       //
    RecCount: Word;
    FreeSize: Word;
    TableID: Word;
    RawData: string;
  end;

function ColTypeToStr(AType: Integer): string;
begin
  case AType of
    SY_COL_TYPE_SMALLINT: Result := 'INT';
    SY_COL_TYPE_INT:      Result := 'INT';
    SY_COL_TYPE_NUMERIC:  Result := 'NUMERIC';
    SY_COL_TYPE_INT2:     Result := 'INT';
    SY_COL_TYPE_DATE:     Result := 'DATE';
    SY_COL_TYPE_CHAR:     Result := 'CHAR';
    SY_COL_TYPE_CHAR2:    Result := 'CHAR';
    SY_COL_TYPE_TEXT:     Result := 'TEXT';
    SY_COL_TYPE_TEXT2:    Result := 'TEXT';
    SY_COL_TYPE_DATETIME: Result := 'DATETIME';
    SY_COL_TYPE_BOOLEAN:  Result := 'BOOLEAN';
    SY_COL_TYPE_UINT64:   Result := 'INT';
    SY_COL_TYPE_UINT32:   Result := 'INT';
    SY_COL_TYPE_INT64:    Result := 'INT';
  else
    Result := 'UNKNOWN_'+IntToStr(AType);
  end;
end;

function SybaseColTypeToInt(AType: string): Integer;
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
  if s = 'NULL' then Result := SY_COL_TYPE_UNKNOWN
  else if s = 'INTEGER' then Result := SY_COL_TYPE_INT
  else if s = 'INT' then Result := SY_COL_TYPE_INT
  else if s = 'INT2' then Result := SY_COL_TYPE_INT
  else if s = 'INT8' then Result := SY_COL_TYPE_INT
  else if s = 'TINYINT' then Result := SY_COL_TYPE_INT
  else if s = 'SMALLINT' then Result := SY_COL_TYPE_INT
  else if s = 'MEDIUMINT' then Result := SY_COL_TYPE_INT
  else if s = 'BIGINT' then Result := SY_COL_TYPE_INT
  else if s = 'TEXT' then Result := SY_COL_TYPE_CHAR
  else if s = 'VARCHAR' then Result := SY_COL_TYPE_CHAR
  else if s = 'NVARCHAR' then Result := SY_COL_TYPE_CHAR
  else if s = 'CHAR' then Result := SY_COL_TYPE_CHAR
  else if s = 'NCHAR' then Result := SY_COL_TYPE_CHAR
  else if s = 'CHARACTER' then Result := SY_COL_TYPE_CHAR
  else if s = 'CLOB' then Result := SY_COL_TYPE_CHAR
  else if s = 'BLOB' then Result := SY_COL_TYPE_UNKNOWN
  else if s = 'REAL' then Result := SY_COL_TYPE_UNKNOWN
  else if s = 'DOUBLE' then Result := SY_COL_TYPE_UNKNOWN
  else if s = 'FLOAT' then Result := SY_COL_TYPE_UNKNOWN
  else if s = 'NUMERIC' then Result := SY_COL_TYPE_NUMERIC
  else if s = 'DATE' then Result := SY_COL_TYPE_DATE
  else if s = 'DATETIME' then Result := SY_COL_TYPE_DATETIME
  else if s = 'BOOLEAN' then Result := SY_COL_TYPE_BOOLEAN
  else Result := SY_COL_TYPE_UNKNOWN;
end;

function SybaseColTypeToDbFieldType(AType: string): TFieldType;
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
  else if s = 'NUMERIC' then Result := ftFloat
  else if s = 'DATE' then Result := ftDate
  else if s = 'DATETIME' then Result := ftDateTime
  else if s = 'BOOLEAN' then Result := ftBoolean
  else Result := ftUnknown;
end;

function IsNullValue(const ANullBmp: TByteDynArray; AIndex: Integer): Boolean;
var
  NullFlagIndex, NullFlagOffs: Integer;
begin
  //NullFlagIndex := Length(ANullBmp) - (AIndex div 8) - 1;
  NullFlagIndex := (AIndex div 8);
  NullFlagOffs := AIndex mod 8;
  if (NullFlagIndex < Length(ANullBmp)) and (NullFlagIndex >= 0) then
    //Result := (ANullBmp[NullFlagIndex] and (Byte(1) shl NullFlagOffs)) <> 0
    Result := (ANullBmp[NullFlagIndex] and (Byte($80) shr NullFlagOffs)) = 0
  else
    Result := False;
end;

{ TDBReaderSybase }

procedure TDBReaderSybase.AfterConstruction;
var
  TmpTable: TSybaseTableInfo;
begin
  inherited;
  FIsSingleTable := False;
  FTableList := TSybaseTableInfoList.Create();

  {// sys_schema
  TmpTable := TSybaseTableInfo.Create();
  TmpTable.RootPageNum := 1;
  TmpTable.TableType := SY_TABLE_TYPE_SYS;
  TmpTable.SetSQL('CREATE TABLE sys_schema(type text, name text, tbl_name text, rootpage integer, sql text, rowid integer PRIMARY KEY);');
  TableList.Add(TmpTable);
  FSchemaInfo := TmpTable;  }

  // sys_pages
  TmpTable := TSybaseTableInfo.Create();
  TmpTable.TableID := -1;
  TmpTable.TableType := SY_TABLE_TYPE_SYS;
  TmpTable.SetSQL('CREATE TABLE sys_pages(PageID INTEGER, PageType INTEGER, PagePos INTEGER,'
    + ' RecCount INTEGER, '   // [3]
    + ' FreeSize INTEGER, '   // [4]
    + ' TableID INTEGER, '    // [5]
    + ' Info TEXT, '          // [6]
    + ' RawHeader TEXT(240));'); // [7]
  TableList.Add(TmpTable);
  FPagesInfo := TmpTable;

  // base.systable
  TmpTable := TSybaseTableInfo.Create();
  TmpTable.TableID := $1;
  TmpTable.TableType := SY_TABLE_TYPE_SYS;
  TmpTable.RowCount := -1;
  TmpTable.SetSQL('CREATE TABLE systable('
  + ' table_id INT(4) NOT NULL,'
  + ' file_id INT(2) NOT NULL,'
  + ' count INT(8) NOT NULL,'
  + ' first_page INT(4) NOT NULL,'
  + ' last_page INT(4) NOT NULL,'
  + ' primary_root INT(4) NOT NULL,'
  + ' creator INT(4) NOT NULL,'
  + ' first_ext_page INT(4) NOT NULL,'
  + ' last_ext_page INT(4) NOT NULL,'
  + ' table_page_count INT(4) NOT NULL,'
  + ' ext_page_count INT(4) NOT NULL,'
  + ' table_name CHAR(128) NOT NULL,'
  + ' table_type TEXT NOT NULL,'
  + ' view_def TEXT,'
  + ' remarks TEXT,'
  + ' replicate INT(1) NOT NULL,'
  + ' existing_obj INT(1),'
  + ' remote_location TEXT,'
  + ' remote_objtype INT(1),'
  + ' srvid INT(4),'
  + ' server_type INT(4) NOT NULL,'
  + ' primary_hash_limit INT(2) NOT NULL,'
  + ' page_map_start INT(4) NOT NULL,'
  + ' source TEXT'
  + ');');
  TableList.Add(TmpTable);

  // base.syscolumn
  TmpTable := TSybaseTableInfo.Create();
  TmpTable.TableID := $2;
  TmpTable.TableType := SY_TABLE_TYPE_SYS;
  TmpTable.RowCount := -1;
  TmpTable.SetSQL('CREATE TABLE syscolumn('
  + ' table_id INT(4) NOT NULL,'
  + ' column_id INT(4) NOT NULL,'
  + ' pkey CHAR(1) NOT NULL,'
  + ' domain_id INT(2) NOT NULL,'
  + ' nulls CHAR(1) NOT NULL,'
  + ' width INT(2) NOT NULL,'
  + ' scale INT(2) NOT NULL,'
  + ' unused INT(4) NOT NULL,'
  + ' max_identity INT(8) NOT NULL,'
  + ' column_name CHAR(128) NOT NULL,'
  + ' remarks TEXT,'
  + ' default TEXT,'
  + ' unused2 TEXT,'
  + ' user_type INT(2)'
  + ');');
  TableList.Add(TmpTable);

  // !!!!
  // sample tables
  {TmpTable := TSybaseTableInfo.Create();
  TmpTable.TableID := $1BD;
  TmpTable.TableType := SY_PAGE_TYPE_DATA;
  TmpTable.RowCount := -1;
  TmpTable.SetSQL('CREATE TABLE department(dept_id INTEGER, dept_name CHAR(40), dept_head_id INTEGER);');
  TableList.Add(TmpTable); }

end;

procedure TDBReaderSybase.BeforeDestruction;
begin
  FreeAndNil(FTableList);
  inherited;
end;

function TDBReaderSybase.FillTableInfoText(ATableName: string; ALines: TStrings): Boolean;
var
  i: Integer;
  TmpTable: TSybaseTableInfo;
  s: string;
begin
  Result := False;
  TmpTable := TableList.GetByName(ATableName);
  if not Assigned(TmpTable) then
    Exit;

  ALines.Add(Format('== Table Name=%s  RowCount=%d', [TmpTable.TableName, TmpTable.RowCount]));
  ALines.Add(Format('TableID=%x', [TmpTable.TableID]));
  ALines.Add(Format('ColCount=%d', [TmpTable.ColCount]));
  ALines.Add(Format('NullCount=%d', [TmpTable.NullCount]));
  ALines.Add(Format('BoolCount=%d', [TmpTable.BoolCount]));
  // PageNum array
  s := '';
  for i := Low(TmpTable.PageIdArr) to High(TmpTable.PageIdArr) do
    s := s + Format('%x,', [TmpTable.PageIdArr[i]]);
  //Delete(s, Length(s)-1, 1);
  ALines.Add(Format('PageNum[%d]=(%s)', [TmpTable.PageIdCount, s]));
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
end;

procedure TDBReaderSybase.FillTablesList;
var
  TmpTable, TabList, ColList: TSybaseTableInfo;
  TabRow, ColRow: TDbRowItem;
  i, ii: Integer;
  TabID: Cardinal;
  sName, sType: string;
begin
  FIsMetadataLoaded := True;
  // normalize page num arrays
  for i := 0 to TableList.Count - 1 do
  begin
    TmpTable := TableList.GetItem(i);
    SetLength(TmpTable.PageIdArr, TmpTable.PageIdCount);
  end;

  TabList := TableList.GetByTableID(1);
  ColList := TableList.GetByTableID(2);
  if Assigned(TabList) and Assigned(ColList) then
  begin
    ReadTable(TabList.TableName, MaxInt, TabList);
    ReadTable(ColList.TableName, MaxInt, ColList);
    // set tables from schema
    for i := 0 to TabList.Count - 1 do
    begin
      TabRow := TabList.GetItem(i);

      TabID := TabRow.Values[0];

      TmpTable := TableList.GetByTableID(TabID);
      if Assigned(TmpTable) then
      begin
        // skip predefined tables
        if (TmpTable.TableID = 1)
        or (TmpTable.TableID = 2) then
          Continue;

        TmpTable.TableName := VarToStrDef(TabRow.Values[11], TmpTable.TableName);
        TmpTable.TableSQL := 'CREATE TABLE';
        TmpTable.Clear();

        // columns
        TmpTable.ColCount := 0;
        TmpTable.NullCount := 0;
        TmpTable.BoolCount := 0;
        if VarToInt(TabRow.Values[6]) = 0 then    // creator
          TmpTable.TableType := SY_TABLE_TYPE_SYS;

        SetLength(TmpTable.FieldsDef, 0);
        SetLength(TmpTable.FieldInfoArr, 0);

        for ii := 0 to ColList.Count - 1 do
        begin
          ColRow := ColList.GetItem(ii);
          if VarToInt(ColRow.Values[0]) <> TabID then
            Continue;

          sName := VarToStrDef(ColRow.Values[9], '');
          sType := ColTypeToStr(VarToInt(ColRow.Values[3]));
          sType := sType + '(' + IntToStr(VarToInt(ColRow.Values[5])) + ')';
          if VarToStrDef(ColRow.Values[2], 'N') = 'Y' then
            sType := sType + ' PRIMARY KEY';
          if VarToStrDef(ColRow.Values[4], 'N') = 'N' then
            sType := sType + ' NOT NULL';

          TmpTable.AddFieldDef(sName, sType);
        end;
      end;
    end;
  end;
end;

function TDBReaderSybase.GetTableByIndex(AIndex: Integer): TDbRowsList;
begin
  Result := TableList[AIndex];
end;

function TDBReaderSybase.GetTablesCount: Integer;
begin
  Result := TableList.Count;
end;

function TDBReaderSybase.OpenFile(AFileName: string; AStream: TStream): Boolean;
var
  PageBuf: TByteDynArray;
  //FileHead: TSybaseFileHeader;
  rdr: TRawDataReader;
  iPagePos: Int64;
  iCurPageID: Cardinal;
  bt: Integer;
begin
  Result := inherited OpenFile(AFileName, AStream);
  if not Result then Exit;
  Result := False;
  FPageSize := $800;

  // read header
  SetLength(PageBuf, FPageSize);
  FFile.Position := 0;
  FFile.Read(PageBuf[0], FPageSize);
  rdr.Init(PageBuf[0], False);

  rdr.SetPosition($09);   // 03, 07, 0F
  bt := rdr.ReadUInt8;

  rdr.SetPosition($1A);   // 0A, 0B, 0C
  bt := rdr.ReadUInt8;
  case bt of
    $0A: FPageSize := $0400;
    $0B: FPageSize := $0800;
    $0C: FPageSize := $1000;
    $0D: FPageSize := $2000;
    $0E: FPageSize := $4000;
    $0F: FPageSize := $8000;
  end;
  SetLength(PageBuf, FPageSize);

  // scan pages
  iCurPageID := 0;
  iPagePos := 0;
  while iPagePos + FPageSize <= FFile.Size do
  begin
    FFile.Position := iPagePos;
    FFile.Read(PageBuf[0], FPageSize);
    rdr.Init(PageBuf[0], False);

    ReadDataPage(PageBuf, iPagePos, nil, nil);

    Inc(iCurPageID);
    iPagePos := iCurPageID * FPageSize;

    if Assigned(OnPageReaded) then
      OnPageReaded(Self);
  end;

  SetLength(PageBuf, 0);

  FillTablesList();
end;

function TDBReaderSybase.ReadDataPage(var APageBuf: TByteDynArray; APagePos: Int64;
  ATableInfo: TSybaseTableInfo; AList: TDbRowsList): Boolean;
var
  rdr: TRawDataReader;
  PageHead: TSybasePageHead;
  RecOffsArr: array of Word;
  i, iRowOffs, iRowOffs2, iRowSize: Integer;
  CurPageID: Cardinal;
  TmpRow: TDbRowItem;
  sPageHead: string;
  bufA, bufB: array [0..5] of Byte;
begin
(*
   first 6 bytes swapped with last 6 bytes of page
   == 20 bytes page header
   00  u32 PageID
   01  u8  PageType
   02  u8  ??
   06  u16 Rec count
   08  u16 Free size
   0A

   12  u16 Table part num

   == Rec offs array of u16  (big-endian)
   14  u16

   == rec data
*)

  Result := False;
  CurPageID := (APagePos div FPageSize);
  rdr.Init(APageBuf[0], False);

  if APagePos > 0 then
  begin
    // swap first and last bytes
    rdr.ReadToBuffer(bufA, 6);
    rdr.SetPosition(FPageSize-6);
    rdr.ReadToBuffer(bufB, 6);
    Move(bufB, APageBuf[0], 6);
    Move(bufA, APageBuf[FPageSize-6], 6);
    rdr.SetPosition(0);
  end;

  // page header
  rdr.ReadUInt32;   // PageID
  PageHead.PageType := rdr.ReadUInt8;
  rdr.ReadUInt8;    // ??
  //rdr.ReadUInt8;
  PageHead.RecCount := rdr.ReadUInt16;
  PageHead.FreeSize := rdr.ReadUInt16;
  rdr.SetPosition($12);
  PageHead.TableID := rdr.ReadUInt16;

  TmpRow := nil;
  sPageHead := '';
  if IsDebugPages and (not FIsMetadataLoaded) then
  begin
    // raw data
    rdr.SetPosition(0);
    PageHead.RawData := rdr.ReadBytes(20);

    // sys_pages
    TmpRow := TDbRowItem.Create(FPagesInfo);
    SetLength(TmpRow.Values, Length(FPagesInfo.FieldsDef));
    FPagesInfo.Add(TmpRow);

    TmpRow.Values[0] := CurPageID;
    TmpRow.Values[1] := PageHead.PageType;
    TmpRow.Values[2] := APagePos;
    TmpRow.Values[3] := PageHead.RecCount;
    TmpRow.Values[4] := PageHead.FreeSize;
    TmpRow.Values[5] := PageHead.TableID;
    TmpRow.Values[6] := '';   // info
    sPageHead := BufToHex(PageHead.RawData[1], 4)
      + ' ' + BufToHex(PageHead.RawData[4], 1)
      + ' ' + BufToHex(PageHead.RawData[5], 1)
      + ' ' + BufToHex(PageHead.RawData[7], 2)
      + ' ' + BufToHex(PageHead.RawData[9], 2)
      + ' ' + BufToHex(PageHead.RawData[11], 8)
      + ' ' + BufToHex(PageHead.RawData[19], 2);
    TmpRow.Values[7] := sPageHead;
  end;

  if (PageHead.PageType = 0) or (PageHead.TableID = 0) then
    Exit;
 
  // check page
  if IsDebugPages and (not FIsMetadataLoaded) then
  begin
    if Assigned(ATableInfo) {and (ATableInfo.RootPageID <> CurPageID)} then
      Exit;
    // find/add table for page
    if not Assigned(ATableInfo) then
      ATableInfo := TableList.GetByTableID(PageHead.TableID);
    if not Assigned(ATableInfo) then
    begin
      ATableInfo := TSybaseTableInfo.Create();
      ATableInfo.TableID := PageHead.TableID;
      ATableInfo.TableName := Format('Table_%x', [PageHead.TableID]);
      ATableInfo.RowCount := PageHead.RecCount;
      ATableInfo.TableType := SY_PAGE_TYPE_PAGEMAP;
      TableList.Add(ATableInfo);
      // raw data columns
      ATableInfo.AddFieldDef('RowID', 'INT');
      ATableInfo.AddFieldDef('Offs', 'INT');
      ATableInfo.AddFieldDef('Size', 'INT');
      ATableInfo.AddFieldDef('RawData', 'TEXT(250)');
      ATableInfo.AddFieldDef('PageData', 'TEXT(250)');
    end;
    if PageHead.PageType = SY_PAGE_TYPE_DATA then
      ATableInfo.AddPageID(CurPageID);
    ATableInfo.RawHead := sPageHead;
  end;

  // rec offs list
  rdr.SetPosition(20);
  SetLength(RecOffsArr, PageHead.RecCount);
  if PageHead.RecCount > 0 then
  begin
    if Assigned(TmpRow) then
      TmpRow.Values[6] := '+++';
    if (PageHead.RecCount * SizeOf(Word)) >= FPageSize then
      Exit;
    Assert(PageHead.RecCount * SizeOf(Word) < FPageSize);
    rdr.ReadToBuffer(RecOffsArr[0], PageHead.RecCount * SizeOf(Word)); // big-endian!

    iRowOffs2 := FPageSize;
    for i := Low(RecOffsArr) to High(RecOffsArr) do
    begin
      if RecOffsArr[i] = 0 then   // deleted record ?
        Continue;
      iRowOffs := SY_PAGE_HEAD_SIZE + RecOffsArr[i];
      iRowSize := iRowOffs2 - iRowOffs;

      if (iRowSize < 0) or (not (iRowOffs < FPageSize)) then
      begin
        if Assigned(TmpRow) then
          TmpRow.Values[6] := '---';
        Exit;
      end;
      Assert(iRowOffs < FPageSize);
      rdr.SetPosition(iRowOffs);

      if FIsMetadataLoaded then
        ReadRowData(APageBuf, iRowOffs, iRowSize, ATableInfo, AList)
      else
        ReadRowDataRaw(APageBuf, iRowOffs, iRowSize, ATableInfo, AList);

      iRowOffs2 := iRowOffs;

      {if PageHead.PageType = DB3_PAGE_TYPE_TABLE_LEAF then
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
      end; }
    end;
  end;

  Result := True;
end;

function TDBReaderSybase.ReadNumeric(var rdr: TRawDataReader): string;
var
  i, iVarSize, iShift, iPos: Integer;
  s: string;
begin
  // <len> <shift> [len bytes]
  // 51432 = 5_14_32 = 05 0E 20 (little-enddian) len=3, shift=C0
  // shift C0 = (10^0), C1 = (10^2), BF = (10^-2)
  // if len = 0, there no shift
  iVarSize := rdr.ReadUInt8;
  iShift := rdr.ReadUInt8;
  s := '';
  if iVarSize > 0 then
  begin
  for i := 1 to iVarSize do
      s := Format('%.2d', [rdr.ReadUInt8]) + s;
    iShift := $C0 - iShift;
  end
  else
  begin
    s := Format('%.2d', [iShift]);
    iShift := 0;
  end;

  while (iShift < 0) do
  begin
    s := s + '00';
    Inc(iShift);
  end;
  if (iShift > 0) then
  begin
    iPos := Length(s) - iShift - 1;
    Result := Copy(s, 1, iPos) + '.' + Copy(s, iPos+1, MaxInt);
  end
  else
    Result := s;

  if Copy(Result, 1, 1) = '0' then
    Result := Copy(Result, 2, MaxInt);
end;

function TDBReaderSybase.ReadRowDataRaw(const APageBuf: TByteDynArray; ARowOffs, ARowSize: Integer;
  ATableInfo: TSybaseTableInfo; AList: TDbRowsList): Boolean;
var
  rdr: TRawDataReader;
  TmpRow: TDbRowItem;
  s: string;
begin
  Result := False;
  if (ARowSize <= 0) or (ATableInfo.TableType <> SY_PAGE_TYPE_PAGEMAP) then
    Exit;
  rdr.Init(APageBuf[0], True);
  rdr.SetPosition(ARowOffs);

  if not Assigned(AList) then
    AList := ATableInfo;
  TmpRow := TDbRowItem.Create(AList);
  AList.Add(TmpRow);
  SetLength(TmpRow.Values, ATableInfo.ColCount);

  s := rdr.ReadBytes(ARowSize);

  TmpRow.Values[0] := AList.Count;
  TmpRow.Values[1] := ARowOffs;
  TmpRow.Values[2] := ARowSize;
  if (ARowSize > 0) then
  begin
    TmpRow.Values[3] := DataAsStr(s[1], ARowSize); // BufferToHex(s[1], ARowSize);
    TmpRow.RawData := s;
  end;
  TmpRow.Values[4] := ATableInfo.RawHead;
  Result := True;
end;

function TDBReaderSybase.ReadRowData(const APageBuf: TByteDynArray; ARowOffs, ARowSize: Integer;
  ATableInfo: TSybaseTableInfo; AList: TDbRowsList): Boolean;
var
  rdr: TRawDataReader;
  TmpRow: TDbRowItem;
  NullBmp: TByteDynArray;
  BoolArr: TByteDynArray;
  RowSize, TmpPos, iColType, iColSize, iColCount: Integer;
  i, iCol, iNullCol, iVarSize, iBitmapLen, iBool: Integer;
  i8: Byte;
  i32: LongInt;
  i64: Int64;
  v: Variant;
  dt: TDateTime;

begin
  // == record format:
  // RowSize     u16
  // NullBitmap  (NullCount div 8) bytes
  // columns data     (except BoolCols)
  // BoolCols    u8    boolean columns, if > 1

  Result := False;
  rdr.Init(APageBuf[0], False);
  rdr.SetPosition(ARowOffs);
  RowSize := rdr.ReadUInt16;

  if (RowSize and $4000) <> 0 then  // reference ?
  begin
    // u32  page id
    // u8   record
  end;

  if (RowSize > ARowSize) and (not IsDebugRows) then
    Exit;

  // Null bitmap
  iBitmapLen := ((ATableInfo.NullCount + 7) div 8); // bytes count
  SetLength(NullBmp, iBitmapLen);
  if iBitmapLen > 0 then
    rdr.ReadToBuffer(NullBmp[0], iBitmapLen);

  if not Assigned(AList) then
    AList := ATableInfo;
  TmpRow := TDbRowItem.Create(AList);
  AList.Add(TmpRow);

  // Columns
  iColCount := Length(AList.FieldsDef);
  SetLength(TmpRow.Values, iColCount);

  TmpPos := rdr.GetPosition;
  // raw data
  if IsDebugRows then
  begin
    SetLength(TmpRow.RawOffs, Length(ATableInfo.FieldsDef));
    rdr.SetPosition(ARowOffs);
    TmpRow.RawData := rdr.ReadBytes(ARowSize);
    rdr.SetPosition(TmpPos);
  end;

  // BoolCols
  if ATableInfo.BoolCount > 1 then
  begin
    SetLength(BoolArr, ATableInfo.BoolCount);
    rdr.SetPosition(ARowSize - ATableInfo.BoolCount);
    rdr.ReadToBuffer(BoolArr[0], ATableInfo.BoolCount);
    rdr.SetPosition(TmpPos);
  end;

  if (RowSize > ARowSize) then
    Exit;

  iCol := 0;
  iNullCol := -1;
  iBool := 0;  // boolean field count
  while (iCol < iColCount) and (rdr.GetPosition <= (ARowOffs + RowSize)) do
  begin
    Assert(iCol < ATableInfo.ColCount, Format('Table %s body read >%d columns!', [ATableInfo.TableName, ATableInfo.ColCount]));
    if IsDebugRows then
      TmpRow.RawOffs[iCol] := rdr.GetPosition() - ARowOffs;

    v := Null;
    iColType := ATableInfo.FieldInfoArr[iCol].FieldType;
    iColSize := ATableInfo.FieldInfoArr[iCol].FieldSize;

    // only nullable columns in bitmap
    if ATableInfo.FieldInfoArr[iCol].IsNullable then
    begin
      Inc(iNullCol);
      if IsNullValue(NullBmp, iNullCol) then
      begin
        TmpRow.Values[iCol] := Null;
        Inc(iCol);
        Continue;
      end;
    end;

    case iColType of
       SY_COL_TYPE_INT,
       SY_COL_TYPE_SMALLINT,
       SY_COL_TYPE_INT2,
       SY_COL_TYPE_UINT64,
       SY_COL_TYPE_UINT32:
       begin
          case iColSize of
            1: v := rdr.ReadInt8;
            2: v := rdr.ReadInt16;
            8: v := rdr.ReadInt64;
          else
            v := rdr.ReadInt32;
          end;
       end;

       SY_COL_TYPE_BOOLEAN:
       begin
         if ATableInfo.BoolCount > 1 then
           v := (BoolArr[iBool] <> 0)
         else
           v := (rdr.ReadUInt8 <> 0);
         Inc(iBool);
         {if iBool = 0 then
           i8 := rdr.ReadUInt8;
         // bit from cumulative bitmap
         v := (((i8 shr iBool) and $1) <> 0);
         Inc(iBool);
         if iBool > 7 then
           iBool := 0;  }
       end;

       SY_COL_TYPE_DATE,
       SY_COL_TYPE_DATETIME:
       begin
         i32 := rdr.ReadInt32; // munutes
         dt := (i32 div 1440) - 109512;  // days - epoch
         if iColType = SY_COL_TYPE_DATETIME then
       begin
         i32 := rdr.ReadInt32;
           // todo: subminutes
         end;
         v := dt;
       end;

       SY_COL_TYPE_NUMERIC:
         v := ReadNumeric(rdr);

       SY_COL_TYPE_CHAR,
       SY_COL_TYPE_CHAR2,
       SY_COL_TYPE_TEXT,
       SY_COL_TYPE_TEXT2:
       begin
         iVarSize := rdr.ReadUInt8;
         v := rdr.ReadBytes(iVarSize);
         if iVarSize = $FF then
         begin
           // overflow ptr (14 bytes)
           rdr.ReadBytes(14);
         end;
       end;
    end;

    TmpRow.Values[iCol] := v;
    Inc(iCol);
  end;

  Result := True;
end;

procedure TDBReaderSybase.ReadTable(AName: string; ACount: Int64; AList: TDbRowsList);
var
  TmpTable: TSybaseTableInfo;
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

  SetLength(PageBuf, FPageSize);
  // read pages
  for i := Low(TmpTable.PageIdArr) to High(TmpTable.PageIdArr) do
  begin
    iPagePos := (TmpTable.PageIdArr[i]) * FPageSize;
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

{ TSybaseTableInfo }

procedure TSybaseTableInfo.AddFieldDef(AName, AType: string);
var
  n, nTypeSize: Integer;
  sTypeName, sTypeSize: string;
begin
  n := Pos('(', AType);
  nTypeSize := 0;
  if n > 0 then
  begin
    sTypeName := Copy(AType, 1, n-1);
    sTypeSize := Copy(AType, n+1, MaxInt);
    n := Pos(')', sTypeSize);
    if n > 0 then
      sTypeSize := Copy(sTypeSize, 1, n-1);
    // NUMERIC(12,2)
    n := Pos(',', sTypeSize);
    if n > 0 then
      sTypeSize := Copy(sTypeSize, 1, n-1);
    nTypeSize := StrToIntDef(sTypeSize, 0);
  end
  else
    sTypeName := AType;

  n := Length(FieldInfoArr);

  SetLength(FieldInfoArr, n+1);
  FieldInfoArr[n].FieldName := AName;
  FieldInfoArr[n].FieldSize := nTypeSize;
  FieldInfoArr[n].TypeName := AType;
  FieldInfoArr[n].FieldType := SybaseColTypeToInt(AType);
  FieldInfoArr[n].IsPrimaryKey := (Pos('PRIMARY KEY', AType) > 0);
  FieldInfoArr[n].IsNullable := (Pos('NOT NULL', AType) = 0);

  SetLength(FieldsDef, n+1);
  FieldsDef[n].Name := AName;
  FieldsDef[n].TypeName := AType;
  FieldsDef[n].FieldType := SybaseColTypeToDbFieldType(sTypeName);
  FieldsDef[n].Size := nTypeSize;

  Inc(ColCount);
  if FieldInfoArr[n].IsNullable then
    Inc(NullCount);

  if FieldInfoArr[n].FieldType = SY_COL_TYPE_BOOLEAN then
    Inc(BoolCount);
end;

procedure TSybaseTableInfo.AddPageID(APageID: Cardinal);
begin
  // store PageID
  Inc(PageIdCount);
  if PageIdCount >= Length(PageIdArr) then
    SetLength(PageIdArr, Length(PageIdArr) + 32);
  PageIdArr[PageIdCount-1] := APageID;
end;

function TSybaseTableInfo.IsEmpty: Boolean;
begin
  Result := (RowCount = 0);
end;

function TSybaseTableInfo.IsGhost: Boolean;
begin
  Result := (TableSQL = '');
end;

function TSybaseTableInfo.IsSystem: Boolean;
begin
  Result := (TableType = SY_TABLE_TYPE_SYS);
end;

procedure TSybaseTableInfo.SetSQL(ASql: string);
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
    s := RemoveQuotes(s, '""');
    s := RemoveQuotes(s, '[]');
    AddFieldDef(s, Trim(sField));
  end;
end;

{ TSybaseTableInfoList }

function TSybaseTableInfoList.GetByName(AName: string): TSybaseTableInfo;
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

{function TSybaseTableInfoList.GetByPageID(ANum: Cardinal): TSybaseTableInfo;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := GetItem(i);
    if Result.RootPageID = ANum then
      Exit;
  end;
  Result := nil;
end; }

function TSybaseTableInfoList.GetByTableID(ATabID: Cardinal): TSybaseTableInfo;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := GetItem(i);
    if Result.TableID = ATabID then
      Exit;
  end;
  Result := nil;
end;

function TSybaseTableInfoList.GetItem(AIndex: Integer): TSybaseTableInfo;
begin
  Result := TSybaseTableInfo(Get(AIndex));
end;

procedure TSybaseTableInfoList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Action = lnDeleted then
    TSybaseTableInfo(Ptr).Free;
end;

function _DoSortByName(Item1, Item2: Pointer): Integer;
var
  TmpItem1, TmpItem2: TSybaseTableInfo;
begin
  TmpItem1 := TSybaseTableInfo(Item1);
  TmpItem2 := TSybaseTableInfo(Item2);
  Result := AnsiCompareStr(TmpItem1.TableName, TmpItem2.TableName);
end;

procedure TSybaseTableInfoList.SortByName;
begin
  Sort(@_DoSortByName);
end;

end.
