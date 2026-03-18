unit DBReaderRaima;

(*
Raima database file reader (.dbd)

Author: Sergey Bodrov, 2025 Minsk
License: MIT

https://wiki.openstreetmap.org/wiki/OSM_Map_On_Magellan/Format/raimadb
https://raima.com/2-space-saving-techniques-for-size-sensitive-databases/

Tested on Velocis (L2.00)

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
  TRdmFileRec = record
    Name: string;   // 50 zero-terminated
    Flags: Byte;    // 1  $63=Closed
    DataType: Byte; // 1  $62=blob, $64=data, $6B=key
    Compress: Word; // 2  Compression Data?
    SlotSize: Word; // 2  Slot size (aligned record length)
    PageSize: Word; // 2  Page size = $200
    Flags2: Word;   // 2  $40=Compressed ?
  end;

  TRdmTableRec = record
    FileIndex: Word;  // 2 Index in files list
    RecSize: Word;    // 2 Record length
    PKOffset: Word;   // 2 PrimaryKey field offset
    FieldIndex: Word; // 2 Index of first field in fields list
    FieldCount: Word; // 2 Fields count
    Unknown_0A: Word; // 2 $0 or $10
    Name: string;     // $0A-terminated
  end;

  TRdmFieldRec = record
    Kind: Byte;        // u8  field kind ($75=unique  $6E=normal, $64=key)
    DataType: Byte;    // u8  field data type
    Size: Word;        // u16 field data length
    Dimension: Word;   // u16 items count (for arrays and strings)
    Sub1: Word;        // u16
    Sub2: Word;        // u16
    ExtFileId: Word;   // u16 related file ID (for keys and blobs)
    Sub4: Word;        // u16 unique (incremental) for each ExtFileId
    Offset: Word;      // u16 offset
    TableId: Word;     // u16 table index
    Flags: Word;       // u16 ($4=unsigned)
    //Sub6: Word;        // u8
    Name: string;      // $0A-terminated
  end;

  TRdmRelationRec = record
    // owner field
    Kind: Word;       // 2 type  ($61='a', $6C='l')
    TableId: Word;    // 2 Owner TableID
    Offset: Word;     // 2 Owner offset
    RelationId: Word; // 2 RelationId
    N5: Integer;      // 2  = 1  (item field count?)
    N6: Integer;      // 2  = 0
    // item field
    RelTabId: Word;    // 2 related table ID
    RelTabOffs: Word;  // 2 related table offset
    RelN1: Word;       // 2 0 or increment (for RelN2=1)
    RelN2: Word;       // 2 0 or 1
    Name: string;
  end;

type
  { TRdmTableInfo }

  TRdmTableInfo = class(TDbRowsList)
  public
    Fields: array of TRdmFieldRec;
    FileName: string;
    FileId: Integer;
    TableId: Integer;
    PageSize: Integer;
    SlotSize: Integer;  // slot for record
    RecSize: Integer;

    procedure AfterConstruction; override;
    // predefined table
    function IsSystem(): Boolean; override;

    procedure AddFieldDef(AName: string; AType, ASize: Integer);
  end;

  { TRdmTableInfoList }

  TRdmTableInfoList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function GetItem(AIndex: Integer): TRdmTableInfo;
    function GetByName(AName: string): TRdmTableInfo;
    function GetByTableID(ATableID: Integer): TRdmTableInfo;
    procedure SortByName();
  end;

  { TDBReaderRaima }

  TDBReaderRaima = class(TDBReader)
  private
    FPagesList: TRdmTableInfo;
    FTablesList: TRdmTableInfoList;
    FBlobStream: TStream;
    FBlobFileId: Integer;  // FileID for FBlobStream
    FBlobUnitSize: Integer;

    FIsMetadataLoaded: Boolean;
    FDBPath: string;

    // APagePos contain offset to next page after reading
    function ReadDataPage(var APageBuf: TByteDynArray; var APagePos: Int64;
      ATableInfo: TRdmTableInfo; AList: TDbRowsList): Boolean;

    // read optional BLOB data from raw position data
    function ReadBlobData(const ARaw: AnsiString; ABlobFileID: Integer): AnsiString;

    procedure FillTablesList();

  public
    FilesArr: array of TRdmFileRec;
    TablesArr: array of TRdmTableRec;
    FieldsArr: array of TRdmFieldRec;
    RelationArr: array of TRdmRelationRec;

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
  RDM_FIELD_TYPE_FLOAT        = $46; // 'F' Float (8 byte) Float64
  RDM_FIELD_TYPE_BLOB         = $62; // 'b' Blob (4 byte) blob addr
  RDM_FIELD_TYPE_CHAR         = $63; // 'c' Char (1 byte) UInt8
  RDM_FIELD_TYPE_INTEGER      = $6C; // 'l' Long (4 byte) Int32
  RDM_FIELD_TYPE_SHORT        = $73; // 's' Short (2 byte) Int16
  // fiction
  RDM_FIELD_TYPE_REL_OWNER    = $26; // '&' Relation owner (16 byte)
  RDM_FIELD_TYPE_REL_ITEM     = $40; // '@' Relation item (18 byte)

  RDM_FIELD_FLAG_UNSIGNED     = $04; // unsigned integer (also for date/time)

  //RDM_REC_HEAD_SIZE           = 4;   // TabId (2 byte) + SomeID (2 byte)
  RDM_BLOB_ADDR_NULL          = #0#0#0#0;  // null blob

type
  // file header
  TRdmFileHeaderRec = record
    Signature: array [0..5] of Byte; // 6 signature, version
    PageSize: Word;          // 2 Page size
    FilesCount: Word;        // 2 Files count
    TablesCount: Word;       // 2 Tables count
    FieldsCount: Word;       // 2 fields count
    RelOwnerCount: Word;     // 2 relations owner count
    RelItemsCount: Word;     // 2 relations items count
    Unknown_12: Word;        // 2
    Unknown_14: Word;        // 2
  end;

  // blob unit header (size = 18)
  TRdmBlobUnitRec = record
    BlobType: Cardinal;  // [00] 4 BlobType? =2
    NextUnit: Cardinal;  // [04] 4 next unit index
    PrevUnit: Cardinal;  // [08] 4 prev unit index
    PartNum: Cardinal;   // [0C] 4 blob part index
    DataSize: Cardinal;  // [10] 4 data size (first part = total size)
    Sub3: Word;          // [14] 2 CRC? depends on size
    Data: AnsiString;    // [16]
  end;

  // record address
  TRdmRecAddrRec = record
    FileId: Word; // 2 FileId
    UnknId: Word; // 2 ??
    RecId: Word;  // 2 RecId
  end;

  // relation owner field
  TRdmRelOwnerRec = record
    Count: Cardinal;       // 4 sub-items count
    First: TRdmRecAddrRec; // first item address
    Last: TRdmRecAddrRec;  // last item address
  end;

  // relation item field
  TRdmRelItemRec = record
    Owner: TRdmRecAddrRec;  // owner addr
    Next: TRdmRecAddrRec;   // next item addr
    Prev: TRdmRecAddrRec;   // prev item addr
  end;

function RdmFieldTypeToDbFieldType(AValue: Integer): TFieldType;
begin
  case AValue of
    RDM_FIELD_TYPE_FLOAT:     Result := ftFloat;
    RDM_FIELD_TYPE_BLOB:      Result := ftBlob;
    RDM_FIELD_TYPE_CHAR:      Result := ftString;
    RDM_FIELD_TYPE_INTEGER:   Result := ftInteger;
    RDM_FIELD_TYPE_SHORT:     Result := ftInteger;
    RDM_FIELD_TYPE_REL_OWNER: Result := ftBlob;
    RDM_FIELD_TYPE_REL_ITEM:  Result := ftBlob;
  else
    Result := ftUnknown;
  end;
end;

function RdmFieldTypeName(AValue, ASize: Integer): string;
begin
  case AValue of
    RDM_FIELD_TYPE_FLOAT:     Result := 'FLOAT';
    RDM_FIELD_TYPE_BLOB:      Result := 'BLOB';
    RDM_FIELD_TYPE_CHAR:      Result := 'CHAR';
    RDM_FIELD_TYPE_INTEGER:   Result := 'INT';
    RDM_FIELD_TYPE_SHORT:     Result := 'SHORT';
    RDM_FIELD_TYPE_REL_OWNER: Result := 'OWNER';
    RDM_FIELD_TYPE_REL_ITEM:  Result := 'RELATION';
  else
    Result := 'Unknown_' + IntToHex(AValue, 4);
  end;
  if ASize <> 0 then
  begin
    Result := Format('%s(%d)', [Result, ASize]);
  end;
end;

{ TRdmTableInfo }

procedure TRdmTableInfo.AfterConstruction;
begin
  inherited AfterConstruction;
  Fields := [];
  TableId := -1;  // system
  //SetLength(FieldsDef, 4);
end;

function TRdmTableInfo.IsSystem(): Boolean;
begin
  Result := (TableId = -1);
end;

procedure TRdmTableInfo.AddFieldDef(AName: string; AType, ASize: Integer);
var
  n: Integer;
begin
  n := Length(FieldsDef);
  SetLength(FieldsDef, n+1);
  FieldsDef[n].Name := AName;
  FieldsDef[n].FieldType := RdmFieldTypeToDbFieldType(AType);
  FieldsDef[n].Size := ASize;
end;

{ TRdmTableInfoList }

procedure TRdmTableInfoList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited Notify(Ptr, Action);
  if Action = lnDeleted then
    TRdmTableInfo(Ptr).Free;
end;

function TRdmTableInfoList.GetItem(AIndex: Integer): TRdmTableInfo;
begin
  Result := nil;
  if AIndex < Count then
    Result := TRdmTableInfo(Get(AIndex));
end;

function TRdmTableInfoList.GetByName(AName: string): TRdmTableInfo;
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

function TRdmTableInfoList.GetByTableID(ATableID: Integer): TRdmTableInfo;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := GetItem(i);
    if Result.TableId = ATableID then
      Exit;
  end;
  Result := nil;
end;

function _DoSortByName(Item1, Item2: Pointer): Integer;
var
  TmpItem1, TmpItem2: TRdmTableInfo;
begin
  TmpItem1 := TRdmTableInfo(Item1);
  TmpItem2 := TRdmTableInfo(Item2);
  Result := AnsiCompareStr(TmpItem1.TableName, TmpItem2.TableName);
end;

procedure TRdmTableInfoList.SortByName();
begin
  Sort(@_DoSortByName);
end;

{ TDBReaderRaima }

procedure TDBReaderRaima.AfterConstruction;
begin
  inherited;
  //FIsSingleTable := True;
  FPagesList := TRdmTableInfo.Create();
  FPagesList.TableName := 'sys_pages';
  FPagesList.AddFieldDef('Offs', RDM_FIELD_TYPE_INTEGER, 4);
  FPagesList.AddFieldDef('Type', RDM_FIELD_TYPE_CHAR, 40);
  FPagesList.AddFieldDef('Size', RDM_FIELD_TYPE_INTEGER, 4);
  FPagesList.AddFieldDef('RecCount', RDM_FIELD_TYPE_INTEGER, 4);

  FTablesList := TRdmTableInfoList.Create;
  //FTablesList.Add(FPagesList);
end;

procedure TDBReaderRaima.BeforeDestruction;
begin
  FreeAndNil(FTablesList);
  FreeAndNil(FPagesList);
  inherited;
end;

function TDBReaderRaima.FillTableInfoText(ATableName: string; ALines: TStrings): Boolean;
var
  i: Integer;
  s: string;
  TmpTable: TRdmTableInfo;
begin
  Result := False;
  TmpTable := FTablesList.GetByName(ATableName);
  if not Assigned(TmpTable) then
    Exit;

  with TmpTable do
  begin
    ALines.Add(Format('== Table Id=%d  Name=%s', [TmpTable.TableId, TableName]));
    ALines.Add(Format('RecSize=%d', [TmpTable.RecSize]));
    ALines.Add(Format('SlotSize=%d', [TmpTable.SlotSize]));
    ALines.Add(Format('FileName=%s', [TmpTable.FileName]));
    ALines.Add(Format('FileId=%d', [TmpTable.FileId]));

    ALines.Add(Format('== Fields  Count=%d', [Length(Fields)]));
    for i := Low(Fields) to High(Fields) do
    begin
      s := Format('%.2d Name=%-20s  Type=%s[%s] %-8s  Dim=%d  Size=%d  Offs=%d  Sub=%d %d  %d %d  %d',
        [
          i,
          Fields[i].Name,
          Chr(Fields[i].DataType),
          Chr(Fields[i].Kind),
          RdmFieldTypeName(Fields[i].DataType, Fields[i].Size),
          Fields[i].Dimension,
          Fields[i].Size,
          Fields[i].Offset,
          Fields[i].Sub1,
          Fields[i].Sub2,
          Fields[i].ExtFileID,
          Fields[i].Sub4,
          Fields[i].Flags
        ]);
      ALines.Add(s);
    end;

  end;
end;

function TDBReaderRaima.GetTablesCount(): Integer;
begin
  Result := FTablesList.Count;
end;

function TDBReaderRaima.GetTableByIndex(AIndex: Integer): TDbRowsList;
begin
  Result := FTablesList.GetItem(AIndex);
end;

function TDBReaderRaima.OpenFile(AFileName: string; AStream: TStream): Boolean;
begin
  Result := inherited OpenFile(AFileName, AStream);
  if not Result then Exit;

  FDBPath := IncludeTrailingPathDelimiter(ExtractFileDir(AFileName));

  // todo check signature
  if UpperCase(ExtractFileExt(AFileName)) <> '.DBD' then
  begin
    LogInfo('Not database definition file!');
    Result := False;
    Exit;
  end;

  if FFile.Size > $FFFFFF then // 16 Mb
  begin
    LogInfo('Database definition file size is too large!');
    Result := False;
    Exit;
  end;

  FillTablesList();
end;

function TDBReaderRaima.ReadDataPage(var APageBuf: TByteDynArray;
  var APagePos: Int64; ATableInfo: TRdmTableInfo; AList: TDbRowsList): Boolean;
var
  rdr: TRawDataReader;
  RawData: TByteDynArray;
  iPageID, iRowID, Unkn2, Unkn3: Cardinal;
  i, iTabId, iRowPos, iTmpPos, iPageRecSize: Integer;
  TabInfo: TRdmTableInfo;
  FieldType: Byte;
  FieldSize: Integer;
  TmpRow: TDbRowItem;
  v: Variant;
  s: string;
begin
  Result := False;
  rdr.Init(APageBuf[0], False);
  RawData := [];
  TabInfo := ATableInfo;

  if APagePos = 0 then
  begin
    // first page
    // 4  file ID ?
    // 4
    // 4
    // 4  records count
    // records (4 byte)
    Exit;
  end;

  // 4  records count ?
  iPageID := rdr.ReadUInt32;

  iPageRecSize := ATableInfo.SlotSize;

  while (rdr.GetPosition() + 6) < Length(APageBuf) do
  begin
    iRowPos := rdr.GetPosition();

    // records
    // 2 TableId
    // 2 FileId
    // 2 ??  = 0
    // 2 RecId ?
    iTabId := rdr.ReadUInt16;
    Unkn2 := rdr.ReadUInt16;
    Unkn3 := rdr.ReadUInt16;
    iRowID := rdr.ReadUInt16;

    // skip records from other tables
    if iTabId <> ATableInfo.TableId then
    begin
      rdr.SetPosition(iRowPos + iPageRecSize);
      Continue;
    end;

    TmpRow := TDbRowItem.Create(AList);
    AList.Add(TmpRow);

    if IsDebugRows then
    begin
      iTmpPos := rdr.GetPosition();
      rdr.SetPosition(iRowPos);
      TmpRow.RawData := rdr.ReadBytes(iPageRecSize);
      rdr.SetPosition(iTmpPos);
      // !!!
      //BufToFile(TmpRow.RawData[1], Length(TmpRow.RawData), 'rec.raw');
    end;

    // read fields
    SetLength(TmpRow.Values, Length(TabInfo.Fields));
    for i := Low(TabInfo.Fields) to High(TabInfo.Fields) do
    begin
      FieldType := TabInfo.Fields[i].DataType;
      FieldSize := TabInfo.Fields[i].Size;
      rdr.SetPosition(iRowPos + TabInfo.Fields[i].Offset);

      //iTmpPos := rdr.GetPosition();
      v := Null;
      case FieldType of
        RDM_FIELD_TYPE_FLOAT:
        begin
          if FieldSize = 8 then
            v := rdr.ReadDouble
          else
            v := rdr.ReadSingle;
        end;
        RDM_FIELD_TYPE_BLOB:
        begin
          s := rdr.ReadBytes(FieldSize);
          if s <> RDM_BLOB_ADDR_NULL then
          begin
            s := ReadBlobData(s, TabInfo.Fields[i].ExtFileId);
            s := WinCPToUTF8(s);
            v := s;
          end;
        end;
        RDM_FIELD_TYPE_CHAR:
        begin
          //s := rdr.ReadBytes(FieldSize);
          //s := Trim(s);
          s := rdr.ReadCString(FieldSize);
          s := WinCPToUTF8(s);
          v := s;
        end;
        RDM_FIELD_TYPE_INTEGER:
        begin
          if (TabInfo.Fields[i].Flags and RDM_FIELD_FLAG_UNSIGNED) = 0 then
            v := rdr.ReadInt32
          else
            v := rdr.ReadUInt32;
        end;
        RDM_FIELD_TYPE_SHORT:
        begin
          if (TabInfo.Fields[i].Flags and RDM_FIELD_FLAG_UNSIGNED) = 0 then
            v := rdr.ReadInt16
          else
            v := rdr.ReadUInt16;
        end;
      else
        s := rdr.ReadBytes(FieldSize);
        v := BufferToHex(s[1], FieldSize);
      end;
      TmpRow.Values[i] := v;

    end;

    rdr.SetPosition(iRowPos + iPageRecSize);
  end;
end;

function TDBReaderRaima.ReadBlobData(const ARaw: AnsiString; ABlobFileID: Integer): AnsiString;
var
  s: string;
  UnitIndex, n: Cardinal;
  rdr: TRawDataReader;
  nPos: Int64;
  sData: AnsiString;
  BlobUnit: TRdmBlobUnitRec;
begin
  Result := '';
  if (FBlobFileId <> ABlobFileID) or (not Assigned(FBlobStream)) then
  begin
    FreeAndNil(FBlobStream);
    if ABlobFileID > High(FilesArr) then
      Exit;
    s := FDBPath + FilesArr[ABlobFileID].Name;
    FBlobStream := TFileStream.Create(s, fmOpenRead + fmShareDenyNone);
    FBlobFileId := ABlobFileID;
    FBlobUnitSize := FilesArr[ABlobFileID].PageSize;
  end;
  if not Assigned(FBlobStream) then
    Exit;

  UnitIndex := 0;
  if (Length(ARaw) > 0) and (Length(ARaw) <= 4) then
  begin
    rdr.Init(ARaw[1], False);
    // 4 unit index
    UnitIndex := rdr.ReadUInt32;
  end;

  if UnitIndex = 0 then
    Exit;


  nPos := UnitIndex * FBlobUnitSize; // unit position
  if nPos >= FBlobStream.Size then
  begin
    Result := BufferToHex(ARaw[1], 4);
    Exit;
  end;

  while nPos + FBlobUnitSize < FBlobStream.Size do
  begin
    FBlobStream.Position := nPos;
    SetLength(sData, FBlobUnitSize);
    FBlobStream.Read(sData[1], FBlobUnitSize);
    rdr.Init(sData[1], False);

    // read blob unit
    BlobUnit.BlobType := rdr.ReadUInt32;      // [00] 4 BlobType? UnitsCount? (=2 for single unit)
    BlobUnit.NextUnit := rdr.ReadUInt32;      // [04] 4 next unit index
    BlobUnit.PrevUnit := rdr.ReadUInt32;      // [08] 4 prev unit index
    BlobUnit.PartNum := rdr.ReadUInt32;       // [0C] 4 blob part index
    BlobUnit.DataSize := rdr.ReadUInt32;      // [10] 4 data size
    BlobUnit.Sub3 := rdr.ReadUInt16;          // [14] 2 CRC? depends on size
    //Result := Result + Format('t=%d n=%d p=%d i=%d s=%d [%4x] ',
    //  [BlobUnit.BlobType, BlobUnit.NextUnit, BlobUnit.PrevUnit, BlobUnit.PartNum, BlobUnit.DataSize, BlobUnit.Sub3]);
    n := FBlobUnitSize - rdr.GetPosition();
    if n > BlobUnit.DataSize then
      n := BlobUnit.DataSize;

    BlobUnit.Data := rdr.ReadBytes(n);
    Result := Result + BlobUnit.Data;

    if BlobUnit.NextUnit = 0 then
      Exit;
    nPos := BlobUnit.NextUnit * FBlobUnitSize; // unit position
  end;

end;

procedure TDBReaderRaima.FillTablesList();
type
  TSomeRec = record
    n1: Integer;
    n2: Integer;
    n3: Integer;
    n4: Integer;
    n5: Integer;
    n6: Integer;
    Raw: string;
    Name: string;
  end;

var
  PageBuf: TByteDynArray;
  FileHeader: TRdmFileHeaderRec;
  rdr: TRawDataReader;
  i, ii, iFile, iField, iPos: Integer;
  s: string;
  bt: Byte;
  TmpTable: TRdmTableInfo;
  TmpRow: TDbRowItem;
begin
  // == read database definition file

  // init reader
  PageBuf := [];
  SetLength(PageBuf, FFile.Size);
  FFile.Position := 0;
  FFile.Read(PageBuf[0], FFile.Size);
  rdr.Init(PageBuf[0], False);

  // read file header
  rdr.ReadToBuffer(FileHeader.Signature, 6); // 6 signature, version
  FileHeader.PageSize := rdr.ReadUInt16;     // 2 Page size
  FileHeader.FilesCount := rdr.ReadUInt16;   // 2 Files count
  FileHeader.TablesCount := rdr.ReadUInt16;  // 2 Tables count
  FileHeader.FieldsCount := rdr.ReadUInt16;  // 2 fields count
  FileHeader.RelOwnerCount := rdr.ReadUInt16;   // 2 relations owner count
  FileHeader.RelItemsCount := rdr.ReadUInt16;   // 2 relations items count
  FileHeader.Unknown_12 := rdr.ReadUInt16;   // 2
  FileHeader.Unknown_14 := rdr.ReadUInt16;   // 2

  // read files records
  SetLength(FilesArr, FileHeader.FilesCount);
  for i := 0 to FileHeader.FilesCount-1 do
  begin
    iPos := rdr.GetPosition();
    FilesArr[i].Name := rdr.ReadCString(50); // 50 zero-terminated
    rdr.SetPosition(iPos + 50);
    FilesArr[i].Flags := rdr.ReadUInt8;     // 1  $63=Closed
    FilesArr[i].DataType := rdr.ReadUInt8;  // 1
    FilesArr[i].Compress := rdr.ReadUInt16; // 2
    FilesArr[i].SlotSize := rdr.ReadUInt16; // 2  Slot size (aligned record length)
    FilesArr[i].PageSize := rdr.ReadUInt16; // 2  Page size = $200
    FilesArr[i].Flags2 := rdr.ReadUInt16;   // 2  $40=Compressed

    //LogInfo(Format('File %2d: %-20s PageSize=%4d  RecLen=%d',
    //  [i, FilesArr[i].Name, FilesArr[i].PageSize, FilesArr[i].RecLen]));
  end;

  // read tables records
  SetLength(TablesArr, FileHeader.TablesCount);
  for i := 0 to FileHeader.TablesCount-1 do
  begin
    TablesArr[i].FileIndex := rdr.ReadUInt16;  // 2 Index in files list
    TablesArr[i].RecSize := rdr.ReadUInt16;    // 2 Record length
    TablesArr[i].PKOffset := rdr.ReadUInt16;   // 2 Primary key field offset
    TablesArr[i].FieldIndex := rdr.ReadUInt16; // 2 Index of first field in fields list
    TablesArr[i].FieldCount := rdr.ReadUInt16; // 2 Fields count
    TablesArr[i].Unknown_0A := rdr.ReadUInt16; // 2 $0
  end;

  // read fields records
  SetLength(FieldsArr, FileHeader.FieldsCount);
  for i := 0 to FileHeader.FieldsCount-1 do
  begin
    FieldsArr[i].Kind := rdr.ReadUInt8;        // u8 field kind
    FieldsArr[i].DataType := rdr.ReadUInt8;    // u8 field data type
    FieldsArr[i].Size := rdr.ReadUInt16;       // u16 field length
    FieldsArr[i].Dimension := rdr.ReadUInt16;  // u16 items count
    FieldsArr[i].Sub1 := rdr.ReadUInt16;       // u16
    FieldsArr[i].Sub2 := rdr.ReadUInt16;       // u16
    FieldsArr[i].ExtFileId := rdr.ReadUInt16;  // u16
    FieldsArr[i].Sub4 := rdr.ReadUInt16;       // u16
    FieldsArr[i].Offset := rdr.ReadUInt16;     // u16 offset
    FieldsArr[i].TableId := rdr.ReadUInt16;    // u16 table index
    FieldsArr[i].Flags := rdr.ReadUInt16;      // u16 flags
    //FieldsArr[i].Sub6 := rdr.ReadUInt8;        // u8
  end;

  // relation owners (12 bytes) records
  SetLength(RelationArr, FileHeader.RelOwnerCount);
  for i := 0 to FileHeader.RelOwnerCount-1 do
  begin
    RelationArr[i].Kind       := rdr.ReadUInt16; // 2
    RelationArr[i].TableId    := rdr.ReadUInt16; // 2
    RelationArr[i].Offset     := rdr.ReadUInt16; // 2
    RelationArr[i].RelationId := rdr.ReadUInt16; // 2
    RelationArr[i].N5 := rdr.ReadUInt16; // 2
    RelationArr[i].N6 := rdr.ReadUInt16; // 2
  end;

  // relation items (8 bytes)
  for i := 0 to FileHeader.RelItemsCount-1 do
  begin
    RelationArr[i].RelTabId   := rdr.ReadUInt16; // 2
    RelationArr[i].RelTabOffs := rdr.ReadUInt16; // 2
    RelationArr[i].RelN1      := rdr.ReadUInt16; // 2
    RelationArr[i].RelN2      := rdr.ReadUInt16; // 2
  end;

  // skip Unknown_12 (4 bytes) records
  for i := 0 to FileHeader.Unknown_12-1 do
  begin
    s := rdr.ReadBytes(4);
    LogInfo(Format('Unknown_12[%2d]: %s',[i, BufferToHex(s[1], Length(s))]));
  end;

  // references (8 bytes) records
  for i := 0 to FileHeader.Unknown_14-1 do
  begin
    // 2 primary key FieldId
    // 2 foreign key FieldId
    // 2 flags
    // 2 = $61 'a'
    s := rdr.ReadBytes(8);
    LogInfo(Format('Reference[%2d]: %s',[i, BufferToHex(s[1], Length(s))]));
  end;

  // read tables names ($0A-terminated)
  for i := 0 to FileHeader.TablesCount-1 do
  begin
    s := '';
    repeat
      bt := rdr.ReadUInt8;
      if bt = $0A then
        TablesArr[i].Name := s
      else
        s := s + Chr(bt);
    until bt = $0A;
  end;

  // read fields names ($0A-terminated)
  for i := 0 to FileHeader.FieldsCount-1 do
  begin
    s := '';
    repeat
      bt := rdr.ReadUInt8;
      if bt = $0A then
        FieldsArr[i].Name := s
      else
        s := s + Chr(bt);
    until bt = $0A;
  end;

  // Relations names
  for i := 0 to FileHeader.RelOwnerCount-1 do
  begin
    s := '';
    repeat
      bt := rdr.ReadUInt8;
      if bt = $0A then
      begin
        RelationArr[i].Name := s;
      end
      else
        s := s + Chr(bt);
    until bt = $0A;
  end;

  // fill tables list
  for i := 0 to FileHeader.TablesCount-1 do
  begin
    TmpTable := TRdmTableInfo.Create();
    FTablesList.Add(TmpTable);

    TmpTable.TableName := TablesArr[i].Name;
    TmpTable.TableId := i;
    TmpTable.RecSize := TablesArr[i].RecSize;
    iFile := TablesArr[i].FileIndex;
    TmpTable.FileId := iFile;
    TmpTable.FileName := FilesArr[iFile].Name;
    TmpTable.PageSize := FilesArr[iFile].PageSize;
    TmpTable.SlotSize := FilesArr[iFile].SlotSize;

    if TablesArr[i].FieldCount = $FFFF then
    begin
      // SYSTEM table
      Continue;
    end;

    // table fields
    SetLength(TmpTable.Fields, TablesArr[i].FieldCount);
    SetLength(TmpTable.FieldsDef, TablesArr[i].FieldCount);
    for ii := 0 to TablesArr[i].FieldCount-1 do
    begin
      iField := TablesArr[i].FieldIndex + ii;
      if iField > High(FieldsArr) then
      begin
        TmpTable.Fields[ii].Name := 'unknown';
        TmpTable.FieldsDef[ii].Name := 'unknown';
        Continue;
      end;
      TmpTable.Fields[ii] := FieldsArr[iField];
    end;
  end;
  FreeAndNil(FFile);

  // add relations to tables
  for i := Low(RelationArr) to High(RelationArr) do
  begin
    // relation owner
    TmpTable := FTablesList.GetByTableID(RelationArr[i].TableId);
    if Assigned(TmpTable) then
    begin
      iField := Length(TmpTable.Fields);
      SetLength(TmpTable.Fields, iField + 1);
      TmpTable.Fields[iField].TableId := RelationArr[i].TableId;
      TmpTable.Fields[iField].Name := RelationArr[i].Name + '_OWNER';
      TmpTable.Fields[iField].Kind := Byte(RelationArr[i].Kind);
      TmpTable.Fields[iField].DataType := RDM_FIELD_TYPE_REL_OWNER;
      TmpTable.Fields[iField].Offset := RelationArr[i].Offset;
      TmpTable.Fields[iField].Size := 16;        // u16 field length
    end;

    // relation item
    TmpTable := FTablesList.GetByTableID(RelationArr[i].RelTabId);
    if Assigned(TmpTable) then
    begin
      iField := Length(TmpTable.Fields);
      SetLength(TmpTable.Fields, iField + 1);
      TmpTable.Fields[iField].TableId := RelationArr[i].RelTabId;
      TmpTable.Fields[iField].Name := RelationArr[i].Name;
      TmpTable.Fields[iField].Kind := Byte(RelationArr[i].Kind);
      TmpTable.Fields[iField].DataType := RDM_FIELD_TYPE_REL_ITEM;
      TmpTable.Fields[iField].Offset := RelationArr[i].RelTabOffs;
      TmpTable.Fields[iField].Size := 18;        // u16 field length
    end;
  end;

  // sync fields definitions
  for i := 0 to FTablesList.Count-1 do
  begin
    TmpTable := FTablesList.GetItem(i);
    SetLength(TmpTable.FieldsDef, Length(TmpTable.Fields));
    for ii := 0 to Length(TmpTable.Fields)-1 do
    begin
      // field definition
      TmpTable.FieldsDef[ii].FieldType := RdmFieldTypeToDbFieldType(TmpTable.Fields[ii].DataType);
      TmpTable.FieldsDef[ii].Name := TmpTable.Fields[ii].Name;
      TmpTable.FieldsDef[ii].TypeName := RdmFieldTypeName(TmpTable.Fields[ii].DataType, TmpTable.Fields[ii].Size);
      TmpTable.FieldsDef[ii].Size := TmpTable.Fields[ii].Size;
      TmpTable.FieldsDef[ii].RawOffset := TmpTable.Fields[ii].Offset;
    end;
  end;


  // == fill system tables
  // -- files
  TmpTable := TRdmTableInfo.Create();
  FTablesList.Add(TmpTable);
  TmpTable.TableName := 'sys_files';
  TmpTable.AddFieldDef('Id', RDM_FIELD_TYPE_SHORT, 2);
  TmpTable.AddFieldDef('Name', RDM_FIELD_TYPE_CHAR, 128);
  TmpTable.AddFieldDef('Flags', RDM_FIELD_TYPE_SHORT, 2);
  TmpTable.AddFieldDef('DataType', RDM_FIELD_TYPE_SHORT, 2);
  TmpTable.AddFieldDef('Compress', RDM_FIELD_TYPE_SHORT, 2);
  TmpTable.AddFieldDef('SlotSize', RDM_FIELD_TYPE_SHORT, 2);
  TmpTable.AddFieldDef('PageSize', RDM_FIELD_TYPE_SHORT, 2);
  TmpTable.AddFieldDef('Flags2', RDM_FIELD_TYPE_SHORT, 2);
  for i := Low(FilesArr) to High(FilesArr) do
  begin
    TmpRow := TDbRowItem.Create(TmpTable);
    TmpTable.Add(TmpRow);
    SetLength(TmpRow.Values, Length(TmpTable.FieldsDef));
    TmpRow.Values[0] := i;
    TmpRow.Values[1] := FilesArr[i].Name;
    TmpRow.Values[2] := FilesArr[i].Flags;
    TmpRow.Values[3] := FilesArr[i].DataType;
    TmpRow.Values[4] := FilesArr[i].Compress;
    TmpRow.Values[5] := FilesArr[i].SlotSize;
    TmpRow.Values[6] := FilesArr[i].PageSize;
    TmpRow.Values[7] := FilesArr[i].Flags2;
  end;

  // -- tables
  TmpTable := TRdmTableInfo.Create();
  FTablesList.Add(TmpTable);
  TmpTable.TableName := 'sys_tables';
  TmpTable.AddFieldDef('Id', RDM_FIELD_TYPE_SHORT, 2);
  TmpTable.AddFieldDef('Name', RDM_FIELD_TYPE_CHAR, 128);
  TmpTable.AddFieldDef('FileIndex', RDM_FIELD_TYPE_SHORT, 2);
  TmpTable.AddFieldDef('PKOffset', RDM_FIELD_TYPE_SHORT, 2);
  TmpTable.AddFieldDef('FieldIndex', RDM_FIELD_TYPE_SHORT, 2);
  TmpTable.AddFieldDef('FieldCount', RDM_FIELD_TYPE_SHORT, 2);
  TmpTable.AddFieldDef('Unknown_0A', RDM_FIELD_TYPE_SHORT, 2);
  for i := Low(TablesArr) to High(TablesArr) do
  begin
    TmpRow := TDbRowItem.Create(TmpTable);
    TmpTable.Add(TmpRow);
    SetLength(TmpRow.Values, Length(TmpTable.FieldsDef));
    TmpRow.Values[0] := i;
    TmpRow.Values[1] := TablesArr[i].Name;
    TmpRow.Values[2] := TablesArr[i].FileIndex;
    TmpRow.Values[3] := TablesArr[i].PKOffset;
    TmpRow.Values[4] := TablesArr[i].FieldIndex;
    TmpRow.Values[5] := TablesArr[i].FieldCount;
    TmpRow.Values[6] := TablesArr[i].Unknown_0A;
  end;

  // -- fields
  TmpTable := TRdmTableInfo.Create();
  FTablesList.Add(TmpTable);
  TmpTable.TableName := 'sys_fields';
  TmpTable.AddFieldDef('Id', RDM_FIELD_TYPE_SHORT, 2);
  TmpTable.AddFieldDef('Name', RDM_FIELD_TYPE_CHAR, 128);
  TmpTable.AddFieldDef('Kind', RDM_FIELD_TYPE_SHORT, 2);
  TmpTable.AddFieldDef('DataType', RDM_FIELD_TYPE_SHORT, 2);
  TmpTable.AddFieldDef('Size', RDM_FIELD_TYPE_SHORT, 2);
  TmpTable.AddFieldDef('Dimension', RDM_FIELD_TYPE_SHORT, 2);
  TmpTable.AddFieldDef('Sub1', RDM_FIELD_TYPE_SHORT, 2);
  TmpTable.AddFieldDef('Sub2', RDM_FIELD_TYPE_SHORT, 2);
  TmpTable.AddFieldDef('ExtFileId', RDM_FIELD_TYPE_SHORT, 2);
  TmpTable.AddFieldDef('Sub4', RDM_FIELD_TYPE_SHORT, 2);
  TmpTable.AddFieldDef('Offset', RDM_FIELD_TYPE_SHORT, 2);
  TmpTable.AddFieldDef('TableId', RDM_FIELD_TYPE_SHORT, 2);
  TmpTable.AddFieldDef('Flags', RDM_FIELD_TYPE_SHORT, 2);
  for i := Low(FieldsArr) to High(FieldsArr) do
  begin
    TmpRow := TDbRowItem.Create(TmpTable);
    TmpTable.Add(TmpRow);
    SetLength(TmpRow.Values, Length(TmpTable.FieldsDef));
    TmpRow.Values[0] := i;
    TmpRow.Values[1] := FieldsArr[i].Name;
    TmpRow.Values[2] := FieldsArr[i].Kind;
    TmpRow.Values[3] := FieldsArr[i].DataType;
    TmpRow.Values[4] := FieldsArr[i].Size;
    TmpRow.Values[5] := FieldsArr[i].Dimension;
    TmpRow.Values[6] := FieldsArr[i].Sub1;
    TmpRow.Values[7] := FieldsArr[i].Sub2;
    TmpRow.Values[8] := FieldsArr[i].ExtFileId;
    TmpRow.Values[9] := FieldsArr[i].Sub4;
    TmpRow.Values[10] := FieldsArr[i].Offset;
    TmpRow.Values[11] := FieldsArr[i].TableId;
    TmpRow.Values[12] := FieldsArr[i].Flags;
  end;

  // -- relations
  TmpTable := TRdmTableInfo.Create();
  FTablesList.Add(TmpTable);
  TmpTable.TableName := 'sys_relations';
  TmpTable.AddFieldDef('Id', RDM_FIELD_TYPE_SHORT, 2);
  TmpTable.AddFieldDef('Name', RDM_FIELD_TYPE_CHAR, 128);
  TmpTable.AddFieldDef('DataType', RDM_FIELD_TYPE_SHORT, 2);
  TmpTable.AddFieldDef('TableId', RDM_FIELD_TYPE_SHORT, 2);
  TmpTable.AddFieldDef('Offset', RDM_FIELD_TYPE_SHORT, 2);
  TmpTable.AddFieldDef('RelationId', RDM_FIELD_TYPE_SHORT, 2);
  TmpTable.AddFieldDef('N5', RDM_FIELD_TYPE_SHORT, 2);
  TmpTable.AddFieldDef('N6', RDM_FIELD_TYPE_SHORT, 2);
  TmpTable.AddFieldDef('RelTabId', RDM_FIELD_TYPE_SHORT, 2);
  TmpTable.AddFieldDef('RelTabOffs', RDM_FIELD_TYPE_SHORT, 2);
  TmpTable.AddFieldDef('RelN1', RDM_FIELD_TYPE_SHORT, 2);
  TmpTable.AddFieldDef('RelN2', RDM_FIELD_TYPE_SHORT, 2);
  for i := Low(RelationArr) to High(RelationArr) do
  begin
    TmpRow := TDbRowItem.Create(TmpTable);
    TmpTable.Add(TmpRow);
    SetLength(TmpRow.Values, Length(TmpTable.FieldsDef));
    TmpRow.Values[0] := i;
    TmpRow.Values[1] := RelationArr[i].Name;
    TmpRow.Values[2] := RelationArr[i].Kind;
    TmpRow.Values[3] := RelationArr[i].TableId;
    TmpRow.Values[4] := RelationArr[i].Offset;
    TmpRow.Values[5] := RelationArr[i].RelationId;
    TmpRow.Values[6] := RelationArr[i].N5;
    TmpRow.Values[7] := RelationArr[i].N6;
    TmpRow.Values[8] := RelationArr[i].RelTabId;
    TmpRow.Values[9] := RelationArr[i].RelTabOffs;
    TmpRow.Values[10] := RelationArr[i].RelN1;
    TmpRow.Values[11] := RelationArr[i].RelN2;
  end;

  FIsMetadataLoaded := True;
end;

procedure TDBReaderRaima.ReadTable(AName: string; ACount: Int64; AList: TDbRowsList);
var
  i: Integer;
  TmpRow: TDbRowItem;
  TmpTable: TRdmTableInfo;
  iPageSize: Integer;
  iPagePos: Int64;
  PageBuf: TByteDynArray;
begin
  AList.Clear;
  AList.TableName := AName;
  TmpTable := FTablesList.GetByName(AName);
  if not Assigned(TmpTable) then
    Exit;
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
    iPageSize := TmpTable.PageSize;
    PageBuf := [];
    SetLength(PageBuf, iPageSize);
    iPagePos := 0;
    if not inherited OpenFile(FDBPath + TmpTable.FileName) then
    begin
      LogInfo('File not found: ' + TmpTable.FileName);
      Exit;
    end;
    while iPagePos + iPageSize <= FFile.Size do
    begin
      FFile.Position := iPagePos;
      FFile.Read(PageBuf[0], iPageSize);

      ReadDataPage(PageBuf, iPagePos, TmpTable, AList);
      Inc(iPagePos, iPageSize);

      if Assigned(OnPageReaded) then
        OnPageReaded(Self);
    end;
  end;

end;

end.
