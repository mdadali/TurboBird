unit DBReaderDbisam;

(*
DBISAM database file reader (.dat, .idx, .blb)

Author: Sergey Bodrov, 2025 Minsk
License: MIT

https://github.com/linville/pydbisam/blob/main/NOTES.md

File structure:
* Header (0x200 bytes)
* Fields definition (0x300 bytes each)
* Rows

Record structure:
* Header
  * flags (2 byte) 1 = deleted
  * IndexA (2 byte)
  * IndexB (2 byte)
  * CRC (16 byte)
  * Marker (2 byte) = 1
* Fields
  * flags (1 byte) 0 = NULL
  * value (Field.Size bytes)


? ZLIB compression algorithm.
? Blowfish block cipher encryption algorithm with 128-bit MD5 hash keys for encryption.

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
  TDbisamHeaderRec = packed record
    Unknown_0: array [0..7] of Byte;       // 8   file size?
    FileSignature: array [0..15] of Byte;  // 16  [06 8A BE 8E 59 23 64 CB   40 3D 71 D2 E3 BC 64 D0]
    Unknown_1B: Byte;                      // 1  = 1
    Unknown_1C: Cardinal;                  // 4  = 0
    NextEndingRec: Cardinal;               // 4
    LastRecordId: Cardinal;                // 4
    LastAutoInc: Cardinal;                 // 4
    TotalRows: Cardinal;                   // 4
    RowSize: Word;                         // 2
    TotalFields: Word;                     // 2
    LastUpdated: Double;                   // 8 IEEE-754, days since 3798/12/28
    DescriptionLen: Byte;                  // 1
    Description: AnsiString;
    UserVersionMajor: Word;                // [C1] 2
    UserVersionMinor: Byte;                // 1
  end;

  TDbisamFieldRec = record
    Num: Word;         // [00] 2 Starts at 1
    NameLen: Byte;     // [02] 1
    Name: string;      // [03] Name (NameLen bytes)
    FieldType: Word;   // [A4] 2
    Length: Word;      // [A6] 2 (string only)
    Sub: Byte;         // 1 ??
    Size: Word;        // 2 field size
    Sub2: Byte;        // 1 ??
    Offset: Word;      // 2 offset within row
  end;

const

  DBISAM_FIELD_TYPE_STRING    =  1;  // String (length from column definition)
  DBISAM_FIELD_TYPE_DATE      =  2;  // Date (4 byte) Days -1 since AD 1, Jan 0
  DBISAM_FIELD_TYPE_BLOB      =  3;  // Blob (8 byte) address for the .blb file
  DBISAM_FIELD_TYPE_BOOL      =  4;  // Boolean (1 byte)
  DBISAM_FIELD_TYPE_SHORT_INT =  5;  // Int16 (2 byte)
  DBISAM_FIELD_TYPE_INT       =  6;  // Int32 (4 byte)
  DBISAM_FIELD_TYPE_DOUBLE    =  7;  // Double (8 byte) IEEE-754
  DBISAM_FIELD_TYPE_TIMESTAMP = 11;  // Timestamp (8 byte) milliseconds since AD 1, Jan 0
  DBISAM_FIELD_TYPE_CURRENCY  = 5383; // Currency (8 byte)
  DBISAM_FIELD_TYPE_MEMO      = 5635; // Memo (8 byte) blob
  DBISAM_FIELD_TYPE_AUTOINC   = 7430; // Autoincrement (4 byte) Int32

type
  TDBReaderDbisam = class(TDBReader)
  private
    FBlobStream: TStream;
    FBlobUnitSize: Integer;              // BlobPos = UnitNum * UnitSize
    // after header and fields definitions
    FFirstRowOffs: Int64;

    // read optional BLOB data from raw position data
    function ReadBlobData(const ARaw: AnsiString): AnsiString;

  public
    FileHeader: TDbisamHeaderRec;
    Fields: array of TDbisamFieldRec;
    TableName: string;

    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;

    function OpenFile(AFileName: string; AStream: TStream = nil): Boolean; override;
    // Read table data from DB to AList
    // AName - table name
    // ACount - how many items read
    procedure ReadTable(AName: string; ACount: Int64 = MaxInt; AList: TDbRowsList = nil); override;
    // get detailed multi-line description of table
    function FillTableInfoText(ATableName: string; ALines: TStrings): Boolean; override;

  end;

implementation

const
  DBISAM_FILE_HEADER_SIZE = $200;
  DBISAM_FIELD_DEF_SIZE   = $300;
  DBISAM_BLOB_UNIT_SIZE   = $200;
  DBISAM_BLOB_UNIT_HEAD_SIZE = 18;
  DBISAM_BLOB_ADDR_NULL   = #0#0#0#0#0#0#0#0;

  DBISAM_REC_STATE_DELETED = $01;

type
  // row header structure
  TDbisamRowHeadRec = packed record
    Flags: Word;  // Row is deleted (=1)
    IndexA: Word;
    IndexB: Word;
    CRC: array [0..15] of Byte;
    Marker: Word; // Trailing \x01 marker
  end;

  // blob unit header (size = 18)
  TDbisamBlobUnitRec = record
    PrevUnit: Integer;   // [00] 4 prev unit index
    NextUnit: Integer;   // [04] 4 next unit index
    DataSize: Integer;   // [08] 2 unit data size
    ID: Cardinal;        // [0A] 4 ID ? (only first unit)
    TotalSize: Integer;  // [0E] 4 total data size (only first unit)
    Data: AnsiString;    // [12]
  end;

function DbisamFieldTypeToDbFieldType(AValue: Integer): TFieldType;
begin
  case AValue of
    DBISAM_FIELD_TYPE_STRING:    Result := ftString;
    DBISAM_FIELD_TYPE_DATE:      Result := ftDate;
    DBISAM_FIELD_TYPE_BLOB:      Result := ftBlob;
    DBISAM_FIELD_TYPE_BOOL:      Result := ftBoolean;
    DBISAM_FIELD_TYPE_SHORT_INT: Result := ftInteger;
    DBISAM_FIELD_TYPE_INT:       Result := ftInteger;
    DBISAM_FIELD_TYPE_DOUBLE:    Result := ftFloat;
    DBISAM_FIELD_TYPE_TIMESTAMP: Result := ftTimeStamp;
    DBISAM_FIELD_TYPE_CURRENCY:  Result := ftCurrency;
    DBISAM_FIELD_TYPE_MEMO:      Result := ftMemo;
    DBISAM_FIELD_TYPE_AUTOINC:   Result := ftInteger;
  else
    Result := ftUnknown;
  end;
end;

function DbisamFieldTypeName(AValue, ASize: Integer): string;
begin
  case AValue of
    DBISAM_FIELD_TYPE_STRING:    Result := 'String(' + IntToStr(ASize) + ')';
    DBISAM_FIELD_TYPE_DATE:      Result := 'Date';
    DBISAM_FIELD_TYPE_BLOB:      Result := 'Blob';
    DBISAM_FIELD_TYPE_BOOL:      Result := 'Boolean';
    DBISAM_FIELD_TYPE_SHORT_INT: Result := 'ShortInt';
    DBISAM_FIELD_TYPE_INT:       Result := 'Integer';
    DBISAM_FIELD_TYPE_DOUBLE:    Result := 'Float';
    DBISAM_FIELD_TYPE_TIMESTAMP: Result := 'TimeStamp';
    DBISAM_FIELD_TYPE_CURRENCY:  Result := 'Currency';
    DBISAM_FIELD_TYPE_MEMO:      Result := 'Memo';
    DBISAM_FIELD_TYPE_AUTOINC:   Result := 'Autoincrement';
  else
    Result := 'Unknown';
  end;
end;

{ TDBReaderDbisam }

procedure TDBReaderDbisam.AfterConstruction;
begin
  inherited;
  FIsSingleTable := True;
  FBlobUnitSize := DBISAM_BLOB_UNIT_SIZE;
end;

procedure TDBReaderDbisam.BeforeDestruction;
begin
  FreeAndNil(FBlobStream);
  inherited;
end;

function TDBReaderDbisam.FillTableInfoText(ATableName: string; ALines: TStrings): Boolean;
var
  i, yy: Integer;
  s: string;
begin
  Result := False;

  ALines.Add(Format('== Table Name=%s  TotalRows=%d', [TableName, FileHeader.TotalRows]));
  //ALines.Add(Format('LastUpdate=%.2d-%.2d-%.2d', [yy + FileHeader.LastUpdate[0], DbfHeader.LastUpdate[1], DbfHeader.LastUpdate[2]]));
  ALines.Add(Format('Description=%s', [FileHeader.Description]));
  ALines.Add(Format('== Fields  Count=%d', [Length(Fields)]));
  for i := Low(Fields) to High(Fields) do
  begin
    s := Format('%.2d Num=%d  Name=%s  Type=%d %s  Size=%d  Offs=%d',
      [
        i,
        Fields[i].Num,
        Fields[i].Name,
        Fields[i].FieldType,
        DbisamFieldTypeName(Fields[i].FieldType, Fields[i].Length),
        Fields[i].Size,
        Fields[i].Offset
      ]);
    ALines.Add(s);
  end;
end;

function TDBReaderDbisam.OpenFile(AFileName: string; AStream: TStream): Boolean;
var
  PageBuf: TByteDynArray;
  rdr: TRawDataReader;
  i, iFieldSize, n, nOffs: Integer;
  s: string;
begin
  FreeAndNil(FBlobStream);
  Fields := [];
  FillChar(FileHeader, SizeOf(FileHeader), #0);

  Result := inherited OpenFile(AFileName, AStream);
  if not Result then Exit;
  TableName := ExtractFileName(AFileName);

  // init reader
  SetLength(PageBuf, DBISAM_FILE_HEADER_SIZE);
  FFile.Position := 0;
  FFile.Read(PageBuf[0], DBISAM_FILE_HEADER_SIZE);
  rdr.Init(PageBuf[0], False);

  // read header
  rdr.ReadToBuffer(FileHeader.Unknown_0, 8);
  rdr.ReadToBuffer(FileHeader.FileSignature, 16);
  FileHeader.Unknown_1B := rdr.ReadUInt8;
  FileHeader.Unknown_1C := rdr.ReadUInt32;
  FileHeader.NextEndingRec := rdr.ReadUInt32;
  FileHeader.LastRecordId := rdr.ReadUInt32;
  FileHeader.LastAutoInc := rdr.ReadUInt32;
  FileHeader.TotalRows := rdr.ReadUInt32;
  FileHeader.RowSize := rdr.ReadUInt16;
  FileHeader.TotalFields := rdr.ReadUInt16;
  FileHeader.LastUpdated := rdr.ReadDouble;
  FileHeader.DescriptionLen := rdr.ReadUInt8;
  FileHeader.Description := rdr.ReadBytes(FileHeader.DescriptionLen);
  rdr.SetPosition($C1);
  FileHeader.UserVersionMajor := rdr.ReadUInt16;
  FileHeader.UserVersionMinor := rdr.ReadUInt8;

  // read fields definition
  SetLength(PageBuf, DBISAM_FIELD_DEF_SIZE * FileHeader.TotalFields);
  FFile.Position := $200;
  FFile.Read(PageBuf[0], DBISAM_FIELD_DEF_SIZE * FileHeader.TotalFields);
  rdr.Init(PageBuf[0], False);

  SetLength(Fields, FileHeader.TotalFields);
  for i := 0 to FileHeader.TotalFields - 1 do
  begin
    rdr.SetPosition(i * DBISAM_FIELD_DEF_SIZE);
    Fields[i].Num := rdr.ReadUInt16;
    n := rdr.ReadUInt8; // NameLen
    Fields[i].Name := Trim(rdr.ReadBytes(n)); // up to 161 bytes
    rdr.SetPosition((i * DBISAM_FIELD_DEF_SIZE) + 164);
    Fields[i].FieldType := rdr.ReadUInt16;
    Fields[i].Length := rdr.ReadUInt16; // string length
    Fields[i].Sub := rdr.ReadUInt8; // ??
    Fields[i].Size := rdr.ReadUInt16; // field size
    Fields[i].Sub2 := rdr.ReadUInt8; // ??
    Fields[i].Offset := rdr.ReadUInt16;
  end;

  FFirstRowOffs := DBISAM_FILE_HEADER_SIZE + (DBISAM_FIELD_DEF_SIZE * FileHeader.TotalFields);

  // read memo data (optional)
  s := Copy(AFileName, 1, Length(AFileName)-4) + '.blb';
  if FileExists(s) then
  begin
    FBlobStream := TFileStream.Create(s, fmOpenRead + fmShareDenyNone);
  end;
end;

function TDBReaderDbisam.ReadBlobData(const ARaw: AnsiString): AnsiString;
var
  UnitIndex, UnitID: Cardinal;
  nPos: Int64;
  rdr: TRawDataReader;
  sData: AnsiString;
  BlobUnit: TDbisamBlobUnitRec;
begin
  Result := '';
  if not Assigned(FBlobStream) then Exit;
  UnitIndex := 0;
  if (Length(ARaw) > 0) and (Length(ARaw) <= 8) then
  begin
    rdr.Init(ARaw[1], False);
    // 4 unit index
    UnitIndex := rdr.ReadUInt32;
    // 4 unit ID
    UnitID := rdr.ReadUInt32;
  end;
  sData := '';

  if UnitIndex <= 0 then
    Exit;

  nPos := UnitIndex * FBlobUnitSize; // unit position
  while nPos + FBlobUnitSize < FBlobStream.Size do
  begin
    FBlobStream.Position := nPos;
    SetLength(sData, FBlobUnitSize);
    FBlobStream.Read(sData[1], FBlobUnitSize);

    // read blob unit
    rdr.Init(sData[1], False);
    BlobUnit.PrevUnit := rdr.ReadInt32;
    BlobUnit.NextUnit := rdr.ReadInt32;
    BlobUnit.DataSize := rdr.ReadInt16;
    BlobUnit.ID := rdr.ReadUInt32;
    BlobUnit.TotalSize := rdr.ReadInt32;
    BlobUnit.Data := '';
    if (BlobUnit.DataSize > 0)  and (BlobUnit.DataSize <= (FBlobUnitSize - DBISAM_BLOB_UNIT_HEAD_SIZE)) then
      BlobUnit.Data := rdr.ReadBytes(BlobUnit.DataSize)
    else
      asm nop end; // for breakpoint

    Result := Result + BlobUnit.Data;

    if BlobUnit.NextUnit = 0 then
      Break;

    nPos := BlobUnit.NextUnit * FBlobUnitSize;
  end;
end;

procedure TDBReaderDbisam.ReadTable(AName: string; ACount: Int64; AList: TDbRowsList);
var
  i, n: Integer;
  bFlags: Byte;
  TmpRow: TDbRowItem;
  rdr: TRawDataReader;
  RowHead: TDbisamRowHeadRec;
  dt: TDateTime;
  s, sAddr: string;
begin
  AList.Clear;
  AList.TableName := AName;

  // set field defs
  SetLength(AList.FieldsDef, Length(Fields));
  for i := 0 to Length(Fields) - 1 do
  begin
    AList.FieldsDef[i].Name := Fields[i].Name;
    AList.FieldsDef[i].TypeName := DbisamFieldTypeName(Fields[i].FieldType, Fields[i].Length);
    AList.FieldsDef[i].FieldType := DbisamFieldTypeToDbFieldType(Fields[i].FieldType);
    AList.FieldsDef[i].Size := Fields[i].Size;
    AList.FieldsDef[i].RawOffset := Fields[i].Offset;
  end;

  FFile.Position := FFirstRowOffs;
  for n := 1 to FileHeader.TotalRows do
  begin
    if FFile.Position + FileHeader.RowSize >= FFile.Size then
      Break;
    TmpRow := TDbRowItem.Create(AList);
    AList.Add(TmpRow);
    SetLength(TmpRow.RawData, FileHeader.RowSize);
    FFile.Read(TmpRow.RawData[1], FileHeader.RowSize);

    rdr.Init(TmpRow.RawData[1], False);
    // read row header
    RowHead.Flags := rdr.ReadUInt16;
    RowHead.IndexA := rdr.ReadUInt16;
    RowHead.IndexB := rdr.ReadUInt16;
    rdr.ReadToBuffer(RowHead.CRC, 16);
    RowHead.Marker := rdr.ReadUInt16;

    if RowHead.Flags = DBISAM_REC_STATE_DELETED then
    begin
      // deleted
      AList.Remove(TmpRow);
      Continue;
    end;

    SetLength(TmpRow.Values, Length(Fields));
    for i := 0 to Length(Fields) - 1 do
    begin
      // read field
      rdr.SetPosition(Fields[i].Offset);
      bFlags := rdr.ReadUInt8;
      if bFlags = 0 then
      begin
        TmpRow.Values[i] := Null;
        //Continue;
      end;
      case Fields[i].FieldType of
        DBISAM_FIELD_TYPE_STRING:
        begin
          s := rdr.ReadBytes(Fields[i].Length);
          {$ifdef FPC}
          s := WinCPToUTF8(s);
          {$endif}
          TmpRow.Values[i] := s;
        end;
        DBISAM_FIELD_TYPE_DATE:
        begin
          dt := rdr.ReadInt32;
          TmpRow.Values[i] := dt;
        end;
        DBISAM_FIELD_TYPE_BLOB:
        begin
          //s := rdr.ReadBytes(8);
          //TmpRow.Values[i] := BufferToHex(s[1], 8);
          sAddr := rdr.ReadBytes(8); // blob addr
          if sAddr = DBISAM_BLOB_ADDR_NULL then
            TmpRow.Values[i] := Null
          else
            TmpRow.Values[i] := ReadBlobData(sAddr);
        end;
        DBISAM_FIELD_TYPE_BOOL:      TmpRow.Values[i] := (rdr.ReadUInt8 = 1);
        DBISAM_FIELD_TYPE_SHORT_INT: TmpRow.Values[i] := rdr.ReadInt16;
        DBISAM_FIELD_TYPE_INT:       TmpRow.Values[i] := rdr.ReadInt32;
        DBISAM_FIELD_TYPE_DOUBLE:    TmpRow.Values[i] := rdr.ReadDouble;
        DBISAM_FIELD_TYPE_TIMESTAMP:
        begin
          dt := rdr.ReadDouble;
          TmpRow.Values[i] := dt;
        end;
        DBISAM_FIELD_TYPE_CURRENCY:  TmpRow.Values[i] := rdr.ReadDouble; //rdr.ReadCurrency;
        DBISAM_FIELD_TYPE_MEMO:
        begin
          sAddr := rdr.ReadBytes(8); // blob addr
          if sAddr = DBISAM_BLOB_ADDR_NULL then
            TmpRow.Values[i] := Null
          else
          begin
            s := ReadBlobData(sAddr);
            {$ifdef FPC}
            s := WinCPToUTF8(s);
            {$endif}
            if s = '' then
              s := BufferToHex(sAddr[1], 8);
            TmpRow.Values[i] := s;
          end;
        end;
        DBISAM_FIELD_TYPE_AUTOINC:   TmpRow.Values[i] := rdr.ReadInt32;
      end;
    end;

    if ((n mod 100) = 0) and Assigned(OnPageReaded) then
      OnPageReaded(Self);
  end;
end;

end.
