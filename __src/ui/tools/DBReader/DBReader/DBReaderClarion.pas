unit DBReaderClarion;

(*
Clarion database file reader (.dat)

Author: Sergey Bodrov, 2025 Minsk
License: MIT

* Clarion Techinical Bulletin 117.
* http://www.clarionlife.net/content/view/38/29/
* http://www.clarion-software.com/index.php?group=1&author=bas&id=134486

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
  TClarionFieldRec = record
    DataType: Byte;    // u8  data type
    Name: string;      // cs16  name (zero-terminated)
    Offset: Word;      // u16 offset
    Size: Word;        // u16 items total size
    DecPrec: Byte;     // u8 decimal precision
    DecSize: Byte;     // u8 decimal digits count
    ArrCount: Word;    // (optional) for some field types
    PicCount: Word;    // (optional) for some field types
  end;

type
  // file header
  TClarionFileHeaderRec = record
    Signature: Word;         // 2 file signature
    Attributes: Word;        // 2 attributes
    KeyCount: Byte;          // 1 keys count
    RecCount: Cardinal;      // 4 records count
    DelCount: Cardinal;      // 4 deleted records count
    FieldCount: Word;        // 2 fields count
    PictCount: Word;         // 2 pictures count
    ArrCount: Word;          // 2 arrays count
    RecSize: Word;           // 2 record size (include header)
    DataOffs: Cardinal;      // 4 data area offset
    LogEOF: Cardinal;        // 4 logical end of file
    LogBOF: Cardinal;        // 4 logical begin of file
    FreeRec: Cardinal;       // 4 first usable deleted record
    RecName: string;         // 12 record name without prefix
    MemoName: string;        // 12 memo name without prefix
    FileNamePrefix: string;  // 3 file name prefix
    RecPrefix: string;       // 3 record name prefix
    MemoBlockSize: Word;     // 2 size of memo
    MemoFieldSize: Word;     // 2 memo column size
    LockCount: Cardinal;     // 4 lock count
    ChangeTime: Cardinal;    // 4 time of last change
    ChangeDate: Cardinal;    // 4 date of last change
    CRC: Word;               // 2 CRC
  end;

type
  { TDBReaderClarion }

  TDBReaderClarion = class(TDBReader)
  private
    FFileHeaderSize: Integer;

    function ReadRowData(const APageBuf: TByteDynArray; ARowOffs, ARowSize: Integer;
      AList: TDbRowsList): Boolean;

  public
    FileHeader: TClarionFileHeaderRec;
    Fields: array of TClarionFieldRec;

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
  CLA_FILE_HEADER_SIZE = $200;

const
  CLA_FIELD_TYPE_LONG         = $01; // Long (4 byte) Int32
  CLA_FIELD_TYPE_REAL         = $02; // Double (8 byte) Real64
  CLA_FIELD_TYPE_STRING       = $03; // String
  CLA_FIELD_TYPE_PICTURE      = $04; // Picture
  CLA_FIELD_TYPE_BYTE         = $05; // Byte (1 byte) UInt8
  CLA_FIELD_TYPE_SHORT        = $06; // Short (2 byte) Int16
  CLA_FIELD_TYPE_GROUP        = $07; // Group
  CLA_FIELD_TYPE_DECIMAL      = $08; // Decimal (<16 byte) BCD


function ClarionFieldTypeToDbFieldType(AValue: Integer): TFieldType;
begin
  case AValue of
    CLA_FIELD_TYPE_LONG:     Result := ftInteger; // Long (4 byte) Int32
    CLA_FIELD_TYPE_REAL:     Result := ftFloat;   // Double (8 byte) Real64
    CLA_FIELD_TYPE_STRING:   Result := ftString;  // String
    CLA_FIELD_TYPE_PICTURE:  Result := ftBlob;    // Picture
    CLA_FIELD_TYPE_BYTE:     Result := ftInteger; // Byte (1 byte) UInt8
    CLA_FIELD_TYPE_SHORT:    Result := ftInteger; // Short (2 byte) Int16
    CLA_FIELD_TYPE_DECIMAL:  Result := ftBCD;     // Decimal (<16 byte) BCD
    //CLA_FIELD_TYPE_GROUP:    Result := ftUnknown; // Group
  else
    Result := ftUnknown;
  end;
end;

function ClarionFieldTypeName(AValue, ASize: Integer): string;
begin
  case AValue of
    CLA_FIELD_TYPE_LONG:     Result := 'LONG';
    CLA_FIELD_TYPE_REAL:     Result := 'REAL';
    CLA_FIELD_TYPE_STRING:   Result := 'STRING';
    CLA_FIELD_TYPE_PICTURE:  Result := 'PICTURE';
    CLA_FIELD_TYPE_BYTE:     Result := 'BYTE';
    CLA_FIELD_TYPE_SHORT:    Result := 'SHORT';
    CLA_FIELD_TYPE_DECIMAL:  Result := 'DECIMAL';
    CLA_FIELD_TYPE_GROUP:    Result := 'GROUP';
  else
    Result := 'Unknown';
  end;
end;

{ TDBReaderClarion }

procedure TDBReaderClarion.AfterConstruction;
begin
  inherited;
  FIsSingleTable := True;
end;

procedure TDBReaderClarion.BeforeDestruction;
begin
  inherited;
end;

function TDBReaderClarion.FillTableInfoText(ATableName: string; ALines: TStrings): Boolean;
var
  i: Integer;
  s: string;
begin
  ALines.Add(Format('== Table Name=%s', [ATableName]));
  //ALines.Add(Format('LastUpdate=%.2d-%.2d-%.2d', [yy + FileHeader.LastUpdate[0], DbfHeader.LastUpdate[1], DbfHeader.LastUpdate[2]]));
  //ALines.Add(Format('Description=%s', [FileHeader.Description]));
  ALines.Add(Format('== Fields  Count=%d', [Length(Fields)]));
  for i := Low(Fields) to High(Fields) do
  begin
    s := Format('%.2d Name=%-16s  Type=%d %s  Size=%d  Offs=%d',
      [
        i,
        Fields[i].Name,
        Fields[i].DataType,
        ClarionFieldTypeName(Fields[i].DataType, Fields[i].Size),
        Fields[i].Size,
        Fields[i].Offset
      ]);
    ALines.Add(s);
  end;
  Result := True;
end;

function TDBReaderClarion.OpenFile(AFileName: string; AStream: TStream): Boolean;
var
  PageBuf: TByteDynArray;
  rdr: TRawDataReader;
  i, n: Integer;
begin
  Result := inherited OpenFile(AFileName, AStream);
  if not Result then Exit;

  // init reader
  PageBuf := [];
  SetLength(PageBuf, CLA_FILE_HEADER_SIZE);
  FFile.Position := 0;
  FFile.Read(PageBuf[0], CLA_FILE_HEADER_SIZE);
  rdr.Init(PageBuf[0], False);

  // read header
  with FileHeader do
  begin
    Signature := rdr.ReadUInt16;
    Attributes := rdr.ReadUInt16;
    KeyCount := rdr.ReadUInt8;
    RecCount := rdr.ReadUInt32;
    DelCount := rdr.ReadUInt32;
    FieldCount := rdr.ReadUInt16;
    PictCount := rdr.ReadUInt16;
    ArrCount := rdr.ReadUInt16;
    RecSize := rdr.ReadUInt16;
    DataOffs := rdr.ReadUInt32;
    LogEOF := rdr.ReadUInt32;
    LogBOF := rdr.ReadUInt32;
    FreeRec := rdr.ReadUInt32;
    RecName := rdr.ReadCString(12, True); // 12 record name without prefix
    MemoName := rdr.ReadCString(12, True); // 12 memo name without prefix
    FileNamePrefix := rdr.ReadCString(3, True); // 3 file name prefix
    RecPrefix := rdr.ReadCString(3, True); // 3 record name prefix
    MemoBlockSize := rdr.ReadUInt16;
    MemoFieldSize := rdr.ReadUInt16;
    LockCount := rdr.ReadUInt32;
    ChangeTime := rdr.ReadUInt32;
    ChangeDate := rdr.ReadUInt32;
    CRC := rdr.ReadUInt16;
  end;

  FFileHeaderSize := FileHeader.DataOffs;
  if FFileHeaderSize > CLA_FILE_HEADER_SIZE then
  begin
    // reload header
    n := rdr.GetPosition();
    SetLength(PageBuf, FFileHeaderSize);
    FFile.Position := 0;
    FFile.Read(PageBuf[0], FFileHeaderSize);
    rdr.Init(PageBuf[0], False);
    rdr.SetPosition(n);
  end;

  // read fields records
  SetLength(Fields, FileHeader.FieldCount);
  for i := 0 to FileHeader.FieldCount-1 do
  begin
    Fields[i].DataType := rdr.ReadUInt8;    // u8 field data type
    Fields[i].Name := rdr.ReadCString(16, True);
    Fields[i].Offset := rdr.ReadUInt16;     // u16 offset
    Fields[i].Size := rdr.ReadUInt16;       // u16 field length
    Fields[i].DecPrec := rdr.ReadUInt8;     // u8 decimal precision
    Fields[i].DecSize := rdr.ReadUInt8;     // u8 decimal digits count
    Fields[i].ArrCount := rdr.ReadUInt16;   // (optional) for some field types
    Fields[i].PicCount := rdr.ReadUInt16;   // (optional) for some field types
  end;

  Result := True;
end;

function TDBReaderClarion.ReadRowData(const APageBuf: TByteDynArray; ARowOffs,
  ARowSize: Integer; AList: TDbRowsList): Boolean;
var
  rdr: TRawDataReader;
  nRecNext: Cardinal;
  TmpRow: TDbRowItem;
  i, iFieldSize: Integer;
  btRecFlags, btFieldType: Byte;
  v: Variant;
  s: string;
begin
  Result := False;
  if (not Assigned(AList)) then
    Exit;

  rdr.Init(APageBuf[ARowOffs], False);

  btRecFlags := rdr.ReadUInt8; // u8 record type and status
  nRecNext := rdr.ReadUInt32;  // u32 pointer for next deleted record or memo if active

  TmpRow := TDbRowItem.Create(AList);
  AList.Add(TmpRow);
  if IsDebugRows then
  begin
    rdr.SetPosition(0);
    TmpRow.RawData := rdr.ReadBytes(ARowSize);
  end;

  SetLength(TmpRow.Values, Length(Fields));

  for i := 0 to Length(Fields)-1 do
  begin
    btFieldType := Fields[i].DataType;
    iFieldSize := Fields[i].Size;

    rdr.SetPosition(Fields[i].Offset + 1+4); // skip RecType + RecNext
    v := Null;
    case btFieldType of
      CLA_FIELD_TYPE_BYTE:     v := rdr.ReadUInt8;
      CLA_FIELD_TYPE_SHORT:    v := rdr.ReadInt16;
      CLA_FIELD_TYPE_LONG:     v := rdr.ReadInt32;
      CLA_FIELD_TYPE_REAL:     v := rdr.ReadDouble;
      CLA_FIELD_TYPE_DECIMAL:
      begin
        s := rdr.ReadBytes(iFieldSize);
        if s <> '' then
          v := BufferToHex(s[1], Length(s));
      end;
      {CLA_FIELD_TYPE_STRING:
      begin
        s := rdr.ReadBytes(iFieldSize);
        s := WinCPToUTF8(s);
        v := s;
      end; }
      CLA_FIELD_TYPE_STRING:
      begin
        s := rdr.ReadCString(iFieldSize);
        s := WinCPToUTF8(s);
        v := s;
      end;
      CLA_FIELD_TYPE_PICTURE:
      begin
        s := rdr.ReadBytes(iFieldSize);
        v := s;
      end;
      CLA_FIELD_TYPE_GROUP:
      begin
        s := rdr.ReadBytes(iFieldSize);
        if s <> '' then
          v := BufferToHex(s[1], Length(s));
      end;

    end;

    TmpRow.Values[i] := v;
  end;

end;

procedure TDBReaderClarion.ReadTable(AName: string; ACount: Int64; AList: TDbRowsList);
var
  i: Integer;
  TmpRow: TDbRowItem;
  iRecSize, iRecOffs, iReadSize: Integer;
  iRecPos: Int64;
  PageBuf: TByteDynArray;
begin
  AList.Clear;
  AList.TableName := AName;
  // fields definitions
  SetLength(AList.FieldsDef, Length(Fields));
  for i := Low(Fields) to High(Fields) do
  begin
    AList.FieldsDef[i].FieldType := ClarionFieldTypeToDbFieldType(Fields[i].DataType);
    AList.FieldsDef[i].Name := Fields[i].Name;
    AList.FieldsDef[i].TypeName := ClarionFieldTypeName(Fields[i].DataType, Fields[i].Size);
    AList.FieldsDef[i].Size := Fields[i].Size;
    AList.FieldsDef[i].RawOffset := Fields[i].Offset;
  end;

  // scan records
  iRecSize := FileHeader.RecSize;
  PageBuf := [];
  iReadSize := iRecSize * $100;
  SetLength(PageBuf, iReadSize);
  iRecPos := FFileHeaderSize;
  while iRecPos + iRecSize <= FFile.Size do
  begin
    FFile.Position := iRecPos;
    iReadSize := FFile.Read(PageBuf[0], Length(PageBuf));
    if iReadSize < Length(PageBuf) then
      SetLength(PageBuf, iReadSize);

    iRecOffs := 0;
    while (iRecOffs + iRecSize) <= Length(PageBuf) do
    begin
      ReadRowData(PageBuf, iRecOffs, iRecSize, AList);
      Inc(iRecPos, iRecSize);
      Inc(iRecOffs, iRecSize);
    end;

    if Assigned(OnPageReaded) then
      OnPageReaded(Self);
  end;

end;

end.
