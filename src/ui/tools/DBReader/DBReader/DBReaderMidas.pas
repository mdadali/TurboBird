unit DBReaderMidas;

(*
Midas (TClientDataSet, CDS) database file reader

Author: Sergey Bodrov, 2024 Minsk
License: MIT

https://alexeevd.narod.ru/papers/cds_format.htm

*)

{$ifdef FPC}
  {$MODE Delphi}
{$endif}

interface

uses
  SysUtils, Classes, Variants, DB, DBReaderBase;

type
  TDSProp = record
    PropName: string;
    PropSize: Word;
    PropType: Word;
    PropData: array of Byte;
  end;

  TDSFieldDef = record
    FieldName: string;
    FieldSize: Word;
    FieldType: Word;
    FieldAttrs: Word;
    FieldPropCount: Word;
    FieldProps: array of TDSProp;
  end;

  TDBReaderMidas = class(TDBReader)
  private
    FDbVersion: Integer;
    FColCount: Integer;
    FRecCount: Cardinal;     // Record count
    FFieldsDef: array of TDSFieldDef;
    FFirstRecPosition: Int64;

    // ASize - bytes of data size number (1-byte, 2-word, 4-DWord)
    function ReadVarSizeData(ASize: Byte): AnsiString;
    function ReadFixSizeData(ASize: Integer): AnsiString;

    function ReadInt(ASize: Byte): Integer;
    function ReadInt64(ASize: Byte): Int64;
    function ReadUInt(ASize: Byte): Cardinal;
    function ReadBool(ASize: Byte): Boolean;
    function ReadFloat(): Double;
    function ReadDate(): TDateTime;
    function ReadTime(): TDateTime;
    function ReadTimestamp(): TDateTime;

    function ReadProp(var AProp: TDSProp): Boolean;
    function ReadFieldDef(var AFieldDef: TDSFieldDef): Boolean;

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
  end;

implementation

type
  TDSPacketHeader = packed record
    Signature: Cardinal;       // Signature = [$96, $19, $E0, $BD]
    Version: Integer;          // 1
    Int32_18: Integer;         // 18
    FieldCount: Word;          // Field count
    RecordCount: Cardinal;     // Record count
    Int32_3: Integer;          // 3
    MetaSize: Word;            // probably, metadata size
  end;

  {TDSRecord = record
    Status: Byte;
    StatusBits: array of Byte;
  end; }

const
  FIELD_TYPE_UNKNOWN = 0;  // Unknown
  FIELD_TYPE_INT     = 1;  // Signed integer
  FIELD_TYPE_UINT    = 2;  // Unsigned integer
  FIELD_TYPE_BOOL    = 3;  // Boolean
  FIELD_TYPE_FLOAT   = 4;  // IEEE float
  FIELD_TYPE_BCD     = 5;  // BCD
  FIELD_TYPE_DATE    = 6;  // Date (32 bit)
  FIELD_TYPE_TIME    = 7;  // Time (32 bit)
  FIELD_TYPE_TIMESTAMP = 8; // Time-stamp (64 bit)
  FIELD_TYPE_ZSTRING = 9;  // Multi-byte string
  FIELD_TYPE_UNICODE = 10; // Unicode string
  FIELD_TYPE_BYTES   = 11; // Bytes
  FIELD_TYPE_ADT     = 12; // Abstract Data Type
  FIELD_TYPE_ARRAY   = 13; // Array type (not attribute)
  FIELD_TYPE_NESTED  = 14; // Embedded (nested table type)
  FIELD_TYPE_REF     = 15; // Reference

  MASK_FIELD_TYPE    = $3F;  // mask to retrieve Field Type
  MASK_VARYNG_FLD    = $40;  // Varying attribute type
  MASK_ARRAY_FLD     = $80;  // Array attribute type

  FIELD_ATTR_HIDDEN    = $01; // Field is hidden
  FIELD_ATTR_READONLY  = $02; // Field is readonly
  FIELD_ATTR_REQUIRED  = $04; // Field value required
  FIELD_ATTR_LINK      = $08; // Linking field

  REC_STAT_UNMODIFIED  = $00; // Unmodified record
  REC_STAT_ORG         = $01; // Original record (was changed)
  REC_STAT_DELETED     = $02; // Record was deleted
  REC_STAT_NEW         = $04; // Record was inserted
  REC_STAT_MODIFIED    = $08; // Record was changed
  REC_STAT_UNUSED      = $20; // Record not used anymore (hole)
  REC_STAT_UPD         = $40; // Detail modification Ins/Del/Mod. Can be combined with other status.

function FieldTypeToDbFieldType(AFieldType: Word): TFieldType;
begin
  case AFieldType and MASK_FIELD_TYPE of
    FIELD_TYPE_INT:  Result := ftInteger;
    FIELD_TYPE_UINT: Result := ftInteger;
    FIELD_TYPE_BOOL: Result := ftBoolean;
    FIELD_TYPE_FLOAT: Result := ftFloat;
    FIELD_TYPE_BCD: Result := ftBCD;
    FIELD_TYPE_DATE: Result := ftDate;
    FIELD_TYPE_TIME: Result := ftTime;
    FIELD_TYPE_TIMESTAMP: Result := ftDateTime;
    FIELD_TYPE_ZSTRING: Result := ftString;
    FIELD_TYPE_UNICODE: Result := ftWideString;
    FIELD_TYPE_BYTES: Result := ftBytes;
    FIELD_TYPE_ADT: Result := ftADT;
    FIELD_TYPE_ARRAY: Result := ftArray;
    FIELD_TYPE_NESTED: Result := ftDataSet;
    FIELD_TYPE_REF: Result := ftReference;
  else
    Result := ftUnknown;
  end;
end;

function FieldTypeSizeToStr(AFieldType, AFieldSize: Word): string;
begin
  case AFieldType and MASK_FIELD_TYPE of
    FIELD_TYPE_INT:  Result := 'INT';
    FIELD_TYPE_UINT: Result := 'UINT';
    FIELD_TYPE_BOOL: Result := 'BOOL';
    FIELD_TYPE_FLOAT: Result := 'FLOAT';
    FIELD_TYPE_BCD: Result := 'BCD';
    FIELD_TYPE_DATE: Result := 'DATE';
    FIELD_TYPE_TIME: Result := 'TIME';
    FIELD_TYPE_TIMESTAMP: Result := 'TIMESTAMP';
    FIELD_TYPE_ZSTRING: Result := 'CHAR(' + IntToStr(AFieldSize) + ')';
    FIELD_TYPE_UNICODE: Result := 'UNICODE(' + IntToStr(AFieldSize) + ')';
    FIELD_TYPE_BYTES: Result := 'BYTES(' + IntToStr(AFieldSize) + ')';
    FIELD_TYPE_ADT: Result := 'ADT';
    FIELD_TYPE_ARRAY: Result := 'ARRAY(' + IntToStr(AFieldSize) + ')';
    FIELD_TYPE_NESTED: Result := 'NESTED';
    FIELD_TYPE_REF: Result := 'REF';
  else
    Result := 'UNKNOWN';
  end;

  if (AFieldType and MASK_VARYNG_FLD) <> 0 then
    Result := 'VAR_' + Result;

  if (AFieldType and MASK_ARRAY_FLD) <> 0 then
    Result := 'ARR_' + Result;
end;

{ TDBReaderMidas }

procedure TDBReaderMidas.AfterConstruction;
begin
  inherited;
  FIsSingleTable := True;
end;

procedure TDBReaderMidas.BeforeDestruction;
begin
  FreeAndNil(FFile);
  inherited;
end;

function TDBReaderMidas.FillTableInfoText(ATableName: string; ALines: TStrings): Boolean;
var
  i, ii: Integer;
  FieldDef: TDSFieldDef;
begin
  Result := False;
  if not Assigned(ALines) then Exit;

  with ALines do
  begin
    Add(Format('== Fields  Count=%d', [FColCount]));
    for i := 0 to Length(FFieldsDef) - 1 do
    begin
      FieldDef := FFieldsDef[i];
      Add(Format('%.2d  FieldName=%s  Type=%s  Size=%d  Attrs=%d  PropCount=%d',
        [i,
         FieldDef.FieldName,
         FieldTypeSizeToStr(FieldDef.FieldType, FieldDef.FieldSize),
         FieldDef.FieldSize,
         FieldDef.FieldAttrs,
         FieldDef.FieldPropCount]));

      for ii := 0 to FieldDef.FieldPropCount - 1 do
      begin
        Add(Format('    PropName=%s  Type=%s  Size=%d',
          [
           FieldDef.FieldProps[ii].PropName,
           FieldTypeSizeToStr(FieldDef.FieldProps[ii].PropType, FieldDef.FieldProps[ii].PropSize),
           FieldDef.FieldProps[ii].PropSize
           ]));
      end;

    end;
  end;
  Result := True;
end;

function TDBReaderMidas.ReadFixSizeData(ASize: Integer): AnsiString;
begin
  Result := '';
  if ASize > 0 then
  begin
    SetLength(Result, ASize);
    FFile.ReadBuffer(Result[1], ASize);
  end;
end;

function TDBReaderMidas.ReadFloat: Double;
begin
  Result := 0;
  FFile.ReadBuffer(Result, SizeOf(Result));
end;

function TDBReaderMidas.ReadInt(ASize: Byte): Integer;
begin
  Result := 0;
  Assert(ASize <= 4, 'ReadInt(' + IntToStr(ASize) + ')');
  FFile.ReadBuffer(Result, ASize);
end;

function TDBReaderMidas.ReadInt64(ASize: Byte): Int64;
begin
  Result := 0;
  Assert(ASize <= 8, 'ReadInt64(' + IntToStr(ASize) + ')');
  FFile.ReadBuffer(Result, ASize);
end;

function TDBReaderMidas.ReadUInt(ASize: Byte): Cardinal;
begin
  Result := 0;
  Assert(ASize <= 4, 'ReadUInt(' + IntToStr(ASize) + ')');
  FFile.ReadBuffer(Result, ASize);
end;

function TDBReaderMidas.ReadBool(ASize: Byte): Boolean;
begin
  Result := (ReadInt(ASize) <> 0);
end;

procedure TDBReaderMidas.ReadTable(AName: string; ACount: Int64; AList: TDbRowsList);
var
  i, iByteOffs, iBitOffs, iRowCount: Integer;
  btRecStatus, iByteMask: Byte;
  iStatusBytesCount: Integer;
  FieldsStatus: array of Byte;
  sData: AnsiString;
  TmpRow: TDbRowItem;
  FieldSize: Word;
begin
  // columns status bitmap
  iStatusBytesCount := (FColCount div 4);
  if (FColCount mod 4) > 0 then
    Inc(iStatusBytesCount);
  SetLength(FieldsStatus, iStatusBytesCount);

  if Assigned(AList) then
  begin
    AList.TableName := ExtractFileName(FileName);
    SetLength(AList.FieldsDef, FColCount);
    for i := 0 to FColCount - 1 do
    begin
      AList.FieldsDef[i].Name := FFieldsDef[i].FieldName;
      AList.FieldsDef[i].FieldType := FieldTypeToDbFieldType(FFieldsDef[i].FieldType);
      AList.FieldsDef[i].Size := FFieldsDef[i].FieldSize;
      AList.FieldsDef[i].TypeName := FieldTypeSizeToStr(FFieldsDef[i].FieldType, FFieldsDef[i].FieldSize);
    end;
  end;

  FFile.Position := FFirstRecPosition;
  // records
  iRowCount := 0;
  while (FFile.Position < FFile.Size) and (ACount > 0) and (iRowCount < FRecCount) do
  begin
    // record status
    FFile.ReadBuffer(btRecStatus, SizeOf(btRecStatus));
    // fields status bitmap
    FFile.ReadBuffer(FieldsStatus[0], iStatusBytesCount);

    TmpRow := TDbRowItem.Create(AList);
    SetLength(TmpRow.Values, FColCount);

    // fields data
    for i := 0 to FColCount - 1 do
    begin
      // Field status, 2 bits for field
      // BLANK_NULL = 1; { 'real' NULL }
      // BLANK_NOTCHANGED = 2; { Not changed , compared to original value }
      iByteOffs := i div 4;
      iBitOffs := (i mod 4) * 2;
      iByteMask := $3 shl iBitOffs;
      if ((FieldsStatus[iByteOffs] and iByteMask) shr iBitOffs) <> 0 then
      begin
        TmpRow.Values[i] := Null;
        Continue;
      end;

      FieldSize := FFieldsDef[i].FieldSize;
      case (FFieldsDef[i].FieldType and MASK_FIELD_TYPE) of
        FIELD_TYPE_INT:
        begin
          case FieldSize of
            1, 2, 4: TmpRow.Values[i] := ReadInt(FieldSize);
            8: TmpRow.Values[i] := ReadInt64(FieldSize);
          end;
        end;
        FIELD_TYPE_UINT:
          TmpRow.Values[i] := ReadUInt(FieldSize);
        FIELD_TYPE_BOOL:
          TmpRow.Values[i] := ReadBool(FieldSize);
        FIELD_TYPE_FLOAT:
          TmpRow.Values[i] := ReadFloat();
        FIELD_TYPE_BCD:
        begin
          LogInfo(Format('Field %s type BCD not supported', [FFieldsDef[i].FieldName]));
          FFile.Seek(FieldSize, soFromCurrent);
        end;
        FIELD_TYPE_DATE:
          TmpRow.Values[i] := ReadDate();
        FIELD_TYPE_TIME:
          TmpRow.Values[i] := ReadTime();
        FIELD_TYPE_TIMESTAMP:
          TmpRow.Values[i] := ReadTimestamp();
        FIELD_TYPE_ZSTRING,
        FIELD_TYPE_UNICODE,
        FIELD_TYPE_BYTES:
        begin
          if (FFieldsDef[i].FieldType and MASK_VARYNG_FLD) > 0 then
            sData := ReadVarSizeData(FieldSize)
          else
          begin
            sData := ReadFixSizeData(FieldSize);
          end;
          TmpRow.Values[i] := sData;
          if not Assigned(AList) then
            LogInfo(Format('Field %s=%s', [FFieldsDef[i].FieldName, sData]));
        end;
        FIELD_TYPE_ADT:
        begin
          LogInfo(Format('Field %s type ADT not supported', [FFieldsDef[i].FieldName]));
          FFile.Seek(FieldSize, soFromCurrent);
        end;
        FIELD_TYPE_ARRAY:
        begin
          LogInfo(Format('Field %s type ARRAY not supported', [FFieldsDef[i].FieldName]));
          FFile.Seek(FieldSize, soFromCurrent);
        end;
        FIELD_TYPE_NESTED:
        begin
          LogInfo(Format('Field %s type NESTED not supported', [FFieldsDef[i].FieldName]));
          FFile.Seek(FieldSize, soFromCurrent);
        end;
        FIELD_TYPE_REF:
        begin
          LogInfo(Format('Field %s type REFERENCE not supported', [FFieldsDef[i].FieldName]));
          FFile.Seek(FieldSize, soFromCurrent);
        end;
      else
        FFile.Seek(FieldSize, soFromCurrent);
      end;
    end;

    if Assigned(AList) then
      AList.Add(TmpRow)
    else
      TmpRow.Free();

    Inc(iRowCount);
    Dec(ACount);
    if ((iRowCount mod 100) = 0) and Assigned(OnPageReaded) then
      OnPageReaded(Self);
  end;

end;

function TDBReaderMidas.ReadDate: TDateTime;
var
  ts: TTimeStamp;
begin
  //Result := ReadInt(4) - 693594;  // Midas epoch
  ts.Date := ReadInt(4);
  ts.Time := 0;
  if ts.Date <= 0 then
    Result := 0
  else
    Result := TimeStampToDateTime(ts);
end;

function TDBReaderMidas.ReadTime: TDateTime;
var
  ts: TTimeStamp;
begin
  ts.Date := 1;
  ts.Time := ReadInt(4);
  Result := Frac(TimeStampToDateTime(ts));
end;

function TDBReaderMidas.ReadTimestamp: TDateTime;
var
  ts: TTimeStamp;
  c: Comp;
begin
  // Float type value of milliseconds
  c := Trunc(ReadFloat);
  ts := MSecsToTimeStamp(c);
  Result := TimeStampToDateTime(ts);
end;

// Выдает HEX-строку содержимого буфера
function BufferToHex(const Buffer; BufferSize: Integer): string;
var
  i: Integer;
  pb: PByte;
begin
  Result := '';
  pb := @Buffer;
  for i := 0 to BufferSize - 1 do
  begin
    if i > 0 then Result := Result + ' ';
    Result := Result + IntToHex(pb^, 2);
    Inc(pb);
  end;
end;

function TDBReaderMidas.ReadVarSizeData(ASize: Byte): AnsiString;
var
  Size4: Cardinal;
begin
  Result := '';
  Assert(ASize <= 4, 'ReadVarSizeData(' + IntToStr(ASize) + ')');
  Size4 := ReadInt(ASize);
  if Size4 > 0 then
  begin
    SetLength(Result, Size4);
    FFile.ReadBuffer(Result[1], Size4);
  end;
end;

function TDBReaderMidas.ReadProp(var AProp: TDSProp): Boolean;
var
  btLen: Byte;
  iSize: Cardinal;
begin
  // prop name
  AProp.PropName := '';
  FFile.ReadBuffer(btLen, 1);
  if btLen > 0 then
  begin
    SetLength(AProp.PropName, btLen);
    FFile.ReadBuffer(AProp.PropName[1], btLen);
  end;
  Assert(AProp.PropName <> '', 'ReadProp name empty');
  // prop size, type
  FFile.ReadBuffer(AProp.PropSize, SizeOf(Word));
  FFile.ReadBuffer(AProp.PropType, SizeOf(Word));
  // prop data
  if (AProp.PropType and MASK_ARRAY_FLD) <> 0 then
  begin
    // array of PropSize
    iSize := 0;
    FFile.ReadBuffer(iSize, SizeOf(Cardinal));
    FFile.Seek(iSize * AProp.PropSize, soFromCurrent);
  end
  else
  if AProp.PropSize > 0 then
  begin
    iSize := AProp.PropSize;
    if (AProp.PropType and MASK_VARYNG_FLD) <> 0 then
    begin
      // read size
      iSize := ReadInt(AProp.PropSize);
    end;
    SetLength(AProp.PropData, iSize);
    FFile.ReadBuffer(AProp.PropData[0], iSize);
  end;
  Result := True;
end;

function TDBReaderMidas.ReadFieldDef(var AFieldDef: TDSFieldDef): Boolean;
var
  btLen: Byte;
  i: Integer;
begin
  Result := False;
  // field name
  AFieldDef.FieldName := '';
  FFile.ReadBuffer(btLen, 1);
  if btLen > 0 then
  begin
    SetLength(AFieldDef.FieldName, btLen);
    FFile.ReadBuffer(AFieldDef.FieldName[1], btLen);
  end;
  Assert(AFieldDef.FieldName <> '', 'ReadFieldDef name empty');
  // size, type, attrs
  FFile.ReadBuffer(AFieldDef.FieldSize, SizeOf(Word));
  FFile.ReadBuffer(AFieldDef.FieldType, SizeOf(Word));
  FFile.ReadBuffer(AFieldDef.FieldAttrs, SizeOf(Word));
  // props
  FFile.ReadBuffer(AFieldDef.FieldPropCount, SizeOf(Word));
  if AFieldDef.FieldPropCount > 0 then
  begin
    SetLength(AFieldDef.FieldProps, AFieldDef.FieldPropCount);
    for i := 0 to AFieldDef.FieldPropCount - 1 do
    begin
      if not ReadProp(AFieldDef.FieldProps[i]) then
        Exit;
    end;
  end;
  Result := True;
end;

function TDBReaderMidas.OpenFile(AFileName: string; AStream: TStream): Boolean;
var
  //RawData: array [0..2047] of Byte;
  Signature: Cardinal;
  iSigPos: Int64;
  PktHead: TDSPacketHeader;
  TmpProp: TDSProp;
  iPropsCount: Word;
  i: Integer;
begin
  Result := inherited OpenFile(AFileName, AStream);
  if not Result then Exit;
  Result := False;

  // find signature
  Signature := 0;
  iSigPos := -1;
  while (Signature <> $BDE01996) and (iSigPos + 4 < FFile.Size) do
  begin
    Inc(iSigPos);
    FFile.Position := iSigPos;
    FFile.ReadBuffer(Signature, SizeOf(Signature));
  end;
  if (Signature <> $BDE01996) then
  begin
    LogInfo('BDE01996 Signature not found');
    Exit;
  end;

  // read header
  FFile.Position := iSigPos;
  FFile.ReadBuffer(PktHead, SizeOf(PktHead));

  FDbVersion := PktHead.Version;
  FColCount := PktHead.FieldCount;
  FRecCount := PktHead.RecordCount;

  // read field defs
  SetLength(FFieldsDef, FColCount);
  for i := 0 to FColCount - 1 do
  begin
    if not ReadFieldDef(FFieldsDef[i]) then Exit;
  end;

  // read packet props
  FFile.ReadBuffer(iPropsCount, SizeOf(iPropsCount));
  for i := 0 to iPropsCount - 1 do
  begin
    if not ReadProp(TmpProp) then Exit;
  end;
  FFirstRecPosition := FFile.Position;

  //ReadTable('', 100);
  Result := True;
end;

end.
