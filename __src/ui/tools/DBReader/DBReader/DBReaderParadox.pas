unit DBReaderParadox;

(*
Paradox database file reader

Author: Sergey Bodrov, 2024 Minsk
License: MIT

Thanks to Randy Beck (rb@randybeck.com) for file structure description!
http://www.randybeck.com
*)

interface

uses
  SysUtils, Classes, Variants, DB, DBReaderBase;

type
  TPdxFieldDef = record
    FieldName: string;
    FieldSize: Byte;
    FieldType: Byte;
  end;

  TDBReaderParadox = class(TDBReader)
  private
    FDbVersion: Integer;
    FBlockSize: Integer;
    FRecSize: Integer;
    FRecCount: Integer;
    FFirstBlockPosition: Int64;
    FColCount: Integer;
    FTableName: string;

  public
    FieldDefs: array of TPdxFieldDef;

    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;

    function OpenFile(AFileName: string; AStream: TStream = nil): Boolean; override;
    // Read table data from DB to AList
    // AName - table name
    // ACount - how many items read
    procedure ReadTable(AName: string; ACount: Int64 = MaxInt; AList: TDbRowsList = nil); override;

    property TableName: string read FTableName;
    property ColCount: Integer read FColCount;
    property RecCount: Integer read FRecCount;
  end;


implementation

type
  TPdxFileHeader = packed record
    RecSize: Word;
    HeaderSize: Word;     // Offset to first data block
    FileType: Byte;       // 1-indexed DB; 3-non-indexed DB
    MaxTableSize: Byte;   // Block size: 1-$0400, 2-$0800, 3-$0C00, 4-$1000
    RecCount: Cardinal;   // Number of records in this file
    NextBlock: Word;
    FileBlocks: Word;     // Number of data blocks in the file
    FirstBlock: Word;     // Always 1 unless the table is empty.
    LastBlock: Word;
    Unknown_12: Word;
    ModifiedFlsgs1: Byte;
    IndexFieldNumber: Byte;
    PrimaryIndexWorkspace: Cardinal;
    Unknown_1A: Cardinal;
    Unknown_1E: Byte;
    Unknown_1F: Byte;
    Unknown_20: Byte;
    NumFields: Word;      // Fields (Columns) count
    PrimaryKeyFields: Word;
    Encription1: Cardinal;
    SortOrder: Byte;
    ModifiedFlsgs2: Byte;
    Unknown_2B: Word;
    ChangeCount: Word;
    Unknown_2F: Byte;
    TableNamePtr: Cardinal;
    FldInfoPtr: Cardinal;
    WriteProtected: Byte;
    FileVersionID: Byte;  // 3=3.0; 4=3.5; 5..9=4.x; 10,11=5.x; 12=7.x
    MaxBlocks: Word;
    // ...
  end;

  TPdxBlockHead = packed record
    NextBlock: Word;
    BlockNumber: Word;
    AddDataSize: SmallInt;  // Int16
  end;

  TPdxFieldInfo = packed record
    FType: Byte;
    Size: Byte;
  end;

const
  { Paradox codes for field types }
  pxfAlpha        = $01;
  pxfDate         = $02;
  pxfShort        = $03;
  pxfLong         = $04;
  pxfCurrency     = $05;
  pxfNumber       = $06;
  pxfLogical      = $09;
  pxfMemoBLOb     = $0C;
  pxfBLOb         = $0D;
  pxfFmtMemoBLOb  = $0E;
  pxfOLE          = $0F;
  pxfGraphic      = $10;
  pxfTime         = $14;
  pxfTimestamp    = $15;
  pxfAutoInc      = $16;
  pxfBCD          = $17;
  pxfBytes        = $18;

{ TDBReaderParadox }

procedure TDBReaderParadox.AfterConstruction;
begin
  inherited;
  FIsSingleTable := True;
end;

procedure TDBReaderParadox.BeforeDestruction;
begin
  inherited;

end;

function TDBReaderParadox.OpenFile(AFileName: string; AStream: TStream): Boolean;
var
  RawData: TByteArray;
  FileHead: TPdxFileHeader;
  i, iFieldOffs: Integer;
  FieldInfo: TPdxFieldInfo;
begin
  Result := inherited OpenFile(AFileName, AStream);
  if not Result then Exit;
  Result := False;

  // read header
  FFile.ReadBuffer(RawData, $0800);
  Move(RawData, FileHead, SizeOf(FileHead));

  FBlockSize := $0800;
  case FileHead.MaxTableSize of
    1: FBlockSize := $0400;
    2: FBlockSize := $0800;
    3: FBlockSize := $0C00;
    4: FBlockSize := $1000;
  end;
  FFirstBlockPosition := FileHead.HeaderSize;
  FRecSize := FileHead.RecSize;
  FRecCount := FileHead.RecCount;
  FColCount := FileHead.NumFields;
  SetLength(FieldDefs, FColCount);

  // read field defs
  if FileHead.FileVersionID <= 4 then
    iFieldOffs := $58
  else
    iFieldOffs := $78;
  for i := 0 to FColCount - 1 do
  begin
    Move(RawData[iFieldOffs], FieldInfo, SizeOf(FieldInfo));
    Inc(iFieldOffs, SizeOf(FieldInfo));
    FieldDefs[i].FieldType := FieldInfo.FType;
    FieldDefs[i].FieldSize := FieldInfo.Size;
  end;
  // TableNamePtr       4b
  Inc(iFieldOffs, 4);
  // FieldNamePtrArray  4b * FColCount
  Inc(iFieldOffs, 4 * FColCount);
  // TableName          79b (7.x = 261b)
  FTableName := PAnsiChar(@RawData[iFieldOffs]);
  LogInfo('TableName=' + FTableName);
  //Inc(iFieldOffs, 79);
  Inc(iFieldOffs, 261);
  // FieldNames         char[] zero-terminated
  for i := 0 to FColCount - 1 do
  begin
    FieldDefs[i].FieldName := PAnsiChar(@RawData[iFieldOffs]);
    LogInfo(Format('Column[%d] Type=%d Size=%d Name=%s',
      [i, FieldDefs[i].FieldType, FieldDefs[i].FieldSize, FieldDefs[i].FieldName]));
    Inc(iFieldOffs, Length(FieldDefs[i].FieldName) + 1);
  end;
end;

procedure TDBReaderParadox.ReadTable(AName: string; ACount: Int64; AList: TDbRowsList);
var
  RawData: TByteArray;
  iFieldOffs, iValSize: Integer;

  procedure _SetFieldDef(AIndex: Integer; AFieldType: TFieldType; AOffs: Integer);
  begin
    AList.FieldsDef[AIndex].FieldType := AFieldType;
    AList.FieldsDef[AIndex].Name := FieldDefs[AIndex].FieldName;
    AList.FieldsDef[AIndex].Size := FieldDefs[AIndex].FieldSize;
    AList.FieldsDef[AIndex].RawOffset := AOffs;
  end;

  function _ReadInt(var AValue: Integer; ASize: Integer): Boolean;
  var
    i: Integer;
    Buf: array[0..3] of Byte;
  begin
    AValue := 0;
    Result := False;
    for i := 0 to ASize - 1 do
      Buf[ASize-i-1] := RawData[iFieldOffs + i];

    Move(Buf, AValue, ASize);
    if AValue <> 0 then
    begin
      if ASize = 4 then
      begin
        if (AValue and $80000000) = 0 then
          AValue := -(AValue and $7FFFFFFF)
        else
          AValue := AValue and $7FFFFFFF;
      end
      else
      if ASize = 2 then
      begin
        if (AValue and $8000) = 0 then
          AValue := -(AValue and $7FFF)
        else
          AValue := AValue and $7FFF;
      end;
      Result := True;
    end;
  end;

var
  BlockHead: TPdxBlockHead;
  TmpRow: TDbRowItem;
  i, ii, iRecOffs: Integer;
  IsFieldDefSet: Boolean;
  ValInteger: Integer;
  ValCardinal: Cardinal;
  ValStr: string;
  ValDateTime: TDateTime;
  sValBuf: AnsiString;
begin
  IsFieldDefSet := False;
  if Assigned(AList) then
    SetLength(AList.FieldsDef, FColCount);

  FFile.Position := FFirstBlockPosition;
  while (FFile.Position < FFile.Size) and (ACount > 0) do
  begin
    // read block
    FFile.ReadBuffer(RawData, FBlockSize);
    Move(RawData, BlockHead, SizeOf(BlockHead));
    if BlockHead.AddDataSize >= 0 then
    begin
      // records
      iRecOffs := SizeOf(BlockHead);
      for i := 0 to (BlockHead.AddDataSize div FRecSize) do
      begin
        // fields
        iFieldOffs := iRecOffs;
        TmpRow := TDbRowItem.Create(AList);
        SetLength(TmpRow.Values, FColCount);

        for ii := 0 to FColCount - 1 do
        begin
          iValSize := FieldDefs[ii].FieldSize;
          if FieldDefs[ii].FieldType = pxfBCD then
            iValSize := 17;

          if Assigned(AList) and (not IsFieldDefSet) then
          begin
            case FieldDefs[ii].FieldType of
              pxfAlpha:    _SetFieldDef(ii, ftString, iFieldOffs);
              pxfDate:     _SetFieldDef(ii, ftDate, iFieldOffs);
              pxfShort:    _SetFieldDef(ii, ftWord, iFieldOffs);
              pxfLong:     _SetFieldDef(ii, ftInteger, iFieldOffs);
              pxfCurrency: _SetFieldDef(ii, ftCurrency, iFieldOffs);
              pxfNumber:   _SetFieldDef(ii, ftFloat, iFieldOffs);
              pxfLogical:  _SetFieldDef(ii, ftBoolean, iFieldOffs);
              pxfMemoBLOb,
              pxfFmtMemoBLOb: _SetFieldDef(ii, ftMemo, iFieldOffs);
              pxfBLOb:     _SetFieldDef(ii, ftBlob, iFieldOffs);
              //pxfOLE: //
              //pxfGraphic:
              pxfTime:     _SetFieldDef(ii, ftTime, iFieldOffs);
              pxfTimestamp: _SetFieldDef(ii, ftDateTime, iFieldOffs);
              pxfAutoInc:  _SetFieldDef(ii, ftAutoInc, iFieldOffs);
              pxfBCD:      _SetFieldDef(ii, ftBCD, iFieldOffs);
              pxfBytes:    _SetFieldDef(ii, ftBytes, iFieldOffs);
            end;
          end;

          // read field
          case FieldDefs[ii].FieldType of
            pxfDate:
            begin
              ValInteger := 0;
              if _ReadInt(ValInteger, iValSize) then
              begin
                ValDateTime := ValInteger - 693594;  // days between Year 0000 and Year 1900
                TmpRow.Values[ii] := ValDateTime;
              end
              else
                TmpRow.Values[ii] := Null;
            end;

            pxfShort, pxfLong, pxfAutoInc: // integer
            begin
              ValInteger := 0;
              if _ReadInt(ValInteger, iValSize) then
                TmpRow.Values[ii] := ValInteger
              else
                TmpRow.Values[ii] := Null;
            end;

            pxfAlpha:  // char[]
            begin
              ValStr := '';
              if iValSize > 0 then
              begin
                SetLength(sValBuf, iValSize);
                Move(RawData[iFieldOffs], sValBuf[1], iValSize);
                ValStr := PAnsiChar(sValBuf);
              end;
              if ValStr <> '' then
                TmpRow.Values[ii] := ValStr
              else
                TmpRow.Values[ii] := Null;
            end;

            pxfCurrency, pxfNumber:  // float
            begin
            end;
          else
            begin
              LogInfo(Format('Field[%d] type=%d size=%d', [ii, FieldDefs[ii].FieldType, iValSize]));
            end;
          end;
          // next field
          Inc(iFieldOffs, iValSize);
        end;
        IsFieldDefSet := True;

        if Assigned(AList) then
          AList.Add(TmpRow)
        else
          TmpRow.Free();

        // next record
        Dec(ACount);
        Inc(iRecOffs, FRecSize);
      end;
    end;
    if Assigned(OnPageReaded) then OnPageReaded(Self);

  end;

end;

end.
