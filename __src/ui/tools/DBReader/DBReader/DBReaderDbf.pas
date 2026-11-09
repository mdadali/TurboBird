unit DBReaderDbf;

(*
DBF database file reader

Author: Sergey Bodrov, 2024 Minsk
License: MIT

https://www.dbase.com/Knowledgebase/INT/db7_file_fmt.htm
https://www.clicketyclick.dk/databases/xbase/format/

Memo file (.fpt) header:
00..03   - position of next freee block
04..05   - not used
06..07   - block size
08..511  - not used

Memo block:
00..03   - block signature (0-blob, 1-memo)
04..07   - data size
08..     - data

*)

{$ifdef FPC}
  {$MODE Delphi}
{$endif}

interface

uses
  {$ifdef FPC}
  LConvEncoding,
  {$else}
  Windows, {for OemToChar}
  {$endif}
  SysUtils, Classes, Variants, DBReaderBase, DB;

type
  TDbfHeaderRec = packed record
    FileType: Byte;                   // DBF File type - See DBF_FILE_TYPE_
    LastUpdate: array [0..2] of Byte; // Last update (YYMMDD)
    RecCount: Cardinal;               // Number of records in file
    FirstRowOffs: Word;               // Position of first data record
    RecLength: Word;                  // Length of one data record, including delete flag
    Reserved12: array [12..27] of Byte;
    TableFlags: Byte;                 // see DBF_FILE_FLAG_
    CodePage: Byte;                   // Code page mark
    Reserved30: Word;                 // Reserved, contains 0x00
    //CodePageName: array [0..31] of AnsiChar; // Code page name (DB4+)
  end;

  // After header:
  //   Field subrecords
  //   Header record terminator (0x0D)
  //   (optional) Visual Foxpro only: A 263-byte range that contains the backlink, which is the relative
  //              path of an associated database (.dbc) file, information. If the first byte is 0x00,
  //              the file is not associated with a database. Therefore, database files always contain 0x00.

  TDbfFieldRec = record
    Name: string;                     // Field name with a maximum of 10 characters. If less than 10, it is padded with null characters (0x00).
    FieldType: AnsiChar;              // Field type - see DBF_FIELD_TYPE_
    FieldOffs: Cardinal;              // Displacement of field in record
    FieldLen: Byte;                   // Length of field (in bytes)
    DecimalLen: Byte;                 // Number of decimal places
    FieldFlags: Byte;                 // Field flags - see DBF_FIELD_FLAG_
  end;

const
  MaxFieldsDb3 = 128;  // for dBase III
  MaxFieldsVFP = 255;  // for Visual Foxpro

  DBF_FILE_TYPE_FB      = $02;  // FoxBASE
  DBF_FILE_TYPE_DB3     = $03;  // FoxBASE+/Dbase III plus, no memo
  DBF_FILE_TYPE_DB7     = $04;  // dBase Level 7
  DBF_FILE_TYPE_VFP     = $30;  // Visual FoxPro
  DBF_FILE_TYPE_VFP_A   = $31;  // Visual FoxPro, autoincrement enabled
  DBF_FILE_TYPE_VFP_V   = $32;  // Visual FoxPro with field type Varchar or Varbinary
  DBF_FILE_TYPE_DB4     = $43;  // dBASE IV SQL table files, no memo
  DBF_FILE_TYPE_DB4_S   = $63;  // dBASE IV SQL system files, no memo
  DBF_FILE_TYPE_DB3_M   = $83;  // FoxBASE+/dBASE III PLUS, with memo
  DBF_FILE_TYPE_DB4_M   = $8B;  // dBASE IV with memo
  DBF_FILE_TYPE_DB4_SM  = $CB;  // dBASE IV SQL table files, with memo
  DBF_FILE_TYPE_FP2     = $F5;  // FoxPro 2.x (or earlier) with memo
  DBF_FILE_TYPE_HP6     = $E5;  // HiPer-Six format with SMT memo file
  DBF_FILE_TYPE_FB2     = $FB;  // FoxBASE

  DBF_FILE_FLAG_CDX     = $01;  // file has a structural .cdx
  DBF_FILE_FLAG_MEMO    = $02;  // file has a Memo field
  DBF_FILE_FLAG_DBC     = $04;  // file is a database (.dbc)

  DBF_FIELD_TYPE_CHAR     = 'C';  // C - Character
  DBF_FIELD_TYPE_CURRENCY = 'Y';  // Y - Currency (Visual Foxpro)
  DBF_FIELD_TYPE_NUMERIC  = 'N';  // N - Numeric
  DBF_FIELD_TYPE_FLOAT    = 'F';  // F - Float
  DBF_FIELD_TYPE_DATE     = 'D';  // D - Date
  DBF_FIELD_TYPE_DATETIME = 'T';  // T - DateTime (Visual Foxpro)
  DBF_FIELD_TYPE_DOUBLE   = 'B';  // B - Double (Visual Foxpro)
  DBF_FIELD_TYPE_INTEGER  = 'I';  // I - Integer (Visual Foxpro)
  DBF_FIELD_TYPE_BOOL     = 'L';  // L - Logical
  DBF_FIELD_TYPE_MEMO     = 'M';  // M - Memo
  DBF_FIELD_TYPE_GENERAL  = 'G';  // G - General
  DBF_FIELD_TYPE_PICTURE  = 'P';  // P - Picture
  DBF_FIELD_TYPE_AUTOINC  = '+';  // + - Autoincrement (dBase Level 7)
  DBF_FIELD_TYPE_DOUBLE7  = 'O';  // O - Double (dBase Level 7)
  DBF_FIELD_TYPE_TIMESTAM = '@';  // @ - Timestamp (dBase Level 7)
  DBF_FIELD_TYPE_VARCHAR  = 'V';  // V - Varchar type (Visual Foxpro, character field with variable size, real size in the last byte of field)


  DBF_FIELD_FLAG_SYS      = $01;  // System Column (not visible to user)
  DBF_FIELD_FLAG_NULL     = $02;  // Column can store null values
  DBF_FIELD_FLAG_BIN      = $04;  // Binary column (for CHAR and MEMO only)
  //DBF_FIELD_FLAG_NBIN     = $06;  // When a field is NULL and binary (Integer, Currency, and Character/Memo fields)
  DBF_FIELD_FLAG_AUTOINC  = $0C;  // Column is autoincrementing

  DBF_REC_STATE_NORM      = ' ';
  DBF_REC_STATE_DELETED   = '*';

  DBF_CODEPAGE_437        = 001;  // Codepage 437 DOS Latin
  DBF_CODEPAGE_850        = 002;  // Codepage 850 DOS Western European
  DBF_CODEPAGE_1252       = 003;  // Codepage 1252 Windows Western European
  DBF_CODEPAGE_866        = 038;  // Codepage 866 DOS Russian
  DBF_CODEPAGE_852        = 100;  // Codepage 852 DOS Central European
  DBF_CODEPAGE_866_2      = 101;  // Codepage 866 DOS Russian
  DBF_CODEPAGE_895        = 104;  // Codepage 895 Japanese-Roman
  DBF_CODEPAGE_1250       = 200;  // Codepage 1250 Windows Central European
  DBF_CODEPAGE_1251       = 201;  // Codepage 1251 Windows Russian

type
  TDBReaderDbf = class(TDBReader)
  private
    //FRowsList: TDbRowsList;
    //FDbVersion: Integer;
    //FIsMetadataReaded: Boolean;        // true after FillTableInfo()
    FMemoStream: TStream;
    FMemoUnitSize: Integer;              // MemoPos = UnitNum * UnitSize

    function ReadString(const ARaw: AnsiString; AStart, ALen: Word): string;
    // read optional MEMO data from raw position data
    function ReadMemoData(const ARaw: AnsiString): AnsiString;

  public
    DbfHeader: TDbfHeaderRec;
    DbfFields: array of TDbfFieldRec;
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

    //property RowsList: TDbRowsList read FRowsList;
  end;

implementation

type
  // dBase field structure
  TDbfFieldSubRec1 = packed record
    Name: array [0..10] of AnsiChar;  // Field name with a maximum of 10 characters. If less than 10, it is padded with null characters (0x00).
    FieldType: AnsiChar;              // Field type - see DBF_FIELD_TYPE_
    FieldOffs: Cardinal;              // Displacement of field in record
    FieldLen: Byte;                   // Length of field (in bytes)
    DecimalLen: Byte;                 // Number of decimal places
    FieldFlags: Byte;                 // Field flags - see DBF_FIELD_FLAG_
    AutoIncNext: Cardinal;            // Value of autoincrement Next value
    AutoIncStep: Byte;                // Value of autoincrement Step value
    Reserved24: array [24..31] of Byte;
  end;
  // dBase 4 field structure
  TDbfFieldSubRec4 = packed record
    Name: array [0..31] of AnsiChar;  // Field name with a maximum of 32 characters. If less than 32, it is padded with null characters (0x00).
    FieldType: AnsiChar;              // Field type - see DBF_FIELD_TYPE_
    FieldLen: Byte;                   // Length of field (in bytes)
    DecimalLen: Byte;                 // Number of decimal places
    Reserved35: Word;
    FieldFlags: Byte;                 // Field flags - see DBF_FIELD_FLAG_
    Reserved37: Word;
    AutoIncNext: Cardinal;            // Value of autoincrement Next value
    Reserved44: array [44..47] of Byte;
  end;

const
  DBF_MEMO_HEADER_SIZE = 512;

function DbfFieldTypeToDbFieldType(AValue: AnsiChar): TFieldType;
begin
  case AValue of
    DBF_FIELD_TYPE_CHAR:     Result := ftString;  // C - Character
    DBF_FIELD_TYPE_CURRENCY: Result := ftCurrency;  // Y - Currency (Visual Foxpro)
    DBF_FIELD_TYPE_NUMERIC:  Result := ftFloat;  // N - Numeric
    DBF_FIELD_TYPE_FLOAT:    Result := ftFloat;  // F - Float
    DBF_FIELD_TYPE_DATE:     Result := ftDate;  // D - Date
    DBF_FIELD_TYPE_DATETIME: Result := ftDateTime;  // T - DateTime (Visual Foxpro)
    DBF_FIELD_TYPE_DOUBLE:   Result := ftFloat;  // B - Double (Visual Foxpro)
    DBF_FIELD_TYPE_INTEGER:  Result := ftInteger;  // I - Integer (Visual Foxpro)
    DBF_FIELD_TYPE_BOOL:     Result := ftBoolean;  // L - Logical
    DBF_FIELD_TYPE_MEMO:     Result := ftMemo;  // M - Memo
    DBF_FIELD_TYPE_GENERAL:  Result := ftString;  // G - General
    DBF_FIELD_TYPE_PICTURE:  Result := ftBlob;  // P - Picture
    DBF_FIELD_TYPE_AUTOINC:  Result := ftInteger;  // + - Autoincrement (dBase Level 7)
    DBF_FIELD_TYPE_DOUBLE7:  Result := ftFloat;  // O - Double (dBase Level 7)
    DBF_FIELD_TYPE_TIMESTAM: Result := ftTimeStamp;  // @ - Timestamp (dBase Level 7)
    DBF_FIELD_TYPE_VARCHAR:  Result := ftString;  // V - Varchar type (Visual Foxpro, character field with variable size, real size in the last byte of field)
  else
    Result := ftUnknown;
  end;
end;

function DbfFieldTypeName(AType: AnsiChar; ASize, AFlags: Byte): string;
begin
  case AType of
    DBF_FIELD_TYPE_CHAR:     Result := 'Character';
    DBF_FIELD_TYPE_CURRENCY: Result := 'Currency';
    DBF_FIELD_TYPE_NUMERIC:  Result := 'Numeric';
    DBF_FIELD_TYPE_FLOAT:    Result := 'Float';
    DBF_FIELD_TYPE_DATE:     Result := 'Date';
    DBF_FIELD_TYPE_DATETIME: Result := 'DateTime';
    DBF_FIELD_TYPE_DOUBLE:   Result := 'Double';
    DBF_FIELD_TYPE_INTEGER:  Result := 'Integer';
    DBF_FIELD_TYPE_BOOL:     Result := 'Logical';
    DBF_FIELD_TYPE_MEMO:     Result := 'Memo';
    DBF_FIELD_TYPE_GENERAL:  Result := 'General';
    DBF_FIELD_TYPE_PICTURE:  Result := 'Picture';
    DBF_FIELD_TYPE_AUTOINC:  Result := 'Autoincrement';
    DBF_FIELD_TYPE_DOUBLE7:  Result := 'Double';
    DBF_FIELD_TYPE_TIMESTAM: Result := 'Timestamp';
    DBF_FIELD_TYPE_VARCHAR:  Result := 'Varchar';
  else
    Result := 'Unknown';
  end;

  Result := Result + '(' + IntToStr(ASize) + ')';

  if (AFlags and DBF_FIELD_FLAG_SYS) > 0 then
    Result := Result + ' SYS';
  if (AFlags and DBF_FIELD_FLAG_NULL) > 0 then
    Result := Result + ' NULL';
  if (AFlags and DBF_FIELD_FLAG_BIN) > 0 then
    Result := Result + ' BIN';
  if (AFlags and DBF_FIELD_FLAG_AUTOINC) > 0 then
    Result := Result + ' AINC';
end;

function DbfCodepageName(AValue: Byte): string;
begin
  case AValue of
    DBF_CODEPAGE_866:      Result := 'Codepage 866 Russian MS-DOS';
    DBF_CODEPAGE_866_2:    Result := 'Codepage 866 Russian MS-DOS';
    DBF_CODEPAGE_1251:     Result := 'Codepage 1251 Russian Windows';
  else
    Result := 'Unknown codepage (' + IntToStr(AValue) + ')';
  end;
end;

function DateStrToDate(AStr: string): TDateTime;
var
  i: Integer;
  yy, mm, dd: Word;
begin
  // filter input
  for i := Length(AStr)-1 downto 1 do
  begin
    if not (AStr[i] in ['0'..'9']) then
      Delete(AStr, i, 1);
  end;

  yy := StrToIntDef(Copy(AStr, 1, 4), 1900);
  if (yy < 1900) or (yy > 3000) then
    yy := 1900;
  mm := StrToIntDef(Copy(AStr, 5, 2), 1);
  if (mm < 1) or (mm > 12) then
    mm := 1;
  dd := StrToIntDef(Copy(AStr, 7, 2), 1);
  if (dd < 1) or (dd > 31) then
    dd := 1;
  Result := EncodeDate(yy, mm, dd);
end;

function SwapEndiness(AVal: Integer): Integer;
begin
  Result := (((AVal shr 0) and $FF) shl 24)
         or (((AVal shr 8) and $FF) shl 16)
         or (((AVal shr 16) and $FF) shl 8)
         or (((AVal shr 24) and $FF) shl 0);
end;

{ TDBReaderDbf }

procedure TDBReaderDbf.AfterConstruction;
begin
  inherited;
  FIsSingleTable := True;
  //FRowsList := TDbRowsList.Create();
end;

procedure TDBReaderDbf.BeforeDestruction;
begin
  //FreeAndNil(FRowsList);
  FreeAndNil(FMemoStream);
  inherited;
end;

function TDBReaderDbf.FillTableInfoText(ATableName: string; ALines: TStrings): Boolean;
var
  i, yy: Integer;
  s: string;
begin
  Result := False;
  // flags
  s := '';
  if (DbfHeader.TableFlags and DBF_FILE_FLAG_CDX) <> 0 then
    s := s + 'CDX,';
  if (DbfHeader.TableFlags and DBF_FILE_FLAG_MEMO) <> 0 then
    s := s + 'MEMO,';
  if (DbfHeader.TableFlags and DBF_FILE_FLAG_DBC) <> 0 then
    s := s + 'DBC,';
  if (DbfHeader.TableFlags and $F8) <> 0 then
    s := s + IntToHex((DbfHeader.TableFlags and $F8), 2) + ',';

  Delete(s, Length(s), 1);

  yy := 1900;
  if DbfHeader.LastUpdate[0] < 80 then
    yy := 2000;

  ALines.Add(Format('== Table Name=%s  RecCount=%d  Type=$%.2x  Flags=%s', [TableName, DbfHeader.RecCount, DbfHeader.FileType, s]));
  ALines.Add(Format('LastUpdate=%.2d-%.2d-%.2d', [yy + DbfHeader.LastUpdate[0], DbfHeader.LastUpdate[1], DbfHeader.LastUpdate[2]]));
  ALines.Add(Format('Codepage=%s', [DbfCodepageName(DbfHeader.CodePage)]));
  ALines.Add(Format('== Fields  Count=%d', [Length(DbfFields)]));
  for i := Low(DbfFields) to High(DbfFields) do
  begin
    s := Format('%.2d Name=%s  Type=%s %s  Size=%d',
      [
        i,
        DbfFields[i].Name,
        DbfFields[i].FieldType,
        DbfFieldTypeName(DbfFields[i].FieldType, DbfFields[i].FieldLen, DbfFields[i].FieldFlags),
        DbfFields[i].FieldLen
      ]);
    ALines.Add(s);
  end;
end;

function TDBReaderDbf.OpenFile(AFileName: string; AStream: TStream): Boolean;
var
  DbfField1: TDbfFieldSubRec1;
  DbfField4: TDbfFieldSubRec4;
  iFieldSize, n, nOffs: Integer;
  CodePageName: array [0..31] of AnsiChar; // Code page name (DB4+)
  s: string;
begin
  FreeAndNil(FMemoStream);
  SetLength(DbfFields, 0);
  FillChar(DbfHeader, SizeOf(DbfHeader), #0);
  FillChar(CodePageName, SizeOf(CodePageName), #0);

  Result := inherited OpenFile(AFileName, AStream);
  if not Result then Exit;
  TableName := ExtractFileName(AFileName);

  // read header
  FFile.Position := 0;
  FFile.Read(DbfHeader, SizeOf(DbfHeader));

  iFieldSize := SizeOf(DbfField1);
  if DbfHeader.FileType in [DBF_FILE_TYPE_DB7] then
  begin
    // read codepage name
    FFile.Read(CodePageName, SizeOf(CodePageName));
    if Trim(CodePageName) = 'db866ru0' then
      DbfHeader.CodePage := DBF_CODEPAGE_866;
    FFile.Position := $44;
    iFieldSize := SizeOf(DbfField4);
  end;

  // read field defs
  n := 0;
  nOffs := 1; // skip rec state byte
  while FFile.Position + iFieldSize < DbfHeader.FirstRowOffs do
  begin
    //FieldPos := FFile.Position;
    if iFieldSize = SizeOf(DbfField1) then
    begin
      FFile.Read(DbfField1, SizeOf(DbfField1));
      if DbfField1.Name[0] = #$0D then
        Break;

      n := Length(DbfFields);
      SetLength(DbfFields, n+1);
      // copy field rec
      DbfFields[n].Name := DbfField1.Name;
      DbfFields[n].FieldType := DbfField1.FieldType;
      DbfFields[n].FieldOffs := DbfField1.FieldOffs;
      DbfFields[n].FieldLen := DbfField1.FieldLen;
      DbfFields[n].DecimalLen := DbfField1.DecimalLen;
      DbfFields[n].FieldFlags := DbfField1.FieldFlags;
    end
    else if iFieldSize = SizeOf(DbfField4) then
    begin
      // dBase 4
      FFile.Read(DbfField4, SizeOf(DbfField4));
      if DbfField4.Name[0] = #$0D then
        Break;

      n := Length(DbfFields);
      SetLength(DbfFields, n+1);
      // copy field rec
      DbfFields[n].Name := DbfField4.Name;
      DbfFields[n].FieldType := DbfField4.FieldType;
      DbfFields[n].FieldOffs := 0;
      DbfFields[n].FieldLen := DbfField4.FieldLen;
      DbfFields[n].DecimalLen := DbfField4.DecimalLen;
      DbfFields[n].FieldFlags := DbfField4.FieldFlags;
    end
    else
      Break;

    // update field offset
    if DbfFields[n].FieldOffs = 0 then
      DbfFields[n].FieldOffs := nOffs;
    Inc(nOffs, DbfFields[n].FieldLen);
  end;

  // read memo data (optional)
  s := Copy(AFileName, 1, Length(AFileName)-4) + '.fpt';
  if FileExists(s) then
  begin
    FMemoStream := TFileStream.Create(s, fmOpenRead + fmShareDenyNone);
    FMemoStream.Position := 4;
    FMemoStream.Read(FMemoUnitSize, SizeOf(FMemoUnitSize));
    FMemoUnitSize := SwapEndiness(FMemoUnitSize);
    if FMemoUnitSize < 10 then
      FMemoUnitSize := DBF_MEMO_HEADER_SIZE;
  end;
end;

function TDBReaderDbf.ReadMemoData(const ARaw: AnsiString): AnsiString;
var
  nNum, nPos: Int64;
  sData: AnsiString;
  nMarker: LongWord; // UInt32
  iLen: LongInt; // Int32
begin
  Result := '';
  if not Assigned(FMemoStream) then Exit;
  nNum := 0;
  if (Length(ARaw) > 0) and (Length(ARaw) <= 4) then
  begin
    System.Move(ARaw[1], nNum, Length(ARaw));
    //nNum := Swap(nNum);
  end;
  //nNum := StrToIntDef(ARaw, 0);
  sData := '';

  if nNum > 0 then
  begin
    nPos := nNum * FMemoUnitSize;
    while nPos + FMemoUnitSize < FMemoStream.Size do
    begin
      FMemoStream.Position := nPos;
      SetLength(sData, FMemoUnitSize);
      FMemoStream.Read(sData[1], FMemoUnitSize);
      nMarker := 0;
      System.Move(sData[1], nMarker, SizeOf(nMarker));
      nMarker := SwapEndiness(nMarker);
      if (nMarker = $0008FFFF) or (nMarker = 1) then
      begin
        // dBase4 block: <marker> <size> <data>
        // Type_$30: <1> <size> <data>
        System.Move(sData[5], iLen, SizeOf(iLen));
        iLen := SwapEndiness(iLen);
        if (iLen > 0) and ((nPos + 8 + iLen) < FMemoStream.Size) then
        begin
          FMemoStream.Position := nPos + 8;
          SetLength(Result, iLen);
          FMemoStream.Read(Result[1], iLen);
          Exit;
        end;
        //if iLen > (FMemoBlockSize - 8) then
        //  iLen := (FMemoBlockSize - 8);
        //Result := Result + Copy(sData, 9, iLen-1);
        //if iLen < (FMemoBlockSize - 8) then
        //  Exit;
      end
      else
      begin
        // dBase3 block: <data> <$1A $1A>
        iLen := Pos(#$1A#$1A, sData);
        if iLen > 0 then
        begin
          Result := Result + Copy(sData, 1, iLen-1);
          Exit;
        end
        else
          Result := Result + sData;
      end;
      //Inc(nPos, FMemoUnitSize);
      nPos := FMemoStream.Position;
    end;
  end;
end;

function TDBReaderDbf.ReadString(const ARaw: AnsiString; AStart, ALen: Word): string;
begin
  Result := '';
  SetLength(Result, ALen);
  if ALen > 0 then
    System.Move(ARaw[AStart+1], Result[1], ALen);

  if Result = '' then
    Exit;

  case DbfHeader.CodePage of
    DBF_CODEPAGE_866,
    DBF_CODEPAGE_866_2:
    begin
      {$ifndef FPC}
      OemToChar(PChar(Result), PChar(Result));
      {$else}
      Result := CP866ToUTF8(Result);
      {$endif}
    end;

    DBF_CODEPAGE_1251:
    begin
      {$ifdef FPC}
      Result := CP1251ToUTF8(Result);
      {$endif}
    end;
  end;
end;

procedure TDBReaderDbf.ReadTable(AName: string; ACount: Int64; AList: TDbRowsList);
var
  i, n: Integer;
  TmpRow: TDbRowItem;
  s: string;
begin
  AList.Clear;
  AList.TableName := AName;

  // set field defs
  SetLength(AList.FieldsDef, Length(DbfFields));
  for i := 0 to Length(DbfFields) - 1 do
  begin
    AList.FieldsDef[i].Name := DbfFields[i].Name;
    AList.FieldsDef[i].TypeName := DbfFieldTypeName(DbfFields[i].FieldType, DbfFields[i].FieldLen, DbfFields[i].FieldFlags);
    AList.FieldsDef[i].FieldType := DbfFieldTypeToDbFieldType(DbfFields[i].FieldType);
    AList.FieldsDef[i].Size := DbfFields[i].FieldLen;
    AList.FieldsDef[i].RawOffset := DbfFields[i].FieldOffs;
  end;

  FFile.Position := DbfHeader.FirstRowOffs;
  for n := 1 to DbfHeader.RecCount do
  begin
    if FFile.Position + DbfHeader.RecLength >= FFile.Size then
      Break;
    TmpRow := TDbRowItem.Create(AList);
    AList.Add(TmpRow);
    SetLength(TmpRow.RawData, DbfHeader.RecLength);
    FFile.Read(TmpRow.RawData[1], DbfHeader.RecLength);

    if TmpRow.RawData[1] = DBF_REC_STATE_DELETED then
    begin
      // deleted
      AList.Remove(TmpRow);
      Continue;
    end;

    SetLength(TmpRow.Values, Length(DbfFields));
    for i := 0 to Length(DbfFields) - 1 do
    begin
      // read field
      s := ReadString(TmpRow.RawData, Word(DbfFields[i].FieldOffs), DbfFields[i].FieldLen);
      if Trim(s) = '' then
      begin
        case DbfFields[i].FieldType of
          DBF_FIELD_TYPE_CURRENCY,
          DBF_FIELD_TYPE_NUMERIC,
          DBF_FIELD_TYPE_FLOAT,
          DBF_FIELD_TYPE_DOUBLE,
          DBF_FIELD_TYPE_DATE,
          DBF_FIELD_TYPE_DATETIME,
          DBF_FIELD_TYPE_INTEGER,
          DBF_FIELD_TYPE_BOOL,
          DBF_FIELD_TYPE_GENERAL:
          begin
            TmpRow.Values[i] := Null;
            Continue;
          end;
        end;
      end;

      case DbfFields[i].FieldType of
        DBF_FIELD_TYPE_CHAR:     TmpRow.Values[i] := TrimRight(s);
        DBF_FIELD_TYPE_CURRENCY: TmpRow.Values[i] := StrToCurrDef(Trim(s), 0);
        DBF_FIELD_TYPE_NUMERIC,
        DBF_FIELD_TYPE_FLOAT,
        DBF_FIELD_TYPE_DOUBLE:   TmpRow.Values[i] := StrToFloatDef(Trim(s), 0);
        DBF_FIELD_TYPE_DATE:     TmpRow.Values[i] := DateStrToDate(Trim(s));
        DBF_FIELD_TYPE_DATETIME: TmpRow.Values[i] := StrToDateTimeDef(Trim(s), 0);
        DBF_FIELD_TYPE_INTEGER:  TmpRow.Values[i] := StrToIntDef(Trim(s), 0);
        DBF_FIELD_TYPE_BOOL:     TmpRow.Values[i] := (Trim(s) = 'T');
        DBF_FIELD_TYPE_MEMO:     TmpRow.Values[i] := ReadMemoData(s);
        DBF_FIELD_TYPE_GENERAL:  TmpRow.Values[i] := s;
      end;
    end;

    if ((n mod 100) = 0) and Assigned(OnPageReaded) then
      OnPageReaded(Self);
  end;
end;

end.
