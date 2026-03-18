unit DBReaderBerkley;

(*
Berkley database file reader (not tested!)

Author: Sergey Bodrov, 2024
License: MIT

https://transactional.blog/building-berkeleydb/page-format
*)

interface

uses
  SysUtils, Classes, Variants, DBReaderBase;

type
  TDBReaderBerkley = class(TDBReader)
  private
    FPageSize: Integer;
    FDbVersion: Integer;
    FIsMetadataReaded: Boolean;        // true after FillTableInfo()

  public
    IsLogPages: Boolean;

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
  TBdbMetaPageHeader = packed record
    LSN: Int64;              // Log sequence number (not used)
    PgNo: Cardinal;          // Current page number (not used)
    Magic: Cardinal;         // $53162
    Version: Integer;
    PageSize: Integer;
    ec: Byte;
    PageType: Byte;
    mf: Byte;
    un: Byte;  // unused
    Free: Cardinal;          // Free list page number
    LastPgNo: Cardinal;      // Last page number
    NParts: Cardinal;
    KeyCount: Cardinal;
    RecordCount: Cardinal;
    Flags: Cardinal;
    UID: array [0..19] of Byte;
    un2: Cardinal; // unused
    MinKey: Cardinal;
    ReLen: Cardinal;
    RePad: Cardinal;
    Root: Cardinal;          // The page number of the root of the btree
  end;

  TBdbPageHeader = packed record
    LSN: Int64;              // Log sequence number
    PgNo: Cardinal;          // Current page number
    PrevPgNo: Cardinal;      // Previous page number
    NextPgNo: Cardinal;      // Next page number
    Entries: Word;           // Number of items on the page
    HFOffset: Word;          // High free byte page offset
    TreeLevel: Byte;         // Btree tree level (1=leaf, >1=internal)
    PageType: Byte;          // Page type  PAGE_TYPE_
  end;

  TDataEntryRec = packed record
    Len: Word;          // Key/data item length
    EType: Byte;        // Page type and delete flag.
    //Data: AnsiString;
  end;

  TOverEntryRec = packed record
    Len: Word;          // 12
    EType: Byte;        // ENTRY_TYPE_OVER
    Pad: Byte;          // Padding, unused.
    PgNo: Cardinal;     // Page number of referenced page.
    TotalLen: Cardinal; // Total length of item.
  end;

  TInternEntryRec = packed record
    Len: Word;          // Key/data item length
    EType: Byte;        // Page type and delete flag.
    Pad: Byte;          // Padding, unused.
    PgNo: Cardinal;     // Page number of referenced page.
    NRecs: Cardinal;    // Subtree record count
    //Data: AnsiString;   // Variable length key/data item
  end;

const
  PAGE_TYPE_NODE    = 3;  // internal node
  PAGE_TYPE_LEAF    = 5;  // leaf node
  PAGE_TYPE_META    = 9;  // DB meta page

  ENTRY_TYPE_DATA   = 1;  // KeyDataEntry(TreeLevel=1)/InternalEntry
  ENTRY_TYPE_OVER   = 3;  // OverflowEntry

function BdbPageTypeToStr(AValue: Byte): string;
begin
  case AValue of
    PAGE_TYPE_NODE: Result := 'Internal node';
    PAGE_TYPE_LEAF: Result := 'Leaf node';
    PAGE_TYPE_META: Result := 'DB meta page';
  else
    Result := 'Undef_' + IntToStr(AValue);
  end;
end;

{ TDBReaderBerkley }

procedure TDBReaderBerkley.AfterConstruction;
begin
  inherited;

end;

procedure TDBReaderBerkley.BeforeDestruction;
begin
  inherited;
end;

function TDBReaderBerkley.FillTableInfoText(ATableName: string; ALines: TStrings): Boolean;
begin
  Result := False;
end;

function TDBReaderBerkley.OpenFile(AFileName: string; AStream: TStream): Boolean;
var
  MetaHead: TBdbMetaPageHeader;
  PageHead: TBdbPageHeader;
  RawPage: TByteArray;
  i, nPage, iOffs, iLen: Integer;
  nPos: Int64;
  OffsArr: array of Word;
  sData: AnsiString;
  eData: TDataEntryRec;
  eOver: TOverEntryRec;
  eIntern: TInternEntryRec;
begin
  Result := inherited OpenFile(AFileName, AStream);
  if not Result then Exit;

  // read header page
  FFile.Read(MetaHead, SizeOf(MetaHead));

  //if MetaHead.Magic <> $53162 then
  //begin
  //  LogInfo(Format('Not Berkley database: %s', [AFileName]));
  //  Exit;
  //end;


  FPageSize := MetaHead.PageSize;
  FDbVersion := MetaHead.Version;
  LogInfo(Format('=== %d %s ===', [0, BdbPageTypeToStr(MetaHead.PageType)]));
  LogInfo(Format('PageSize=%d', [FPageSize]));
  LogInfo(Format('Version=%d', [FDbVersion]));

  // pages list
  OffsArr := [];
  sData := '';
  nPage := 1;
  nPos := nPage * FPageSize;
  while nPos < (FFile.Size - 1024) do
  begin
    FFile.Position := nPos;
    FFile.Read(RawPage, FPageSize);
    Move(RawPage, PageHead, SizeOf(PageHead));
    if IsLogPages then
      LogInfo(Format('=== %d %s ===', [nPage, BdbPageTypeToStr(PageHead.PageType)]));

    iOffs := SizeOf(PageHead);
    if PageHead.Entries > 0 then
    begin
      SetLength(OffsArr, PageHead.Entries);
      Move(RawPage[iOffs], OffsArr[0], PageHead.Entries * SizeOf(Word));
      for i := 0 to Length(OffsArr) - 1 do
      begin
        iOffs := OffsArr[i];
        Move(RawPage[iOffs], eData, SizeOf(eData));
        if eData.EType = ENTRY_TYPE_DATA then
        begin
          if PageHead.TreeLevel = 1 then  // KeyDataEntry
          begin
            // key/value pairs
            iOffs := iOffs + SizeOf(eData);
            SetLength(sData, eData.Len);
            Move(RawPage[iOffs], sData[1], eData.Len);
            if (i mod 2) = 0 then
              LogInfo(Format('Key=%s', [sData]))
            else
              LogInfo(Format('Val=%s', [sData]));
          end
          else  // InternalEntry
          begin
            Move(RawPage[iOffs], eIntern, SizeOf(eIntern));
            iOffs := iOffs + SizeOf(eIntern);
            iLen := eIntern.Len - SizeOf(eIntern);
            if iLen > 0 then
            begin
              SetLength(sData, iLen);
              Move(RawPage[iOffs], sData[1], iLen);
              LogInfo(Format('#%d PgNo=%d nRecs=%d data=%s', [i, eIntern.PgNo, eIntern.NRecs, sData]));
            end;
          end;
        end;
      end;

    end;

    Inc(nPage);
    nPos := nPage * FPageSize;
    if nPage > 100 then
      break;
  end;
end;

procedure TDBReaderBerkley.ReadTable(AName: string; ACount: Int64; AList: TDbRowsList);
begin
  inherited;

end;

end.
