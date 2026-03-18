unit FSReaderPst;

(*
MS Outlook Personal Folders File (.pst) reader

Author: Sergey Bodrov, 2024 Minsk
License: MIT

https://learn.microsoft.com/en-us/openspecs/office_file_formats/ms-pst/

todo:
- filter messages by selected folder node
- read Table Context on heap
- mail attachements

*)

interface

uses
  SysUtils, Classes, FSReaderBase, DBReaderBase, DB {for ReadTable};

type
  TPstBlock = class(TObject)
  public
    BlockId: Int64;   // bid
    btKey: Int64;     // for blocks
    ByteIndex: Int64; // ib, file position
    ByteCount: Word;
    RefCount: Word;
    IsReaded: Boolean;
  end;

  TPstBlockList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    ParentList: TPstBlockList;
    function AddBlock(ABlockId, AKey, AByteIndex: Int64; AByteCount, ARefCount: Word): TPstBlock;
    function AddXBlock(ABlockId, AByteIndex: Int64; AByteCount: Word): TPstBlock; // BTree node block
    function GetItem(AIndex: Integer): TPstBlock;
    // todo: binary search
    function GetByBlockID(ABlockID: Int64): TPstBlock;
  end;

  TPstNodeList = class;

  TPstNode = class(TObject)
  private
    FBlockList: TPstBlockList;
    FSubNodeList: TPstNodeList;
  public
    NodeId: Int64;    // nid
    BlockId: Int64;   // bid
    SubNodeBlockId: Int64;
    Name: string;
    ParentNodeId: Int64;
    ParentNode: TPstNode;
    IsMessage: Boolean;

    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;

    property BlockList: TPstBlockList read FBlockList;
    property SubNodeList: TPstNodeList read FSubNodeList;
  end;

  TPstNodeList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function GetItem(AIndex: Integer): TPstNode;
    function GetByNodeID(ANodeID: Int64): TPstNode;
    // todo: binary search
    function GetByBlockID(ABlockID: Int64): TPstNode;
  end;

  TFSReaderPst = class(TFSReader)
  private
    FBlockList: TPstBlockList; // data blocks
    FNodeList: TPstNodeList;   // data nodes
    FBTreeList: TPstBlockList;  // BTree index nodes
    FVer: Word;             // File format version. 14,15 ANSI; 23+ Unicode; 37 Windows Information Protection
    FVerClient: Word;       // Client file format version
    FCryptMethod: Byte;
    FIsUnicode: Boolean;
    FIsMetadataReaded: Boolean;

    // read BTree index item, create new BTree items or node/block item
    procedure ReadBTreeItem(AItem: TPstBlock);
    // fill node data block list, links to other nodes
    procedure ReadNodeInfo(ANode: TPstNode; AIsSubNode: Boolean);
    // fill node subnodes list
    procedure ReadNodeSubnodes(ANode: TPstNode);
    // read and decode raw data from block
    function ReadBlockData(ABlock: TPstBlock; out AData: TBytes; ADecrypt: Boolean = False): Boolean;
    // read data from block heap, return True on success
    function ReadBlockHeap(ANode: TPstNode; ABlock: TPstBlock; ARowItem: TDbRowItem): Boolean;
    // read data from subnode
    function ReadSubNodeData(ASubNode: TPstNode; var AData: AnsiString): Boolean;

    function IsInternalBlockID(ABlockID: Int64): Boolean;
    procedure DecodePermute(const AData; ASize: Integer);
    procedure DecodeCyclic(const AData; ASize, AKey: Integer);
    function GetBlockSize(ABlockID: Int64): Integer;

  public
    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;

    function OpenFile(AFileName: string; AStream: TStream = nil): Boolean; override;
    // Read table data from DB to AList
    // AName - table name
    // ACount - how many items read
    procedure ReadTable(AName: string; ACount: Int64 = MaxInt; AList: TDbRowsList = nil);

    property BlockList: TPstBlockList read FBlockList;
    property NodeList: TPstNodeList read FNodeList;
  end;

implementation

const
  PST_HEADER_SIZE = 1024;
  PST_BTPAGE_SIZE = 512;
  PST_BLOCK_SIZE  = 8192;

  PST_NDB_CRYPT_NONE          = $00; // Data blocks are not encoded
  PST_NDB_CRYPT_PERMUTE       = $01; // Encoded with the Permutation algorithm
  PST_NDB_CRYPT_CYCLIC        = $02; // Encoded with the Cyclic algorithm
  PST_NDB_CRYPT_EDPCRYPTED    = $10; // Encrypted with Windows Information Protection

  PST_NID_TYPE_HID            = $00; // Heap node
  PST_NID_TYPE_INTERNAL       = $01; // Internal node
  PST_NID_TYPE_NORMAL_FOLDER  = $02; // Normal Folder object
  PST_NID_TYPE_SEARCH_FOLDER  = $03; // Search Folder object
  PST_NID_TYPE_NORMAL_MESSAGE = $04; // Normal Message object
  PST_NID_TYPE_ATTACHMENT     = $05; // Attachment object

  PST_HEAP_NODE_TYPE_TABLE    = $7C; // Table Context (TC/HN)
  PST_HEAP_NODE_TYPE_BTREE    = $B5; // BTree-on-Heap (BTH)
  PST_HEAP_NODE_TYPE_PROPERTY = $BC; // Property Context (PC/BTH)

type
  TPstBrefRec = record  // Block REFerence
    bid: Int64;   // Block ID   bit[30 or 62] is Internal flag
    ib: Int64;    // Byte Index (position in file)
  end;

  TPstRootRec = record
    FileEof: Int64;         // size of the PST file, in bytes
    AMapLast: Int64;        // position if last AMap page
    AMapFree: Int64;        // total free space in all AMaps
    PMapFree: Int64;        // total free space in all PMaps
    BrefNbt: TPstBrefRec;   // reference to root page of the Node BTree (NBT)
    BrefBbt: TPstBrefRec;   // reference to root page of the Block BTree (BBT)
    AMapValid: Byte;        // 0 - invalid AMaps
  end;

  TPstHeapID = record
    hid: Cardinal;
    function GetType: Byte;   // 0x0
    function GetIndex: Word;  // 1-based HID index
    function GetBlock: Word;  // 0-based data block index
    function GetDebugStr(): string;
  end;

  TPstHeapNodeRec = record
    PageMapOffs: Word;      // byte offset to the HN page Map record
    BlockSig: Byte;         // 0xEC Block signature
    ClientSig: Byte;        // PST_HEAP_NODE_TYPE_
    UserRoot: TPstHeapID;   // HID that points to the User Root record
    FillLevel: Cardinal;    // Per-block Fill Level Map
    AllocCount: Word;       // number of items (allocations)
    AllocFree: Word;        //  number of freed items
    AllocArr: array of Word; // byte offset to the beginning of the allocation
  end;

  TPstHeapItemRec = record
    ItemType: Byte;         // bTypeBTH
    KeySize: Byte;          // Size of the BTree Key value, in bytes. = 2, 4, 8, 16
    EntSize: Byte;          // Size of the data value, in bytes. 1..32
    IdxLevels: Byte;        // Index depth, 0-data, >0 - btree
    HidRoot: TPstHeapID;    // HID that points to the BTH entries
  end;

  TPstPropRec = record
    PropId: Word;           // Property ID (PROP_TAG_)
    PropType: Word;         // Property type (PROP_TYPE_)
    ValueHnid: TPstHeapID;  // points to heap or subnode, where value data
    Data: AnsiString;
    function AsDebugStr(): string;
    function ValueAsStr(): string;
    function IsDataOnValue(): Boolean; // data on Value/HNID
  end;

const
  CryptBytesR: array[Byte] of Byte = (
     65,  54,  19,  98, 168,  33, 110, 187, 244,  22, 204,   4, 127, 100, 232,  93,
     30, 242, 203,  42, 116, 197,  94,  53, 210, 149,  71, 158, 150,  45, 154, 136,
     76, 125, 132,  63, 219, 172,  49, 182,  72,  95, 246, 196, 216,  57, 139, 231,
     35,  59,  56, 142, 200, 193, 223,  37, 177,  32, 165,  70,  96,  78, 156, 251,
    170, 211,  86,  81,  69, 124,  85,   0,   7, 201,  43, 157, 133, 155,   9, 160,
    143, 173, 179,  15,  99, 171, 137,  75, 215, 167,  21,  90, 113, 102,  66, 191,
     38,  74, 107, 152, 250, 234, 119,  83, 178, 112,   5,  44, 253,  89,  58, 134,
    126, 206,   6, 235, 130, 120,  87, 199, 141,  67, 175, 180,  28, 212,  91, 205,
    226, 233,  39,  79, 195,   8, 114, 128, 207, 176, 239, 245,  40, 109, 190,  48,
     77,  52, 146, 213,  14,  60,  34,  50, 229, 228, 249, 159, 194, 209,  10, 129,
     18, 225, 238, 145, 131, 118, 227, 151, 230,  97, 138,  23, 121, 164, 183, 220,
    144, 122,  92, 140,   2, 166, 202, 105, 222,  80,  26,  17, 147, 185,  82, 135,
     88, 252, 237,  29,  55,  73,  27, 106, 224,  41,  51, 153, 189, 108, 217, 148,
    243,  64,  84, 111, 240, 198, 115, 184, 214,  62, 101,  24,  68,  31, 221, 103,
     16, 241,  12,  25, 236, 174,   3, 161,  20, 123, 169,  11, 255, 248, 163, 192,
    162,   1, 247,  46, 188,  36, 104, 117,  13, 254, 186,  47, 181, 208, 218,  61);
  CryptBytesS: array[Byte] of Byte = (
     20,  83,  15,  86, 179, 200, 122, 156, 235, 101,  72,  23,  22,  21, 159,   2,
    204,  84, 124, 131,   0,  13,  12,  11, 162,  98, 168, 118, 219, 217, 237, 199,
    197, 164, 220, 172, 133, 116, 214, 208, 167, 155, 174, 154, 150, 113, 102, 195,
     99, 153, 184, 221, 115, 146, 142, 132, 125, 165,  94, 209,  93, 147, 177,  87,
     81,  80, 128, 137,  82, 148,  79,  78,  10, 107, 188, 141, 127, 110,  71,  70,
     65,  64,  68,   1,  17, 203,   3,  63, 247, 244, 225, 169, 143,  60,  58, 249,
    251, 240,  25,  48, 130,   9,  46, 201, 157, 160, 134,  73, 238, 111,  77, 109,
    196,  45, 129,  52,  37, 135,  27, 136, 170, 252,   6, 161,  18,  56, 253,  76,
     66, 114, 100,  19,  55,  36, 106, 117, 119,  67, 255, 230, 180,  75,  54,  92,
    228, 216,  53,  61,  69, 185,  44, 236, 183,  49,  43,  41,   7, 104, 163,  14,
    105, 123,  24, 158,  33,  57, 190,  40,  26,  91, 120, 245,  35, 202,  42, 176,
    175,  62, 254,   4, 140, 231, 229, 152,  50, 149, 211, 246,  74, 232, 166, 234,
    233, 243, 213,  47, 112,  32, 242,  31,   5, 103, 173,  85,  16, 206, 205, 227,
     39,  59, 218, 186, 215, 194,  38, 212, 145,  29, 210,  28,  34,  51, 248, 250,
    241,  90, 239, 207, 144, 182, 139, 181, 189, 192, 191,   8, 151,  30, 108, 226,
     97, 224, 198, 193,  89, 171, 187,  88, 222,  95, 223,  96, 121, 126, 178, 138);
  CryptBytesI: array[Byte] of Byte = (
     71, 241, 180, 230,  11, 106, 114,  72, 133,  78, 158, 235, 226, 248, 148,  83,
    224, 187, 160,   2, 232,  90,   9, 171, 219, 227, 186, 198, 124, 195,  16, 221,
     57,   5, 150,  48, 245,  55,  96, 130, 140, 201,  19,  74, 107,  29, 243, 251,
    143,  38, 151, 202, 145,  23,   1, 196,  50,  45, 110,  49, 149, 255, 217,  35,
    209,   0,  94, 121, 220,  68,  59,  26,  40, 197,  97,  87,  32, 144,  61, 131,
    185,  67, 190, 103, 210,  70,  66, 118, 192, 109,  91, 126, 178,  15,  22,  41,
     60, 169,   3,  84,  13, 218,  93, 223, 246, 183, 199,  98, 205, 141,   6, 211,
    105,  92, 134, 214,  20, 247, 165, 102, 117, 172, 177, 233,  69,  33, 112,  12,
    135, 159, 116, 164,  34,  76, 111, 191,  31,  86, 170,  46, 179, 120,  51,  80,
    176, 163, 146, 188, 207,  25,  28, 167,  99, 203,  30,  77,  62,  75,  27, 155,
     79, 231, 240, 238, 173,  58, 181,  89,   4, 234,  64,  85,  37,  81, 229, 122,
    137,  56, 104,  82, 123, 252,  39, 174, 215, 189, 250,   7, 244, 204, 142,  95,
    239,  53, 156, 132,  43,  21, 213, 119,  52,  73, 182,  18,  10, 127, 113, 136,
    253, 157,  24,  65, 125, 147, 216,  88,  44, 206, 254,  36, 175, 222, 184,  54,
    200, 161, 128, 166, 153, 152, 168,  47,  14, 129, 101, 115, 228, 194, 162, 138,
    212, 225,  17, 208,   8, 139,  42, 242, 237, 154, 100,  63, 193, 108, 249, 236);


const
  // Property types
  PROP_TYPE_INT16        = $0002; // 2 Int16
  PROP_TYPE_INT32        = $0003; // 4 Int32
  PROP_TYPE_FLOAT32      = $0004; // 4 Single
  PROP_TYPE_FLOAT64      = $0005; // 8 Double
  PROP_TYPE_CURRENCY     = $0006; // 8 Currency
  PROP_TYPE_FLOAT_TIME   = $0007; // 8 TDateTime
  PROP_TYPE_ERROR_CODE   = $000A; // 4 Int32
  PROP_TYPE_BOOLEAN      = $000B; // 1 Byte
  PROP_TYPE_INT64        = $0014; // 8 Int64
  PROP_TYPE_STRING       = $001F; // ? Null-terminated wide string UTF-16LE
  PROP_TYPE_STRING8      = $001E; // ? Null-terminated byte string
  PROP_TYPE_TIME         = $0040; // 8 Int64 100-nanosec since 1601-01-01
  PROP_TYPE_GUID         = $0048; // 16 GUID
  PROP_TYPE_SERVER_ID    = $00FB; // ? Int16 Count + data
  PROP_TYPE_RESTRICTION  = $00FD; // ? byte array
  PROP_TYPE_RULE_ACTION  = $00FE; // ? Int16 Count + data
  PROP_TYPE_BINARY       = $0102; // ? Count * Byte
  // multiple values
  PROP_TYPE_MULTI_FLAG   = $1000; // ? Int32 Count * PROP_TYPE
  PROP_TYPE_MULTI_MASK   = $0FFF; //

  PROP_TYPE_NONE         = $0000; // 0
  PROP_TYPE_NULL         = $0001; // 0
  PROP_TYPE_OBJECT       = $000D; // COM object

  // -- Property tags
  // Message object envelope property
  PROP_TAG_IMPORTANCE    = $0017; // (int32) level of importance assigned by the end user to the Message object
  PROP_TAG_MESSAGE_CLASS = $001A; // (wstr) denotes the specific type of the Message object
  PROP_TAG_SUBJECT       = $0037; // (wstr) subject of the email message
  PROP_TAG_CLIENT_SUBMIT_TIME = $0039; // (time) time, in UTC, when the email message is submitted
  PROP_TAG_SENT_REPRESENTING_SEARCH_KEY = $003B; // (binary) binary-comparable key that represents the end user who is represented by the sending mailbox owner
  PROP_TAG_SENT_REPRESENTING_ENTRY_ID   = $0041; // (binary) identifier of the end user who is represented by the sending mailbox owner.
  PROP_TAG_SENT_REPRESENTING_NAME       = $0042; // (wstr) display name for the end user who is represented by the sending mailbox owner
  PROP_TAG_SENT_REPRESENTING_ADDRESS_TYPE = $0064; // (wstr) email address type
  PROP_TAG_SENT_REPRESENTING_EMAIL_ADDRESS = $0065; // (wstr) email address for the end user who is represented by the sending mailbox owner.
  PROP_TAG_CONVERSATION_TOPIC = $0070; // (wstr) unchanging copy of the original subject
  PROP_TAG_CONVERSATION_INDEX = $0071; // (binary) indicates the relative position of this message within a conversation thread
  PROP_TAG_TRANSPORT_MESSAGE_HEADERS = $007D; // (wstr) transport-specific message envelope information for email
  // Recipient property
  PROP_TAG_SENDER_ENTRY_ID = $0C19; // (binary) identifies an address book EntryID that contains the address book EntryID of the sending mailbox owner
  PROP_TAG_SENDER_NAME   = $0C1A; // (wstr) display name of the sending mailbox owner
  PROP_TAG_SENDER_SEARCH_KEY = $0C1D; // (binary) identifies an address book search key
  PROP_TAG_SENDER_ADDRESS_TYPE = $0C1E; // (wstr) email address type of the sending mailbox owner
  PROP_TAG_SENDER_EMAIL_ADDRESS = $0C1F; // (wstr) email address of the sending mailbox owner
  // Non-transmittable Message property
  PROP_TAG_DISPLAY_CC    = $0E03; // (wstr)  list of carbon copy (Cc) recipient display names
  PROP_TAG_DISPLAY_TO    = $0E04; // (wstr) list of the primary recipient display names, separated by semicolons, when an email message has primary recipients
  PROP_TAG_MESSAGE_DELIVERY_TIME = $0E06; // (time) time (in UTC) when the server received the message
  PROP_TAG_MESSAGE_FLAGS = $0E07; // (int32) status of the Message object
  PROP_TAG_MESSAGE_SIZE  = $0E08; // (int32) size, in bytes, consumed by the Message object on the server
  // Message content property
  PROP_TAG_BODY          = $1000; // (wstr) message body text in plain text format
  PROP_TAG_INTERNET_MESSAGE_ID = $1035; // (wstr) corresponds to the message-id field
  // Multi-purpose property that can appear on all or most objects
  PROP_TAG_DISPLAY_NAME  = $3001; // (wstr) display name of the folder
  PROP_TAG_COMMENT       = $3004; // (wstr) address book object comment
  PROP_TAG_CREATION_TIME = $3007; // (time) time, in UTC, that the object was created
  PROP_TAG_LAST_MODIFICATION_TIME = $3008; // (time) time, in UTC, of the last modification to the object
  PROP_TAG_SEARCH_KEY    = $300B; // (binary) unique binary-comparable key that identifies an object for a search
  // Folder and address book container property
  PROP_TAG_CONTENT_COUNT = $3602; // (int32) number of rows under the header row
  PROP_TAG_CONTENT_UNREAD_COUNT = $3603; // (int32) TAG_CONTENT_COUNT where TAG_READ is False
  PROP_TAG_SUBFOLDERS    = $360A; // (bool) a folder has subfolders
  PROP_TAG_CONTAINER_CLASS = $3613; // (wstr) describes the type of Message object that a folder contains
  //
  PROP_TAG_INTERNET_CODEPAGE = $3FDE; // (int32) code page used for the BODY property
  // Provider-defined internal non-transmittable property
  PROP_TAG_USER_ENTRY_ID = $6619; // ??


// assign property to DbRow column
// see also AssignMainListFieldDefs()
procedure AssignMailPropToDbRow(const AProp: TPstPropRec; ARowItem: TDbRowItem);
var
  iCol: Integer;
begin
  // tag to col index
  iCol := -1;
  case AProp.PropId of
    PROP_TAG_MESSAGE_DELIVERY_TIME: iCol := 0;  // date
    PROP_TAG_SENDER_NAME:           icol := 1;  // From name
    PROP_TAG_SENDER_EMAIL_ADDRESS:  iCol := 2;  // From addr
    PROP_TAG_DISPLAY_TO:            iCol := 3;  // To name,addr
    PROP_TAG_SUBJECT:               iCol := 4;
    PROP_TAG_BODY:                  iCol := 5;
    PROP_TAG_TRANSPORT_MESSAGE_HEADERS: iCol := 6;
  end;
  if iCol < 0 then Exit;

  if Length(ARowItem.Values) < 7 then
    SetLength(ARowItem.Values, 7);

  ARowItem.Values[iCol] := AProp.ValueAsStr;
end;

// assign mail messages list table FieldDef
// must correspond to AssignMailPropToDbRow()
procedure AssignMainListFieldDefs(AList: TDbRowsList);

  procedure SetFieldDef(var ARec: TDbFieldDefRec; AName, ATypeName: string; AFieldType: TFieldType);
  begin
    ARec.Name := AName;
    ARec.TypeName := ATypeName;
    ARec.FieldType := AFieldType;
    ARec.Size := 0;
    ARec.RawOffset := 0;
  end;

begin
  SetLength(AList.FieldsDef, 7);

  SetFieldDef(AList.FieldsDef[0], 'DeliveryTime', 'Timestamp', ftString);
  SetFieldDef(AList.FieldsDef[1], 'SenderName', 'String', ftString);
  SetFieldDef(AList.FieldsDef[2], 'SenderEmail', 'String', ftString);
  SetFieldDef(AList.FieldsDef[3], 'DisplayTo', 'String', ftString);
  SetFieldDef(AList.FieldsDef[4], 'Subject', 'String', ftString);
  SetFieldDef(AList.FieldsDef[5], 'Body', 'Memo', ftMemo);
  SetFieldDef(AList.FieldsDef[6], 'TransportMessageHeaders', 'Memo', ftMemo);
end;

{ TDBReaderPst }

procedure TFSReaderPst.AfterConstruction;
begin
  inherited;
  FBlockList := TPstBlockList.Create();
  FNodeList := TPstNodeList.Create();
  FBTreeList := TPstBlockList.Create();
end;

procedure TFSReaderPst.BeforeDestruction;
begin
  FreeAndNil(FBTreeList);
  FreeAndNil(FNodeList);
  FreeAndNil(FBlockList);
  inherited;
end;

procedure TFSReaderPst.DecodePermute(const AData; ASize: Integer);
var
  pb: PByte;
  i: Integer;
begin
  pb := Addr(AData);
  for i := 0 to ASize - 1 do
  begin
    pb^ := CryptBytesI[pb^];
    Inc(pb);
  end;
end;

function TFSReaderPst.GetBlockSize(ABlockID: Int64): Integer;
var
  TmpBlock: TPstBlock;
begin
  Result := -1;
  TmpBlock := BlockList.GetByBlockID(ABlockID);
  if Assigned(TmpBlock) then
    Result := TmpBlock.ByteCount;
end;

function TFSReaderPst.IsInternalBlockID(ABlockID: Int64): Boolean;
begin
  if FIsUnicode then
    Result := ((ABlockID shr 62) = 1)
  else
    Result := ((ABlockID shr 30) = 1);
end;

{$Q-}
procedure TFSReaderPst.DecodeCyclic(const AData; ASize, AKey: Integer);
var
  pb: PByte;
  b: Integer;
  w: Word;
begin
  pb := Addr(AData);
  w := Word(AKey) xor Word(AKey shr 16);
  while ASize > 0 do
  begin
    Dec(ASize);
    b := pb^;
    b := Byte(b + Byte(w));
    b := CryptBytesR[Byte(b)];
    b := Byte(b + Byte(w shr 8));
    b := CryptBytesS[Byte(b)];
    b := Byte(b - Byte(w shr 8));
    b := CryptBytesI[Byte(b)];
    b := Byte(b - Byte(w));
    pb^ := Byte(b);
    Inc(pb);
    w := w + 1;
  end;
end;
{$Q+}

function TFSReaderPst.OpenFile(AFileName: string; AStream: TStream): Boolean;
var
  Buf: TByteArray;
  rdr: TRawDataReader;
  tmpDWord: Cardinal;
  RootRec: TPstRootRec;
  TmpNode: TPstNode;
  TmpXBlock: TPstBlock;
  i, ii, iCount: Integer;
begin
  Result := inherited OpenFile(AFileName, AStream);
  if not Result then Exit;
  FIsMetadataReaded := False;
  FBlockList.Clear();
  FNodeList.Clear();
  FBTreeList.Clear();

  // read header
  FFile.Read(Buf[0], PST_HEADER_SIZE);
  rdr.Init(Buf[0]);
  tmpDWord := rdr.ReadUInt32();  // Magic "!BDN"
  if tmpDWord <> $4E444221 then
  begin
    LogInfo('!Wrong file signature');
    Exit;
  end;
  rdr.ReadUInt32(); // CRC
  tmpDWord := rdr.ReadUInt16(); // MagicClient $4D53
  FVer := rdr.ReadUInt16();
  FVerClient := rdr.ReadUInt16();
  FIsUnicode := (FVer >= 23);

  // ROOT structure
  if FIsUnicode then
  begin
    // 4+4+2+2+2+1+1+4+4=24 +8+8 +4+128 + 8
    rdr.SetPosition(180);
    //rdr.ReadBytes(72);
    rdr.ReadUInt32(); // reserved
    RootRec.FileEof := rdr.ReadInt64();
    RootRec.AMapLast := rdr.ReadInt64();
    RootRec.AMapFree := rdr.ReadInt64();
    RootRec.PMapFree := rdr.ReadInt64();
    RootRec.BrefNbt.bid := rdr.ReadInt64();
    RootRec.BrefNbt.ib := rdr.ReadInt64();
    RootRec.BrefBbt.bid := rdr.ReadInt64();
    RootRec.BrefBbt.ib := rdr.ReadInt64();
    RootRec.AMapValid := rdr.ReadUInt8();
  end
  else
  begin
    // 4+4+2+2+2+1+1+4+4=24 +8+8 +4+128
    rdr.SetPosition(172);
    //rdr.ReadBytes(40);
    rdr.ReadUInt32(); // reserved
    RootRec.FileEof := rdr.ReadUInt32();
    RootRec.AMapLast := rdr.ReadUInt32();
    RootRec.AMapFree := rdr.ReadUInt32();
    RootRec.PMapFree := rdr.ReadUInt32();
    RootRec.BrefNbt.bid := rdr.ReadUInt32();
    RootRec.BrefNbt.ib := rdr.ReadUInt32();
    RootRec.BrefBbt.bid := rdr.ReadUInt32();
    RootRec.BrefBbt.ib := rdr.ReadUInt32();
    RootRec.AMapValid := rdr.ReadUInt8();
  end;

  // crypt method
  if FIsUnicode then
  begin
    // 180+72+4+128+128+1
    rdr.SetPosition(513);
    FCryptMethod := rdr.ReadUInt8();
  end
  else
  begin
    // 172+40+128+128+1
    rdr.SetPosition(469);
    FCryptMethod := rdr.ReadUInt8();
  end;

  // DList (Density list)
  //FFile.Position := $4200;

  // == read nodes
  // BTree root node
  FBTreeList.AddXBlock(RootRec.BrefNbt.bid, RootRec.BrefNbt.ib, 0);
  // read nodes btree levels
  iCount := 0;
  while iCount <> FBTreeList.Count do
  begin
    iCount := FBTreeList.Count;
    for i := 0 to iCount - 1 do
    begin
      TmpXBlock := FBTreeList.GetItem(i);
      if not TmpXBlock.IsReaded then
        ReadBTreeItem(TmpXBlock);
    end;
  end;
  FBTreeList.Clear();

  // == read blocks, assign to nodes
  // BTree root node
  FBTreeList.AddXBlock(RootRec.BrefBbt.bid, RootRec.BrefBbt.ib, 0);
  // read blocks btree levels
  iCount := 0;
  while iCount <> FBTreeList.Count do
  begin
    iCount := FBTreeList.Count;
    for i := 0 to iCount - 1 do
    begin
      TmpXBlock := FBTreeList.GetItem(i);
      if not TmpXBlock.IsReaded then
        ReadBTreeItem(TmpXBlock);
    end;
  end;
  FBTreeList.Clear();

  // !! block map
  {sl := TStringList.Create();
  try
    for i := 0 to BlockList.Count - 1 do
    begin
      TmpNode := BlockList.GetItem(i);
      sl.Add(Format('bid=$%.16x  ib=$%.16x  cb=%d  rc=%d', [TmpNode.BlockId, TmpNode.ByteIndex, TmpNode.BlockByteCount, TmpNode.BlockRefCount]));
    end;
    sl.SaveToFile('pst_block_map.txt');
  finally
    sl.Free();
  end;  }

  if IsDebugPages then
    LogInfo(Format('Nodes=%d  Blocks=%d', [NodeList.Count, BlockList.Count]));
  // == prepare nodes
  for i := 0 to NodeList.Count - 1 do
  begin
    TmpNode := NodeList.GetItem(i);
    ReadNodeInfo(TmpNode, False);
    for ii := 0 to TmpNode.BlockList.Count - 1 do
      ReadBlockHeap(TmpNode, TmpNode.BlockList.GetItem(ii), nil);
  end;

  FIsMetadataReaded := True;
end;

procedure TFSReaderPst.ReadBTreeItem(AItem: TPstBlock);
var
  TmpNode: TPstNode;
  TmpBlock: TPstBlock;
  Buf: TByteArray;
  rdr: TRawDataReader;
  EntCount, EntLevel: Integer;
  PageType: Byte;
  i: Integer;
begin
  // btree page info
  FFile.Position := AItem.ByteIndex;
  FFile.Read(Buf[0], PST_BTPAGE_SIZE);
  rdr.Init(Buf[0]);
  if FIsUnicode then
    rdr.SetPosition(PST_BTPAGE_SIZE - (16+4+4))
  else
    rdr.SetPosition(PST_BTPAGE_SIZE - (12+4));
  EntCount := rdr.ReadUInt8;
  rdr.ReadUInt8;  // entry max count
  rdr.ReadUInt8; // entry total size for this level
  EntLevel := rdr.ReadUInt8;
  if FIsUnicode then
    rdr.ReadUInt32;  // padding
  // trailer
  PageType := rdr.ReadUInt8;
  // read entries
  rdr.SetPosition(0);
  for i := 0 to EntCount - 1 do
  begin
    if (EntLevel = 0) and (PageType = $81) then  // Node BTree ENTRY (NBTENTRY)
    begin
      TmpNode := TPstNode.Create();
      TmpNode.BlockList.ParentList := FBlockList;
      NodeList.Add(TmpNode);
      if FIsUnicode then
      begin
        TmpNode.NodeId := rdr.ReadInt64();
        TmpNode.BlockId := rdr.ReadInt64();
        TmpNode.SubNodeBlockId := rdr.ReadInt64();
        TmpNode.ParentNodeId := rdr.ReadUInt32();
        rdr.ReadUInt32(); // padding for Unicode
      end
      else
      begin
        TmpNode.NodeId := rdr.ReadUInt32();
        TmpNode.BlockId := rdr.ReadUInt32();
        TmpNode.SubNodeBlockId := rdr.ReadUInt32();
        TmpNode.ParentNodeId := rdr.ReadUInt32();
      end;
    end
    else
    if (EntLevel = 0) and (PageType = $80) then  // Block BTree ENTRY (BBTENTRY)
    begin
      TmpBlock := TPstBlock.Create();
      BlockList.Add(TmpBlock);
      if FIsUnicode then
      begin
        TmpBlock.BlockId := rdr.ReadInt64();
        TmpBlock.ByteIndex := rdr.ReadInt64();
        TmpBlock.ByteCount := rdr.ReadUInt16();
        TmpBlock.RefCount := rdr.ReadUInt16();
        rdr.ReadUInt32(); // padding for Unicode
      end
      else
      begin
        TmpBlock.BlockId := rdr.ReadUInt32();
        TmpBlock.ByteIndex := rdr.ReadUInt32();
        TmpBlock.ByteCount := rdr.ReadUInt16();
        TmpBlock.RefCount := rdr.ReadUInt16();
      end;
      // !!!
      if (TmpBlock.ByteIndex > $0FFFFFFF)
      or ((TmpBlock.BlockId and $0FFFFFFF00000000) > 0)
      or (TmpBlock.RefCount > 20) then
        LogInfo('!invalid block');
    end
    else  // intermediate BTree ENTRY (BTENTRY)
    begin
      TmpBlock := TPstBlock.Create();
      FBTreeList.Add(TmpBlock);
      if FIsUnicode then
      begin
        TmpBlock.btKey := rdr.ReadInt64();
        TmpBlock.BlockId := rdr.ReadInt64();
        TmpBlock.ByteIndex := rdr.ReadInt64();
      end
      else
      begin
        TmpBlock.btKey := rdr.ReadUInt32();
        TmpBlock.BlockId := rdr.ReadUInt32();
        TmpBlock.ByteIndex := rdr.ReadUInt32();
      end;
    end;
  end;
  AItem.IsReaded := True;
end;

function TFSReaderPst.ReadBlockData(ABlock: TPstBlock; out AData: TBytes; ADecrypt: Boolean): Boolean;
var
  iDataSize: Integer;
begin
  Result := False;
  if not Assigned(ABlock) then Exit;

  iDataSize := ABlock.ByteCount;
  if (ABlock.ByteIndex <= 0) or (ABlock.ByteCount = 0) then
  begin
    LogInfo(Format('!Block $%x empty!', [ABlock.BlockId]));
    Exit;
  end;
  Assert(iDataSize <= PST_BLOCK_SIZE, 'Block size > max');
  Assert(iDataSize > 0, 'Block size = 0');
  if iDataSize > PST_BLOCK_SIZE then
    iDataSize := PST_BLOCK_SIZE;

  // read data
  SetLength(AData, iDataSize);
  FFile.Position := ABlock.ByteIndex;
  FFile.Read(AData[0], iDataSize);
  //if not IsInternalBlockID(ABlock.BlockId) then
  if ADecrypt then
  begin
    // Decrypt data
    if FCryptMethod = PST_NDB_CRYPT_PERMUTE then
      DecodePermute(AData[0], iDataSize)
    else
    if FCryptMethod = PST_NDB_CRYPT_CYCLIC then
       DecodeCyclic(AData[0], iDataSize, Integer(ABlock.BlockId));
    //!!!
    //BufToFile(AData[0], ABlock.ByteCount, IntToHex(ANode.NodeId, 8) + '_dec.data');
  end;
  Result := True;
end;

function TFSReaderPst.ReadBlockHeap(ANode: TPstNode; ABlock: TPstBlock; ARowItem: TDbRowItem): Boolean;
var
  i, iItem, iPos, iSize: Integer;
  iPropPos, iPropSize: Integer;
  BlockData: TBytes;
  rdr: TRawDataReader;
  HeapNode: TPstHeapNodeRec;
  HeapItem: TPstHeapItemRec;
  Prop: TPstPropRec;
  TmpSubNode: TPstNode;
begin
  // Data Block contain Heap-on-Node (HN):
  // HNHDR - HeapNode header.
  //   ClientSig - items type (Table, BTree or Prop)
  //   UserRoot - index of user info root item
  // ..Items..
  // HNPAGEMAP - HeapNode items allocation

  // Item type BTRee-on-heap (BTH):
  // BTHHEADER
  // (Bytes) Name - Description
  // (1) bType - 0xB5 or 0xBC
  // (1) cbKey
  // (1) cbEnt
  // (1) bIdxLevels
  // (4 )hidRoot - root item index
  Result := False;
  if IsDebugPages then
    LogInfo(Format('== BlockID=$%x  ByteIndex=$%x  BlockByteCount=%d',
      [ABlock.BlockId, ABlock.ByteIndex, ABlock.ByteCount]));
  if (ABlock.ByteIndex = 0) or (ABlock.ByteCount = 0) then Exit;

  if not ReadBlockData(ABlock, BlockData, not IsInternalBlockID(ABlock.BlockId)) then Exit;
  rdr.Init(BlockData[0]);

  // read HeapNode header HNHDR
  HeapNode.PageMapOffs := rdr.ReadUInt16;
  HeapNode.BlockSig := rdr.ReadUInt8;
  if HeapNode.BlockSig <> $EC then
    Exit;
  //Assert(HeapNode.BlockSig = $EC);
  HeapNode.ClientSig := rdr.ReadUInt8;
  Assert(HeapNode.ClientSig in [PST_HEAP_NODE_TYPE_TABLE, PST_HEAP_NODE_TYPE_BTREE, PST_HEAP_NODE_TYPE_PROPERTY]);
  HeapNode.UserRoot.hid := rdr.ReadUInt32;
  HeapNode.FillLevel := rdr.ReadUInt32;
  // read HeapNode page map HNPAGEMAP
  rdr.SetPosition(HeapNode.PageMapOffs);
  HeapNode.AllocCount := rdr.ReadUInt16;
  HeapNode.AllocFree := rdr.ReadUInt16;
  SetLength(HeapNode.AllocArr, HeapNode.AllocCount+1);
  for i := 0 to HeapNode.AllocCount do // +1 unused
  begin
    HeapNode.AllocArr[i] := rdr.ReadUInt16;
  end;
  if IsDebugPages then
    LogInfo(Format('-- HeapNode  BlockSig=$%x  ClientSig=$%x  AllocCount=%d  AllocFree=%d  UserRoot=%s',
      [HeapNode.BlockSig, HeapNode.ClientSig, HeapNode.AllocCount, HeapNode.AllocFree, HeapNode.UserRoot.GetDebugStr]));

  // -- enumerate heap items
  iPos := 0;
  iSize := 0;
  {s := '';
  for i := 0 to HeapNode.AllocCount-1 do
  begin
    iSize := HeapNode.AllocArr[i+1] - HeapNode.AllocArr[i];
    if s <> '' then
      s := s + '  ';
    s := s + IntToHex(HeapNode.AllocArr[i], 4) + '.' + IntToStr(iSize);
  end;
  LogInfo(Format('- HeapNode Alloc(%s)', [s]));  }

  // -- read heap items
  {// user root item
  iItem := HeapNode.UserRoot.GetIndex;
  if (iItem >= 0) and (iItem < HeapNode.AllocCount) then
    rdr.SetPosition(HeapNode.AllocArr[iItem])
  else
  begin
    LogInfo('!Bad heap item');
    Exit;
  end;  }

  //for i := 0 to HeapNode.AllocCount-1 do
  begin
    {iPos := HeapNode.AllocArr[i];
    iSize := HeapNode.AllocArr[i+1] - HeapNode.AllocArr[i];
    rdr.SetPosition(iPos);
    // dump data
    s := rdr.ReadBytes(iSize);
    LogInfo(Format('-- HeapItem [%.4x.%.6d]=%s', [iPos, iSize, DataAsStr(s[1], Length(s)) ]));
    rdr.SetPosition(iPos);  }


    if HeapNode.ClientSig = PST_HEAP_NODE_TYPE_TABLE then  // TC/HN
    begin
      if IsDebugPages then
        LogInfo(Format('--- HeapItem [%.4x.%.6d] (Table) ',
          [iPos, iSize]));
      // todo: read Table
    end
    else
    if HeapNode.ClientSig = PST_HEAP_NODE_TYPE_BTREE then  // BTH
    begin
      // BTHHEADER
      HeapItem.ItemType := rdr.ReadUInt8;
      HeapItem.KeySize := rdr.ReadUInt8;
      HeapItem.EntSize := rdr.ReadUInt8;
      HeapItem.IdxLevels := rdr.ReadUInt8;
      HeapItem.HidRoot.hid := rdr.ReadUInt32;
      if IsDebugPages then
        LogInfo(Format('--- HeapItem [%.4x.%.6d] (BTree) ItemType=$%x  KeySize=%d  EntSize=%d  IdxLevels=%d  Root=%s',
          [iPos, iSize, HeapItem.ItemType, HeapItem.KeySize, HeapItem.EntSize, HeapItem.IdxLevels, HeapItem.HidRoot.GetDebugStr]));
    end
    else
    if HeapNode.ClientSig = PST_HEAP_NODE_TYPE_PROPERTY then // PC/BTH
    begin
      // BTHHEADER
      iItem := HeapNode.UserRoot.GetIndex-1;
      if (iItem < 0) and (iItem >= HeapNode.AllocCount) then
      begin
        LogInfo('!Bad heap item');
        Exit;
      end;
      iPos := HeapNode.AllocArr[iItem];
      iSize := HeapNode.AllocArr[iItem+1] - HeapNode.AllocArr[iItem];
      rdr.SetPosition(iPos);

      HeapItem.ItemType := rdr.ReadUInt8; // =$BC
      HeapItem.KeySize := rdr.ReadUInt8;  // =2
      HeapItem.EntSize := rdr.ReadUInt8;  // =6
      HeapItem.IdxLevels := rdr.ReadUInt8;
      HeapItem.HidRoot.hid := rdr.ReadUInt32;
      if IsDebugPages then
        LogInfo(Format('--- HeapItem [%.4x.%.6d] (Prop) ItemType=$%x  KeySize=%d  EntSize=%d  IdxLevels=%d  Root=%s',
          [iPos, iSize, HeapItem.ItemType, HeapItem.KeySize, HeapItem.EntSize, HeapItem.IdxLevels, HeapItem.HidRoot.GetDebugStr]));

      // items PC BTH records
      iItem := HeapItem.HidRoot.GetIndex-1;
      if (iItem < 0) and (iItem >= HeapNode.AllocCount) then
      begin
        LogInfo('!Bad heap item');
        Exit;
      end;
      iPos := HeapNode.AllocArr[iItem];
      iSize := HeapNode.AllocArr[iItem+1] - HeapNode.AllocArr[iItem];

      // read PC BTH record
      while iSize >= (HeapItem.KeySize + HeapItem.EntSize) do
      begin
        rdr.SetPosition(iPos);
        Dec(iSize, (HeapItem.KeySize + HeapItem.EntSize));
        Prop.PropId := rdr.ReadUInt16;
        Prop.PropType := rdr.ReadUInt16;
        Prop.ValueHnid.hid := rdr.ReadUInt32;
        iPos := rdr.GetPosition();
        //LogInfo(Format('--- Prop Id=$%x  Type=$%d  Value(Type=%d Index=$%x  Block=$%x)',
        //  [Prop.PropId, Prop.PropType, Prop.ValueHnid.GetType, Prop.ValueHnid.GetIndex, Prop.ValueHnid.GetOffs]));
        iItem := Prop.ValueHnid.GetIndex-1;
        Prop.Data := '';
        // prop data in same block
        if Prop.IsDataOnValue then
          // no need to read data
        else
        if (Prop.ValueHnid.GetType() = 0) and (Prop.ValueHnid.GetBlock() = 0)
        and (iItem >= 0) and (iItem < HeapNode.AllocCount) then
        begin
          iPropPos := HeapNode.AllocArr[iItem];
          iPropSize := HeapNode.AllocArr[iItem+1] - HeapNode.AllocArr[iItem];
          rdr.SetPosition(iPropPos);
          Prop.Data := rdr.ReadBytes(iPropSize);
        end
        else
        begin
          TmpSubNode := ANode.SubNodeList.GetByNodeID(Prop.ValueHnid.hid);
          if Assigned(TmpSubNode) then
          begin
            ReadSubNodeData(TmpSubNode, Prop.Data);
          end
          else if Prop.ValueHnid.hid <> 0 then
            LogInfo(Format('!SubNode_$%x not found!', [Prop.ValueHnid.hid]));
        end;

        if IsDebugPages then
          LogInfo(Format('---- Prop %s', [Prop.AsDebugStr]));
        // -- process property
        case Prop.PropId of
          PROP_TAG_DISPLAY_NAME: ANode.Name := Prop.ValueAsStr;
          PROP_TAG_BODY: ANode.IsMessage := True;
        end;

        if ANode.IsMessage and Assigned(ARowItem) then
        begin
          // set row value
          AssignMailPropToDbRow(Prop, ARowItem);
        end;
      end;

      {if HeapItem.HidRoot.GetType = 0 then // HID, item stored in data block
      begin
        if HeapItem.IdxLevels = 0 then // actual data
        begin
          // read PC BTH record
          while iSize >= (HeapItem.KeySize + HeapItem.EntSize) do
          begin
            Dec(iSize, (HeapItem.KeySize + HeapItem.EntSize));
            Prop.PropId := rdr.ReadUInt16;
            Prop.PropType := rdr.ReadUInt16;
            Prop.ValueHnid.hid := rdr.ReadUInt32;
            //LogInfo(Format('--- Prop Id=$%x  Type=$%d  Value(Type=%d Index=$%x  Block=$%x)',
            //  [Prop.PropId, Prop.PropType, Prop.ValueHnid.GetType, Prop.ValueHnid.GetIndex, Prop.ValueHnid.GetOffs]));
            LogInfo(Format('--- Prop %s', [Prop.AsDebugStr]));
          end;
        end
        else  // key-hid  indexes
        begin
          while iSize >= (HeapItem.KeySize + HeapItem.EntSize) do
          begin
            Dec(iSize, (HeapItem.KeySize + HeapItem.EntSize));
            rdr.ReadBytes(HeapItem.KeySize);
            rdr.ReadBytes(HeapItem.EntSize);
          end;
        end;
      end
      else  // NID, item stored in subnode block
      begin

      end; }
    end;
  end;
  Result := True;
end;

procedure TFSReaderPst.ReadNodeInfo(ANode: TPstNode; AIsSubNode: Boolean);
var
  TmpBlock, NewBlock: TPstBlock;
  TmpSubNode: TPstNode;
  BlockData: TBytes;
  rdr: TRawDataReader;
  iBlockID: Int64;
  i, ii, iCurBlock, iDataSize, iTotalSize: Integer;
  BlockType, BlockLevel: Byte;
  EntryCount: Integer;
  sNode, s: string;
begin
  // Data Tree (BlockType=$01):
  // XXBLOCK   - level 2  - references to XBLOCK
  // XBLOCK    - level 1  - references to DataBlock
  // DataBlock - level 0  - Data Bytes (encoded)

  sNode := '= Node';
  if AIsSubNode then
    sNode := '== SubNode';

  if IsDebugPages then
    LogInfo(Format('%sID=$%x  ParentNodeID=$%x  Block(ID=$%x  Size=%d)  SubNodeBlock(ID=$%x  Size=%d)',
      [sNode, ANode.NodeId, ANode.ParentNodeId, ANode.BlockID, GetBlockSize(ANode.BlockID), ANode.SubNodeBlockId, GetBlockSize(ANode.SubNodeBlockId)]));

  // read subnodes
  ANode.BlockList.Clear();
  ReadNodeSubnodes(ANode);

  // enumerate subnodes
  ANode.BlockList.Clear();
  s := '';
  for i := 0 to ANode.SubNodeList.Count - 1 do
  begin
    TmpSubNode := ANode.SubNodeList.GetItem(i);
    s := s + Format('SubNodeID=$%x', [TmpSubNode.NodeId]);
    if TmpSubNode.SubNodeBlockId <> 0 then
      s := s + Format('(SubBlockID=$%x)', [TmpSubNode.SubNodeBlockId]);
    s := s + '; ';
    ReadNodeInfo(TmpSubNode, True);
  end;
  if IsDebugPages and (s <> '') then
    LogInfo(Format('%sID=$%x  SubNodes: %s', [sNode, ANode.NodeId, s]));

  // set links to parents
  if ANode.ParentNodeId <> 0 then
    ANode.ParentNode := NodeList.GetByNodeID(ANode.ParentNodeId);
  // assign data block
  if ANode.BlockId <> 0 then
  begin
    TmpBlock := BlockList.GetByBlockID(ANode.BlockId);
    if Assigned(TmpBlock) then
      ANode.BlockList.Add(TmpBlock)
    else
      LogInfo(Format('!Node Root BlockID $%x not found!', [ANode.BlockId]));
  end;

  iCurBlock := 0;
  while iCurBlock < ANode.BlockList.Count do
  begin
    TmpBlock := ANode.BlockList.GetItem(iCurBlock);
    iBlockID := TmpBlock.BlockId;
    iDataSize := TmpBlock.ByteCount;
    if (TmpBlock.ByteIndex <= 0) or (TmpBlock.ByteCount = 0) then
    begin
      LogInfo(Format('!Node $%x Block part $%x empty!', [ANode.NodeId, iBlockID]));
      ANode.BlockList.Delete(iCurBlock);
      Continue;
    end;
    Assert(iDataSize <= PST_BLOCK_SIZE, 'Block size > max');
    Assert(iDataSize > 0, 'Block size = 0');
    if iDataSize > PST_BLOCK_SIZE then
      iDataSize := PST_BLOCK_SIZE;

    // read data
    if IsInternalBlockID(iBlockID) then
    begin
      // internal block - read BlockID of sub-blocks
      if not ReadBlockData(TmpBlock, BlockData) then
        Exit;
      rdr.Init(BlockData[0]);

      BlockType := rdr.ReadUInt8;    // 0x01
      Assert(BlockType = $01);
      BlockLevel := rdr.ReadUInt8;
      Assert(BlockLevel < 3);
      EntryCount := rdr.ReadUInt16;
      rdr.ReadInt32;  // total size
      for ii := 0 to EntryCount - 1 do
      begin
        if FIsUnicode then
          iBlockID := rdr.ReadInt64
        else
          iBlockID := rdr.ReadUInt32;

        NewBlock := BlockList.GetByBlockID(iBlockID);
        if Assigned(NewBlock) then
          ANode.BlockList.Add(NewBlock)
        else
        begin
          LogInfo(Format('!Node $%x Block part $%x not found!', [ANode.NodeId, iBlockID]));
          //Exit;
        end;
      end;
      // XBlock not needed anymore
      ANode.BlockList.Delete(iCurBlock);
    end
    else // data block
      Inc(iCurBlock);
  end;
end;

procedure TFSReaderPst.ReadNodeSubnodes(ANode: TPstNode);
var
  TmpBlockList: TPstBlockList;
  TmpBlock: TPstBlock;
  BlockData: TBytes;
  SubNode: TPstNode;
  rdr: TRawDataReader;
  iDataBlockID, iSubNodeBlockID, iSubNodeID: Int64;
  ii, iCurBlock: Integer;
  BlockType, BlockLevel: Byte;
  EntryCount: Integer;
begin
  // Subnode Tree (BlockType=$02):
  // SIBLOCK   - level 1  - array of SIENTRY (intermediate block entry)
  // SLBLOCK   - level 0  - array of SLENTRY (leaf block entry)

  // tree block list
  TmpBlockList := ANode.BlockList;

  if ANode.SubNodeBlockId <> 0 then
  begin
    TmpBlock := BlockList.GetByBlockID(ANode.SubNodeBlockId);
    if Assigned(TmpBlock) then
      TmpBlockList.Add(TmpBlock)
    else
      LogInfo(Format('!SubNode Root BlockID $%x not found!', [TmpBlock.BlockId]));
  end;

  // get next block
  iCurBlock := 0;
  while iCurBlock < TmpBlockList.Count do
  begin
    TmpBlock := TmpBlockList.GetItem(iCurBlock);
    if not ReadBlockData(TmpBlock, BlockData) then
    begin
      LogInfo(Format('!Node $%x SubNode Block part $%x not readed!', [ANode.NodeId, TmpBlock.BlockID]));
      TmpBlockList.Delete(iCurBlock);
      Continue;
    end;
    rdr.Init(BlockData[0]);

    // read data
    BlockType := rdr.ReadUInt8;  // 0x02
    Assert(BlockType = $2);
    BlockLevel := rdr.ReadUInt8;
    Assert(BlockLevel < 2);
    EntryCount := rdr.ReadUInt16;
    if FIsUnicode then
      rdr.ReadInt32;  // padding
    for ii := 0 to EntryCount - 1 do
    begin
      if BlockLevel = 0 then // SLENTRY (leaf block entry)
      begin
        if FIsUnicode then
        begin
          iSubNodeID := rdr.ReadInt64;
          iDataBlockID := rdr.ReadInt64;
          iSubNodeBlockID := rdr.ReadInt64;
        end
        else
        begin
          iSubNodeID := rdr.ReadUInt32;
          iDataBlockID := rdr.ReadUInt32;
          iSubNodeBlockID := rdr.ReadUInt32;
        end;
        SubNode := TPstNode.Create();
        SubNode.BlockList.ParentList := BlockList;
        SubNode.NodeId := (iSubNodeID and $FFFFFFFF);  // NodeID is 32-bit
        SubNode.BlockId := iDataBlockID;
        SubNode.SubNodeBlockId := iSubNodeBlockID;
        ANode.SubNodeList.Add(SubNode);
      end
      else if BlockLevel = 0 then // SIENTRY (intermediate block entry)
      begin
        if FIsUnicode then
        begin
          iSubNodeID := rdr.ReadInt64;
          iDataBlockID := rdr.ReadInt64;
        end
        else
        begin
          iSubNodeID := rdr.ReadUInt32;
          iDataBlockID := rdr.ReadUInt32;
        end;
        TmpBlock := BlockList.GetByBlockID(iDataBlockID);
        if Assigned(TmpBlock) then
          TmpBlockList.Add(TmpBlock)
        else
          LogInfo(Format('!SubNode leaf BlockID $%x not found!', [TmpBlock.BlockId]));
      end;
    end;
    // SubBlock not needed anymore
    TmpBlockList.Delete(iCurBlock);
  end;
end;

function TFSReaderPst.ReadSubNodeData(ASubNode: TPstNode; var AData: AnsiString): Boolean;
var
  TmpBlock, NewBlock: TPstBlock;
  BlockData: TBytes;
  rdr: TRawDataReader;
  i, ii, iDataSize, EntryCount: Integer;
  iBlockID: Int64;
  BlockType, BlockLevel: Byte;
begin
  Result := False;
  AData := '';
  // check for XBLOCK in DATA
  if ASubNode.BlockList.Count = 1 then
  begin
    TmpBlock := ASubNode.BlockList.GetItem(0);
    if (TmpBlock.ByteCount < 1000) and (TmpBlock.ByteCount > 8) then
    begin
      if ReadBlockData(TmpBlock, BlockData) then
      begin
        rdr.Init(BlockData[0]);
        BlockType := rdr.ReadUInt8;    // 0x01
        BlockLevel := rdr.ReadUInt8;
        EntryCount := rdr.ReadUInt16;
        rdr.ReadInt32; // total size
        if (BlockType = $01) and (BlockLevel < 3) then
        begin
          for ii := 0 to EntryCount - 1 do
          begin
            if FIsUnicode then
              iBlockID := rdr.ReadInt64
            else
              iBlockID := rdr.ReadUInt32;

            NewBlock := BlockList.GetByBlockID(iBlockID);
            if Assigned(NewBlock) then
            begin
              ASubNode.BlockList.Add(NewBlock);
            end
            else
            begin
              LogInfo(Format('!SubNode $%x Block part $%x not found!', [ASubNode.NodeId, iBlockID]));
              //Exit;
            end;
          end;
          ASubNode.BlockList.Delete(0);
        end;
      end;
    end
  end;

  i := 0;
  if i < ASubNode.BlockList.Count then
    TmpBlock := ASubNode.BlockList.GetItem(i)
  else
    TmpBlock := BlockList.GetByBlockID(ASubNode.BlockId);

  while Assigned(TmpBlock) do
  begin
    iDataSize := TmpBlock.ByteCount;
    if (TmpBlock.ByteIndex <= 0) or (TmpBlock.ByteCount = 0) then
    begin
      LogInfo(Format('!SubNode $%x Block $%x empty!', [ASubNode.NodeId, TmpBlock.BlockId]));
      Exit;
    end;
    // read data
    if not IsInternalBlockID(TmpBlock.BlockId) then
    //if iDataSize > 1000 then
    begin
      if ReadBlockData(TmpBlock, BlockData, True) then
      begin
        rdr.Init(BlockData[0]);
        AData := AData + rdr.ReadBytes(iDataSize);
        Result := True;
      end;
    end
    else  // XBLOCK
    begin
      ReadBlockData(TmpBlock, BlockData);
      Assert(False, 'Subnode data tree!');
    end;
    TmpBlock := nil;
    // next block
    Inc(i);
    if i < ASubNode.BlockList.Count then
      TmpBlock := ASubNode.BlockList.GetItem(i);
  end;

end;

procedure TFSReaderPst.ReadTable(AName: string; ACount: Int64; AList: TDbRowsList);
var
  i, ii: Integer;
  TmpNode: TPstNode;
  TmpRow: TDbRowItem;
begin
  if not FIsMetadataReaded then Exit;

  // prepare fielda defs
  AssignMainListFieldDefs(AList);
  AList.TableName := 'Messages';

  for i := 0 to NodeList.Count - 1 do
  begin
    TmpNode := NodeList.GetItem(i);
    if not TmpNode.IsMessage then
      Continue;

    TmpRow := TDbRowItem.Create(AList);
    for ii := 0 to TmpNode.BlockList.Count - 1 do
    begin
      if ReadBlockHeap(TmpNode, TmpNode.BlockList.GetItem(ii), TmpRow) then
      begin
        AList.Add(TmpRow);
        TmpRow := nil;
        Continue;
      end;
    end;

    if Assigned(TmpRow) then
      FreeAndNil(TmpRow);
  end;
end;

{ TPstBlockList }

function TPstBlockList.AddBlock(ABlockId, AKey, AByteIndex: Int64; AByteCount,
  ARefCount: Word): TPstBlock;
begin
  Result := GetByBlockID(ABlockId);
  if not Assigned(Result) then
  begin
    Result := TPstBlock.Create();
    Result.BlockId := ABlockId;
    Add(Result);
  end;
  Result.btKey := AKey;
  Result.ByteIndex := AByteIndex;
  Result.ByteCount := AByteCount;
  Result.RefCount := ARefCount;
end;

function TPstBlockList.AddXBlock(ABlockId, AByteIndex: Int64; AByteCount: Word): TPstBlock;
begin
  Result := AddBlock(ABlockId, 0, AByteIndex, AByteCount, 0);
end;

function TPstBlockList.GetByBlockID(ABlockID: Int64): TPstBlock;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := GetItem(i);
    if Result.BlockId = ABlockID then
      Exit;
  end;
  Result := nil;
end;

function TPstBlockList.GetItem(AIndex: Integer): TPstBlock;
begin
  Result := TPstBlock(Get(AIndex));
end;

procedure TPstBlockList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if (Action = lnDeleted) and (not Assigned(ParentList)) then
    TPstBlock(Ptr).Free;
end;

{ TPstNodeList }

function TPstNodeList.GetByBlockID(ABlockID: Int64): TPstNode;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := GetItem(i);
    if Result.BlockId = ABlockID then
      Exit;
  end;
  Result := nil;
end;

function TPstNodeList.GetByNodeID(ANodeID: Int64): TPstNode;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := GetItem(i);
    if Result.NodeId = ANodeID then
      Exit;
  end;
  Result := nil;
end;

function TPstNodeList.GetItem(AIndex: Integer): TPstNode;
begin
  Result := TPstNode(Get(AIndex));
end;

procedure TPstNodeList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Action = lnDeleted then
    TPstNode(Ptr).Free;
end;

{ TPstHeapID }

function TPstHeapID.GetType: Byte;
begin
  // 5 bit
  //Result := Byte(hid shr 27);
  Result := Byte(hid and $1F);
end;

function TPstHeapID.GetIndex: Word;
begin
  // 11 bit
  //Result := Word((hid shr 16) and $7FFF);
  Result := Word((hid shr 5)) and $07FF;
end;

function TPstHeapID.GetBlock: Word;
begin
  // 16 bit
  Result := Word(hid shr 16);
end;

function TPstHeapID.GetDebugStr: string;
begin
  if GetType = 0 then
    Result := Format('[%x.%x.%x]', [GetType, GetIndex, GetBlock])
  else
    Result := Format('[Node_$%x]', [hid]);
end;

{ TPstPropRec }

function TPstPropRec.AsDebugStr: string;
var
  sData, sHid: string;
begin
  // property type
  case (PropType and PROP_TYPE_MULTI_MASK) of
    PROP_TYPE_NONE:         Result := 'None'; // 0
    PROP_TYPE_NULL:         Result := 'Null'; // 0
    PROP_TYPE_OBJECT:       Result := 'COM_Obj'; // COM object
    PROP_TYPE_INT16:        Result := 'Int16';
    PROP_TYPE_INT32:        Result := 'Int32';
    PROP_TYPE_FLOAT32:      Result := 'Single';
    PROP_TYPE_FLOAT64:      Result := 'Double';
    PROP_TYPE_CURRENCY:     Result := 'Currency';
    PROP_TYPE_FLOAT_TIME:   Result := 'TDateTime';
    PROP_TYPE_ERROR_CODE:   Result := 'ErrCode';
    PROP_TYPE_BOOLEAN:      Result := 'Bool';
    PROP_TYPE_INT64:        Result := 'Int64';
    PROP_TYPE_STRING:       Result := 'WideStr'; // ? Null-terminated wide string UTF-16LE
    PROP_TYPE_STRING8:      Result := 'Str'; // ? Null-terminated byte string
    PROP_TYPE_TIME:         Result := 'Timestamp'; // 8 Int64 100-nanosec since 1601-01-01
    PROP_TYPE_GUID:         Result := 'GUID'; // 16 GUID
    PROP_TYPE_SERVER_ID:    Result := 'ServerId'; // ? Int16 Count + data
    PROP_TYPE_RESTRICTION:  Result := 'Restriction'; // ? byte array
    PROP_TYPE_RULE_ACTION:  Result := 'RuleAction'; // ? Int16 Count + data
    PROP_TYPE_BINARY:       Result := 'Binary'; // ? Count * Byte
  else
    Result := 'Unknown_$' + IntToHex(PropType, 4);
  end;
  // data
  sData := ValueAsStr();

  sHid := '';
  case (PropType and PROP_TYPE_MULTI_MASK) of
    PROP_TYPE_OBJECT,
    PROP_TYPE_FLOAT64,
    PROP_TYPE_CURRENCY,
    PROP_TYPE_FLOAT_TIME,
    PROP_TYPE_INT64,
    PROP_TYPE_STRING,
    PROP_TYPE_STRING8,
    PROP_TYPE_TIME,
    PROP_TYPE_GUID,
    PROP_TYPE_SERVER_ID,
    PROP_TYPE_RESTRICTION,
    PROP_TYPE_RULE_ACTION,
    PROP_TYPE_BINARY:
      sHid := ValueHnid.GetDebugStr();
  end;

  // multiple values
  if (PropType and PROP_TYPE_MULTI_FLAG) > 0 then
    Result := 'Multi_' + Result;

  Result := Format('id=%.4x %s%s', [PropId, Result, sHid]);
  if sData <> '' then
    Result := Result + '=' + sData;
end;

function TPstPropRec.ValueAsStr(): string;
var
  iLen: Integer;
  rdr: TRawDataReader;
  i64: Int64;
  dt: TDateTime;
begin
  Result := '';
  if Length(Data) >= 8 then
    rdr.Init(Data[1]);
  case (PropType and PROP_TYPE_MULTI_MASK) of
    PROP_TYPE_INT16:        Result := IntToStr(ValueHnid.hid);
    PROP_TYPE_INT32:        Result := IntToStr(ValueHnid.hid);
    //PROP_TYPE_FLOAT32:      sData := 'Single';
    //PROP_TYPE_FLOAT64:      sData := 'Double';
    //PROP_TYPE_CURRENCY:     sData := 'Currency';
    //PROP_TYPE_FLOAT_TIME:   sData := 'TDateTime';
    PROP_TYPE_ERROR_CODE:   Result := IntToStr(ValueHnid.hid);
    PROP_TYPE_BOOLEAN:      Result := IntToStr(ValueHnid.hid);
    //PROP_TYPE_INT64:        sData := 'Int64';
    PROP_TYPE_STRING:       Result := WideDataToStr(Data);
    PROP_TYPE_STRING8:      Result := Data; // ? Null-terminated byte string
    PROP_TYPE_TIME:         // 8 Int64 100-nanosec since 1601-01-01
    begin
      if Length(Data) >= 8 then
      begin
        i64 := rdr.ReadInt64;
        i64 := i64 div 10000;  // msecs
        dt := EncodeDate(1601, 1, 1) + (i64 div MSecsPerDay); // epoch + days
        dt := dt + (1 - ((i64 mod MSecsPerDay) / MSecsPerDay));
        //Result := IntToStr(i64);
        Result := FormatDateTime('YYYY-MM-DD HH:NN:SS', dt);
      end;
    end;
    //PROP_TYPE_GUID:         sData := 'GUID'; // 16 GUID
    //PROP_TYPE_SERVER_ID:    sData := 'ServerId'; // ? Int16 Count + data
    //PROP_TYPE_RESTRICTION:  sData := 'Restriction'; // ? byte array
    //PROP_TYPE_RULE_ACTION:  sData := 'RuleAction'; // ? Int16 Count + data
    PROP_TYPE_BINARY:                               // ? Count * Byte
    begin
      iLen := Length(Data);
      if iLen > 80 then
        iLen := 80;
      if Data <> '' then
        Result := DataAsStr(Data[1], iLen);
    end;
  end;

end;

function TPstPropRec.IsDataOnValue: Boolean;
begin
  case (PropType and PROP_TYPE_MULTI_MASK) of
    PROP_TYPE_NONE,
    PROP_TYPE_NULL,
    PROP_TYPE_INT16,
    PROP_TYPE_INT32,
    PROP_TYPE_FLOAT32,
    PROP_TYPE_ERROR_CODE,
    PROP_TYPE_BOOLEAN: Result := True;
  else
    Result := False;
  end;
end;

{ TPstNode }

procedure TPstNode.AfterConstruction;
begin
  inherited;
  FBlockList := TPstBlockList.Create();
  FSubNodeList := TPstNodeList.Create();
end;

procedure TPstNode.BeforeDestruction;
begin
  FreeAndNil(FSubNodeList);
  FreeAndNil(FBlockList);
  inherited;
end;

end.
