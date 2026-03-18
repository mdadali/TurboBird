unit FSReaderMtf;

(*
Microsoft Tape File reader

Author: Sergey Bodrov, 2024 Minsk
License: MIT

https://en.wikipedia.org/wiki/Microsoft_Tape_Format
http://laytongraphics.com/mtf/MTF_100a.PDF

*)


interface

uses
  SysUtils, Classes, FSReaderBase;

type
  TMtfAddr = packed record
    Size: Word;
    Offs: Word;
  end;

  TFSReaderMtf = class(TFSReader)
  private
    FBlockPos: Int64;
    FBlockSize: Integer;
    FStrType: Byte;
    FDatasetPos: Int64;

    // Mdf export
    FMQDANum: Integer;
    FMdfNum: Integer;
    FMdfPos: Int64;
    FMdfSize: Int64;
    FMdfName: string;

    // Align position in file by 4 byte
    procedure FilePosAlign();
    // if Result = True, set FBlockPos to next block
    function ReadStream(): Boolean;
    function ReadString(const AAddr: TMtfAddr): string;

    procedure DumpToFile(APos, ASize: Int64; AFileName: string);

  public
    IsSaveStreams: Boolean;

    function OpenFile(AFileName: string; AStream: TStream = nil): Boolean; override;

    property MdfPos: Int64 read FMdfPos;
    property MdfSize: Int64 read FMdfSize;
    property MdfName: string read FMdfName;
  end;

implementation

type
  TMtfBlkType = array [0..3] of AnsiChar;
  TMtfDateTime = array [0..4] of Byte;
  TMtfChunkHead = packed record
    ChunkType: TMtfBlkType;  //
    ChunkSize: LongWord;     //
  end;

  { Common Block Header }
  TMtfBlockHeader = packed record
    BlockType: TMtfBlkType;  // DBLK Type
    BlockAttr: LongWord;     // Block Attributes
    DataOffs: Word;          // Offset To First Event
    OsId: Byte;              // OS ID
    OsVer: Byte;             // OS Version
    DispSize: UInt64;        // Displayable Size
    FmtLogAddr: UInt64;      // Format Logical Address
    Reserv28: UInt64;        // Reserved
    CtrlBlockId: LongWord;   // Control Block ID
    Reserv36: LongWord;      // Reserved
    OsDataAddr: TMtfAddr;    // OS Specific Data addr
    StrType: Byte;           // String Type
    Reserv49: Byte;          // Reserved
    CRC16: Word;             // Header Checksum
  end;

  {  }
  TMtfTapeHeader = packed record
    BlockHeader: TMtfBlockHeader; // Common Block Header
    MediaFamilyId: LongWord;     // Media Family ID
    TapeAttrs: LongWord;         // TAPE Attributes
    MediaSeqNum: Word;           // Media Sequence Number
    PasswEncript: Word;          // Password Encryption Algorithm
    SfmBlockSize: Word;          // Soft Filemark Block Size
    MediaCatType: Word;          // Media Based Catalog Type
    MediaNameAddr: TMtfAddr;     // Media Name
    MediaLabelAddr: TMtfAddr;    // Media Description/Media Label
    MediaPasswAddr: TMtfAddr;    // Media Password
    SoftwareNameAddr: TMtfAddr;  // Software Name
    FmtSize: Word;               // Format Logical Block Size
    VendorId: Word;              // Software Vendor ID
    MediaDate: TMtfDateTime;     // Media Date
    MtfMajorVer: Byte;           // MTF Major Version
  end;

  { Start of Data Set Descriptor Block header }
  TMtfSsetHeader = packed record
    BlockHeader: TMtfBlockHeader; // Common Block Header
    SsetAttrs: LongWord;         // SSET Attributes
    PasswEncript: Word;          // Password Encryption Algorithm
    Compress: Word;              // Software Compression Algorithm
    VendorId: Word;              // Software Vendor ID
    Number: Word;                // Data Set Number
    NameAddr: TMtfAddr;          // Data Set Name
    DescrAddr: TMtfAddr;         // Data Set Description
    PasswAddr: TMtfAddr;         // Data Set Password
    UserNameAddr: TMtfAddr;      // User Name
    PhysBlockAddr: UInt64;       // Physical Block Address (PBA)
    MediaDate: TMtfDateTime;     // Media Write Date
    MajorVer: Byte;              // Software Major Version
    MinorVer: Byte;              // Software Minor Version
    TimeZone: Shortint;          // Time Zone
    MtfMinorVer: Byte;           // MTF Minor Version
    MediaCatalogVer: Byte;       // Media Catalog Version
  end;

  { Volume Descriptor Block header }
  TMtfVolbHeader = packed record
    BlockHeader: TMtfBlockHeader; // Common Block Header
    Attrs: LongWord;              // VOLB Attributes
    DeviceNameAddr: TMtfAddr;     // Device Name
    VolumeNameAddr: TMtfAddr;     // Volume Name
    MachineNameAddr: TMtfAddr;    // Machine Name
    MediaDate: TMtfDateTime;      // Media Write Date
  end;

  { Directory Descriptor Block }
  TMtfDirbHeader = packed record
    BlockHeader: TMtfBlockHeader; // Common Block Header
    Attrs: LongWord;              // DIRB Attributes
    LastModifDate: TMtfDateTime;  // Last Modification Date
    CreatDate: TMtfDateTime;      // Creation Date
    BackupDate: TMtfDateTime;     // Backup Date
    LastAccessDate: TMtfDateTime; // Last Access Date
    DirId: LongWord;              // Directory ID
    DirNameAddr: TMtfAddr;        // Directory Name
  end;

  { File Descriptor Block }
  TMtfFileHeader = packed record
    BlockHeader: TMtfBlockHeader; // Common Block Header
    Attrs: LongWord;              // FILE Attributes
    LastModifDate: TMtfDateTime;  // Last Modification Date
    CreatDate: TMtfDateTime;      // Creation Date
    BackupDate: TMtfDateTime;     // Backup Date
    LastAccessDate: TMtfDateTime; // Last Access Date
    DirId: LongWord;              // Directory ID
    FileId: LongWord;             // File ID
    FileNameAddr: TMtfAddr;       // File Name
  end;

  TMtfSfmbHeader = packed record
    BlockHeader: TMtfBlockHeader; // Common Block Header
    EntryCount: LongWord;         // Number of Filemark Entries
    EntryUsed: LongWord;          // Filemark Entries Used
    PbaArray: LongWord;           // PBA of Previous Filemarks Array (FmtSize - 60)
  end;

  { Stream Header }
  TMtfStreamHeader = packed record
    StreamId: TMtfBlkType;
    FilesysAttrs: Word;
    FormatAttrs: Word;
    StreamLen: Int64;
    Encrypt: Word;
    Compress: Word;
    CRC16: Word;
  end;

  TMtfSfinHeader = packed record
    ChunkType: TMtfBlkType;  //
    ChunkSize: LongWord;     //
    Unknown8: LongWord;
    Unknown12: LongWord;
    Unknown16: LongWord;
    DataNum: LongWord;
    Unknown24: LongWord;
    DataNum2: LongWord;
    Unknown32: array [32..47] of Byte;
    NameAddr: TMtfAddr;
    FileNameAddr: TMtfAddr;
    Unknown102: array [55..248] of Byte;
    FileSize: Int64;
  end;

const
  MTF_TAPE = 'TAPE';      // TAPE descriptor block
  MTF_SSET = 'SSET';      // Start of data SET descriptor block
  MTF_VOLB = 'VOLB';      // VOLume descriptor Block
  MTF_DIRB = 'DIRB';      // DIRectory descriptor Block
  MTF_FILE = 'FILE';      // FILE descriptor block
  MTF_CFIL = 'CFIL';      // Corrupt object descriptor block
  MTF_ESPB = 'ESPB';      // End of Set Pad descriptor Block
  MTF_ESET = 'ESET';      // End of SET descriptor block
  MTF_EOTM = 'EOTM';      // End Of Tape Marker descriptor block
  MTF_SFMB = 'SFMB';      // Soft FileMark descriptor Block
  // guessed
  MTF_MSCI = 'MSCI';      // MS sql Content Index descriptor block
  MTF_MSDA = 'MSDA';      // MS sql DAta files descriptor block
  MTF_MSTL = 'MSTL';      // MS sql Transaction Log descriptor block

  MTF_STREAM_STAN = 'STAN';  // Standard, non-specific file data stream.
  MTF_STREAM_PNAM = 'PNAM';  // Directory name in stream.
  MTF_STREAM_FNAM = 'FNAM';  // Supports extended length file names
  MTF_STREAM_CSUM = 'CSUM';  // Checksum of previous stream data
  MTF_STREAM_CRPT = 'CRPT';  // Previous stream was corrupt
  MTF_STREAM_SPAD = 'SPAD';  // Pad to next DBLK stream
  MTF_STREAM_SPAR = 'SPAR';  // Sparse data
  //MTF_STREAM_TSMP = 'TSMP';  // LMO set map stream
  //MTF_STREAM_TFDD = 'TFDD';  // LMO FDD stream
  MTF_STREAM_MQCI = 'MQCI';  // MS SQL backup info
  MTF_STREAM_MQDA = 'MQDA';  // MS SQL backup data
  //'OTCP'

  MTF_STR_ANSI = 1;          // single byte ANSI
  MTF_STR_UNICODE = 2;       // two byte Unicode

{ TFSReaderMtf }

procedure TFSReaderMtf.DumpToFile(APos, ASize: Int64; AFileName: string);
var
  PrevPos: Int64;
  FileMode: Word;
  fs: TFileStream;
begin
  PrevPos := FFile.Position;
  FileMode := fmCreate;
  if FileExists(AFileName) then
    FileMode := fmOpenWrite or fmShareDenyNone;

  FFile.Position := APos;
  fs := TFileStream.Create(AFileName, FileMode);
  try
    if ASize > 0 then
      fs.CopyFrom(FFile, ASize);
    fs.Size := ASize;
  finally
    fs.Free();
  end;
  FFile.Position := PrevPos;
end;

procedure TFSReaderMtf.FilePosAlign;
var
  nPos: Int64;
begin
  nPos := FFile.Position;
  if (nPos mod 4) > 0 then
    FFile.Position := nPos + (4-(nPos mod 4));
end;

function TFSReaderMtf.OpenFile(AFileName: string; AStream: TStream): Boolean;
var
  hdr: TMtfBlockHeader;
  hdr_tape: TMtfTapeHeader;
  hdr_sfmb: TMtfSfmbHeader;
  hdr_sset: TMtfSsetHeader;
  hdr_volb: TMtfVolbHeader;
  hdr_dirb: TMtfDirbHeader;
  hdr_file: TMtfFileHeader;
  hdr_stream: TMtfStreamHeader;
  NextBlockPos: Int64;
  IsEndOfBlock: Boolean;
begin
  Result := inherited OpenFile(AFileName, AStream);
  if not Result then Exit;
  
  Result := False;
  FMQDANum := 0;
  FMdfNum := 0;
  FMdfPos := 0;
  FMdfSize := 0;
  FMdfName := '';

  FBlockPos := 0;
  LogInfo(Format('==== File %s  Size=%d', [FileName, FFile.Size]));

  // check file is TAPE
  FFile.Position := FBlockPos;
  FFile.Read(hdr, SizeOf(hdr));
  if hdr.BlockType <> MTF_TAPE then
  begin
    if hdr.BlockType = 'MSSQ' then
      LogInfo(Format('! Compressed MSSQLBAK not supported!', []))
    else
      LogInfo(Format('! Not TAPE format!', []));
    Exit;
  end;

  while FBlockPos + SizeOf(hdr) < FFile.Size do
  begin
    FFile.Position := FBlockPos;
    FFile.Read(hdr, SizeOf(hdr));
    LogInfo(Format('== Block %s DataOffs=%d FLA=%d DispSize=%d', [hdr.BlockType, hdr.DataOffs, hdr.FmtLogAddr, hdr.DispSize]));
    //if hdr.OsDataAddr.Size > 0 then
    //  LogMsg(Format('OsData=%s', [ReadString(hdr.OsDataAddr)]));
    FStrType := hdr.StrType;
    FFile.Position := FBlockPos;
    NextBlockPos := FBlockPos + FBlockSize;
    IsEndOfBlock := False;

    if hdr.BlockType = MTF_TAPE then
    begin
      // decode Media Header
      FFile.Read(hdr_tape, SizeOf(hdr_tape));
      FBlockSize := hdr_tape.FmtSize;
      NextBlockPos := FBlockPos + FBlockSize;
      LogInfo(Format('MediaName=%s', [ReadString(hdr_tape.MediaNameAddr)]));
      LogInfo(Format('MediaLabel=%s', [ReadString(hdr_tape.MediaLabelAddr)]));
      LogInfo(Format('MediaPassw=%s', [ReadString(hdr_tape.MediaPasswAddr)]));
      LogInfo(Format('SoftwareName=%s', [ReadString(hdr_tape.SoftwareNameAddr)]));
      LogInfo(Format('Format Logical Block Size: %d', [hdr_tape.FmtSize]));
      // skip header data
      FFile.Position := FBlockPos + hdr.DataOffs;
      while not IsEndOfBlock do
        IsEndOfBlock := ReadStream();
      NextBlockPos := FFile.Position;
    end
    else
    if hdr.BlockType = MTF_SFMB then
    begin
      // Soft Filemark Descriptor Block
      FFile.Read(hdr_sfmb, SizeOf(hdr_sfmb));
      LogInfo(Format('Number of Filemark Entries: %d', [hdr_sfmb.EntryCount]));
      LogInfo(Format('Filemark Entries Used: %d', [hdr_sfmb.EntryUsed]));
      // skip header data
      FFile.Position := FBlockPos + hdr.DataOffs;
      IsEndOfBlock := FFile.Position >= NextBlockPos;
      while not IsEndOfBlock do
        IsEndOfBlock := ReadStream();
      NextBlockPos := FFile.Position;
    end
    else
    if hdr.BlockType = MTF_SSET then
    begin
      // dataset
      FDatasetPos := FBlockPos;
      FFile.Read(hdr_sset, SizeOf(hdr_sset));
      LogInfo(Format('Dataset Number: %d', [hdr_sset.Number]));
      LogInfo(Format('DatasetName=%s', [ReadString(hdr_sset.NameAddr)]));
      LogInfo(Format('DatasetDescription=%s', [ReadString(hdr_sset.DescrAddr)]));
      LogInfo(Format('DatasetPassword=%s', [ReadString(hdr_sset.PasswAddr)]));
      LogInfo(Format('UserName=%s', [ReadString(hdr_sset.UserNameAddr)]));
      LogInfo(Format('PhysBlockAddr (PBA): %d', [hdr_sset.PhysBlockAddr]));
      // skip header data
      FFile.Position := FBlockPos + hdr.DataOffs;
      IsEndOfBlock := FFile.Position >= NextBlockPos;
      while not IsEndOfBlock do
        IsEndOfBlock := ReadStream();
      NextBlockPos := FFile.Position;
    end
    else
    if hdr.BlockType = MTF_VOLB then
    begin
      // Volume Descriptor Block
      FFile.Read(hdr_volb, SizeOf(hdr_volb));
      LogInfo(Format('DeviceName=%s', [ReadString(hdr_volb.DeviceNameAddr)]));
      LogInfo(Format('VolumeName=%s', [ReadString(hdr_volb.VolumeNameAddr)]));
      LogInfo(Format('MachineName=%s', [ReadString(hdr_volb.MachineNameAddr)]));
      // skip header data
      FFile.Position := FBlockPos + hdr.DataOffs;
      IsEndOfBlock := FFile.Position >= NextBlockPos;
      while not IsEndOfBlock do
        IsEndOfBlock := ReadStream();
      NextBlockPos := FFile.Position;
    end
    else
    if hdr.BlockType = MTF_DIRB then
    begin
      // Directory Descriptor Block
      FFile.Read(hdr_dirb, SizeOf(hdr_dirb));
      LogInfo('Dir ID: ' + IntToStr(hdr_dirb.DirId));
      LogInfo('Dir name: ' + ReadString(hdr_dirb.DirNameAddr));
      // skip header data
      FFile.Position := FBlockPos + hdr.DataOffs;
      IsEndOfBlock := FFile.Position >= NextBlockPos;
      while not IsEndOfBlock do
        IsEndOfBlock := ReadStream();
      NextBlockPos := FFile.Position;
    end
    else
    if hdr.BlockType = MTF_FILE then
    begin
      // File Descriptor Block
      FFile.Read(hdr_file, SizeOf(hdr_file));
      LogInfo('Dir ID: ' + IntToStr(hdr_file.DirId));
      LogInfo('File ID: ' + IntToStr(hdr_file.FileId));
      LogInfo('File name: ' + ReadString(hdr_file.FileNameAddr));

      // skip header data
      FFile.Position := FBlockPos + hdr.DataOffs;
      IsEndOfBlock := FFile.Position >= NextBlockPos;
      while not IsEndOfBlock do
        IsEndOfBlock := ReadStream();
      NextBlockPos := FFile.Position;
    end
    else
    begin
      // unknown block (or stream?)
      // skip header data
      if hdr.DataOffs > 0 then
        FFile.Position := FBlockPos + hdr.DataOffs
      else
      begin
        LogInfo('Wrong header size: ' + IntToStr(hdr.DataOffs));
        Exit;
      end;
      IsEndOfBlock := FFile.Position >= NextBlockPos;
      while not IsEndOfBlock do
        IsEndOfBlock := ReadStream();
      NextBlockPos := FFile.Position;
    end;

    if NextBlockPos = 0 then
      Inc(FBlockPos, FBlockSize)
    else
      FBlockPos := NextBlockPos;
    if FBlockPos > $FFFFFFFF then
      Exit;
  end;

  Result := True;
end;

function TFSReaderMtf.ReadStream: Boolean;
var
  hdr_stream: TMtfStreamHeader;
  nPos, nPosChunk, nPosChunkEnd, CurBlockPos: Int64;
  s: string;
  hdr_chunk: TMtfChunkHead;
  hdr_sfin: TMtfSfinHeader;
begin
  Result := False;
  //FFile.Seek(4, soFromCurrent);
  nPos := FFile.Position;
  if nPos + SizeOf(hdr_stream) > FFile.Size then
  begin
    FFile.Position := FFile.Size;
    Result := True;
    Exit;
  end;
  
  FFile.Read(hdr_stream, SizeOf(hdr_stream));

  // check for block instead of stream
  if (hdr_stream.StreamId = MTF_TAPE)
  or (hdr_stream.StreamId = MTF_SSET)
  or (hdr_stream.StreamId = MTF_VOLB)
  or (hdr_stream.StreamId = MTF_DIRB)
  or (hdr_stream.StreamId = MTF_FILE)
  or (hdr_stream.StreamId = MTF_CFIL)
  or (hdr_stream.StreamId = MTF_ESPB)
  or (hdr_stream.StreamId = MTF_ESET)
  or (hdr_stream.StreamId = MTF_EOTM)
  or (hdr_stream.StreamId = MTF_SFMB)
  then
  begin
    // rewind back to block start
    FFile.Position := nPos;
    Result := True;
    Exit;
  end;

  nPos := FFile.Position + hdr_stream.StreamLen;
  if (nPos mod 4) > 0 then
    nPos := nPos + (4-(nPos mod 4));

  if hdr_stream.StreamId = MTF_STREAM_SPAD then
  begin
    // last stream in block
    LogInfo('Stream SPAD size=' + IntToStr(hdr_stream.StreamLen));
    Result := True;
  end
  else
  if hdr_stream.StreamId = MTF_STREAM_STAN then
  begin
    LogInfo('Stream STAN size=' + IntToStr(hdr_stream.StreamLen));
  end
  else
  if hdr_stream.StreamId = MTF_STREAM_MQCI then
  begin
    LogInfo('Stream MQCI size=' + IntToStr(hdr_stream.StreamLen));
    nPosChunk := FFile.Position;
    nPosChunkEnd := nPosChunk + hdr_stream.StreamLen;
    if hdr_stream.StreamLen > 0 then
    begin
      // align to 4 bytes
      //FilePosAlign();
      //DumpToFile(FFile.Position, hdr_stream.StreamLen, 'MQCI_' + IntToStr(FFile.Position) + '.data');
    end;
    // read chunks
    while nPosChunk < nPosChunkEnd do
    begin
      FFile.Position := nPosChunk;
      FFile.Read(hdr_chunk, SizeOf(hdr_chunk));
      s := hdr_chunk.ChunkType;
      LogInfo(Format('Chunk %s size=%d', [s, hdr_chunk.ChunkSize]));
      // dump chunk
      //{$define MTF_DUMP_CHUNK}
      {$ifdef MTF_DUMP_CHUNK}
      DumpToFile(nPosChunk, hdr_chunk.ChunkSize, Format('%s_%d.data', [s, nPosChunk]));
      {$endif}

      // SFIN chunk
      if hdr_chunk.ChunkType = 'SFIN' then
      begin
        CurBlockPos := FBlockPos;
        FBlockPos := nPosChunk;
        FFile.Position := nPosChunk;
        FFile.Read(hdr_sfin, SizeOf(hdr_sfin));
        LogInfo('Name: ' + ReadString(hdr_sfin.NameAddr));
        s := ReadString(hdr_sfin.FileNameAddr);
        LogInfo('File name: ' + s);
        LogInfo('File size: ' + IntToStr(hdr_sfin.FileSize));
        LogInfo('File number: ' + IntToStr(hdr_sfin.DataNum));
        FBlockPos := CurBlockPos;

        // only first file
        if (LowerCase(ExtractFileExt(s)) = '.mdf') and (FMdfName = '') then
        begin
          FMdfNum := hdr_sfin.DataNum;
          FMdfSize := hdr_sfin.FileSize;
          FMdfName := ExtractFileName(s);
        end;
      end;

      // next chunk
      nPosChunk := nPosChunk + 0 + hdr_chunk.ChunkSize;
    end;
  end
  else
  if hdr_stream.StreamId = MTF_STREAM_MQDA then
  begin
    Inc(FMQDANum);
    LogInfo('Stream MQDA size=' + IntToStr(hdr_stream.StreamLen));
    if hdr_stream.StreamLen > 0 then
    begin
      // align to 4 bytes
      FilePosAlign();
      if IsSaveStreams then
        DumpToFile(FFile.Position, hdr_stream.StreamLen, Format('MQDA_%d.data', [FFile.Position]));
      if (FMQDANum = FMdfNum) and (FMdfSize > 0) and (FMdfName <> '') then
      begin
        FMdfPos := FFile.Position;
        if hdr_stream.StreamLen > FMdfSize then
          FMdfSize := hdr_stream.StreamLen;

        //DumpToFile(FMdfPos, FMdfSize, FMdfName);
        LogInfo('Inner file: ' + FMdfName);
      end;
    end;
  end
  else
  begin
    LogInfo(Format('Stream %s size=%d', [hdr_stream.StreamId, hdr_stream.StreamLen]));
    if hdr_stream.StreamLen <= 0 then
      raise Exception.Create('Wrong stream size: ' + IntToStr(hdr_stream.StreamLen));
  end;
  //FFile.Seek(hdr_stream.StreamLen, soFromCurrent);
  FFile.Position := nPos;
end;

function TFSReaderMtf.ReadString(const AAddr: TMtfAddr): string;
var
  nPos: Int64;
  ws: WideString;
begin
  // position inside block
  nPos := FBlockPos + AAddr.Offs;
  if AAddr.Size > 0 then
  begin
    if FStrType = MTF_STR_ANSI then
    begin
      SetLength(Result, AAddr.Size);
      FFile.Position := nPos;
      FFile.Read(Result[1], AAddr.Size);
    end
    else
    if FStrType = MTF_STR_UNICODE then
    begin
      SetLength(ws, AAddr.Size);
      FFile.Position := nPos;
      FFile.Read(ws[1], AAddr.Size);
      Result := ws;
      SetLength(Result, Length(ws) div 2);
    end;
  end
  else
    Result := '';
end;

end.
