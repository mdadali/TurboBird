(*

Generic CRC8, CRC16, CRC32 calculation

Usage example:

var
  DataStr: AnsiString = '123456789';
  DataBuf: array[0..255] of Byte;
begin
  Result := GetCRC32(DataStr[1], Length(DataStr));
  Result := GetCRC32(DataBuf, SizeOf(DataBuf));
end;

Sergey Bodrov (serbod@gmail.com)
MIT License
*)
unit CRCUnit;

interface

type
  TCRC8Table = array [Byte] of Byte;
  TCRC16Table = array [Byte] of Word;
  TCRC32Table = array [Byte] of LongWord;

{ Reverse bits order }
function BitFlip(B: Byte): Byte;

{ CRC8 parameters
Name                Poly  Init  Final  Reflect
----------------------------------------------
CRC8_DEFAULT        $07,  $00,  $00,   False
CRC8_SAE_J1850      $1D,  $FF,  $FF,   False
CRC8_SAE_J1850_ZERO $1D,  $00,  $00,   False
CRC8_8H2F           $2F,  $FF,  $FF,   False
CRC8_CDMA2000       $9B,  $FF,  $00,   False
CRC8_DARC           $39,  $00,  $00,   True
CRC8_DVB_S2         $D5,  $00,  $00,   False
CRC8_EBU            $1D,  $FF,  $00,   True
CRC8_ICODE          $1D,  $FD,  $00,   False
CRC8_ITU            $07,  $00,  $55,   False
CRC8_MAXIM          $31,  $00,  $00,   True
CRC8_ROHC           $07,  $FF,  $00,   True
CRC8_WCDMA          $9B,  $00,  $00,   True
}

{ Returns CRC8 }
function GetCRC8(const AData; ADataSize: Integer; APoly: Byte = $07;
  AInit: Byte = $00; AFinal: Byte = $00; AReflect: Boolean = False): Byte;

{ Calculate CRC8 lookup table }
procedure FillCRC8Table(out ATable: TCRC8Table; APoly: Byte = $07; AReflect: Boolean = False);

{ Get CRC8 using pre-calculated lookup table }
function GetCRC8Fast(const AData; ADataSize: Integer; const ATable: TCRC8Table;
  AInit: Byte = $00; AFinal: Byte = $00): Byte;

{ CRC16 parameters:
Name                Poly   Init   Final  Reflect
------------------------------------------------
CRC16_CCIT_ZERO     $1021, $0000, $0000, False
CRC16_ARC           $8005, $0000, $0000, True
CRC16_IBM           $8005, $0000, $0000, True
CRC16_CCITT_AUG     $1021, $1D0F, $0000, False
CRC16_BUYPASS       $8005, $0000, $0000, False
CRC16_CCITT_FALSE   $1021, $FFFF, $0000, False
CRC16_CDMA2000      $C867, $FFFF, $0000, False
CRC16_DDS_110       $8005, $800D, $0000, False
CRC16_DECT_R        $0589, $0000, $0001, False
CRC16_DECT_X        $0589, $0000, $0000, False
CRC16_DNP           $3D65, $0000, $FFFF, True
CRC16_EN_13757      $3D65, $0000, $FFFF, False
CRC16_GENIBUS       $1021, $FFFF, $FFFF, False
CRC16_MAXIM         $8005, $0000, $FFFF, True
CRC16_MCRF4XX       $1021, $FFFF, $0000, True
CRC16_RIELLO        $1021, $B2AA, $0000, True
CRC16_T10_DIF       $8BB7, $0000, $0000, False
CRC16_TELEDISK      $A097, $0000, $0000, False
CRC16_TMS37157      $1021, $89EC, $0000, True
CRC16_USB           $8005, $FFFF, $FFFF, True
CRC16_A             $1021, $C6C6, $0000, True
CRC16_KERMIT        $1021, $0000, $0000, True
CRC16_MODBUS        $8005, $FFFF, $0000, True
CRC16_X_25          $1021, $FFFF, $FFFF, True
CRC16_XMODEM        $1021, $0000, $0000, False
}

{ Returns CRC16 }
function GetCRC16(const AData; ADataSize: Integer; APoly: Word = $1021;
  AInit: Word = $0000; AFinal: Word = $0000; AReflect: Boolean = False): Word;

{ Calculate CRC16 lookup table }
procedure FillCRC16Table(out ATable: TCRC16Table; APoly: Word = $1021; AReflect: Boolean = False);
{ Get CRC16 using pre-calculated lookup table }
function GetCRC16Fast(const AData; ADataSize: Integer; const ATable: TCRC16Table;
  AInit: Word = $0000; AFinal: Word = $0000; AReflect: Boolean = False): Word;

{ CRC32 parameters
Name                Poly       Init       Final      Reflect
------------------------------------------------------------
CRC32               $04C11DB7, $FFFFFFFF, $FFFFFFFF, True
CRC32_BZIP2         $04C11DB7, $FFFFFFFF, $FFFFFFFF, False
CRC32_C             $1EDC6F41, $FFFFFFFF, $FFFFFFFF, True
CRC32_D             $A833982B, $FFFFFFFF, $FFFFFFFF, True
CRC32_MPEG2         $04C11DB7, $FFFFFFFF, $00000000, False
CRC32_POSIX         $04C11DB7, $00000000, $FFFFFFFF, False
CRC32_Q             $814141AB, $00000000, $00000000, False
CRC32_JAMCRC        $04C11DB7, $FFFFFFFF, $00000000, True
CRC32_XFER          $000000AF, $00000000, $00000000, False
}

{ Returns CRC32 }
function GetCRC32(const AData; ADataSize: Integer; APoly: LongWord = $04C11DB7;
  AInitCRC: LongWord = $FFFFFFFF; AFinal: LongWord = $FFFFFFFF; AReflect: Boolean = True): LongWord;

{ Calculate CRC32 lookup table }
procedure CRC32Init(APoly: LongWord = $04C11DB7; AReflect: Boolean = True);
{ Returns CRC32 by IEEE 802.3 standard }
function GetCRC32Fast(const AData; ADataSize: Integer;
  AInitCRC: LongWord = $FFFFFFFF; AFinal: LongWord = $FFFFFFFF; AReflect: Boolean = True): LongWord;

{ CRC32, for STM32 controllers. Attention! Len must be a multiple of 4
  It named Reverse by mistake, in legacy code }
function GetCRC32Reverse(ABufPtr: PLongWord; ALen: Integer; AInitCRC: LongWord = $FFFFFFFF): LongWord;

var
  CRC32Table: TCRC32Table;


implementation


function BitFlip(B: Byte): Byte;
const
  N: array[0..15] of Byte = (0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15);
begin
  Result := N[B div 16] or N[B mod 16] shl 4;
end;

function BitFlipLongWord(A: LongWord): LongWord;
begin
  A := ((A shl 16) and $FFFF0000) or ((A shr 16) and $0000FFFF);
  A := ((A shl 08) and $FF00FF00) or ((A shr 08) and $00FF00FF);
  A := ((A shl 04) and $F0F0F0F0) or ((A shr 04) and $0F0F0F0F);
  A := ((A shl 02) and $CCCCCCCC) or ((A shr 02) and $33333333);
  A := ((A shl 01) and $AAAAAAAA) or ((A shr 01) and $55555555);
  Result := A;
end;

function GetCRC8(const AData; ADataSize: Integer; APoly: Byte; AInit: Byte; AFinal: Byte;
  AReflect: Boolean): Byte;
var
  BufPtr: PByte;
  i: Integer;
begin
  BufPtr := PByte(Addr(AData));
  Result := AInit;
  if AReflect then
  begin
    APoly := BitFlip(APoly);
    while ADataSize > 0 do
    begin
      Dec(ADataSize);
      Result := Result xor BufPtr^;
      Inc(BufPtr);
      for i := 0 to 7 do
      begin
        if (Result and $01) <> 0 then
          Result := ((Result shr 1) xor APoly)
        else
          Result := (Result shr 1);
      end;
    end;
  end
  else
  begin
    while ADataSize > 0 do
    begin
      Dec(ADataSize);
      Result := Result xor BufPtr^;
      Inc(BufPtr);
      for i := 0 to 7 do
      begin
        if (Result and $80) <> 0 then
          Result := (((Result and $7F) shl 1) xor APoly)
        else
          Result := (Result shl 1);
      end;
    end;
  end;
  Result := Result xor AFinal;
end;

procedure FillCRC8Table(out ATable: TCRC8Table; APoly: Byte; AReflect: Boolean);
var
  i, ii, c: Byte;
begin
  if AReflect then
  begin
    APoly := BitFlip(APoly);
    for i := Low(Byte) to High(Byte) do
    begin
      c := i;
      for ii := 1 to 8 do
      begin
        if (c and $1) <> 0 then
          c := (c shr 1) xor APoly
        else
          c := (c shr 1);
      end;
      ATable[i] := c;
    end;
  end
  else
  begin
    for i := Low(Byte) to High(Byte) do
    begin
      c := i;
      for ii := 1 to 8 do
      begin
        if (c and $80) <> 0 then
          c := ((c and $7F) shl 1) xor APoly
        else
          c := (c shl 1);
      end;
      ATable[i] := c;
    end;
  end;
end;

function GetCRC8Fast(const AData; ADataSize: Integer; const ATable: TCRC8Table;
  AInit: Byte; AFinal: Byte): Byte;
var
  BufPtr: PByte;
begin
  BufPtr := PByte(Addr(AData));
  Result := AInit;
  while ADataSize > 0 do
  begin
    Dec(ADataSize);
    Result := ATable[BufPtr^ xor Result];
    Inc(BufPtr);
  end;
  Result := Result xor AFinal;
end;

function GetCRC16(const AData; ADataSize: Integer; APoly, AInit, AFinal: Word;
  AReflect: Boolean): Word;
var
  BufPtr: PByte;
  i: Integer;
begin
  BufPtr := PByte(Addr(AData));
  Result := AInit;
  if AReflect then
  begin
    APoly := BitFlip(Byte(APoly shr 8)) or (Word(BitFlip(Byte(APoly))) shl 8);
    while ADataSize > 0 do
    begin
      Dec(ADataSize);
      Result := Result xor Word(BufPtr^ and $FF);
      Inc(BufPtr);
      for i := 0 to 7 do
      begin
        if (Result and $0001) <> 0 then
          Result := ((Result shr 1) xor APoly)
        else
          Result := (Result shr 1);
      end;
    end;
  end
  else
  begin
    while ADataSize > 0 do
    begin
      Dec(ADataSize);
      Result := Result xor (Word(BufPtr^ and $FF) shl 8);
      Inc(BufPtr);
      for i := 0 to 7 do
      begin
        if (Result and $8000) <> 0 then
          Result := (((Result and $7FFF) shl 1) xor APoly)
        else
          Result := (Result shl 1);
      end;
    end;
  end;
  Result := Result xor AFinal;
end;

procedure FillCRC16Table(out ATable: TCRC16Table; APoly: Word; AReflect: Boolean);
var
  i, ii: Byte;
  c: Word;
begin
  if AReflect then
  begin
    APoly := BitFlip(Byte(APoly shr 8)) or (Word(BitFlip(Byte(APoly))) shl 8);
    for i := Low(Byte) to High(Byte) do
    begin
      c := Word(i);
      for ii := 1 to 8 do
      begin
        if (c and $1) <> 0 then
          c := (c shr 1) xor APoly
        else
          c := (c shr 1);
      end;
      ATable[i] := c;
    end;
  end
  else
  begin
    for i := Low(Byte) to High(Byte) do
    begin
      c := Word(i) shl 8;
      for ii := 1 to 8 do
      begin
        if (c and $8000) <> 0 then
          c := ((c and $7FFF) shl 1) xor APoly
        else
          c := (c shl 1);
      end;
      ATable[i] := c;
    end;
  end;
end;

function GetCRC16Fast(const AData; ADataSize: Integer; const ATable: TCRC16Table;
  AInit: Word; AFinal: Word; AReflect: Boolean): Word;
var
  BufPtr: PByte;
begin
  BufPtr := PByte(Addr(AData));
  Result := AInit;
  if AReflect then
  begin
    while ADataSize > 0 do
    begin
      Dec(ADataSize);
      Result := (Result shr 8) xor ATable[Byte(BufPtr^ xor Result)];
      Inc(BufPtr);
    end;
  end
  else
  begin
    while ADataSize > 0 do
    begin
      Dec(ADataSize);
      Result := (Result shl 8) xor ATable[BufPtr^ xor Byte(Result shr 8)];
      Inc(BufPtr);
    end;
  end;
  Result := Result xor AFinal;
end;

procedure CRC32Init(APoly: LongWord; AReflect: Boolean);
var
  c: LongWord;
  i, ii: integer;
begin
  if AReflect then
  begin
    APoly := BitFlipLongWord(APoly);
    for i := Low(Byte) to High(Byte) do
    begin
      c := i;
      for ii := 1 to 8 do
      begin
        if (c and $1) <> 0 then
          c := (c shr 1) xor APoly
        else
          c := (c shr 1);
      end;
      CRC32Table[i] := c;
    end;
  end
  else
  begin
    for i := Low(Byte) to High(Byte) do
    begin
      c := LongWord(i) shl 24;
      for ii := 1 to 8 do
      begin
        if (c and $80000000) <> 0 then
          c := ((c and $7FFFFFFF) shl 1) xor APoly
        else
          c := (c shl 1);
      end;
      CRC32Table[i] := c;
    end;

  end;
end;

function GetCRC32Fast(const AData; ADataSize: Integer;
  AInitCRC: LongWord; AFinal: LongWord; AReflect: Boolean): LongWord;
var
  BufPtr: PByte;
begin
  BufPtr := PByte(Addr(AData));
  Result := AInitCRC;
  if AReflect then
  begin
    while ADataSize > 0 do
    begin
      Dec(ADataSize);
      Result := (Result shr 8) xor CRC32Table[BufPtr^ xor Byte(Result)];
      Inc(BufPtr);
    end;
  end
  else
  begin
    while ADataSize > 0 do
    begin
      Dec(ADataSize);
      Result := ((Result and $00FFFFFF) shl 8) xor CRC32Table[Byte((Result xor (BufPtr^ shl 24)) shr 24)];
      Inc(BufPtr);
    end;
  end;
  Result := Result xor AFinal;
end;

function GetCRC32(const AData; ADataSize: Integer; APoly: LongWord;
  AInitCRC: LongWord; AFinal: LongWord; AReflect: Boolean = True): LongWord;
var
  BufPtr: PByte;
  i: Integer;
begin
  BufPtr := PByte(Addr(AData));
  Result := AInitCRC;
  if AReflect then
  begin
    APoly := BitFlipLongWord(APoly);
    while ADataSize > 0 do
    begin
      Dec(ADataSize);
      Result := Result xor BufPtr^;
      Inc(BufPtr);
      for i := 1 to 8 do
      begin
        if (Result and $00000001) <> 0 then
          Result := (Result shr 1) xor APoly
        else
          Result := (Result shr 1);
      end;
    end;
  end
  else
  begin
    while ADataSize > 0 do
    begin
      Dec(ADataSize);
      Result := Result xor (BufPtr^ shl 24);
      Inc(BufPtr);
      for i := 1 to 8 do
      begin
        if (Result and $80000000) <> 0 then
          Result := ((Result and $7FFFFFFF) shl 1) xor APoly
        else
          Result := (Result shl 1);
      end;
    end;
  end;
  Result := Result xor AFinal;
end;


function GetCRC32Reverse(ABufPtr: PLongWord; ALen: Integer; AInitCRC: LongWord): LongWord;
var
  i, j: Integer;
begin
  Result := AInitCRC;
  i := 0;
  repeat
    Result := Result xor ABufPtr^;
    Inc(ABufPtr);
    for j := 1 to 32 do
    begin
      if (Result and $80000000) <> 0 then
        Result := (Result shl 1) xor $04C11DB7
      else
        Result := (Result shl 1);
    end;
    Inc(i, 4);
  until i >= ALen;
end;


initialization
  CRC32Init();
end.
