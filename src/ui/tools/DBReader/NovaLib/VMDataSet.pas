{ Virtual memory DataSet, readed from TStream
  for using with DatasetToStream() from DBFunc.pas

 (C) Sergey Bodrov (serbod@gmail.com)
 MIT license }

unit VMDataSet;

interface
uses
  SysUtils, Variants, Classes, DB, SnapBaseDataset, SnapVirtualDataset, MemStreams;

type

  { TVirtualMemDataset }

  TVirtualMemDataset = class(TSnapCustomVirtualDataset)
  private
    FStream: TStream;
    FColCount: LongInt;
    FRowCount: LongInt;
    FFirstRowPos: Cardinal; // position of first row data beginning
    FRowPos: Cardinal;      // position of current row data beginning
    FRowIndex: Integer;    // index number of current row
    FStreamPartsList: TList;
    procedure ReadHeader();
    procedure GotoRow(ARowNum: Integer);
    procedure ReadInt(out Value: LongInt);
    procedure ReadStr(out Value: AnsiString);
    procedure SetStream(Value: TStream);
  protected
    procedure DoDataCount(var Count: LongInt); override;
    procedure DoSetMasterField(Field: TField); override;
    function GetFieldValue(Field: TField): Variant; override;
    procedure GetBlobField(Field: TField; Stream: TStream); override;
    function GetCanModify: Boolean; override;
  public
    property Stream: TStream read FStream write SetStream;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure AssignStream(AStream: TStream);
    // return True if readed end of table
    function ReadFromStream(AStream: TStream; var AFinished: Boolean): Boolean;
  end;

implementation

{ TVirtualMemDataset }

procedure TVirtualMemDataset.ReadInt(out Value: LongInt);
begin
  FStream.ReadBuffer(Value, SizeOf(Value));
end;

procedure TVirtualMemDataset.ReadStr(out Value: AnsiString);
var
  StrLen: LongInt;
begin
  ReadInt(StrLen);
  if StrLen > 0 then
  begin
    SetLength(Value, StrLen);
    FStream.ReadBuffer(PAnsiChar(Value)^, StrLen);
  end
  else
    SetLength(Value, 0);
end;

procedure TVirtualMemDataset.DoDataCount(var Count: LongInt);
begin
  //inherited;
  if FRowCount <= 0 then
  begin
    ReadHeader();
    GotoRow(MaxInt);
  end;
  Count := FRowCount;
end;

procedure TVirtualMemDataset.DoSetMasterField(Field: TField);
begin
  //
end;

function TVirtualMemDataset.GetCanModify: Boolean;
begin
  Result := False;
end;

function TVirtualMemDataset.GetFieldValue(Field: TField): Variant;
var
  i, n: Integer;
  s: AnsiString;
begin
  GotoRow(FCurrent);
  for i := 1 to FColCount do
  begin
    if Field.FieldNo = i then
    begin
      ReadStr(s);
      case Field.DataType of
        ftInteger: Result := StrToIntDef(s, 0);
        ftFloat: Result := StrToFloatDef(s, 0);
        ftDateTime: Result := StrToDateTimeDef(s, 0);
        ftString: Result := s;
        ftMemo: Result := s;
      else
        Result := s;
      end;
      Exit;
    end
    else
    begin
      // read string fake
      ReadInt(n);
      FStream.Position := FStream.Position + n;
    end;
  end;
  Result := '';
end;

procedure TVirtualMemDataset.GetBlobField(Field: TField; Stream: TStream);
var
  i, n: Integer;
  s: AnsiString;
begin
  GotoRow(FCurrent);
  for i := 1 to FColCount do
  begin
    if Field.FieldNo = i then
    begin
      ReadStr(s);
      Stream.Position := 0;
      Stream.Write(PAnsiChar(s)^, Length(s));
      Stream.Position := 0;
      Exit;
    end
    else
    begin
      // read string fake
      ReadInt(n);
      FStream.Position := FStream.Position + n;
    end;
  end;
  //inherited GetBlobField(Field, Stream);
end;

procedure TVirtualMemDataset.GotoRow(ARowNum: Integer);
var
  i, n: Integer;
begin
  if FRowIndex > ARowNum then
  begin
    FRowIndex := 0;
    FRowPos := FFirstRowPos;
  end;

  FStream.Position := FRowPos;
  if FRowIndex = ARowNum then Exit;

  while (FStream.Position < FStream.Size) and (FRowIndex < ARowNum) do
  begin
    //if (n mod 1000) = 0 then Status('Read records: '+IntToStr(n));
    for i:=0 to FColCount-1 do
    begin
      // read string fake
      ReadInt(n);
      FStream.Position := FStream.Position + n;
    end;
    Inc(FRowIndex);
  end;
  FRowPos := FStream.Position;
  if FRowCount < FRowIndex then
    FRowCount := FRowIndex;
end;

procedure TVirtualMemDataset.ReadHeader();
var
  i: Integer;
  n: LongInt;
  s: string;
begin
  FColCount := 0;
  FRowCount := 0;
  FRowIndex := 0;
  FFirstRowPos := 0;
  FRowPos := 0;

  if not Assigned(Stream) then Exit;
  if Stream.Size < 8 then Exit;
  Stream.Position := 0;

  // Read columns
  Self.FieldDefs.Clear();
  ReadInt(FColCount);
  for i := 0 to FColCount-1 do
  begin
    ReadStr(s);
    if s = '' then
      Continue;
    case s[1] of
      's': Self.FieldDefs.Add(Copy(s, 2, MaxInt), ftString, 255);
      'i': Self.FieldDefs.Add(Copy(s, 2, MaxInt), ftInteger);
      'f': Self.FieldDefs.Add(Copy(s, 2, MaxInt), ftFloat);
      't': Self.FieldDefs.Add(Copy(s, 2, MaxInt), ftDateTime);
      'b': Self.FieldDefs.Add(Copy(s, 2, MaxInt), ftMemo);
    end;
  end;

  // Read row count
  //FRowCount := ReadInt();
  ReadInt(n); // current record number
  ReadInt(n); // max records in part

  FFirstRowPos := FStream.Position;
  FRowPos := FFirstRowPos;
end;

procedure TVirtualMemDataset.SetStream(Value: TStream);
begin
  if Assigned(FStream) and (FStream <> Value) then FreeAndNil(FStream);
  FStream := Value;
  if Assigned(FStream) then ReadHeader();
end;

procedure TVirtualMemDataset.AfterConstruction;
begin
  inherited;
  FStream := TMemStream.Create();
end;

procedure TVirtualMemDataset.BeforeDestruction;
begin
  if Assigned(FStream) then FreeAndNil(FStream);
  inherited;
end;

procedure TVirtualMemDataset.AssignStream(AStream: TStream);
begin
  FStream.Size := AStream.Size;
  FStream.Position := 0;
  AStream.Position := 0;
  FStream.CopyFrom(AStream, AStream.Size);
  ReadHeader();
end;

function TVirtualMemDataset.ReadFromStream(AStream: TStream; var AFinished: Boolean): Boolean;
var
  i, ColCount, StrLen, CurIndex, RecCount, MaxRecCount: LongInt;
  ADataPos, TotalPartsSize: Int64;
  TmpStream: TMemStream;
begin
  Result := False;
  AFinished := False;
  ColCount := 0;
  StrLen := 0;
  CurIndex := 0;
  MaxRecCount := 0;

  if not Assigned(AStream) or (AStream.Size = 0) then Exit;

  FStream.Position := FStream.Size;
  AStream.Position := 0;

  try
    // skip header
    //ColCount := ReadInt();
    Assert((SizeOf(ColCount) <= (AStream.Size-AStream.Position)), 'Size='+IntToStr(AStream.Size)+' Pos='+IntToStr(AStream.Position));
    AStream.ReadBuffer(ColCount, SizeOf(ColCount));
    for i:=0 to ColCount-1 do
    begin
      //StrLen := ReadInt();
      Assert((SizeOf(StrLen) <= (AStream.Size-AStream.Position)), 'Size='+IntToStr(AStream.Size)+' Pos='+IntToStr(AStream.Position));
      AStream.ReadBuffer(StrLen, SizeOf(StrLen));
      //if StrLen > 0 then FStream.ReadBuffer(Result[1], StrLen);
      Assert((StrLen <= (AStream.Size-AStream.Position)), 'Size='+IntToStr(AStream.Size)+' Pos='+IntToStr(AStream.Position));
      AStream.Position := AStream.Position + StrLen;
    end;

    // current record number
    AStream.ReadBuffer(CurIndex, SizeOf(CurIndex));

    // max records in part
    AStream.ReadBuffer(MaxRecCount, SizeOf(MaxRecCount));
  except
    // header error
    Exit;
  end;

  ADataPos := AStream.Position;

  // until stream end
  RecCount := 0;
  while (AStream.Position < AStream.Size) do
  begin
    for i := 0 to ColCount-1 do
    begin
      //StrLen := ReadInt();
      AStream.ReadBuffer(StrLen, SizeOf(StrLen));
      //if StrLen > 0 then FStream.ReadBuffer(Result[1], StrLen);
      AStream.Position := AStream.Position + StrLen;
    end;
    Inc(RecCount);
  end;

  AFinished := RecCount < MaxRecCount;

  if CurIndex <= 1 then
  begin
    // first part
    FStream.Size := 0;
    AStream.Position := 0;
    //AssignStream(AStream);
    if Assigned(FStreamPartsList) then
    begin
      // clear parts list
      for i := 0 to FStreamPartsList.Count-1 do
      begin
        TmpStream := TMemStream(FStreamPartsList[i]);
        TmpStream.Free();
      end;
      FreeAndNil(FStreamPartsList);
    end;
  end
  else
  begin
    // next part
    AStream.Position := ADataPos;
  end;

  if not Assigned(FStreamPartsList) then
    FStreamPartsList := TList.Create();

  // keep parts separately, for decrease memory move operations
  TmpStream := TMemStream.Create();
  TmpStream.CopyFrom(AStream, AStream.Size - AStream.Position);
  FStreamPartsList.Add(TmpStream);

  if AFinished then
  begin
    // final part, merge all parts to main stream
    // get total parts size
    TotalPartsSize := 0;
    for i := 0 to FStreamPartsList.Count-1 do
    begin
      TotalPartsSize := TotalPartsSize + TMemStream(FStreamPartsList[i]).Size;
    end;
    // extend stream to new size
    //MainStreamPos := FStream.Position;
    FStream.Size := TotalPartsSize;
    FStream.Position := 0;

    // merge parts
    for i := 0 to FStreamPartsList.Count-1 do
    begin
      TmpStream := TMemStream(FStreamPartsList[i]);
      TmpStream.Position := 0;
      FStream.CopyFrom(TmpStream, TmpStream.Size);
      TmpStream.Free();
    end;
    FreeAndNil(FStreamPartsList);
    ReadHeader();
  end;

  Result := True;
end;

end.
