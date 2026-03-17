{-----------------------------------------------------------------------------
Функции работы с БД (базами данных)

(C) Sergey Bodrov (serbod@gmail.com)
MIT license
-----------------------------------------------------------------------------}
unit DBFunc;

interface

uses
  Variants, Classes, DB
  {$ifdef DEBUG}, Logger{$endif}
  ;

// тип данных в строчный символ
function FieldTypeToChar(ADataType: TFieldType): string;
// строчный символ в тип данных
function CharToFieldType(s: string): TFieldType;
// SQL-запрос в стрим
function QueryToStream(ASql: TStrings; AParams: TParams; AStream: TStream): Boolean;
// SQL-запрос из стрима
function QueryFromStream(ASql: TStrings; AParams: TParams; AStream: TStream): Boolean;
// экспорт датасета в стрим
// ACount - количество записей для экспорта за один раз
// ARecNo - номер текущей записи, начиная с 0
// если не достигнут конец, то ADataSet.Active = True
// при повторном запуске идет продолжение экспорта с последней позиции
function DatasetToStream(ADataSet: TDataSet; AStream: TStream; ACount: LongInt; var ARecNo: LongInt): boolean;
// импорт датасета из стрима
// если не достигнут конец, то ADataSet.Active = True
function DatasetFromStream(ADataSet: TDataSet; AStream: TStream): boolean;

implementation

uses
  {Windows,} SysUtils, RFUtils;

{procedure DeleteDuplicates(DBConnection: TADOConnection; const ATableName, AColName: string);
var
  Query: TADOQuery;
  sl: TStringList;
begin

end;}

function FieldTypeToChar(ADataType: TFieldType): string;
begin
  Result := 's';
  case ADataType of
    ftSmallint, ftInteger, ftWord, ftLargeint, ftAutoInc:
      Result := 'i';
    ftFloat, ftCurrency:
      Result := 'f';
    ftDate, ftTime, ftDateTime, ftTimeStamp:
      Result := 't';
    ftMemo, ftGraphic:
      Result := 'b';
  end;
end;

function CharToFieldType(s: string): TFieldType;
begin
  Result := ftString;
  if s = 's' then Result := ftString
  else if s = 'i' then Result := ftInteger
  else if s = 'f' then Result := ftFloat
  else if s = 't' then Result := ftDateTime
  else if s = 'b' then Result := ftMemo;
end;

{
STR SQL text
INT Parameters count
  STR Param name (first char - data type)
  STR Param value
  ...
}
function QueryToStream(ASql: TStrings; AParams: TParams; AStream: TStream): Boolean;
var
  i: Integer;

procedure WriteInt(AValue: Integer);
begin
  AStream.WriteBuffer(AValue, SizeOf(AValue));
end;

procedure WriteStr(AValue: string);
begin
  WriteInt(Length(AValue));
  if Length(AValue) > 0 then
    AStream.WriteBuffer(AValue[1], Length(AValue));
end;

begin
  try
    // SQL
    WriteStr(ASql.Text);
    // Params
    WriteInt(AParams.Count);
    for i:=0 to AParams.Count-1 do
    begin
      WriteStr(FieldTypeToChar(AParams[i].DataType)+AParams[i].Name);
      WriteStr(AParams[i].Value);
    end;
    Result := True;
  except
    {$ifdef DEBUG}
    LogException();
    _LogError('QueryToStream() '+GetExceptionMessage());
    {$endif}
    Result := False;
  end;
end;

function QueryFromStream(ASql: TStrings; AParams: TParams; AStream: TStream): Boolean;
var
  i, ParamCount: Integer;
  TmpParam: TParam;
  s: string;

function ReadInt(): Integer;
begin
  AStream.ReadBuffer(Result, SizeOf(Result));
end;

function ReadStr(): string;
var
  StrLen: Integer;
begin
  StrLen := ReadInt();
  SetLength(Result, StrLen);
  if StrLen > 0 then
    AStream.ReadBuffer(Result[1], StrLen);
end;

begin
  try
    // SQL
    ASql.Text := ReadStr();
    // Parameters
    ParamCount := ReadInt();
    for i := 0 to ParamCount-1 do
    begin
      s := ReadStr();
      if s = '' then
        s := 'sField'+IntToStr(i);
      TmpParam := AParams.CreateParam(CharToFieldType(s[1]), Copy(s, 2, MaxInt), ptUnknown);
      //TmpParam.Name:=Copy(s, 2, MaxInt);
      //TmpParam.DataType:=CharToDataType(s[1]);
      TmpParam.Value := ReadStr();
    end;
    Result := True;
  except
    {$ifdef DEBUG}
    LogException();
    _LogError('QueryFromStream() '+GetExceptionMessage());
    {$endif}
    Result := False;
  end;
end;

{
INT Columns count
  STR Column1 name (first char - data type)
  STR Column2 name (first char - data type)
  ..
INT Records start position number (1..n)
INT Max records in part (0..n)
  STR Field1 content
  STR Field2 content
  ..
  STR Field1 content
  STR Field2 content
  ..
  ..
}
function DatasetToStream(ADataSet: TDataSet; AStream: TStream; ACount: LongInt; var ARecNo: LongInt): boolean;
var
  i, FieldCount: LongInt;
  RecCount: LongInt;
  TmpDateFormat, TmpTimeFormat: string;

procedure WriteInt(AValue: LongInt);
begin
  AStream.Write(AValue, SizeOf(AValue));
end;

procedure WriteStr(const AValue: string);
var
  len: LongInt;
begin
  len := Length(AValue);
  AStream.Write(len, SizeOf(len));
  if len > 0 then
    AStream.Write(AValue[1], len);
end;

begin
  {$ifdef UNICODE}
  TmpDateFormat := FormatSettings.ShortDateFormat;
  TmpTimeFormat := FormatSettings.LongTimeFormat;
  FormatSettings.ShortDateFormat := 'YYYY-MM-DD';
  FormatSettings.LongTimeFormat := 'hh:nn:ss';
  {$else}
  TmpDateFormat := ShortDateFormat;
  TmpTimeFormat := LongTimeFormat;
  ShortDateFormat := 'YYYY-MM-DD';
  LongTimeFormat := 'hh:nn:ss';
  {$endif}
  try
    if not ADataSet.Active then
    begin
      ADataSet.Open();
      //ADataSet.First();  // при открытии оно и так на начале
      ARecNo := 0;
    end;

    // columns
    FieldCount := ADataSet.FieldDefs.Count;
    WriteInt(FieldCount);
    for i:=0 to FieldCount-1 do
    begin
      WriteStr(FieldTypeToChar(ADataSet.FieldDefs[i].DataType) + ADataSet.FieldDefs[i].Name);
    end;

    // records
    RecCount := 0;
    WriteInt(ARecNo); // current record number
    WriteInt(ACount);         // max records in part
    while not ADataSet.Eof do
    begin
      for i := 0 to FieldCount-1 do
      begin
        WriteStr(ADataSet.Fields[i].AsString);
      end;
      Inc(RecCount);
      ADataSet.Next();

      if RecCount >= ACount then
        Break;
    end;
    Inc(ARecNo, RecCount);

    if ADataSet.Eof then
      ADataSet.Close();

    Result := True;
  except
    {$ifdef DEBUG}
    LogException();
    _LogError('DatasetToStream() '+GetExceptionMessage());
    {$endif}
    Result := False;
  end;
  {$ifdef UNICODE}
  FormatSettings.ShortDateFormat := TmpDateFormat;
  FormatSettings.LongTimeFormat := TmpTimeFormat;
  {$else}
  ShortDateFormat := TmpDateFormat;
  LongTimeFormat := TmpTimeFormat;
  {$endif}
end;

function DatasetFromStream(ADataSet: TDataSet; AStream: TStream): Boolean;
var
  ColCount, i: LongInt;
  RowCount, MaxRowCount: LongInt;
  s: string;
  TmpDateFormat, TmpTimeFormat: string;

function ReadInt(): LongInt;
begin
  AStream.ReadBuffer(Result, SizeOf(Result));
end;

function ReadStr(): string;
var
  StrLen: LongInt;
begin
  StrLen := ReadInt();
  SetLength(Result, StrLen);
  if StrLen > 0 then
    AStream.ReadBuffer(Result[1], StrLen);
end;

begin
  {$ifdef UNICODE}
  TmpDateFormat := FormatSettings.ShortDateFormat;
  TmpTimeFormat := FormatSettings.LongTimeFormat;
  FormatSettings.ShortDateFormat := 'YYYY-MM-DD';
  FormatSettings.LongTimeFormat := 'hh:nn:ss';
  {$else}
  TmpDateFormat := ShortDateFormat;
  TmpTimeFormat := LongTimeFormat;
  ShortDateFormat := 'YYYY-MM-DD';
  LongTimeFormat := 'hh:nn:ss';
  {$endif}
  try
    // columns
    if not ADataSet.Active then
      ADataSet.FieldDefs.Clear();

    ColCount := ReadInt();
    for i:=0 to ColCount-1 do
    begin
      s := ReadStr();
      if s = '' then
        Continue;
      if not ADataSet.Active then
      begin
        case s[1] of
          's': ADataSet.FieldDefs.Add(Copy(s, 2, MaxInt), ftString, 255);
          'i': ADataSet.FieldDefs.Add(Copy(s, 2, MaxInt), ftInteger);
          'f': ADataSet.FieldDefs.Add(Copy(s, 2, MaxInt), ftFloat);
          't': ADataSet.FieldDefs.Add(Copy(s, 2, MaxInt), ftDateTime);
          'b': ADataSet.FieldDefs.Add(Copy(s, 2, MaxInt), ftBlob);
        end;
      end;
    end;

    // records
    if not ADataSet.Active then
      ADataSet.Open();
      
    //RowIndex := ReadInt();    // current record number
    ReadInt();
    MaxRowCount := ReadInt(); // max records in part
    RowCount := 0;
    if (AStream.Position < AStream.Size) then
    begin
      // Assert(RowIndex = ADataSet.RecNo, 'RowIndex='+IntToStr(RowIndex)+' ADataSet.RecNo='+IntToStr(ADataSet.RecNo));
      // until stream end
      while (AStream.Position < AStream.Size) do
      begin
        // add record
        ADataSet.Append();
        for i := 0 to ColCount-1 do
        begin
          s := ReadStr();
          ADataSet.Fields[i].AsString := s;
        end;
        ADataSet.Post();
        Inc(RowCount);
      end;
    end;
    if RowCount < MaxRowCount then
      ADataSet.Close();
      
    Result := True;
  except
    {$ifdef DEBUG}
    LogException();
    _LogError('DatasetFromStream() '+GetExceptionMessage());
    {$endif}
    Result := False;
  end;
  {$ifdef UNICODE}
  FormatSettings.ShortDateFormat := TmpDateFormat;
  FormatSettings.LongTimeFormat := TmpTimeFormat;
  {$else}
  ShortDateFormat := TmpDateFormat;
  LongTimeFormat := TmpTimeFormat;
  {$endif}
end;

end.
