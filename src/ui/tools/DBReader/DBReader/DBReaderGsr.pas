unit DBReaderGsr;

(*
Mapsoft GSR file reader (not tested!)

Author: Sergey Bodrov, 2024
License: MIT

Used https://github.com/BrentSherwood/DelphiZLib

GSR is ZIP file, that contain:
_groups.gr
sr5.xml
*.dat
*)

interface

uses
  SysUtils, Classes, Variants, DBReaderBase, DB, RFUtils,
  KaZip, MemStreams, XmlSaxParser;

type
  TGsrTableList = class;

  TGsrTable = class(TDbRowsList)
  private
    FJoinTables: TGsrTableList;
  public
    DataStartPos: Int64;  // position in .dat file
    RowCount: Integer;
    PKFieldName: string;  // primary key field

    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;
    procedure AddFieldDef(AName: string; AType: Integer);

    property JoinTables: TGsrTableList read FJoinTables;
  end;

  TGsrTableList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function GetItem(AIndex: Integer): TGsrTable;
    function GetByName(AName: string): TGsrTable;
  end;

  TDBReaderGsr = class(TDBReader)
  private
    FTableList: TGsrTableList;
    FDbVersion: Integer;
    FIsMetadataReaded: Boolean;        // true after FillTableInfo()
    FZip: TKAZip;
    FDataFile: TStream;
    FXml: TXmlSaxParser;
    FTagPath: TStringList;
    FInTables: Boolean;
    FIgnoreSavemethod: Boolean;
    FLogPrefix: string;

    FCurTable: TGsrTable;
    FParentTable: TGsrTable;
    FCurFieldType: Integer;

    // Read tables description from XML stream
    procedure ReadXmlData(AStream: TStream);
    // read raw metadata
    procedure ReadMetaFile(AStream: TStream);
    // read raw data
    procedure ReadDatFile(AStream: TStream);
    // read table and subtables (recursive)
    procedure ReadGsrTable(AStream: TStream; ATable: TGsrTable; AIndexOnly: Boolean);

    procedure OnFoundTagHandler(NoCaseTag, ActualTag: AnsiString);
    procedure OnFoundTextHandler(AText: AnsiString);
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
    // get tables count
    function GetTablesCount(): Integer; override;
    // get table by index 0..GetTablesCount()-1
    function GetTableByIndex(AIndex: Integer): TDbRowsList; override;

    property TableList: TGsrTableList read FTableList;
  end;

implementation

function Stream2Integer(AStream: TStream): Integer;
begin
  AStream.Read(Result, SizeOf(Result));
end;

function Stream2Double(AStream: TStream): Double;
begin
  AStream.Read(Result, SizeOf(Result));
end;

function Stream2DateTime(AStream: TStream): TDateTime;
begin
  AStream.Read(Result, SizeOf(Result));
end;

function Stream2String(AStream: TStream): string;
var
  L: Integer;
begin
  try
    AStream.Read(L, SizeOf(L));
    if (L > (AStream.Size - AStream.Position)) or (L < 0) then
    begin
      Result := '';
      exit;
      //raise Exception.Create('Ошибка загрузки строки из потока');
    end;
    SetLength(Result, L);
    if L > 0 then
      AStream.Read(Result[1], L);
  except
    raise Exception.Create('Read string from stream error');
  end;
end;

function Stream2Var(AStream: TStream): Variant;
var
  t: Integer;
begin
  AStream.Read(t, SizeOf(t));
  case t of
    varSmallInt,
    varInteger,
    varByte,
    varBoolean:
      Result := Stream2Integer(AStream);
    varSingle,
    varDouble,
    varCurrency:
      Result := Stream2Double(AStream);
    varDate:
      Result := Stream2DateTime(AStream);
    varString,
    varOleStr:
      Result := Stream2String(AStream);
    varNull:
      Result := NULL;
  else
    raise Exception.CreateFmt('Stream2Var: unknown type %x', [t]);
  end;
end;

procedure Stream2VarDummy(AStream: TStream);
var
  t, n: Integer;
begin
  AStream.Read(t, SizeOf(t));
  case t of
    varSmallInt,
    varInteger,
    varByte,
    varBoolean:
      AStream.Seek(SizeOf(Integer), soCurrent);
    varSingle,
    varDouble,
    varCurrency:
      AStream.Seek(SizeOf(Double), soCurrent);
    varDate:
      AStream.Seek(SizeOf(TDateTime), soCurrent);
    varString,
    varOleStr:
    begin
      AStream.Read(n, SizeOf(n));
      AStream.Seek(n, soCurrent);
    end;
    varNull:
  else
    raise Exception.CreateFmt('Stream2Var: unknown type %x', [t]);
  end;
end;

function StripBrackets(const AStr: string): string;
begin
  if (AStr <> '') and (AStr[1] = '[') then
    Result := Copy(AStr, 2, Length(AStr)-2)
  else
    Result := AStr;
end;

{ TDBReaderGsr }

procedure TDBReaderGsr.AfterConstruction;
begin
  inherited;
  FTableList := TGsrTableList.Create();
  FZip := TKAZip.Create(nil);
  FTagPath := TStringList.Create();
end;

procedure TDBReaderGsr.BeforeDestruction;
begin
  FreeAndNil(FDataFile); // optional
  FreeAndNil(FTagPath);
  FreeAndNil(FZip);
  FreeAndNil(FTableList);
  inherited;
end;

function TDBReaderGsr.FillTableInfoText(ATableName: string; ALines: TStrings): Boolean;
var
  i: Integer;
  TmpTable: TGsrTable;
  s: string;
begin
  Result := False;
  TmpTable := FTableList.GetByName(ATableName);
  if not Assigned(TmpTable) then
    Exit;

  ALines.Add(Format('== Table Name=%s  RowCount=%d', [TmpTable.TableName, TmpTable.RowCount]));
  ALines.Add(Format('== Fields  Count=%d', [Length(TmpTable.FieldsDef)]));
  for i := Low(TmpTable.FieldsDef) to High(TmpTable.FieldsDef) do
  begin
    s := Format('%.2d Name=%s  Type=%s', [i, TmpTable.FieldsDef[i].Name, TmpTable.FieldsDef[i].TypeName]);
    ALines.Add(s);
  end;
end;

function TDBReaderGsr.GetTableByIndex(AIndex: Integer): TDbRowsList;
begin
  Result := TableList.GetItem(AIndex);
end;

function TDBReaderGsr.GetTablesCount: Integer;
begin
  Result := TableList.Count;
end;

function TDBReaderGsr.OpenFile(AFileName: string; AStream: TStream): Boolean;
var
  i, n: Integer;
begin
  Result := False;
  if not FileExists(AFileName) then Exit;

  FZip.Open(AFileName);
  try
    // read metadata
    if not Assigned(FDataFile) then
      //FDataFile := TTmpFileStream.Create();
      FDataFile := TMemStream.Create();

    // find meta file
    for i := 0 to FZip.Entries.Count - 1 do
    begin
      if ExtractFileExt(FZip.Entries.Items[i].FileName) = '.meta' then
      begin
        FDataFile.Size := 0;
        FZip.ExtractToStream(FZip.Entries.Items[i], FDataFile);
        ReadMetaFile(FDataFile);
        FIsMetadataReaded := True;
      end;
    end;

    if not FIsMetadataReaded then
    begin
      n := FZip.Entries.IndexOf('sr5.xml');
      if n >= 0 then
      begin
        FDataFile := TTmpFileStream.Create();
        try
          FZip.ExtractToStream(FZip.Entries.Items[n], FDataFile);
          ReadXmlData(FDataFile);
          FIsMetadataReaded := True;
        finally
          FreeAndNil(FDataFile);
        end;
      end;
    end;

    // find data file
    for i := 0 to FZip.Entries.Count - 1 do
    begin
      if ExtractFileExt(FZip.Entries.Items[i].FileName) = '.dat' then
      begin
        FDataFile.Size := 0;
        FZip.ExtractToStream(FZip.Entries.Items[i], FDataFile);
        ReadDatFile(FDataFile);
      end;
    end;

    Result := True;
  finally
    FZip.Close();
  end;
end;

procedure TDBReaderGsr.ReadDatFile(AStream: TStream);
var
  iTable: Integer;
  TmpTable: TGsrTable;
begin
  AStream.Position := 0;
  for iTable := 0 to FTableList.Count - 1 do
  begin
    TmpTable := FTableList.GetItem(iTable);
    TmpTable.DataStartPos := AStream.Position;
    ReadGsrTable(AStream, TmpTable, False);
  end;
end;

procedure TDBReaderGsr.ReadGsrTable(AStream: TStream; ATable: TGsrTable; AIndexOnly: Boolean);
var
  iRow, iJoinTable, iField, iFieldCount: Integer;
  TmpTable: TGsrTable;
  TmpRow: TDbRowItem;
  PrevLogPrefix: string;
begin
  // (int32) RecCount
  // [RecCount][RowCount] (var_type, [var_size]) DataValue
  // [JoinTables] (table)

  try
    if AIndexOnly then
      ATable.DataStartPos := AStream.Position
    else
      AStream.Position := ATable.DataStartPos;

    ATable.RowCount := Stream2Integer(AStream);
    iFieldCount := Length(ATable.FieldsDef);
    for iRow := 0 to ATable.RowCount - 1 do
    begin
      if AIndexOnly then
      begin
        for iField := 0 to iFieldCount - 1 do
          Stream2VarDummy(AStream);
      end
      else
      begin
        TmpRow := TDbRowItem.Create(ATable);
        ATable.Add(TmpRow);
        SetLength(TmpRow.Values, iFieldCount);
        for iField := 0 to iFieldCount - 1 do
        begin
          TmpRow.Values[iField] := Stream2Var(AStream);
        end;
      end;
    end;
    LogInfo(Format(FLogPrefix + 'Table %s (RowCount=%d  ColCount=%d) Pos=%d',
      [ATable.TableName, ATable.RowCount, iFieldCount, AStream.Position]));

    for iJoinTable := 0 to ATable.JoinTables.Count-1 do
    begin
      TmpTable := ATable.JoinTables.GetItem(iJoinTable);
      //LogInfo('Join Table:');
      PrevLogPrefix := FLogPrefix;
      FLogPrefix := FLogPrefix + '  ';
      ReadGsrTable(AStream, TmpTable, True);
      FLogPrefix := PrevLogPrefix;
    end;
  except
    on E: Exception do
      raise Exception.CreateFmt('%s'
       + sLineBreak + 'StreamPos=%d'
       + sLineBreak + 'TableName=%s'
       //+ sLineBreak + 'FieldName=%s  Type=%s)'
       , [E.Message, AStream.Position, ATable.TableName
         {, ATable.FieldsDef[iField].Name, ATable.FieldsDef[iField].TypeName}
         ]);
  end;

end;

procedure TDBReaderGsr.ReadMetaFile(AStream: TStream);
var
  s: string;
  i, n, iCount: Integer;
  TmpTable: TGsrTable;
begin
  // (str) table name
  // (int) field count
  //   (str) field name
  //   (int) field type
  FTableList.Clear();
  AStream.Position := 0;
  while AStream.Size - AStream.Position > 8 do
  begin
    s := Stream2String(AStream);
    iCount := Stream2Integer(AStream);
    TmpTable := TGsrTable.Create();
    TmpTable.TableName := s;
    FTableList.Add(TmpTable);
    for i := 0 to iCount - 1 do
    begin
      s := Stream2String(AStream);
      n := Stream2Integer(AStream);
      TmpTable.AddFieldDef(s, n);
    end;
  end;
end;

procedure TDBReaderGsr.ReadTable(AName: string; ACount: Int64; AList: TDbRowsList);
var
  TmpTable: TGsrTable;
  TmpItem: TDbRowItem;
  i: Integer;
begin
  TmpTable := FTableList.GetByName(AName);
  if Assigned(TmpTable) then
  begin
    // read if needed
    if TmpTable.RowCount <> TmpTable.Count then
      ReadGsrTable(FDataFile, TmpTable, False);

    AList.FieldsDef := TmpTable.FieldsDef;
    if ACount = 0 then
      ACount := TmpTable.Count;
    i := 0;
    while (i < ACount) and (i < TmpTable.Count) do
    begin
      TmpItem := TDbRowItem.Create(AList);
      AList.Add(TmpItem);
      TmpItem.Values := TmpTable.GetItem(i).Values;
      Inc(i);
    end;
  end;
end;

procedure TDBReaderGsr.OnFoundTagHandler(NoCaseTag, ActualTag: AnsiString);
var
  s, sTag: string;
  IsClosing: Boolean;
begin
  // extract tag name
  sTag := ExtractTagName(NoCaseTag);
  IsClosing := IsClosingTag(NoCaseTag);
  if IsClosing then
    FTagPath.Delete(FTagPath.Count-1)
  else if not IsEmptyTag(NoCaseTag) then
    FTagPath.Add(sTag);

  //ExtractTagParamValue()
  if (sTag = 'мап') and (not IsClosing) then
  begin
    s := ExtractTagParamValue(NoCaseTag, 'format');
    if s = '1.04' then
      FDbVersion := 104;
  end
  else
  if sTag = 'tables' then
  begin
    FInTables := not IsClosing;
  end
  else
  if FInTables and (sTag = 'table') then
  begin
    if not IsClosing then
    begin
      if FIgnoreSavemethod then Exit;
      if FTagPath[FTagPath.Count-2] = 'table' then
      begin
        // join table
        FParentTable := FCurTable;
        FCurTable := TGsrTable.Create();
        FParentTable.JoinTables.Add(FCurTable);
      end
      else
      begin
        FCurTable := TGsrTable.Create();
        FTableList.Add(FCurTable);
      end;
    end
    else
    begin
      if Assigned(FParentTable) then
      begin
        FCurTable := FParentTable;
        FParentTable := nil;
      end
      else
      begin
        FCurTable := nil;
        if FTagPath[FTagPath.Count-2] <> 'table' then
          FIgnoreSavemethod := False;
      end;
    end;
  end
  else
  if FInTables and (sTag = 'field') and (not IsClosing) then
  begin
    FCurFieldType := StrToIntDef(ExtractTagParamValue(NoCaseTag, 'type'), 0);
    if ExtractTagParamValue(NoCaseTag, 'fake') <> '' then
      FCurFieldType := 0;
  end
  else
  if FInTables and (sTag = 'savemethod') then
  begin
    // after first savemethod ignore others
    if IsClosing and Assigned(FCurTable) then
      FIgnoreSavemethod := True;
  end;
  {if NoCaseTag = 'name' then
  begin
    sPrevTag := FTagPath[FTagPath.Count-2];
    if sPrevTag = 'table' then
      FCurTable.TableName
    else if sPrevTag = 'field' then
      FCurTable.AddFieldDef(sFieldName, iFieldType);
  end;}
end;

procedure TDBReaderGsr.OnFoundTextHandler(AText: AnsiString);
var
  sTag, sPrevTag: string;
begin
  if FTagPath.Count < 2 then Exit;
  sTag := FTagPath[FTagPath.Count-1];
  sPrevTag := FTagPath[FTagPath.Count-2];
  if FInTables and (sTag = 'name') then
  begin
    if FIgnoreSavemethod then Exit;
    
    if sPrevTag = 'table' then
    begin
      Assert(Assigned(FCurTable));
      FCurTable.TableName := StripBrackets(Trim(AText));
      if FCurTable.TableName = '' then
        Assert(FCurTable.TableName <> '');
    end
    else
    if (sPrevTag = 'field') and Assigned(FCurTable) then
    begin
      FCurTable.AddFieldDef(StripBrackets(AText), FCurFieldType);
    end;
  end;
end;

procedure TDBReaderGsr.ReadXmlData(AStream: TStream);
var
  sData: string;
begin
  FTagPath.Clear();
  FInTables := False;
  FIgnoreSavemethod := False;

  SetLength(sData, AStream.Size);
  AStream.Position := 0;
  AStream.Read(sData[1], Length(sData));
  FXml := TXmlSaxParser.Create(sData);
  FXml.OnFoundTag := OnFoundTagHandler;
  FXml.OnFoundText := OnFoundTextHandler;
  try
    FXml.Exec();
  finally
    FreeAndNil(FXml);
  end;
end;

{
procedure TDBReaderGsr.ReadXmlData(AStream: TStream);
var
  sData: string;
  XmlDoc: IDOMDocument;
  XmlNode: IDOMNode;
  NodeA, NodeT, NodeTChild, NodeSM, NodeSMChild: IDOMNode;
  TmpTable: TGsrTable;
  sFieldName: string;
  iFieldType: Integer;
begin
  SetLength(sData, AStream.Size);
  AStream.Read(sData[1], Length(sData));
  XmlDoc := LoadDocFromString(sData);
  sData := '';

  XmlNode := XmlDoc.firstChild;  // <МАП file="SaveRestore" format="1.04">
  sFieldName := GetAttribute(XmlNode, 'format');
  if sFieldName = '1.04' then
    FDbVersion := 104;

  NodeT := SelectNode(XmlNode, 'tables\table');
  while Assigned(NodeT) do
  begin
    // name
    // groupname
    // savemethod
    //   name
    //   field pkey=bool sync=bool type=int
    //     name
    //     foreigntable
    //   child
    //     childtable
    //     childfield
    //     field

    NodeTChild := NodeT.firstChild;
    while Assigned(NodeTChild) do
    begin
      // table
      TmpTable := TGsrTable.Create();
      FTableList.Add(TmpTable);
      if NodeTChild.nodeName = 'name' then
      begin
        TmpTable.TableName := NodeTChild.nodeValue;
      end
      else
      if NodeTChild.nodeName = 'groupname' then
      else
      if NodeTChild.nodeName = 'savemethod' then
      begin
        NodeSM := NodeTChild;
        while Assigned(NodeSM) do
        begin
          NodeSMChild := NodeSM.firstChild;
          while Assigned(NodeSMChild) do
          begin
            if NodeSMChild.nodeName = 'name' then
            else
            if NodeSMChild.nodeName = 'field' then
            begin
              // fields
              iFieldType := StrToIntDef(GetAttribute(NodeSMChild, 'type'), 0);
              NodeA := NodeSMChild.firstChild;
              if NodeA.nodeName = 'name' then
              begin
                sFieldName := NodeA.nodeValue;
                TmpTable.AddFieldDef(sFieldName, iFieldType);
              end;
            end
            else
            if NodeSMChild.nodeName = 'child' then
            begin

            end;
            NodeSMChild := NodeSMChild.nextSibling;
          end;
          NodeSM := NodeSM.nextSibling;
        end;
      end;
      NodeTChild := NodeTChild.nextSibling;
    end;
    NodeT := NodeT.nextSibling;
  end;
end;
}
{ TGsrTableList }

function TGsrTableList.GetByName(AName: string): TGsrTable;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := GetItem(i);
    if Result.TableName = AName then
      Exit;
  end;
  Result := nil;
end;

function TGsrTableList.GetItem(AIndex: Integer): TGsrTable;
begin
  Result := TGsrTable(Get(AIndex));
end;

procedure TGsrTableList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Action = lnDeleted then
    TGsrTable(Ptr).Free;
end;

{ TGsrTable }

procedure TGsrTable.AddFieldDef(AName: string; AType: Integer);
var
  i: Integer;
begin
  if AType = 0 then Exit;
  i := Length(FieldsDef);
  SetLength(FieldsDef, i+1);
  FieldsDef[i].Name := AName;
  FieldsDef[i].FieldType := DB.TFieldType(AType);
  FieldsDef[i].TypeName := FieldTypeNames[FieldsDef[i].FieldType];
  FieldsDef[i].Size := 0;
end;

procedure TGsrTable.AfterConstruction;
begin
  inherited;
  FJoinTables := TGsrTableList.Create();
end;

procedure TGsrTable.BeforeDestruction;
begin
  FreeAndNil(FJoinTables);
  inherited;
end;

end.
