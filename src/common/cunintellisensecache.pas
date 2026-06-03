unit cUnIntelliSenseCache;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs,
  cSelectSQLParserExt,
  fsimpleobjextractor;

type
  TFieldMeta = record
    FieldName: string;
    TableName: string;
  end;

  TFieldCacheArray = array of TStringList;

  TUnIntelliSenseCache = class
  private
    FInitialized: Boolean;
    FSimpleObjExtractor: TSimpleObjExtractor;

    FTableCache: TStringList;
    FFieldCacheArray: TFieldCacheArray;
    FFieldsLoaded: array of Boolean;

    // optional: Meta-Daten (nur sinnvoll, wenn alles geladen ist)
    FFieldMetaArray: array of array of TFieldMeta;

    function LoadTableCache: Boolean;
    function LoadFieldCache(Index: Integer): Boolean;
    function LoadCache: Boolean;

    procedure DeleteTableCache;
    procedure DeleteFieldCache;

  public
    constructor Create(ASimpleObjExtractor: TSimpleObjExtractor);
    destructor Destroy; override;

    function RefreshCache: Boolean;

    // Lazy-Load APIs
    function FieldsForTable(const TableName: string): TStringList;
    function FieldsForAlias(const AliasName: string;
      Parser: TSelectSQLParserExt): TStringList;

    // optional / legacy
    procedure BuildFieldMetaArray;

    property TableCache: TStringList read FTableCache;
    property FieldCache: TFieldCacheArray read  FFieldCacheArray;

    property Initialized: Boolean read FInitialized;
  end;

implementation

{ ---------------- Table Cache ---------------- }

function TUnIntelliSenseCache.LoadTableCache: Boolean;
begin
  Result := False;

  DeleteTableCache;
  DeleteFieldCache;

  try
    FSimpleObjExtractor.ExtractTableNames(FTableCache, False, False);

    if FTableCache.Count = 0 then
      exit(true);

    SetLength(FFieldCacheArray, FTableCache.Count);
    SetLength(FFieldsLoaded, FTableCache.Count);

    FillChar(FFieldsLoaded[0],
      Length(FFieldsLoaded) * SizeOf(Boolean), 0);

    Result := True;
  except
    on E: Exception do
    begin
      DeleteTableCache;
      DeleteFieldCache;
      MessageDlg('Error while loading table names',
        E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

{ ---------------- Field Cache (Lazy) ---------------- }

function TUnIntelliSenseCache.LoadFieldCache(Index: Integer): Boolean;
begin
  Result := False;

  if (Index < 0) or (Index >= FTableCache.Count) then Exit;
  if FFieldsLoaded[Index] then Exit(True);

  try
    FFieldCacheArray[Index] := TStringList.Create;

    FSimpleObjExtractor.ExtractCleanTableFields(
      FTableCache[Index],
      FFieldCacheArray[Index],
      False,
      ' '
    );

    // first entry = table name
    FFieldCacheArray[Index].Insert(0, FTableCache[Index]);

    FFieldsLoaded[Index] := True;
    Result := True;
  except
    on E: Exception do
    begin
      FreeAndNil(FFieldCacheArray[Index]);
      MessageDlg(
        'Error loading fields for table ' + FTableCache[Index],
        E.Message, mtError, [mbOK], 0
      );
    end;
  end;
end;

{ ---------------- Load Everything ---------------- }

function TUnIntelliSenseCache.LoadCache: Boolean;
var
  i: Integer;
begin
  Result := False;

  if not LoadTableCache then Exit;

  for i := 0 to FTableCache.Count - 1 do
    if not LoadFieldCache(i) then Exit;

  Result := True;
end;

{ ---------------- Delete ---------------- }

procedure TUnIntelliSenseCache.DeleteTableCache;
begin
  if Assigned(FTableCache) then
    FTableCache.Clear;
end;

procedure TUnIntelliSenseCache.DeleteFieldCache;
var
  i: Integer;
begin
  for i := 0 to High(FFieldCacheArray) do
    FreeAndNil(FFieldCacheArray[i]);

  SetLength(FFieldCacheArray, 0);
  SetLength(FFieldsLoaded, 0);
end;

constructor TUnIntelliSenseCache.Create(
  ASimpleObjExtractor: TSimpleObjExtractor);
begin
  inherited Create;

  FSimpleObjExtractor := ASimpleObjExtractor;
  FTableCache := TStringList.Create;

  if not LoadTableCache then
  begin
    FInitialized := False;
    MessageDlg(
      'Error initializing IntelliSense cache',
      'Table metadata could not be loaded.',
      mtError, [mbOK], 0
    );
    Exit;
  end;

  FInitialized := True;
end;

destructor TUnIntelliSenseCache.Destroy;
begin
  DeleteFieldCache;
  FreeAndNil(FTableCache);
  inherited Destroy;
end;

function TUnIntelliSenseCache.RefreshCache: Boolean;
begin
  Result := LoadCache;
end;

{ ---------------- Lazy Access ---------------- }

function TUnIntelliSenseCache.FieldsForTable(
  const TableName: string): TStringList;
var
  Index: Integer;
begin
  Result := nil;

  Index := FTableCache.IndexOf(TableName);
  if Index < 0 then Exit;

  if not FFieldsLoaded[Index] then
    LoadFieldCache(Index);

  Result := FFieldCacheArray[Index];
end;

function TUnIntelliSenseCache.FieldsForAlias(
  const AliasName: string;
  Parser: TSelectSQLParserExt): TStringList;
var
  TableName: string;
begin
  TableName := Parser.ResolveAlias(AliasName);
  Result := FieldsForTable(TableName);
end;

{ ---------------- Optional Meta ---------------- }

procedure TUnIntelliSenseCache.BuildFieldMetaArray;
var
  i, j: Integer;
begin
  SetLength(FFieldMetaArray, Length(FFieldCacheArray));

  for i := 0 to High(FFieldCacheArray) do
  begin
    if not FFieldsLoaded[i] then Continue;

    SetLength(FFieldMetaArray[i],
      FFieldCacheArray[i].Count - 1);

    for j := 1 to FFieldCacheArray[i].Count - 1 do
    begin
      FFieldMetaArray[i][j - 1].FieldName :=
        FFieldCacheArray[i][j];
      FFieldMetaArray[i][j - 1].TableName :=
        FFieldCacheArray[i][0];
    end;
  end;
end;

end.
