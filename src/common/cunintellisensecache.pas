unit cUnIntelliSenseCache;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,  Dialogs,

  fsimpleobjextractor;

type

  TFieldCacheArray = array of TStringList;

  TUnIntelliSenseCache = class
  private
    FInitialized: boolean;
    FSimpleObjExtractor: TSimpleObjExtractor;
    FDBIndex: integer;
    FTableCache: TStringList;
    FFieldCacheArray: TFieldCacheArray;

    function LoadTableCache: boolean;
    function LoadFieldCache: boolean;
    function LoadCache: boolean;
    function DeleteCache: boolean;
    function DeleteTableCache: boolean;
    function DeleteFieldCache: boolean;

  public
    constructor Create(ASimpleObjExtractor: TSimpleObjExtractor);
    destructor  Destroy; override;
    function    RefreshCache: boolean;

    property TableCache: TStringList      read  FTableCache;
    property FieldCache: TFieldCacheArray read  FFieldCacheArray;

    property Initialized: boolean read FInitialized;
  end;

implementation


function TUnIntelliSenseCache.LoadTableCache: boolean;
begin
  Result := False;
  FTableCache.Clear;
  SetLength(FFieldCacheArray, 0);

  try
    FSimpleObjExtractor.ExtractTableNames(FTableCache, False, False);
    SetLength(FFieldCacheArray, FTableCache.Count);
    Result := True;
  except
    on E: Exception do
    begin
      FTableCache.Clear;
      SetLength(FFieldCacheArray, 0);
      MessageDlg('Error while loading table names', E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

function TUnIntelliSenseCache.LoadFieldCache: boolean;
var
  i: Integer;
begin
  Result := False;

  try
    for i := 0 to FTableCache.Count - 1 do
    begin
      FFieldCacheArray[i] := TStringList.Create;

      FSimpleObjExtractor.ExtractCleanTableFields(
        FTableCache[i],
        FFieldCacheArray[i],
        False,
        ' '
      );

      FFieldCacheArray[i].Insert(0, FTableCache[i]);
    end;

    Result := True;
  except
    on E: Exception do
    begin
      // Cleanup to keep cache consistent
      for i := 0 to Length(FFieldCacheArray) - 1 do
      begin
        if Assigned(FFieldCacheArray[i]) then
        begin
          FFieldCacheArray[i].Free;
          FFieldCacheArray[i] := nil;
        end;
      end;

      MessageDlg(
        'Error loading table fields',
        E.Message,
        mtError,
        [mbOK],
        0
      );
    end;
  end;
end;

function TUnIntelliSenseCache.DeleteCache: boolean;
begin
  DeleteFieldCache;
  DeleteTableCache;
end;

function TUnIntelliSenseCache.LoadCache: boolean;
begin
  Result := False;
  try
    DeleteCache;
    if LoadTableCache and LoadFieldCache then
      Result := True;
  except
    Result := False;
  end;
end;

function TUnIntelliSenseCache.DeleteTableCache: boolean;
begin
  FTableCache.Clear;
end;

function TUnIntelliSenseCache.DeleteFieldCache: boolean;
var
  i: Integer;
begin
  Result := False;

  try
    for i := 0 to Length(FFieldCacheArray) - 1 do
    begin
      if Assigned(FFieldCacheArray[i]) then
      begin
        FFieldCacheArray[i].Free;
        FFieldCacheArray[i] := nil;
      end;
    end;

    SetLength(FFieldCacheArray, 0);
    Result := True;
  except
    on E: Exception do
    begin
      MessageDlg(
        'Error clearing field cache',
        E.Message,
        mtError,
        [mbOK],
        0
      );
    end;
  end;
end;

function TUnIntelliSenseCache.RefreshCache: boolean;
begin
  if not LoadCache then
    Exit(False);

  Result := True;
end;

constructor TUnIntelliSenseCache.Create(ASimpleObjExtractor: TSimpleObjExtractor);
begin
  inherited Create;
  FInitialized := true;

  FSimpleObjExtractor := ASimpleObjExtractor;
  FTableCache := TStringList.Create;
  SetLength(FFieldCacheArray, 0);

  if not LoadCache then
  begin
    FInitialized := false;
    MessageDlg(
      'Error initializing IntelliSense cache', 'The cache could not be loaded completely.', mtError, [mbOK], 0);
  end;
end;

destructor  TUnIntelliSenseCache.Destroy;
begin
  DeleteCache;
  inherited;
end;

end.
