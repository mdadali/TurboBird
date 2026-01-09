unit cSelectSQLParserExt;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, IBSQLParser; // hier IBX-Pascal-Unit für TSelectSQLParser

type
  TSelectSQLParserExt = class(TSelectSQLParser)
  private
    FAliasMap: TStringList;
  public
    constructor CreateFromStrings(SQLLines: TStrings); overload;
    constructor CreateFromString(const SQL: string); overload;
    destructor Destroy; override;
    procedure BuildAliasMap;
    function ResolveAlias(const AliasName: string): string;

    property AliasMap: TStringList read FAliasMap;
  end;

implementation

{ TSelectSQLParserExt }

constructor TSelectSQLParserExt.CreateFromStrings(SQLLines: TStrings); overload;
begin
  inherited Create(SQLLines);
  FAliasMap := TStringList.Create;
end;

constructor TSelectSQLParserExt.CreateFromString(const SQL: string); overload;
begin
  inherited Create(SQL);
  FAliasMap := TStringList.Create;
end;

destructor TSelectSQLParserExt.Destroy;
begin
  FAliasMap.Free;
  inherited Destroy;
end;

procedure TSelectSQLParserExt.BuildAliasMap;
var
  Parts: TStringList;
  i: Integer;
  TableName, AliasName: string;
  Tokens: TStringList;
begin
  FAliasMap.Clear;
  FAliasMap.Sorted := True;
  FAliasMap.Duplicates := dupError;

  Parts := TStringList.Create;
  try
    ExtractStrings([','], [' '], PChar(FromClause), Parts);

    for i := 0 to Parts.Count - 1 do
    begin
      // Prüfen auf "AS"
      if Pos(' AS ', UpperCase(Parts[i])) > 0 then
      begin
        TableName := Trim(Copy(Parts[i], 1, Pos(' AS ', UpperCase(Parts[i])) -1 ));
        AliasName := Trim(Copy(Parts[i], Pos(' AS ', UpperCase(Parts[i])) + 4, Length(Parts[i])));
      end
      else
      begin
        Tokens := TStringList.Create;
        try
          ExtractStrings([' '], [], PChar(Parts[i]), Tokens);
          TableName := Tokens[0];
          if Tokens.Count > 1 then
            AliasName := Tokens[1]
          else
            AliasName := TableName;
        finally
          Tokens.Free;
        end;
      end;

      FAliasMap.AddObject(AliasName, TObject(Pointer(AnsiString(TableName))));
    end;
    //ShowMessage(FAliasMap.Text);
  finally
    Parts.Free;
  end;
end;

function TSelectSQLParserExt.ResolveAlias(const AliasName: string): string;
var
  idx: Integer;
begin
  Result := AliasName; // Default: kein Alias, direkt Tabellenname
  if FAliasMap.Count = 0 then Exit;

  idx := FAliasMap.IndexOf(AliasName);
  if idx >= 0 then
    Result := string(PAnsiChar(FAliasMap.Objects[idx]));
end;

end.

