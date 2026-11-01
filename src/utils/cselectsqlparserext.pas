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
  FromText, Part, TableName, AliasName, UpperPart: string;
  Parts, Tokens: TStringList;
  i, p: Integer;
begin
  FAliasMap.Clear;
  FAliasMap.NameValueSeparator := '=';   // wichtig
  if FromClause = '' then Exit;

  FromText := Trim(FromClause);
  FromText := StringReplace(FromText, #13#10, ' ', [rfReplaceAll]);
  FromText := StringReplace(FromText, #10, ' ', [rfReplaceAll]);
  FromText := StringReplace(FromText, #13, ' ', [rfReplaceAll]);

  // JOINs in Kommas umwandeln
  FromText := StringReplace(FromText, ' LEFT OUTER JOIN ', ',', [rfReplaceAll, rfIgnoreCase]);
  FromText := StringReplace(FromText, ' RIGHT OUTER JOIN ', ',', [rfReplaceAll, rfIgnoreCase]);
  FromText := StringReplace(FromText, ' FULL OUTER JOIN ', ',', [rfReplaceAll, rfIgnoreCase]);
  FromText := StringReplace(FromText, ' LEFT JOIN ', ',', [rfReplaceAll, rfIgnoreCase]);
  FromText := StringReplace(FromText, ' RIGHT JOIN ', ',', [rfReplaceAll, rfIgnoreCase]);
  FromText := StringReplace(FromText, ' INNER JOIN ', ',', [rfReplaceAll, rfIgnoreCase]);
  FromText := StringReplace(FromText, ' CROSS JOIN ', ',', [rfReplaceAll, rfIgnoreCase]);
  FromText := StringReplace(FromText, ' JOIN ', ',', [rfReplaceAll, rfIgnoreCase]);

  Parts := TStringList.Create;
  Tokens := TStringList.Create;
  try
    ExtractStrings([','], [' '], PChar(FromText), Parts);

    for i := 0 to Parts.Count - 1 do
    begin
      Part := Trim(Parts[i]);
      if Part = '' then Continue;

      // ON / USING abschneiden
      UpperPart := UpperCase(Part);
      p := Pos(' ON ', UpperPart);
      if p > 0 then Part := Trim(Copy(Part, 1, p - 1));

      UpperPart := UpperCase(Part);
      p := Pos(' USING ', UpperPart);
      if p > 0 then Part := Trim(Copy(Part, 1, p - 1));

      if Part = '' then Continue;
      UpperPart := UpperCase(Part);

      // AS-Variante
      p := Pos(' AS ', UpperPart);
      if p > 0 then
      begin
        TableName := Trim(Copy(Part, 1, p - 1));
        AliasName := Trim(Copy(Part, p + 4, MaxInt));
      end
      else
      begin
        Tokens.Clear;
        ExtractStrings([' '], [], PChar(Part), Tokens);
        if Tokens.Count = 0 then Continue;

        TableName := Tokens[0];
        if Tokens.Count >= 2 then
          AliasName := Tokens[1]
        else
          AliasName := TableName;
      end;

      // Nur erstes Wort behalten
      if Pos(' ', TableName) > 0 then
        TableName := Copy(TableName, 1, Pos(' ', TableName) - 1);
      if Pos(' ', AliasName) > 0 then
        AliasName := Copy(AliasName, 1, Pos(' ', AliasName) - 1);

      if (AliasName <> '') and (TableName <> '') then
        FAliasMap.Values[AliasName] := TableName;   // saubere Speicherung
    end;

  finally
    Parts.Free;
    Tokens.Free;
  end;
end;

function TSelectSQLParserExt.ResolveAlias(const AliasName: string): string;
begin
  Result := AliasName;

  if (FAliasMap = nil) or (FAliasMap.Count = 0) then
    Exit;

  // Zuerst über Values suchen
  if FAliasMap.IndexOfName(AliasName) >= 0 then
    Result := FAliasMap.Values[AliasName]
  else if FAliasMap.IndexOf(AliasName) >= 0 then
    Result := AliasName; // Fallback
end;

end.

