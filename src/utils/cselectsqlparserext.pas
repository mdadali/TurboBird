unit cSelectSQLParserExt;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IBSQLParser;

type
  TSQLContext = (
    ctxEmpty,
    ctxSelectList,
    ctxFrom,
    ctxWhere,
    ctxOrderGroup,
    ctxUnknown
  );

  TSelectSQLParserExt = class(TSelectSQLParser)
  private
    FAliasMap: TStringList;
    FOriginalSQL: string;
  public
    constructor CreateFromStrings(SQLLines: TStrings); overload;
    constructor CreateFromString(const SQL: string); overload;
    destructor Destroy; override;

    procedure BuildAliasMap;
    function ResolveAlias(const AliasName: string): string;
    function GetContext: TSQLContext;

    property AliasMap: TStringList read FAliasMap;
  end;

  //Helper-intellisense
  function GetLastToken(const S: string): string;
  function IsWhereKeyword(const Token: string): Boolean;
  function ShouldShowWhereOperators(const TextBeforeCaret: string): Boolean;
  function GetWordBeforeCursor(Lines: TStrings; CaretY, CaretX: Integer): string;
  function GetTextBeforeCaret(Lines: TStrings; CaretY, CaretX: Integer): string;
  procedure FillKeywords(AList: TStrings; AContext: TSQLContext; ShowWhereOperators: Boolean = False);
  function GetSQLContext(const SQLText: string): TSQLContext;


implementation

{ TSelectSQLParserExt }
function GetLastToken(const S: string): string;
var
  i, EndPos: Integer;
begin
  Result := '';
  if S = '' then Exit;

  i := Length(S);
  // trailing Spaces überspringen
  while (i >= 1) and (S[i] <= ' ') do
    Dec(i);
  if i < 1 then Exit;

  EndPos := i;

  // Quoted Identifier?
  if S[i] = '"' then
  begin
    Dec(i);
    while (i >= 1) and (S[i] <> '"') do
      Dec(i);
    if (i >= 1) and (S[i] = '"') then
      Result := Copy(S, i, EndPos - i + 1);
    Exit;
  end;

  // normaler Identifier / Token
  while (i >= 1) and (S[i] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '$']) do
    Dec(i);

  Result := Copy(S, i + 1, EndPos - i);
end;

function IsWhereKeyword(const Token: string): Boolean;
var
  U: string;
begin
  U := UpperCase(Token);
  Result :=
    (U = 'WHERE') or (U = 'AND') or (U = 'OR') or (U = 'NOT') or
    (U = 'ON') or (U = 'HAVING') or (U = 'EXISTS') or (U = 'SINGULAR') or
    (U = 'BETWEEN') or (U = 'IN') or (U = 'LIKE') or
    (U = 'CONTAINING') or (U = 'IS') or (U = 'NULL');
end;

function ShouldShowWhereOperators(const TextBeforeCaret: string): Boolean;
var
  Token: string;
begin
  Token := GetLastToken(TextBeforeCaret);
  // Nach Identifier (Feld) → Operatoren
  // Nach WHERE/AND/OR/( → keine Operatoren, sondern Felder
  Result := (Token <> '') and not IsWhereKeyword(Token);
end;

constructor TSelectSQLParserExt.CreateFromString(const SQL: string);
begin
  FOriginalSQL := SQL;
  inherited Create(SQL);
  FAliasMap := TStringList.Create;
  FAliasMap.NameValueSeparator := '=';
  FAliasMap.CaseSensitive := False;
end;

constructor TSelectSQLParserExt.CreateFromStrings(SQLLines: TStrings);
begin
  if Assigned(SQLLines) then
    FOriginalSQL := SQLLines.Text
  else
    FOriginalSQL := '';
  inherited Create(SQLLines);
  FAliasMap := TStringList.Create;
  FAliasMap.NameValueSeparator := '=';
  FAliasMap.CaseSensitive := False;
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
  FromText := Trim(FromClause);
  if FromText = '' then Exit;

  FromText := StringReplace(FromText, #13#10, ' ', [rfReplaceAll]);
  FromText := StringReplace(FromText, #10, ' ', [rfReplaceAll]);
  FromText := StringReplace(FromText, #13, ' ', [rfReplaceAll]);

  FromText := StringReplace(FromText, ' LEFT OUTER JOIN ', ',', [rfReplaceAll, rfIgnoreCase]);
  FromText := StringReplace(FromText, ' RIGHT OUTER JOIN ', ',', [rfReplaceAll, rfIgnoreCase]);
  FromText := StringReplace(FromText, ' FULL OUTER JOIN ', ',', [rfReplaceAll, rfIgnoreCase]);
  FromText := StringReplace(FromText, ' LEFT JOIN ', ',', [rfReplaceAll, rfIgnoreCase]);
  FromText := StringReplace(FromText, ' RIGHT JOIN ', ',', [rfReplaceAll, rfIgnoreCase]);
  FromText := StringReplace(FromText, ' INNER JOIN ', ',', [rfReplaceAll, rfIgnoreCase]);
  FromText := StringReplace(FromText, ' CROSS JOIN ', ',', [rfReplaceAll, rfIgnoreCase]);
  FromText := StringReplace(FromText, ' FULL JOIN ', ',', [rfReplaceAll, rfIgnoreCase]);
  FromText := StringReplace(FromText, ' JOIN ', ',', [rfReplaceAll, rfIgnoreCase]);

  Parts := TStringList.Create;
  Tokens := TStringList.Create;
  try
    ExtractStrings([','], [' '], PChar(FromText), Parts);

    for i := 0 to Parts.Count - 1 do
    begin
      Part := Trim(Parts[i]);
      if Part = '' then Continue;

      UpperPart := UpperCase(Part);
      p := Pos(' ON ', UpperPart);
      if p > 0 then
        Part := Trim(Copy(Part, 1, p - 1));

      UpperPart := UpperCase(Part);
      p := Pos(' USING ', UpperPart);
      if p > 0 then
        Part := Trim(Copy(Part, 1, p - 1));

      if Part = '' then Continue;
      UpperPart := UpperCase(Part);

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

      if Pos(' ', TableName) > 0 then
        TableName := Copy(TableName, 1, Pos(' ', TableName) - 1);
      if Pos(' ', AliasName) > 0 then
        AliasName := Copy(AliasName, 1, Pos(' ', AliasName) - 1);

      if (AliasName <> '') and (TableName <> '') then
        FAliasMap.Values[AliasName] := TableName;
    end;
  finally
    Parts.Free;
    Tokens.Free;
  end;
end;

function TSelectSQLParserExt.ResolveAlias(const AliasName: string): string;
var
  i: Integer;
  SearchName: string;
begin
  Result := AliasName;
  if (FAliasMap = nil) or (FAliasMap.Count = 0) then Exit;

  SearchName := AliasName;

  // Quotes entfernen
  if (Length(SearchName) >= 2) and (SearchName[1] = '"') and
     (SearchName[Length(SearchName)] = '"') then
    SearchName := Copy(SearchName, 2, Length(SearchName) - 2);

  if FAliasMap.IndexOfName(SearchName) >= 0 then
  begin
    Result := FAliasMap.Values[SearchName];
    Exit;
  end;

  for i := 0 to FAliasMap.Count - 1 do
  begin
    if SameText(FAliasMap.Names[i], SearchName) then
    begin
      Result := FAliasMap.ValueFromIndex[i];
      Exit;
    end;
  end;
end;

function TSelectSQLParserExt.GetContext: TSQLContext;
var
  SQL, UpperSQL, Rest: string;
  p: Integer;

  function RPos(const SubStr, S: string): Integer;
  var
    i, LenSub: Integer;
  begin
    Result := 0;
    LenSub := Length(SubStr);
    if (LenSub = 0) or (Length(S) < LenSub) then
      Exit;

    for i := Length(S) - LenSub + 1 downto 1 do
    begin
      if Copy(S, i, LenSub) = SubStr then
      begin
        Result := i;
        Exit;
      end;
    end;
  end;

begin
  Result := ctxUnknown;

  SQL := Trim(FOriginalSQL);
  if SQL = '' then
  begin
    Result := ctxEmpty;
    Exit;
  end;

  if NotaSelectStmt then
  begin
    UpperSQL := UpperCase(SQL);
    if (Pos('SELECT', UpperSQL) = 0) and
       (Pos('INSERT', UpperSQL) = 0) and
       (Pos('UPDATE', UpperSQL) = 0) and
       (Pos('DELETE', UpperSQL) = 0) and
       (Pos('EXECUTE', UpperSQL) = 0) then
      Result := ctxEmpty
    else
      Result := ctxUnknown;
    Exit;
  end;

  UpperSQL := UpperCase(SQL);

  // Direkt nach WHERE (Cursor am Ende, Klausel noch leer)
  p := RPos(' WHERE ', UpperSQL);
  if p > 0 then
  begin
    Rest := Trim(Copy(SQL, p + 7, MaxInt));
    if Rest = '' then
    begin
      Result := ctxWhere;
      Exit;
    end;
  end;

  // ORDER BY / GROUP BY vorhanden → eher Order/Group-Kontext
  if (Pos(' ORDER BY ', UpperSQL) > 0) or (Pos(' GROUP BY ', UpperSQL) > 0) then
  begin
    Result := ctxOrderGroup;
    Exit;
  end;

  // Über die geparsten Klauseln
  if (Trim(OrderByClause) <> '') or (Trim(GroupClause) <> '') then
    Result := ctxOrderGroup
  else if (Trim(WhereClause) <> '') or (Trim(HavingClause) <> '') then
    Result := ctxWhere
  else if Trim(FromClause) <> '' then
    Result := ctxFrom
  else if Trim(SelectClause) <> '' then
    Result := ctxSelectList
  else
    Result := ctxEmpty;
end;

//Helpers-Intellisense//////////////////////////////////////////////////////////////////
function GetWordBeforeCursor(Lines: TStrings; CaretY, CaretX: Integer): string;
var
  Line: string;
  X, StartX: Integer;
begin
  Result := '';

  if (Lines = nil) or (CaretY < 1) or (CaretY > Lines.Count) then
    Exit;

  Line := Lines[CaretY - 1];
  X := CaretX;

  // Hinter dem Punkt → ein Zeichen zurück
  if (X > 1) and (X <= Length(Line) + 1) and (Line[X - 1] = '.') then
    Dec(X);

  if X <= 1 then Exit;

  // Quoted Identifier: "Applications".
  if Line[X - 1] = '"' then
  begin
    StartX := X - 2;
    while (StartX >= 1) and (Line[StartX] <> '"') do
      Dec(StartX);

    if (StartX >= 1) and (Line[StartX] = '"') then
      Result := Copy(Line, StartX, X - StartX);
    Exit;
  end;

  // Normaler Identifier
  StartX := X;
  while (StartX > 1) and
        (Line[StartX - 1] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '$']) do
    Dec(StartX);

  if StartX < X then
    Result := Copy(Line, StartX, X - StartX);
end;

function GetTextBeforeCaret(Lines: TStrings; CaretY, CaretX: Integer): string;
var
  i: Integer;
  Line: string;
begin
  Result := '';

  if (Lines = nil) or (CaretY < 1) then
    Exit;

  for i := 0 to CaretY - 2 do
  begin
    if i < Lines.Count then
      Result := Result + Lines[i] + LineEnding;
  end;

  if CaretY <= Lines.Count then
  begin
    Line := Lines[CaretY - 1];
    if CaretX > 1 then
    begin
      if CaretX - 1 > Length(Line) then
        Result := Result + Line
      else
        Result := Result + Copy(Line, 1, CaretX - 1);
    end;
  end;
end;

procedure FillKeywords(AList: TStrings; AContext: TSQLContext;
  ShowWhereOperators: Boolean = False);
begin
  AList.Clear;

  case AContext of
    ctxEmpty:
    begin
      AList.Add('SELECT ');
      AList.Add('SELECT * FROM ');
      AList.Add('SELECT COUNT(*) FROM ');
      AList.Add('SELECT FIRST 1000 * FROM ');
      AList.Add('INSERT INTO ');
      AList.Add('UPDATE ');
      AList.Add('DELETE FROM ');
      AList.Add('EXECUTE PROCEDURE ');
      AList.Add('EXECUTE BLOCK ');
      AList.Add('CREATE ');
      AList.Add('ALTER ');
      AList.Add('DROP ');
      AList.Add('RECREATE ');
      AList.Add('SET ');
      AList.Add('COMMIT');
      AList.Add('ROLLBACK');
    end;

    ctxSelectList:
    begin
      AList.Add('*');
      AList.Add('DISTINCT ');
      AList.Add('ALL ');
      AList.Add('FROM ');
      AList.Add('COUNT(');
      AList.Add('SUM(');
      AList.Add('AVG(');
      AList.Add('MIN(');
      AList.Add('MAX(');
      AList.Add('LIST(');
      AList.Add('COALESCE(');
      AList.Add('NULLIF(');
      AList.Add('CAST(');
      AList.Add('EXTRACT(');
      AList.Add('GEN_ID(');
      AList.Add('IIF(');
      AList.Add('TRIM(');
      AList.Add('UPPER(');
      AList.Add('LOWER(');
      AList.Add('SUBSTRING(');
      AList.Add('CURRENT_DATE');
      AList.Add('CURRENT_TIME');
      AList.Add('CURRENT_TIMESTAMP');
      AList.Add('CASE ');
      AList.Add('WHEN ');
      AList.Add('THEN ');
      AList.Add('ELSE ');
      AList.Add('END');
      AList.Add('AS ');
    end;

    ctxFrom:
    begin
      AList.Add('JOIN ');
      AList.Add('LEFT JOIN ');
      AList.Add('RIGHT JOIN ');
      AList.Add('INNER JOIN ');
      AList.Add('CROSS JOIN ');
      AList.Add('FULL JOIN ');
      AList.Add('ON ');
      AList.Add('AS ');
      AList.Add('WHERE ');
      AList.Add('GROUP BY ');
      AList.Add('ORDER BY ');
      AList.Add('ROWS ');
      AList.Add('UNION ');
      AList.Add('UNION ALL ');
    end;

    ctxWhere:
    begin
      if ShowWhereOperators then
      begin
        // nach Feldnamen
        AList.Add('= ');
        AList.Add('<> ');
        AList.Add('< ');
        AList.Add('> ');
        AList.Add('<= ');
        AList.Add('>= ');
        AList.Add('IS NULL');
        AList.Add('IS NOT NULL');
        AList.Add('BETWEEN ');
        AList.Add('IN (');
        AList.Add('LIKE ');
        AList.Add('CONTAINING ');
        AList.Add('STARTING WITH ');
        AList.Add('SIMILAR TO ');
        AList.Add('AND ');
        AList.Add('OR ');
      end
      else
      begin
        // direkt nach WHERE / AND / OR
        AList.Add('NOT ');
        AList.Add('EXISTS (');
        AList.Add('SINGULAR (');
      end;
    end;

    ctxOrderGroup:
    begin
      AList.Add('ASC');
      AList.Add('DESC');
      AList.Add('NULLS FIRST');
      AList.Add('NULLS LAST');
      AList.Add('HAVING ');
      AList.Add('ORDER BY ');
      AList.Add('GROUP BY ');
      AList.Add('ROWS ');
      AList.Add('UNION ');
      AList.Add('UNION ALL ');
    end;

    ctxUnknown:
    begin
      AList.Add('SELECT ');
      AList.Add('FROM ');
      AList.Add('WHERE ');
      AList.Add('JOIN ');
      AList.Add('ORDER BY ');
      AList.Add('GROUP BY ');
      AList.Add('AND ');
      AList.Add('OR ');
    end;
  end;
end;

function GetSQLContext(const SQLText: string): TSQLContext;
var
  Parser: TSelectSQLParserExt;
begin
  Result := ctxUnknown;

  if Trim(SQLText) = '' then
  begin
    Result := ctxEmpty;
    Exit;
  end;

  Parser := TSelectSQLParserExt.CreateFromString(SQLText);
  try
    Result := Parser.GetContext;
  finally
    Parser.Free;
  end;
end;
//END-Intellisense//////////////////////////////////////////////////////////////////


end.
