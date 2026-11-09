unit uWebTools;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, fphttpclient, opensslsockets;

function WebSearch(const Query: string; out SearchResult: string; out ErrorMsg: string): Boolean;
function WebFetch(const URL: string; out Content: string; out ErrorMsg: string): Boolean;

implementation

function URLEncode(const S: string): string;
var
  i: Integer;
  c: Char;
begin
  Result := '';
  for i := 1 to Length(S) do
  begin
    c := S[i];
    if c in ['A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.', '~'] then
      Result := Result + c
    else if c = ' ' then
      Result := Result + '+'
    else
      Result := Result + '%' + IntToHex(Ord(c), 2);
  end;
end;

function StripHTML(const S: string): string;
var
  i: Integer;
  InTag: Boolean;
  Entity: string;
  j: Integer;
begin
  Result := '';
  InTag := False;
  i := 1;
  while i <= Length(S) do
  begin
    if S[i] = '<' then
    begin
      InTag := True;
      Inc(i);
      Continue;
    end;
    if S[i] = '>' then
    begin
      InTag := False;
      Inc(i);
      if (Result <> '') and (Result[Length(Result)] <> ' ') then
        Result := Result + ' ';
      Continue;
    end;
    if InTag then
    begin
      Inc(i);
      Continue;
    end;
    if S[i] = '&' then
    begin
      Entity := '';
      j := i;
      while (j <= Length(S)) and (S[j] <> ';') and (j - i < 10) do
      begin
        Entity := Entity + S[j];
        Inc(j);
      end;
      if (j <= Length(S)) and (S[j] = ';') then
      begin
        Entity := Entity + ';';
        if Entity = '&amp;' then Result := Result + '&'
        else if Entity = '&lt;' then Result := Result + '<'
        else if Entity = '&gt;' then Result := Result + '>'
        else if Entity = '&quot;' then Result := Result + '"'
        else if Entity = '&apos;' then Result := Result + ''''
        else if Entity = '&nbsp;' then Result := Result + ' '
        else if Entity = '&mdash;' then Result := Result + '—'
        else if Entity = '&ndash;' then Result := Result + '–'
        else if Entity = '&hellip;' then Result := Result + '…'
        else Result := Result + Entity;
        i := j + 1;
        Continue;
      end;
    end;
    Result := Result + S[i];
    Inc(i);
  end;
  while Pos('  ', Result) > 0 do
    Result := StringReplace(Result, '  ', ' ', [rfReplaceAll]);
end;

function WebSearch(const Query: string; out SearchResult: string; out ErrorMsg: string): Boolean;
var
  Client: TFPHTTPClient;
  RespStream, ReqStream: TStringStream;
  HTML: string;
  i, j: Integer;
  Title, Link, Snippet: string;
  Count: Integer;
begin
  Result := False;
  SearchResult := '';
  ErrorMsg := '';
  if Trim(Query) = '' then
  begin
    ErrorMsg := 'Empty search query';
    Exit;
  end;

  Client := TFPHTTPClient.Create(nil);
  RespStream := TStringStream.Create('');
  ReqStream := TStringStream.Create('q=' + URLEncode(Query));
  try
    Client.AddHeader('User-Agent', 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36');
    Client.AddHeader('Content-Type', 'application/x-www-form-urlencoded');
    Client.ConnectTimeout := 8000;
    Client.IOTimeout := 15000;
    ReqStream.Position := 0;
    Client.RequestBody := ReqStream;
    try
      Client.Post('https://html.duckduckgo.com/html/', RespStream);
    except
      on E: Exception do
      begin
        ErrorMsg := 'Search request failed: ' + E.Message;
        Exit;
      end;
    end;

    if Client.ResponseStatusCode <> 200 then
    begin
      ErrorMsg := 'Search HTTP ' + IntToStr(Client.ResponseStatusCode);
      Exit;
    end;

    HTML := RespStream.DataString;
    Count := 0;

    while True do
    begin
      j := Pos('class="result__a"', HTML);
      if j = 0 then Break;

      Link := '';
      i := Pos('href="', HTML);
      if i > 0 then
      begin
        i := i + 6;
        while (i <= Length(HTML)) and (HTML[i] <> '"') do
        begin
          Link := Link + HTML[i];
          Inc(i);
        end;
      end;

      Title := '';
      i := Pos('>', HTML, j);
      if i > 0 then
      begin
        Inc(i);
        while (i <= Length(HTML)) and (Copy(HTML, i, 4) <> '</a>') do
        begin
          Title := Title + HTML[i];
          Inc(i);
        end;
        Title := StripHTML(Title);
      end;

      Snippet := '';
      j := Pos('class="result__snippet"', HTML);
      if j > 0 then
      begin
        i := Pos('>', HTML, j);
        if i > 0 then
        begin
          Inc(i);
          while (i <= Length(HTML)) and (Copy(HTML, i, 4) <> '</a>') do
          begin
            Snippet := Snippet + HTML[i];
            Inc(i);
          end;
          Snippet := StripHTML(Snippet);
        end;
      end;

      if (Title <> '') then
      begin
        Inc(Count);
        SearchResult := SearchResult + Format('[%d] %s', [Count, Title]) + LineEnding;
        SearchResult := SearchResult + '    URL: ' + Link + LineEnding;
        if Snippet <> '' then
          SearchResult := SearchResult + '    ' + Snippet + LineEnding;
        SearchResult := SearchResult + LineEnding;
      end;

      j := Pos('class="result__a"', HTML);
      if j > 0 then
        Delete(HTML, 1, j + 10)
      else
        Break;

      if Count >= 10 then Break;
    end;

    if SearchResult = '' then
      SearchResult := 'No search results found for: ' + Query
    else
      SearchResult := 'Search results for "' + Query + '" (' + IntToStr(Count) + ' results):' + LineEnding + LineEnding + SearchResult;

    Result := True;
  finally
    ReqStream.Free;
    RespStream.Free;
    Client.Free;
  end;
end;

function WebFetch(const URL: string; out Content: string; out ErrorMsg: string): Boolean;
var
  Client: TFPHTTPClient;
  RespStream: TStringStream;
  HTML: string;
begin
  Result := False;
  Content := '';
  ErrorMsg := '';
  if Trim(URL) = '' then
  begin
    ErrorMsg := 'Empty URL';
    Exit;
  end;

  Client := TFPHTTPClient.Create(nil);
  RespStream := TStringStream.Create('');
  try
    Client.AddHeader('User-Agent', 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36');
    Client.ConnectTimeout := 8000;
    Client.IOTimeout := 15000;
    try
      Client.Get(URL, RespStream);
    except
      on E: Exception do
      begin
        ErrorMsg := 'Fetch failed: ' + E.Message;
        Exit;
      end;
    end;

    if Client.ResponseStatusCode <> 200 then
    begin
      ErrorMsg := 'HTTP ' + IntToStr(Client.ResponseStatusCode);
      Exit;
    end;

    HTML := RespStream.DataString;
    Content := StripHTML(HTML);

    if Length(Content) > 50000 then
      Content := Copy(Content, 1, 50000) + LineEnding + '... (truncated, ' + IntToStr(Length(HTML)) + ' bytes total)';

    if Trim(Content) = '' then
      Content := '(empty page)';

    Result := True;
  finally
    RespStream.Free;
    Client.Free;
  end;
end;

end.
PASCAL
file_path">
/home/maurog/alphacode/ai_prompt/uWebTools.pas