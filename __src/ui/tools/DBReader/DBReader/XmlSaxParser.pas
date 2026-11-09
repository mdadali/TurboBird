unit XmlSaxParser;

// Based on Fast HTML Parser (modified) from James Azarja

interface

uses
  SysUtils;

type
  // when tag content found in HTML, including names and values
  // case insensitive analysis available via NoCaseTag
  TOnFoundTag = procedure(NoCaseTag, ActualTag: AnsiString) of object;

  // when text found in the HTML
  TOnFoundText = procedure(Text: AnsiString) of object;


  { TXmlSaxParser }
  TXmlSaxParser = class(TObject)
  private
    FDone: Boolean;
  public
    OnFoundTag: TOnFoundTag;
    OnFoundText: TOnFoundText;
    Raw: PAnsiChar;
    FCurrent : PAnsiChar;
    constructor Create(sRaw: AnsiString); overload;
    //constructor Create(sRaw: UnicodeString); overload;
    constructor Create(pRaw: PAnsiChar); overload;
    procedure Exec;
    procedure NilOnFoundTag(NoCaseTag, ActualTag: AnsiString);
    procedure NilOnFoundText(Text: AnsiString);
    function CurrentPos : Integer;
    property Done: Boolean read FDone write FDone;
  end;

function ExtractTagName(ATagStr: string): string;
function ExtractTagParamValue(ATagStr, AParamName: string): string;
function IsClosingTag(ATagStr: string): Boolean; // </name>
function IsEmptyTag(ATagStr: string): Boolean;   // <name />


implementation

function CopyBuffer(StartIndex: PAnsiChar; Length: Integer): AnsiString;
begin
  SetLength(Result, Length);
  StrLCopy(@Result[1], StartIndex, Length);
end;

function PosEx(const SubStr, S: string; Offset: Cardinal = 1): Integer;
var
  I,X: Integer;
  Len, LenSubStr: Integer;
begin
  if Offset = 1 then
    Result := Pos(SubStr, S)
  else
  begin
    I := Offset;
    LenSubStr := Length(SubStr);
    Len := Length(S) - LenSubStr + 1;
    while I <= Len do
    begin
      if S[I] = SubStr[1] then
      begin
        X := 1;
        while (X < LenSubStr) and (S[I + X] = SubStr[X + 1]) do
          Inc(X);
        if (X = LenSubStr) then
        begin
          Result := I;
          exit;
        end;
      end;
      Inc(I);
    end;
    Result := 0;
  end;
end;

function ExtractTagName(ATagStr: string): string;
var
  n: Integer;
begin
  Result := '';
  for n := 1 to Length(ATagStr) do
  begin
    if Pos(ATagStr[n], '< />') > 0 then
    begin
      if Result <> '' then
        Exit;
    end
    else
      Result := Result + ATagStr[n];
  end;
end;

function ExtractTagParamValue(ATagStr, AParamName: string): string;
var
  n1, n2: Integer;
begin
  Result := '';
  // extract src
  n1 := Pos(AParamName + '="', ATagStr);
  if n1 = 0 then
    n1 := Pos(AParamName + '=''', ATagStr);
  if n1 = 0 then
    Exit;
  n1 := n1 + Length(AParamName + '="');
  n2 := PosEx('"', ATagStr, n1);
  if n2 = 0 then
    n2 := PosEx('''', ATagStr, n1);
  Result := Copy(ATagStr, n1, n2-n1);
end;

function IsClosingTag(ATagStr: string): Boolean;
begin
  Result := (Pos('</', ATagStr) > 0);
end;

function IsEmptyTag(ATagStr: string): Boolean;   // <name />
begin
  Result := (Pos('/>', ATagStr) > 0);
end;

constructor TXmlSaxParser.Create(pRaw: PAnsiChar);
begin
  if pRaw = '' then exit;
  if pRaw = nil then exit;
  Raw:= pRaw;
end;

constructor TXmlSaxParser.Create(sRaw: AnsiString);
begin
  if sRaw = '' then exit;
  Raw:= PAnsiChar(sRaw);
end;

{constructor TXmlSaxParser.Create(sRaw: UnicodeString);
begin
  Create(UTF8Encode(sRaw));
end; }

{ default dummy "do nothing" events if events are unassigned }
procedure TXmlSaxParser.NilOnFoundTag(NoCaseTag, ActualTag: AnsiString);
begin
end;

procedure TXmlSaxParser.NilOnFoundText(Text: AnsiString);
begin
end;

function TXmlSaxParser.CurrentPos: Integer;
begin
  if Assigned(Raw) and Assigned(FCurrent) then
    Result := FCurrent - Raw
  else
    Result := 0;
end;

procedure TXmlSaxParser.Exec;
var
  L: Integer;
  TL: Integer;
  I: Integer;
  TagStart,
  TextStart,
  P: PAnsiChar;   // Pointer to current AnsiChar.
  C: AnsiChar;
begin
  { set nil events once rather than checking for nil each time tag is found }
  if not assigned(OnFoundText) then
    OnFoundText := NilOnFoundText;
  if not assigned(OnFoundTag) then
    OnFoundTag := NilOnFoundTag;

  TL := StrLen(Raw);
  I := 0;
  P := Raw;
  Done := False;
  if P <> nil then
  begin
    TagStart := nil;
    repeat
      TextStart := P;
      { Get next tag position }
      while Not (P^ in [ '<', #0 ]) do
      begin
        Inc(P); Inc(I);
        if I >= TL then
        begin
          Done:= True;
          Break;
        end;
      end;
      if Done then Break;

      { Is there any text before ? }
      if (TextStart <> nil) and (P > TextStart) then
      begin
        L:= P - TextStart;
        { Yes, copy to buffer }
        FCurrent := P;
        OnFoundText( CopyBuffer(TextStart, L) );
      end else
      begin
        TextStart := nil;
      end;
      { No }

      TagStart := P;
      while Not (P^ in [ '>', #0]) do
      begin
        // Find string in tag
        if (P^ = '"') or (P^ = '''') then
        begin
          C:= P^;
          Inc(P); Inc(I); // Skip current AnsiChar " or '

          // Skip until string end
          while Not (P^ in [C, #0]) do
          begin
            Inc(P); Inc(I);
          end;
        end;

        Inc(P); Inc(I);
        if I >= TL then
        begin
          Done := True;
          Break;
        end;
      end;
      if Done then Break;

      { Copy this tag to buffer }
      L := P - TagStart + 1;

      FCurrent := P;
      OnFoundTag( AnsiLowerCase(CopyBuffer(TagStart, L )), CopyBuffer(TagStart, L ) ); //L505: added uppercase
      Inc(P); Inc(I);
      if I >= TL then Break;
    until (Done);
  end;
end;

end.