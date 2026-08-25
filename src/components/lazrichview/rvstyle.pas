unit RVStyle;

{$IFDEF FPC}
  {$mode Delphi}
{$ENDIF}

interface
{.$R RVStyle}
uses
  {$IFDEF FPC}
  LCLType,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, {LibConst,}
  IniFiles;
const
  crJump = 101;

  rvsNormal = 0;
  rvsHeading = 1;
  rvsSubheading = 2;
  rvsKeyword = 3;
  rvsJump1 = 4;
  rvsJump2 = 5;

  LAST_DEFAULT_STYLE_NO = rvsJump2;

{$I RV_Defs.inc}

type
{--------------------------------------------------------------}
  TFontInfo = class(TCollectionItem)
  private
    { Private declarations }
    FFontName: TFontName;
    FSize: Integer;
    FColor: TColor;
    FStyle: TFontStyles;
    {$IFDEF RICHVIEWDEF3}
    FCharSet: TFontCharSet;
    {$ENDIF}
  public
    { Public declarations }
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    { Published declarations }
    {$IFDEF RICHVIEWDEF3}
    property CharSet: TFontCharSet read FCharSet write FCharSet;
    {$ENDIF}
    property FontName: TFontName read FFontNAme write FFontName;
    property Size: Integer read FSize write FSize;
    property Color: TColor read FColor write FColor;
    property Style: TFontStyles read FStyle write FStyle;
  end;
{--------------------------------------------------------------}
  TFontInfos = class(TCollection)
  private
    function GetItem(Index: Integer): TFontInfo;
    procedure SetItem(Index: Integer; Value: TFontInfo);
  public
    constructor Create();
    function Add(): TFontInfo;
    procedure Delete(Index: Integer);
    procedure AddFont(Name: TFontName; Size: Integer;
                        Color: TColor; Style: TFontStyles);
    {$IFDEF RICHVIEWDEF3}
    procedure AddFontEx(Name: TFontName; Size: Integer;
                        Color: TColor; Style: TFontStyles;
                        CharSet: TFontCharSet);
    {$ENDIF}
    property Items[Index: Integer]: TFontInfo read GetItem write SetItem; default;
  end;
{--------------------------------------------------------------}
  TRVStyle = class(TComponent)
  private
    { Private declarations }
    FColor: TColor;
    FHoverColor: TColor;
    FSelColor: TColor;
    FSelTextColor: TColor;
    FCursor: TCursor;
    FTextStyles: TFontInfos;
    FFullRedraw: Boolean;
    function GetTextStyle(Index: Integer): TFontInfo;
    procedure SetTextStyles(ATextStyles: TFontInfos);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { returns index of new style }
    function AddTextStyle(): Integer;
    { removes NONSTANDARD text style}
    procedure DeleteTextStyle(Index: Integer);
    { WARNING: before saving all section will be removed }
    procedure SaveINI(filename, section: string);
    procedure LoadINI(filename, section: string);
  published
    { Published declarations }
    property TextStyles: TFontInfos read FTextStyles write SetTextStyles;
    property JumpCursor: TCursor read FCursor write FCursor;
    property Color: TColor read FColor write FColor;
    property HoverColor: TColor read FHoverColor write FHoverColor;
    property FullRedraw: Boolean read FFullRedraw write FFullRedraw;
    property SelColor: TColor read FSelColor write FSelColor;
    property SelTextColor: TColor read FSelTextColor write FSelTextColor;    
  end;

implementation
{--------------------------------------------------------------}
{ TFontInfo                                                    }
{--------------------------------------------------------------}
constructor TFontInfo.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FFontName := 'Arial';
  FSize := 10;
  FColor := clWindowText;
  FStyle := [];
  {$IFDEF RICHVIEWDEF3}
  FCharSet := DEFAULT_CHARSET;
  {$ENDIF}
end;
{--------------------------------------------------------------}
procedure TFontInfo.Assign(Source: TPersistent);
begin
  if Source is TFontInfo then
  begin
    FFontName :=  TFontInfo(Source).FFontName;
    FSize := TFontInfo(Source).FSize;
    FColor := TFontInfo(Source).FColor;
    FStyle := TFontInfo(Source).FStyle;
    {$IFDEF RICHVIEWDEF3}
    FCharSet := TFontInfo(Source).FCharSet;
    {$ENDIF}
  end
  else
    inherited Assign(Source);
end;
{--------------------------------------------------------------}
{ TFontInfos                                                   }
{--------------------------------------------------------------}
constructor TFontInfos.Create();
begin
  inherited Create(TFontInfo);
end;
{-------------------------------------------------------------}
function TFontInfos.Add(): TFontInfo;
begin
  Result := TFontInfo(inherited Add);
end;
{-------------------------------------------------------------}
procedure TFontInfos.AddFont(Name: TFontName; Size: Integer;
                   Color: TColor; Style: TFontStyles);
var
  v: TFontInfo;
begin
  v := Add();
  v.FontName := Name;
  v.Size := Size;
  v.Color := Color;
  v.Style := Style;
end;
{-------------------------------------------------------------}
{$IFDEF RICHVIEWDEF3}
procedure TFontInfos.AddFontEx(Name: TFontName; Size: Integer;
                   Color: TColor; Style: TFontStyles;
                   CharSet: TFontCharSet);
var
  v: TFontInfo;
begin
  v := Add();
  v.FontName := Name;
  v.Size := Size;
  v.Color := Color;
  v.Style := Style;
  v.CharSet := CharSet;
end;
{$ENDIF}
{-------------------------------------------------------------}
function TFontInfos.GetItem(Index: Integer): TFontInfo;
begin
  Result := TFontInfo(inherited GetItem(Index));
end;
{-------------------------------------------------------------}
procedure TFontInfos.SetItem(Index: Integer; Value: TFontInfo);
begin
  inherited SetItem(Index, Value);
end;
{-------------------------------------------------------------}
procedure TFontInfos.Delete(Index: Integer);
begin
   if (Index <= LAST_DEFAULT_STYLE_NO) or (Index >= Count) then
     Exit;
   Items[Index].Free();
   {
   c:=TFontInfos.Create;
   c.Assign(Self);
   Clear;
   for i:=0 to c.Count-1 do
    if i<>Index then
       AddFont(c[i].FontName, c[i].Size, c[i].Color, c[i].Style);
   c.Free;
   }
end;
{--------------------------------------------------------------}
{ TRVStyle                                                     }
{--------------------------------------------------------------}
constructor TRVStyle.Create(AOwner: TComponent);
var
  fi: TFontInfo;
  i : Integer;
begin
  inherited Create(AOwner);
  FFullRedraw := False;  
  {$IFDEF FPC}
  Screen.Cursors[crJump] := LoadCursorFromLazarusREsource('RV_JUMP_CURSOR');
  {$ELSE}
  Screen.Cursors[crJump] := LoadCursor(hInstance,'RV_JUMP_CURSOR');
  {$ENDIF}
  FCursor := crJump;
  FColor := clWindow;
  FHoverColor := clNone;
  FSelColor := clHighlight;
  FSelTextColor := clHighlightText;
  FTextStyles := TFontInfos.Create;
  for i := 0 to LAST_DEFAULT_STYLE_NO do
  begin
    fi := FTextStyles.Add();
    case i of
      rvsHeading:
      begin
        fi.Style := fi.Style + [fsBold];
        fi.Color := clBlue;
      end;

      rvsSubheading:
      begin
        fi.Style := fi.Style + [fsBold];
        fi.Color := clNavy;
      end;

      rvsKeyword:
      begin
        fi.Style := fi.Style + [fsItalic];
        fi.Color := clMaroon;
      end;

      rvsJump1, rvsJump2:
      begin
        fi.Style := fi.Style + [fsUnderline];
        fi.Color := clGreen;
      end;
    end;
  end;
end;
{--------------------------------------------------------------}
destructor TRVStyle.Destroy();
begin
  FreeAndNil(FTextStyles);
  inherited Destroy;
end;
{--------------------------------------------------------------}
function TRVStyle.GetTextStyle(Index: Integer): TFontInfo;
begin
  Result := FTextStyles[Index];
end;
{--------------------------------------------------------------}
procedure TRVStyle.SetTextStyles(ATextStyles: TFontInfos);
begin
   FTextStyles.Assign(ATextStyles);
end;
{--------------------------------------------------------------}
function TRVStyle.AddTextStyle(): Integer;
begin
   FTextStyles.Add();
   Result := FTextStyles.Count-1;
end;
{--------------------------------------------------------------}
procedure TRVStyle.DeleteTextStyle(Index: Integer);
begin
  FTextStyles.Delete(Index);
end;
{--------------------------------------------------------------}
procedure TRVStyle.SaveINI(filename, section: string);
var
  i: Integer;
  ini: TIniFile;
  s: string;
begin
  ini := TIniFile.Create(filename);
  try
    ini.EraseSection(section);
    ini.WriteInteger(section,'Color', FColor);
    ini.WriteInteger(section,'HoverColor', FHoverColor);
    ini.WriteInteger(section,'SelColor', FSelColor);
    ini.WriteInteger(section,'SelTextColor', FSelTextColor);
    ini.WriteInteger(section,'JumpCursor', FCursor);
    ini.WriteInteger(section,'FontsCount', FTextStyles.Count);
    for i := 0 to FTextStyles.Count-1 do
    begin
      ini.WriteString(section, 'FontName'+IntToStr(i), FTextStyles[i].FontName);
      ini.WriteInteger(section, 'FontSize'+IntToStr(i), FTextStyles[i].Size);
      ini.WriteInteger(section, 'FontColor'+IntToStr(i), FTextStyles[i].Color);
      {$IFDEF RICHVIEWDEF3}
      ini.WriteInteger(section, 'FontCharSet'+IntToStr(i), FTextStyles[i].CharSet);
      {$ENDIF}
      if fsBold in FTextStyles[i].Style then
        s := 'Yes'
      else
        s := 'No';
      ini.WriteString(section, 'FontBold'+IntToStr(i), s);
      if fsUnderline in FTextStyles[i].Style then
        s := 'Yes'
      else
        s := 'No';
      ini.WriteString(section, 'FontUnderline'+IntToStr(i), s);
      if fsStrikeOut in FTextStyles[i].Style then
        s := 'Yes'
      else
        s := 'No';
      ini.WriteString(section, 'FontStrikeOut'+IntToStr(i), s);
      if fsItalic in FTextStyles[i].Style then
        s := 'Yes'
      else
        s := 'No';
      ini.WriteString(section, 'FontItalic'+IntToStr(i), s);
    end;
  finally
    ini.Free();
  end;
end;
{--------------------------------------------------------------}
procedure TRVStyle.LoadINI(filename, section: string);
var
  i, fontcounts: Integer;
  ini: TIniFile;
begin
  ini := TIniFile.Create(filename);
  try
    FColor        := ini.ReadInteger(section, 'Color',        clWindow);
    FHoverColor   := ini.ReadInteger(section, 'HoverColor',   clNone);
    FSelColor     := ini.ReadInteger(section, 'SelColor',     clHighlight);
    FSelTextColor := ini.ReadInteger(section, 'SelTextColor', clHighlightText);
    FCursor       := ini.ReadInteger(section, 'JumpCursor',   crJump);
    fontcounts    := ini.ReadInteger(section, 'FontsCount',   LAST_DEFAULT_STYLE_NO+1);
    if fontcounts < LAST_DEFAULT_STYLE_NO+1 then
      fontcounts := LAST_DEFAULT_STYLE_NO+1;
    while FTextStyles.Count > fontcounts do
     DeleteTextStyle(FTextStyles.Count-1);
    while FTextStyles.Count < fontcounts do
     AddTextStyle();
    for i:=0 to fontcounts-1 do
    begin
      FTextStyles[i].FontName := ini.ReadString(section, 'FontName' + IntToStr(i), 'Arial');
      FTextStyles[i].Size := ini.ReadInteger(section, 'FontSize' + IntToStr(i), 10);
      FTextStyles[i].Color := ini.ReadInteger(section, 'FontColor' + IntToStr(i), clWindowText);
      {$IFDEF RICHVIEWDEF3}
      FTextStyles[i].CharSet := ini.ReadInteger(section, 'FontCharSet' + IntToStr(i), DEFAULT_CHARSET);
      {$ENDIF}
      FTextStyles[i].Style := [];
      if ini.ReadString(section, 'FontBold'+IntToStr(i), 'No') = 'Yes' then
        FTextStyles[i].Style := FTextStyles[i].Style + [fsBold];
      if ini.ReadString(section, 'FontUnderline'+IntToStr(i),'No') = 'Yes' then
        FTextStyles[i].Style := FTextStyles[i].Style + [fsUnderline];
      if ini.ReadString(section, 'FontStrikeOut'+IntToStr(i),'No') = 'Yes' then
        FTextStyles[i].Style := FTextStyles[i].Style + [fsStrikeOut];
      if ini.ReadString(section, 'FontItalic'+IntToStr(i), 'No') = 'Yes' then
        FTextStyles[i].Style := FTextStyles[i].Style + [fsItalic];
    end;
  finally
    ini.Free();
  end;
end;

end.
