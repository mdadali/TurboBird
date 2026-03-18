unit TerminalFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, ActnList, StdCtrls, RVStyle,
  RichView, RFUtils, LazUTF8, Logger, SimpleObj;

type

  { TFrameTerminal }

  TFrameTerminal = class(TFrame)
    actTermClear: TAction;
    alTerm: TActionList;
    Button1: TButton;
    cbTermAutoscroll: TCheckBox;
    cbShowTx: TCheckBox;
    chkLogToFile: TCheckBox;
    panTerminalTop: TPanel;
    rvTerm: TRichView;
    RVStyle1: TRVStyle;
    procedure actTermClearExecute(Sender: TObject);
    procedure cbTermAutoscrollChange(Sender: TObject);
    procedure chkLogToFileChange(Sender: TObject);
    procedure rvTermKeyPress(Sender: TObject; var Key: char);
  private
    FMaxLines: Integer;
    FCurStyleId: Integer;
    FPrevText: string;
    FTermLog: TSimpleFileWriter;
    FTermLogEnabled: Boolean;
    FNeedRepaint: Boolean;

    FOnTermKeyPress: TKeyPressEvent;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure AddText(const AText: string);
    procedure AddData(const AData: AnsiString);
    procedure AddDataTx(const AData: AnsiString);
    procedure Clear();

    property MaxLines: Integer read FMaxLines write FMaxLines;

    property OnTermKeyPress: TKeyPressEvent read FOnTermKeyPress write FOnTermKeyPress;
  end;

implementation

{$R *.lfm}

const
  // terminal text colors
  STYLE_TEXT_BLACK     = 0;
  STYLE_TEXT_RED       = 10;
  STYLE_TEXT_GREEN     = 11;
  STYLE_TEXT_YELLOW    = 12;
  STYLE_TEXT_BLUE      = 13;
  STYLE_TEXT_MAGENTA   = 14;
  STYLE_TEXT_CYAN      = 15;
  STYLE_TEXT_WHITE     = 16;
  // terminal text colors (bright/bold)
  STYLE_TEXT_BBLACK    = 17;
  STYLE_TEXT_BRED      = 18;
  STYLE_TEXT_BGREEN    = 19;
  STYLE_TEXT_BYELLOW   = 20;
  STYLE_TEXT_BBLUE     = 21;
  STYLE_TEXT_BMAGENTA  = 22;
  STYLE_TEXT_BCYAN     = 23;
  STYLE_TEXT_BWHITE    = 24;


{ TFrameTerminal }

procedure TFrameTerminal.actTermClearExecute(Sender: TObject);
begin
  Clear();
end;

procedure TFrameTerminal.cbTermAutoscrollChange(Sender: TObject);
begin
  if cbTermAutoscroll.Checked then
    rvTerm.Options := rvTerm.Options + [rvoScrollToEnd]
  else
    rvTerm.Options := rvTerm.Options - [rvoScrollToEnd];
end;

procedure TFrameTerminal.chkLogToFileChange(Sender: TObject);
begin
  //FPrevText := '';
  FTermLogEnabled := chkLogToFile.Checked;
  if FTermLogEnabled then
  begin
    FTermLog.FileName := 'terminal' + FormatDateTime('-YYYY-MM-DD', Now()) + '.txt';
    FTermLog.Init();
  end
  else
  begin
    FTermLog.Term();
  end;
end;

procedure TFrameTerminal.rvTermKeyPress(Sender: TObject; var Key: char);
begin
  if Assigned(OnTermKeyPress) then OnTermKeyPress(Self, Key);
end;

procedure TFrameTerminal.AfterConstruction;
begin
  inherited AfterConstruction;
  FMaxLines := 10000;
  FCurStyleId := STYLE_TEXT_BGREEN;
  rvTerm.Options := rvTerm.Options + [rvoScrollToEnd];
  cbTermAutoscroll.Checked := (rvoScrollToEnd in rvTerm.Options);
end;

procedure TFrameTerminal.BeforeDestruction;
begin
  FTermLogEnabled := False;
  FTermLog.Term();
  inherited BeforeDestruction;
end;

procedure TFrameTerminal.AddText(const AText: string);
var
  s: string;
  i, n: Integer;
begin
  s := FPrevText;
  for i := 1 to Length(AText) do
  begin
    case AText[i] of
      #9: s := s + '  '; // tab
      #$A, #$D: s := s + AText[i]; // CR/LF
      #$0..#$8, #$B, #$C, #$E..#$1F: ;  // control chars
      else  s := s + AText[i];
    end;
  end;
  //s := StringReplace(s, #9, '  ', [rfReplaceAll]);
  s := WinCPToUTF8(s);
  // если в конце был перевод строки, то запомним его, чтобы вставить в начало
  // следующей строки. Чтобы следующая строка была с новой строки, а не
  // продолжение предыдущей. TRichView игнорирует перевод строки в конце
  FPrevText := '';
  {if Copy(s, Length(s)-1, 2) = sLineBreak then
    FPrevText := sLineBreak; }
  n := Length(s);
  while n > 0 do
  begin
    if s[n] in [#13, #10] then
    begin
      FPrevText := s[n] + FPrevText;
      Dec(n);
    end
    else
      n := 0;
  end;

  {s := AText;
  SetCodePage(RawByteString(s), 1251, False);}
  rvTerm.AddText(s, FCurStyleId);

  if rvTerm.LineCount > FMaxLines then
  begin
    rvTerm.DeleteItems(0, 1000);
    rvTerm.Format();
  end
  else
  begin
    //rvTerm.Format();
    rvTerm.FormatTail();
  end;
  // repaint later
  FNeedRepaint := True;

  if FTermLogEnabled then
  begin
    FTermLog.Write(s);
    {FPrevText := FPrevText + s;
    n := Pos(#13, FPrevText);
    while n > 0 do
    begin
      s := Copy(FPrevText, 1, n-1);
      FPrevText := Trim(Copy(FPrevText, n+1, Maxint));
      n := Pos(#13, FPrevText);
    end; }
  end;
end;

procedure TFrameTerminal.AddData(const AData: AnsiString);
var
  i, n1, n2: Integer;
  iTextPos: Integer;
  //s: AnsiString;
  btChar: Byte;
  TmpVal: Integer;
  //c: AnsiChar;
  StyleId: Integer;
begin
  //AddText(AData);
  if Length(AData) = 0 then
    Exit;

  iTextPos := 1;
  StyleId := FCurStyleId;
  n1 := Pos(#$1B + #$5B, AData);  // ESC[
  while n1 > 0 do
  begin
    n2 := PosEx('m', AData, n1);
    if n2 = 0 then
      Break;

    AddText(Copy(AData, iTextPos, n1-iTextPos));
    iTextPos := n2 + 1;

    StyleId := STYLE_TEXT_BGREEN;
    TmpVal := 0;
    for i := n1+2 to n2 do
    begin
      btChar := Ord(AData[i]);
      if (btChar >= $30) and (btChar <= $39) then
      begin
        TmpVal := TmpVal * 10;
        TmpVal := TmpVal + (btChar - $30);
      end
      else
      begin
        {case TmpVal of
          30: StyleId := STYLE_TEXT_BLACK;
          31: StyleId := STYLE_TEXT_RED;
          32: StyleId := STYLE_TEXT_GREEN;
          33: StyleId := STYLE_TEXT_YELLOW;
          34: StyleId := STYLE_TEXT_BLUE;
          35: StyleId := STYLE_TEXT_MAGENTA;
          36: StyleId := STYLE_TEXT_CYAN;
          37: StyleId := STYLE_TEXT_WHITE;
        else
          StyleId := 0;
        end;  }

        case TmpVal of
          30: StyleId := STYLE_TEXT_BBLACK;
          31: StyleId := STYLE_TEXT_BRED;
          32: StyleId := STYLE_TEXT_BGREEN;
          33: StyleId := STYLE_TEXT_BYELLOW;
          34: StyleId := STYLE_TEXT_BBLUE;
          35: StyleId := STYLE_TEXT_BMAGENTA;
          36: StyleId := STYLE_TEXT_BCYAN;
          37: StyleId := STYLE_TEXT_BWHITE;
        else
          StyleId := STYLE_TEXT_BGREEN;
        end;

        Break; // !!
      end;
    end;
    FCurStyleId := StyleId;

    n1 := PosEx(#$1B + #$5B, AData, n2);
  end;
  AddText(Copy(AData, iTextPos, MaxInt));

  if FNeedRepaint then
  begin
    FNeedRepaint := False;
    if Visible then
      rvTerm.Repaint();
  end;
end;

procedure TFrameTerminal.AddDataTx(const AData: AnsiString);
var
  PrevStyleId: Integer;
begin
  if (AData <> '') and cbShowTx.Checked then
  begin
    PrevStyleId := FCurStyleId;
    FCurStyleId := STYLE_TEXT_GREEN;
    AddText(AData);
    FCurStyleId := PrevStyleId;
  end;

end;

procedure TFrameTerminal.Clear();
begin
  rvTerm.Clear();
end;

end.
