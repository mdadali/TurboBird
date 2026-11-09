unit ValueViewForm;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Grids, Clipbrd, Menus;

type
  TFormRawValue = class(TForm)
    panTop: TPanel;
    panDataView: TPanel;
    memoText: TMemo;
    lbTypeText: TLabel;
    lbType: TLabel;
    lbSize: TLabel;
    lbSizeText: TLabel;
    chkFullRaw: TCheckBox;
    lbRawOffsText: TLabel;
    lbRawOffs: TLabel;
    chkValue: TCheckBox;
    dgHex: TDrawGrid;
    pmHex: TPopupMenu;
    miCopyToClipboard: TMenuItem;
    procedure chkFullRawClick(Sender: TObject);
    procedure chkValueClick(Sender: TObject);
    procedure dgHexDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
    procedure memoTextChange(Sender: TObject);
    procedure memoTextKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure miCopyToClipboardClick(Sender: TObject);
  private
    { Private declarations }
    FSelOffs: Integer;  // offset to selected text
    FSelSize: Integer;  // size of selected text
  public
    { Public declarations }
    Value: Variant;
    RawData: AnsiString;   // Whole data string
    RawOffs: Integer;      // value offset (0-based)
    RawLen: Integer;       // value length
    FieldName: string;
    FieldTypeName: string;
    IsBlob: Boolean;

    procedure UpdateView();

    procedure ShowValue(ARawData: AnsiString; ARawOffs, ARawLen: Integer);
  end;

var
  FormRawValue: TFormRawValue;

implementation

{$R *.lfm}

function DataAsStr(const AData; ALen: Integer): string;
var
  i: Integer;
begin
  Result := '';
  if ALen = 0 then Exit;

  SetLength(Result, ALen);
  Move(AData, Result[1], ALen);
  for i := 1 to ALen do
  begin
    if Ord(Result[i]) < 32 then
      Result[i] := '.';
  end;
end;

function BufferToHex(const Buffer; BufferSize: Integer): string;
var
  i: Integer;
  pb: PByte;
begin
  Result := '';
  pb := @Buffer;
  for i := 0 to BufferSize - 1 do
  begin
    if i > 0 then Result := Result + ' ';
    Result := Result + IntToHex(pb^, 2);
    Inc(pb);
  end;
end;

{ TFormRawValue }

procedure TFormRawValue.chkFullRawClick(Sender: TObject);
begin
  UpdateView();
end;

procedure TFormRawValue.chkValueClick(Sender: TObject);
begin
  UpdateView();
end;

procedure TFormRawValue.dgHexDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
  State: TGridDrawState);
var
  c: TCanvas;
  s: string;
  clText, clFill: TColor;
  {$ifdef FPC}
  ts: TTextStyle;
  {$else}
  tf: TTextFormat;
  {$endif}
  iPos: Integer;
begin
  c := dgHex.Canvas;
  clText := clWindowText;
  clFill := clWindow;

  if ACol = 0 then
  begin
    s := IntToHex(ARow * 8, 4);
    clFill := dgHex.FixedColor;
  end
  else
  begin
    iPos := (ARow * 8) + ACol-1;
    if (iPos < Length(RawData)) then
      s := IntToHex(Ord(RawData[iPos+1]), 2)
    else
      s := '';

    if (iPos >= RawOffs) and (iPos < RawOffs + RawLen) then
      clFill := clYellow;

    if (iPos >= FSelOffs) and (iPos < FSelOffs + FSelSize) then
      //clFill := clSkyBlue;
      clFill := clMoneyGreen;
  end;

  // draw cell
  if c.Brush.Color <> clFill then
  begin
    c.Brush.Color := clFill;
    c.FillRect(Rect);
  end;
  c.Font.Color := clText;
  Rect.Left := Rect.Left + 2;
  Rect.Right := Rect.Right - 2;
  {$ifdef FPC}
  ts := c.TextStyle;
  c.TextRect(Rect, Rect.Left, Rect.Top, s, ts);
  {$else}
  c.TextRect(Rect, s, tf);
  {$endif}
end;

procedure TFormRawValue.memoTextChange(Sender: TObject);
begin
  FSelOffs := memoText.SelStart;
  FSelSize := memoText.SelLength;
  dgHex.Invalidate();
end;

procedure TFormRawValue.memoTextKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  memoTextChange(nil);
end;

procedure TFormRawValue.miCopyToClipboardClick(Sender: TObject);
begin
  if Length(RawData) > 0 then
    Clipboard.AsText := BufferToHex(RawData[1], Length(RawData));
end;

procedure TFormRawValue.ShowValue(ARawData: AnsiString; ARawOffs, ARawLen: Integer);
begin
  RawData := Copy(ARawData, 1, MaxInt);
  RawOffs := ARawOffs;
  RawLen := ARawLen;
  FSelOffs := 0;
  FSelSize := 0;

  dgHex.ColWidths[0] := 40;
  dgHex.RowCount := (Length(RawData) div 8) + 1;
  if FieldName <> '' then
    Caption := FieldName
  else
    Caption := 'Raw value';

  UpdateView();
  Show();
end;

procedure TFormRawValue.UpdateView;
var
  NewOffs, NewLen: Integer;
  s: string;
begin
  lbType.Caption := FieldTypeName;
  lbSize.Caption := IntToStr(RawLen);
  lbRawOffs.Caption := IntToStr(RawOffs);
  s := '';
  {NewOffs := RawOffs+1;
  while NewOffs > Length(RawData) do
  begin
    Dec(NewOffs);
    s := s + '   ';
  end;
  if (NewOffs + (NewLen-1)) < Length(RawData) then
    NewLen := Length(RawData) - NewOffs;   }

  if chkFullRaw.Checked then
  begin
    // full raw dump
    NewOffs := RawOffs+1;
    NewLen := RawLen;
    if NewOffs + NewLen > Length(RawData) then
      NewLen := Length(RawData) - NewOffs - 1;
    if (NewOffs <= Length(RawData)) and (NewLen > 0) then
    begin
      s := BufferToHex(RawData[1], RawOffs);
      s := s + '[' + BufferToHex(RawData[RawOffs+1], RawLen) + ']';
      // next line
      //if (Length(s) mod 23) = 0 then
      //  s := s + sLineBreak;
      NewOffs := RawOffs + 1 + RawLen;
      NewLen := Length(RawData) - NewOffs - 1;
      if (NewOffs <= Length(RawData)) and (NewLen > 0) then
        s := s + BufferToHex(RawData[NewOffs], NewLen);
    end
    else if Length(RawData) > 0 then
    begin
      s := BufferToHex(RawData[1], Length(RawData));
    end;
  end
  else
  begin
    NewLen := RawLen;
    if RawOffs + NewLen > Length(RawData) then
      NewLen := Length(RawData) - RawOffs;

    NewOffs := RawOffs+1;
    if NewOffs <= Length(RawData) then
      s := BufferToHex(RawData[NewOffs], NewLen);
  end;

  //memoHex.Text := s;
  if chkFullRaw.Checked and (Length(RawData) > 0) then
    memoText.Text := DataAsStr(RawData[1], Length(RawData))
  else
  if RawLen > 0 then
  begin
    if RawOffs + RawLen > Length(RawData) then
      memoText.Text := Format('<RawLen=%d Offset=%d Length=%d>', [Length(RawData), RawOffs, RawLen])
    else
      memoText.Text := DataAsStr(RawData[RawOffs+1], RawLen);
  end
  else
  begin
    memoText.Text := '';
  end;

  if chkValue.Checked then
  begin
    s := VarToStr(Value);
    //if s <> '' then
    //  memoHex.Text := BufferToHex(s[1], Length(s));

    if IsBlob and (s <> '') then
      memoText.Text := DataAsStr(s[1], Length(s))
    else
      memoText.Text := s;
  end;
  dgHex.Invalidate();
end;

end.
