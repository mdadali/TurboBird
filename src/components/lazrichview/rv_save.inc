{this file is included in RichView.pas}
{------------------------------------}
procedure ReplaceStr(var str: string; const old, new: string);
var
  p: Integer;
begin
  while true do
  begin
    p := Pos(old, str);
    if p = 0 then
      Break;
    Delete(str, p, Length(old));
    Insert(new, str, p);
  end;
end;
{------------------------------------------------------------}
procedure ReplaceStr2(var str: string; const old, new: string);
var
  p, ptr: Integer;
  s: string;
begin
  s := str;
  ptr := 1;
  while true do
  begin
    p := Pos(old, s);
    if p=0 then
      Break;
    Inc(p, ptr-1);
    Delete(str, p, Length(old));
    Insert(new, str, p);
    ptr := p + Length(new);
    s := Copy(str, ptr, Length(str) + 1 - ptr);
  end;
end;
{------------------------------------------------------------}
function MakeHTMLStr(str: string): string;
begin
  ReplaceStr2(str, '&', '&amp');
  ReplaceStr(str, '>', '&gt');
  ReplaceStr(str, '<', '&lt');
  ReplaceStr(str, '  ', '&nbsp ');
  MakeHTMLStr := str;
end;

{------------------------------------------------------------}
function ColorCode(C: TColor): string;
var
  s: string;
begin
 s := IntToHex(ColorToRGB(c), 6);
 s := Copy(s, 5, 2) + Copy(s, 3, 2) + Copy(s, 1, 2);
 Result := '"#' + s + '"';
end;
{------------------------------------------------------------}
function GetFontSize(pts: Integer): Integer;
begin
  if pts <= 8 then
    Result := 1
  else
  begin
    case pts of
      9..10:  Result := 2;
      11..12: Result := 3;
      13..14: Result := 4;
      15..18: Result := 5;
      19..24: Result := 6;
    else
      Result := 7;
    end;
  end;
end;
{------------------------------------------------------------}
function OpenFontTag(ts: TFontInfo; normalfs: TFontStyles; Relative: Boolean): string;
var
  s: string;
begin
  s := '<FONT size=' + IntToStr(GetFontSize(ts.Size)) + ' color=' + ColorCode(ts.Color)
     + ' face="' + ts.FontName + '">';
  if Relative then
  begin
    if not (fsBold      in ts.Style) and (fsBold      in normalfs) then s := s+'</B>';
    if not (fsItalic    in ts.Style) and (fsItalic    in normalfs) then s := s+'</I>';
    if not (fsUnderline in ts.Style) and (fsUnderline in normalfs) then s := s+'</U>';
    if not (fsStrikeOut in ts.Style) and (fsStrikeOut in normalfs) then s := s+'</S>';
    if (fsBold      in ts.Style) and not (fsBold      in normalfs) then s := s+'<B>';
    if (fsItalic    in ts.Style) and not (fsItalic    in normalfs) then s := s+'<I>';
    if (fsUnderline in ts.Style) and not (fsUnderline in normalfs) then s := s+'<U>';
    if (fsStrikeOut in ts.Style) and not (fsStrikeOut in normalfs) then s := s+'<S>';
  end
  else
  begin
    if (fsBold in ts.Style)      then s := s+'<B>';
    if (fsItalic in ts.Style)    then s := s+'<I>';
    if (fsUnderline in ts.Style) then s := s+'<U>';
    if (fsStrikeOut in ts.Style) then s := s+'<S>';
  end;
  Result := s;
end;
{------------------------------------------------------------}
function CloseFontTag(ts: TFontInfo; normalfs: TFontStyles; Relative: Boolean): string;
var
  s: String;
begin
  s := '';
  if Relative then
  begin
    if (fsBold      in ts.Style) and not (fsBold      in normalfs) then s := s+'</B>';
    if (fsItalic    in ts.Style) and not (fsItalic    in normalfs) then s := s+'</I>';
    if (fsUnderline in ts.Style) and not (fsUnderline in normalfs) then s := s+'</U>';
    if (fsStrikeOut in ts.Style) and not (fsStrikeOut in normalfs) then s := s+'</S>';
    if not (fsBold      in ts.Style) and (fsBold      in normalfs) then s := s+'<B>';
    if not (fsItalic    in ts.Style) and (fsItalic    in normalfs) then s := s+'<I>';
    if not (fsUnderline in ts.Style) and (fsUnderline in normalfs) then s := s+'<U>';
    if not (fsStrikeOut in ts.Style) and (fsStrikeOut in normalfs) then s := s+'<S>';
    end
  else
  begin
    if (fsBold in ts.Style)      then s := s+'</B>';
    if (fsItalic in ts.Style)    then s := s+'</I>';
    if (fsUnderline in ts.Style) then s := s+'</U>';
    if (fsStrikeOut in ts.Style) then s := s+'</S>';
  end;
  s := s + '</FONT>';
  Result := s;
end;
{------------------------------------------------------------}
function TCustomRichView.GetNextFileName(const Path: string): string;
var
  fn: string;
begin
  while True do
  begin
    Inc(imgSaveNo);
    fn := Path + imgSavePrefix + IntToStr(imgSaveNo) + '.bmp';
    Result := fn;
    if not FileExists(fn) then
      Exit;
    if (rvsoOverrideImages in SaveOptions) and ((FileGetAttr(fn) and faReadOnly) = 0) then
      Exit;
  end;
end;
{------------------------------------------------------------}
function TCustomRichView.SavePicture(DocumentSaveFormat: TRVSaveFormat; const Path: string; gr: TGraphic): string;
var
  fn: string;
  bmp: TBitmap;
begin
  { DocumentSaveFormat in this version is ignored }
  fn := GetNextFileName(Path);
  Result := ExtractFileName(fn);
  if (gr is TBitmap) then
  begin
    gr.SaveToFile(fn);
    Exit;
  end;
  bmp := TBitmap.Create();
  try
    bmp.Height := gr.Height;
    bmp.Width := gr.Width;
    bmp.Canvas.Brush.Color := Style.Color;
    bmp.Canvas.Pen.Color := Style.Color;
    bmp.Canvas.FillRect(Rect(0, 0, Width,Height));
    bmp.Canvas.Draw(0, 0, gr);
    bmp.SaveToFile(fn);
  finally
    bmp.Free();
  end;
end;
{------------------------------------------------------------}
function TCustomRichView.SaveHTML(const FileName, Title, ImagesPrefix: string;
  Options: TRVSaveOptions): Boolean;
var
  f: TextFile;
  i, j: Integer;
  Item: TRVItem;
  VisItem, VisItem2: TRVVisualItem;
  needbr: Boolean;
  s: string;
  cpno, jumpno: Integer;
  Bullets: TStringList;
  fn: string;
  bmp: TBitmap;
  rvi: TRVInteger2;
begin
  {$I+}
  Result := False;
  if Style = nil then
    Exit;
  Result := True;
  imgSavePrefix := ImagesPrefix;
  imgSaveNo := 0;
  SaveOptions := Options;
  cpno := 0;
  jumpno := FirstJumpNo;
  Bullets := TStringList.Create();
  try
    AssignFile(f, FileName);
    Rewrite(f);
    try
      WriteLn(f, '<HTML><HEAD><TITLE>' + Title + '</TITLE></HEAD>');
      Writeln(f, '<BODY bgcolor=' + ColorCode(Style.Color));
      if (BackgroundStyle <> bsNoBitmap) and (BackgroundBitmap <> nil) then
      begin
        Writeln(f, '      background=' + SavePicture(rvsfHTML, ExtractFilePath(FileName), BackgroundBitmap));
        if (BackgroundStyle <> bsTiledAndScrolled) then
          Writeln(f, '      bgproperties=fixed');
      end;
      Writeln(f, '      leftmargin=' + IntToStr(LeftMargin) + '>');
      WriteLn(f, OpenFontTag(Style.TextStyles[rvsNormal], Style.TextStyles[rvsNormal].Style, False));
      needbr := False;
      for i := 0 to Items.Count-1 do
      begin
        Item := Items[i];
        case Item.StyleNo of
          rvsBreak:
          begin
            Writeln(f, '<HR noshade size=1>');
            needbr := False;
          end;

          rvsComponent:
          if Assigned(FOnSaveComponentToFile) then
          begin
            s := '';
            FOnSaveComponentToFile(Self, ExtractFilePath(FileName), (Item as TRVVisualItem).gr, rvsfHTML, s);
            if s <> '' then
            begin
              Writeln(f, s);
              needbr := True;
            end;
          end;

          rvsCheckPoint:
          begin
            WriteLn(f);
            WriteLn(f, '<A name=RichViewCheckPoint' + IntToStr(cpno) + '></A>');
            Inc(cpno);
          end;

          rvsPicture:
          begin
            if (Item.Alignment <> taCenter) and Item.FromNewLine then
              WriteLn(f,'<BR>');
            if Item.Alignment = taCenter then
              Write(f,'<CENTER>');
            Write(f, '<IMG src="' + SavePicture(rvsfHTML, ExtractFilePath(FileName), TGraphic((Item as TRVVisualItem).gr)) + '">');
            if Item.Alignment = taCenter then
              Write(f,'</CENTER>');
            needbr := True;
          end;

          rvsBullet, rvsHotSpot:
          begin
            VisItem := (Item as TRVVisualItem);
            if Item.FromNewLine and needbr then
              WriteLn(f,'<BR>');
            fn := '';
            for j := 0 to Bullets.Count-1 do
            begin
              VisItem2 := (Items[TRVInteger2(Bullets.Objects[j]).val] as TRVVisualItem);
              if (VisItem.gr = VisItem2.gr)
              and (VisItem.imgNo = VisItem2.imgNo) then
              begin
                fn := Bullets[j];
              end;
            end;

            if fn = '' then
            begin
              bmp := TBitmap.Create();
              try
                bmp.Width := TImageList(VisItem.gr).Width;
                bmp.Height := TImageList(VisItem.gr).Height;
                bmp.Canvas.Brush.Color := Style.Color;
                bmp.Canvas.Pen.Color := Style.Color;
                bmp.Canvas.FillRect(Rect(0, 0, Width, Height));
                TImageList(VisItem.gr).Draw(bmp.Canvas, 0, 0, VisItem.imgNo);
                fn := SavePicture(rvsfHTML, ExtractFilePath(FileName), bmp);
                rvi     := TRVInteger2.Create();
                rvi.Val := i;
                Bullets.AddObject(fn, rvi);
              finally
                bmp.Free();
              end;
            end;
            s := '';
            if Item.StyleNo = rvsHotSpot then
            begin
              if Assigned(FOnURLNeeded) then
                FOnURLNeeded(Self, jumpno, s);
              Inc(jumpno);
              if s <> '' then
                Write(f, '<A href=' + s + '>');
            end;

            Write(f, '<IMG src="' + fn + '">');
            if s <> '' then
              Write(f, '</A>');
            needbr := True;
          end;

          rvsJump1, rvsJump2:
          begin
            if (Item.Alignment <> taCenter) and Item.FromNewLine and needbr then
              WriteLn(f,'<BR>');
            if (Item.Alignment = taCenter) then
              Write(f,'<CENTER>');
            s := '';
            if Assigned(FOnURLNeeded) then
              FOnURLNeeded(Self, jumpno, s);

            Inc(jumpno);
            if s <> '' then
              Write(f, '<A href=' + s + '>');
            Write(f, OpenFontTag(Style.TextStyles[Item.StyleNo], Style.TextStyles[rvsNormal].Style, True)
                   + MakeHTMLStr(Item.Text) + CloseFontTag(Style.TextStyles[Item.StyleNo], Style.TextStyles[rvsNormal].Style, True));
            if s <> '' then
              Write(f, '</A>');
            needbr := (Item.Alignment <> taCenter);
          end;

          rvsNormal:
          begin
            if (Item.Alignment <> taCenter) and Item.FromNewLine and needbr then
              WriteLn(f, '<BR>');
            if (Item.Alignment = taCenter) then
              Write(f, '<CENTER>' + MakeHTMLStr(Item.Text) + '</CENTER>')
            else
              Write(f, MakeHTMLStr(Item.Text));
            needbr := (Item.Alignment <> taCenter);
          end;

        else
          begin
            if (Item.Alignment <> taCenter) and Item.FromNewLine and needbr then
              WriteLn(f, '<BR>');
            if (Item.Alignment = taCenter) then
              Write(f, '<CENTER>');
            Write(f, OpenFontTag(Style.TextStyles[Item.StyleNo], Style.TextStyles[rvsNormal].Style, True)
              + MakeHTMLStr(Item.Text) + CloseFontTag(Style.TextStyles[Item.StyleNo], Style.TextStyles[rvsNormal].Style, True));
            if (Item.Alignment = taCenter) then
              Write(f, '</CENTER>');
            needbr := (Item.Alignment <> taCenter);
          end;
        end;
      end;
      Writeln(f);
      WriteLn(f, CloseFontTag(Style.TextStyles[rvsNormal], Style.TextStyles[rvsNormal].Style, False));
      WriteLn(f, '</BODY></HTML>');
    finally
      for j := 0 to Bullets.Count-1 do
      begin
        TRVInteger2(Bullets.Objects[j]).Free();
        Bullets.Objects[j] := nil;
      end;
      Bullets.Free();
      CloseFile(f)
    end;
  except
    Result := False;
  end;
end;
{------------------------------------------------------------------}
function TCustomRichView.SaveText(const FileName: string; LineWidth: Integer): Boolean;
var
  f: TextFile;
  i, j: Integer;
  Item: TRVItem;
  s, s2: string;
begin
  {$I+}
  Result := True;
  s := '';
  for j := 1 to LineWidth do
    s := s + '-';

  try
    AssignFile(f, FileName);
    Rewrite(f);
    try
      for i:=0 to Items.Count-1 do
      begin
        Item := Items[i];
        case Item.StyleNo of
          rvsBreak:
          begin
            Writeln(f);
            Write(f, s);
          end;

          rvsCheckPoint: ;

          rvsComponent:
          begin
            if Item.FromNewLine then
              WriteLn(f);
            if Assigned(FOnSaveComponentToFile) then
            begin
              s2 := '';
              FOnSaveComponentToFile(Self, ExtractFilePath(FileName), (Item as TRVVisualItem).gr, rvsfText, s2);
              if s2 <> '' then
                Write(f, s2);
            end;
          end;

          rvsPicture, rvsHotSpot, rvsBullet:
          if Item.FromNewLine then
            WriteLn(f);
          else
          begin
            if Item.FromNewLine then
              WriteLn(f);
            if (Item.Alignment = taCenter) then
            begin
              s2 := '';
              for j := 1 to (LineWidth-Length(Item.Text)) div 2 do
                s2 := s2 + ' ';
              Write(f, s2 + Item.Text)
            end
            else
              Write(f, Item.Text);
          end;
        end;
      end;
    finally
      CloseFile(f)
    end;
  except
    Result := False;
  end;
end;
