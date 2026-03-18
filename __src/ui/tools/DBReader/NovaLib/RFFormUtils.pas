{-----------------------------------------------------------------------------
Функции для форм и фреймов

(C) Sergey Bodrov (serbod@gmail.com)
MIT license
-----------------------------------------------------------------------------}
unit RFFormUtils;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
  LCLIntf, LCLType, LMessages, LazUTF8,
  {$ENDIF}
  Windows,
  SysUtils, Classes, Messages, Types, Forms,
  StdCtrls, ComCtrls, Controls, Clipbrd;

// вызов справки
procedure ShowHelp();
// вопрос да или нет
function ShowQuestionYesNo(ASender: TObject; AText: string; ACaption: string = ''): Boolean;
// предупреждение, вопрос да или нет
function ShowQuestionYesNoWarning(ASender: TObject; AText: string; ACaption: string = ''): Boolean;
// информационное сообщение
procedure ShowMessage(ASender: TObject; AText: string; ACaption: string = '');
// предупреждение
procedure ShowMessageWarning(ASender: TObject; AText: string; ACaption: string = '');
// сообщение об ошибке
procedure ShowMessageError(ASender: TObject; AText: string; ACaption: string = '');
// показать лог-файл (обычно тут заглушка)
procedure ShowLogFile(ASender: TObject; AFileName: string; ACaption: string = '');

// фильтрация ввода IP-адреса в TEdit
procedure IpEditOnKeyPress(Sender: TObject; var Key: char);
// проверка правильности введенного IP-адреса, True если все правильно
function ValidateIpStr(AIpStr: string): Boolean;
// нормализация строки IP-адреса
function NormalizeIpStr(AIpStr: string): string;

// установка стиля "редактирование числа" для TEdit (windows edit control)
// параметр Handle = TEdit.Handle
procedure SetNumEditStyle(Handle: THandle);
// установка стиля "ввод пароля" для TEdit (windows edit control)
// параметр Handle = TEdit.Handle
// PasswordChar - символ замены текста пароля
procedure SetPasswordEditStyle(Handle: THandle; PasswordChar: Integer);

// сохранение/восстановление размеров колонок списка
procedure ListColumnsSizeToStr(AListView: TListView; var AStr: string);
procedure ListColumnsSizeFromStr(AListView: TListView; const AStr: string);

// размеры формы в TRect
procedure FormSizeToRect(AForm: TScrollingWinControl; var ARect: TRect);
procedure FormSizeFromRect(AForm: TScrollingWinControl; var ARect: TRect);

// сохранение/восстановление размеров формы
procedure FormSizeToStr(AForm: TScrollingWinControl; var AStr: string);
procedure FormSizeFromStr(AForm: TScrollingWinControl; const AStr: string);

// для уменьшения напрасной перерисовки
procedure UpdateLabelCaption(Item: TCustomLabel; Text: string);

// Установить текст в буфере обмена (с учетом кодировки)
procedure SetClipboardText(const Text: string);
function GetClipboardText(): string;

// Копирование содержимого списка в буфер обмена
// AColSeparator - разделитель колонок (по умолчанию Tab)
procedure ListViewToClipboard(lv: TListView; AColSeparator: string = #09);


implementation

uses RFUtils;

{$ifndef FPC}
function GetKeyShiftState(): TShiftState;
begin
  Result := [];
  if GetKeyState(VK_CONTROL) < 0 then
    Include(Result, ssCtrl);
  if GetKeyState(VK_SHIFT) < 0 then
    Include(Result, ssShift);
  if GetKeyState(VK_MENU) < 0 then
    Include(Result, ssAlt);
  //if (GetKeyState(VK_LWIN) < 0) or (GetKeyState(VK_RWIN) < 0) then
  //  Include(Result, ssMeta);
end;
{$endif FPC}

// вызов справки
procedure ShowHelp();
var
  HelpKeyword: String;
  Control: TWinControl;
begin
  //if FileExists(Application.HelpFile) then
  //  ShellExecute(0, 'open', 'hh.exe', PChar('"' + Application.HelpFile + '::/' + PageControl.ActivePage.HelpKeyword + '.htm"'), '', SW_SHOWNORMAL);

  HelpKeyword := '';
  if Screen.ActiveControl <> nil then
  begin
    Control := Screen.ActiveControl;
    repeat
      HelpKeyword := Control.HelpKeyword;
      Control := Control.Parent;
    until (HelpKeyword <> '') or (Control = nil);
  end
  else
    HelpKeyword := 'Info';
  {$IFNDEF FPC}
  Application.HelpJump(HelpKeyword);
  {$ENDIF}
end;

function ShowQuestionYesNo(ASender: TObject; AText: string; ACaption: string = ''): Boolean;
var
  h: HWND;
begin
  h := 0;
  //h := Application.Handle;
  if Assigned(ASender) and (ASender is TWinControl) then
    h := (ASender as TWinControl).Handle;
  if ACaption = '' then
    ACaption := Application.Title;
  Result := (MessageBox(h, PChar(AText), PChar(ACaption), MB_YESNO or MB_DEFBUTTON2 or MB_ICONQUESTION or MB_TOPMOST or MB_TASKMODAL) = IDYES);
end;

function ShowQuestionYesNoWarning(ASender: TObject; AText: string; ACaption: string = ''): Boolean;
var
  h: HWND;
begin
  h := 0;
  if Assigned(ASender) and (ASender is TWinControl) then
    h := (ASender as TWinControl).Handle;
  if ACaption = '' then
    ACaption := Application.Title;
  Result := (MessageBox(h, PChar(AText), PChar(ACaption), MB_YESNO or MB_DEFBUTTON2 or MB_ICONWARNING or MB_TOPMOST or MB_TASKMODAL) = IDYES);
end;

procedure ShowMessage(ASender: TObject; AText: string; ACaption: string = '');
var
  h: HWND;
begin
  h := 0;
  if Assigned(ASender) and (ASender is TWinControl) then
    h := (ASender as TWinControl).Handle;
  if ACaption = '' then
    ACaption := Application.Title;
  MessageBox(h, PChar(AText), PChar(ACaption), MB_ICONINFORMATION or MB_TOPMOST or MB_TASKMODAL);
end;

procedure ShowMessageWarning(ASender: TObject; AText: string; ACaption: string = '');
var
  h: HWND;
begin
  h := 0;
  if Assigned(ASender) and (ASender is TWinControl) then
    h := (ASender as TWinControl).Handle;
  if ACaption = '' then
    ACaption := Application.Title;
  MessageBox(h, PChar(AText), PChar(ACaption), MB_ICONWARNING or MB_TOPMOST or MB_TASKMODAL);
end;

procedure ShowMessageError(ASender: TObject; AText: string; ACaption: string = '');
var
  h: HWND;
begin
  h := 0;
  if Assigned(ASender) and (ASender is TWinControl) then
    h := (ASender as TWinControl).Handle;
  if ACaption = '' then
    ACaption := Application.Title;
  MessageBox(h, PChar(AText), PChar(ACaption), MB_ICONERROR or MB_TOPMOST or MB_TASKMODAL);
end;

procedure ShowLogFile(ASender: TObject; AFileName: string; ACaption: string = '');
begin
  if FileExists(AFileName) then
    //ShellExecute(0, 'open', PChar(AFileName), '', '', SW_SHOWNORMAL)
  else
    ShowMessageWarning(ASender, 'Журнал событий отсутствует');
end;

// фильтрация ввода IP-адреса в TEdit
procedure IpEditOnKeyPress(Sender: TObject; var Key: char);
var
  s, sText, sNum: string;
  i, iSelStart, nByte, n, nStart, nDotCount: Integer;
  IpBytes: array [0..3] of Byte;
  IpByteStart: array [0..3] of Integer;
  ed: TCustomEdit;
begin
  if not (Sender is TCustomEdit) then Exit;
  ed := (Sender as TCustomEdit);
  sText := ed.Text;

  // Чтение адреса
  for i := Low(IpBytes) to High(IpBytes) do
    IpBytes[i] := 0;
  for i := Low(IpByteStart) to High(IpByteStart) do
    IpByteStart[i] := 0;
  nByte := 0;
  nDotCount := 0;
  nStart := 1;
  for i := 1 to Length(sText) do
  begin
    if (Copy(sText, i, 1) = '.') and (nByte < High(IpByteStart)) then
    begin
      // часть IP
      n := StrToIntDef(Copy(sText, nStart, i-nStart), 0);
      if n > 255 then
        n := 0;
      if nByte <= High(IpBytes) then
      begin
        IpByteStart[nByte] := nStart;
        IpBytes[nByte] := Byte(n);
      end;
      Inc(nDotCount);
      Inc(nByte);
      nStart := i+1;
    end;
  end;
  // последняя часть IP
  //if nStart <= Length(sText) then
  if nByte <= High(IpBytes) then
  begin
    n := StrToIntDef(Copy(sText, nStart, 99), 0);
    if n > 255 then
      n := 0;
    if nByte <= High(IpBytes) then
    begin
      IpByteStart[nByte] := nStart;
      IpBytes[nByte] := Byte(n);
    end;
  end;

  // номер знака строки, после которого стоит курсор, 0..
  iSelStart := ed.SelStart;
  // определение текущей части
  nByte := 0;
  for i := Low(IpByteStart) to High(IpByteStart) do
  begin
    if (IpByteStart[i] <> 0) and (IpByteStart[i] <= (iSelStart + 1)) then
      nByte := i;
  end;

  if (Pos(Key, '1234567890') > 0) and ((iSelStart - IpByteStart[nByte]) < 2) then
  begin
    // имеющееся число не длинней 3 знаков (для учета ведущих нулей)
    if (IpBytes[nByte] > 9) then
    begin
      // вставляем цифру в имеющийся номер и смотрим, что получится
      // позиция курсора внутри числа
      nStart := iSelStart + 1 - IpByteStart[nByte];
      s := IntToStr(IpBytes[nByte]);
      if nStart = 0 then
        sNum := Key + s
      else
      if nStart = 1 then
        sNum := Copy(s, 1, 1) + Key + Copy(s, 2, 2)
      else
        sNum := s + Key;

      n := StrToIntDef(sNum, 333);
      if (n <= 255) then
        Exit // цифра уместна
      else
        Key := #0;
    end
  end
  else
  if Pos(Key, '.,- ') > 0 then
  begin
    if nDotCount >= 3 then
      Key := #0  // не больше 3-х точек
    else
    //if iSelStart < Length(sText) then
    //  Key := #0  // можно только в конец строки
    if (iSelStart = 0) or (Copy(sText, iSelStart, 1) = '.') then
      Key := #0  // в начале байта нельзя
    else
    if nByte = High(IpBytes) then
      Key := #0  // после последнего байта нельзя
    else
      Key := '.';
  end
  else
  if Key = #8 then
  begin
    // Backspace
  end
  else
  if (Pos(Key, 'cCvVdD'+#22+#3+#4) > 0) and (ssCtrl in GetKeyShiftState()) then
  begin
    // Ctrl-C, Ctrl-V
  end
  else
  begin
    Key := #0;
  end;
end;

function ValidateIpStr(AIpStr: string): Boolean;
var
  i, n: Integer;
  s, ss: string;
begin
  Result := False;
  ss := Copy(AIpStr, 1, MaxInt);
  for i := 0 to 3 do
  begin
    if (i < 3) and (Pos('.', ss) = 0) then
      Exit;
    s := ExtractFirstWord(ss, '.');
    n := StrToIntDef(s, -1);
    if (n < 0) or (n > 255) then
      Exit;
  end;
  Result := True;
end;

// нормализация строки IP-адреса
function NormalizeIpStr(AIpStr: string): string;
var
  i, n: Integer;
  s, ss: string;
begin
  Result := '';
  ss := Copy(AIpStr, 1, MaxInt);
  for i := 0 to 3 do
  begin
    s := ExtractFirstWord(ss, '.');
    n := StrToIntDef(s, -1);
    if (n < 0) or (n > 255) then
      n := 0;

    if Result <> '' then
      Result := Result + '.';
    Result := Result + IntToStr(n);
  end;
end;

// установка стиля "редактирование числа" для TEdit (windows edit control)
// параметр Handle = TEdit.Handle
procedure SetNumEditStyle(Handle: THandle);
begin
  SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) or ES_NUMBER or ES_RIGHT);
end;

// установка стиля "ввод пароля" для TEdit (windows edit control)
// параметр Handle = TEdit.Handle
procedure SetPasswordEditStyle(Handle: THandle; PasswordChar: Integer);
begin
  SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) or ES_PASSWORD);
  SendMessage(Handle, EM_SETPASSWORDCHAR, PasswordChar, 0); // юникод 'black circle' -
end;

// сохранение/восстановление размеров колонок списка
procedure ListColumnsSizeToStr(AListView: TListView; var AStr: string);
var
  i: Integer;
begin
  // настройки ширины колонок списка
  AStr := '';
  for i := 0 to AListView.Columns.Count - 1 do
  begin
    if i > 0 then
      AStr := AStr + ',';
    AStr := AStr + IntToStr(AListView.Columns.Items[i].Width);
  end;
end;

procedure ListColumnsSizeFromStr(AListView: TListView; const AStr: string);
var
  i, n: Integer;
  s: string;
begin
  // настройки ширины колонок списка
  s := AStr;
  for i := 0 to AListView.Columns.Count - 1 do
  begin
    n := StrToIntDef(ExtractFirstWord(s, ','), AListView.Columns.Items[i].Width);
    if (n >= 0) and (n < 9999) then
      AListView.Columns.Items[i].Width := n;
  end;
end;

procedure FormSizeToRect(AForm: TScrollingWinControl; var ARect: TRect);
begin
  if not Assigned(AForm) then Exit;
  ARect.Top := AForm.Top;
  ARect.Left := AForm.Left;
  ARect.Right := AForm.Left + AForm.Width;
  ARect.Bottom := AForm.Top + AForm.Height;
end;

procedure FormSizeFromRect(AForm: TScrollingWinControl; var ARect: TRect);
begin
  if not Assigned(AForm) then Exit;
  if (ARect.Right <= ARect.Left) or (ARect.Bottom <= ARect.Top) then Exit;
  AForm.Top := ARect.Top;
  AForm.Left := ARect.Left;
  AForm.Width := ARect.Right - ARect.Left;
  AForm.Height := ARect.Bottom - ARect.Top;
end;

// сохранение/восстановление размеров формы
procedure FormSizeToStr(AForm: TScrollingWinControl; var AStr: string);
begin
  AStr := IntToStr(AForm.Top)
  +','+IntToStr(AForm.Left)
  +','+IntToStr(AForm.Width)
  +','+IntToStr(AForm.Height);
end;

procedure FormSizeFromStr(AForm: TScrollingWinControl; const AStr: string);
var
  s: string;
begin
  s := AStr;
  AForm.Top := StrToIntDef(ExtractFirstWord(s, ','), AForm.Top);
  AForm.Left := StrToIntDef(ExtractFirstWord(s, ','), AForm.Left);
  AForm.Width := StrToIntDef(ExtractFirstWord(s, ','), AForm.Width);
  AForm.Height := StrToIntDef(ExtractFirstWord(s, ','), AForm.Height);
end;

procedure UpdateLabelCaption(Item: TCustomLabel; Text: string);
begin
  // для уменьшения напрасной перерисовки
  if (Length(Item.Caption) <> Length(Text))
  or (Item.Caption <> Text)
  then
    Item.Caption := Text;
end;

procedure SetClipboardText(const Text: string);
{$ifndef FPC}
var
  wText: WideString;
  Count: Integer;
  Handle: HGLOBAL;
  Ptr: Pointer;
{$endif}
begin
{$ifndef FPC}
  Count := (Length(Text) + 1) * SizeOf(WideChar);
  wText := Text;
  StringToWideChar(Text, PWideChar(wText), Count);
  Handle := GlobalAlloc(GMEM_MOVEABLE, Count);
  try
    //Win32Check(Handle <> 0);
    if not (Handle <> 0) then RaiseLastOSError;
    Ptr := GlobalLock(Handle);
    //Win32Check(Assigned(Ptr));
    if not Assigned(Ptr) then RaiseLastOSError;
    Move(PWideChar(wText)^, Ptr^, Count);
    GlobalUnlock(Handle);
    Clipboard.SetAsHandle(CF_UNICODETEXT, Handle);
  except
    GlobalFree(Handle);
    raise;
  end;
{$else}
  Clipboard.AsText := Text;
{$endif}
end;

function GetClipboardText(): string;
{$ifndef FPC}
var
  pText: PWideChar;
  Handle: HGLOBAL;
{$endif}
begin
{$ifndef FPC}
  Clipboard.Open();
  try
    Handle := Clipboard.GetAsHandle(CF_UNICODETEXT);
    pText := GlobalLock(Handle);
    Result := WideCharToString(pText);
    GlobalUnlock(Handle);
  finally
    Clipboard.Close();
  end;
{$else}
  Result := Clipboard.AsText;
{$endif}
end;

procedure ListViewToClipboard(lv: TListView; AColSeparator: string);
var
  sl: TStringList;
  i, ii: Integer;
  s : string;
  ListItem: TListItem;
begin
  sl := TStringList.Create();
  try
    // шапка таблицы
    s := '';
    for i := 0 to lv.Columns.Count-1 do
    begin
      if i > 0 then
        s := s + AColSeparator;
      s := s + lv.Columns[i].Caption;
    end;
    sl.Append(s);
    
    for i := 0 to lv.Items.Count-1 do
    begin
      ListItem := lv.Items[i];
      s := ListItem.Caption;
      for ii := 0 to ListItem.SubItems.Count-1 do
      begin
        s := s + AColSeparator + ListItem.SubItems[ii];
      end;
      sl.Append(s);
    end;
    SetClipboardText(sl.Text);
  finally
    sl.Free();
  end;
end;

end.
