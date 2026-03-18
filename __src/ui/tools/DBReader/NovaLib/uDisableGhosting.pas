unit uDisableGhosting;

// ёнит отключает в WinXP и выше механизм Window Ghosting
// тем самым решаетс€ проблема с модальными окнами под видимой формой и
// возможностью двигать и закрывать модальные окна не верхнего уровн€.
// с другой стороны, если приложение действительно зан€то, то убивать его придетс€
// с помощью дистпетчера задач.
// Ќичего вызывать не надо. ƒостаточно просто прописать в Uses.

interface

uses Windows;

implementation

procedure DisableGhosting;
var
  User32: HMODULE;
  DisableProcessWindowsGhosting: procedure;
begin
  User32:=GetModuleHandle('USER32');
  if User32<>0 then begin
    DisableProcessWindowsGhosting:=GetProcAddress(User32,'DisableProcessWindowsGhosting');
    if assigned(DisableProcessWindowsGhosting) then
      DisableProcessWindowsGhosting;
  end;
end;

initialization
  DisableGhosting;

end.
