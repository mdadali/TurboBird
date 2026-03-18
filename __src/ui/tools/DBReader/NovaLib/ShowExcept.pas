unit ShowExcept;
 
interface
 
uses
  SysUtils,
  Classes;
 
implementation
 
type
  PRaiseFrame = ^TRaiseFrame;
  TRaiseFrame = packed record
    NextRaise: PRaiseFrame;
    ExceptAddr: Pointer;
    ExceptObject: TObject;
    ExceptionRecord: PExceptionRecord;
  end;
 
var
  // ”казатель на вершину списка исключений
  CurrentRaiseList: Pointer = nil;
 
// ‘ункци€ возвращ€ет текущее исключение со стека
function GetNextException: Pointer;
begin
  if CurrentRaiseList = nil then CurrentRaiseList := RaiseList;
  if CurrentRaiseList <> nil then
  begin
    Result := PRaiseFrame(CurrentRaiseList)^.ExceptObject;
    PRaiseFrame(CurrentRaiseList)^.ExceptObject := nil;
    CurrentRaiseList := PRaiseFrame(CurrentRaiseList)^.NextRaise;
  end
  else
    Result := nil;
end;
 
var
  ExceptionStack: TList;
  E: Exception;  
 
initialization
 
finalization
 
  // —мотрим, есть ли вообще исключени€?
  E := GetNextException;
 
  if E <> nil then
  begin
    ExceptionStack := TList.Create;
    try
 
      // если есть, собираем о них информацию
      while E <> nil do
      begin
        ExceptionStack.Add(E);
        E := GetNextException;
      end;
 
      // и отображаем их в том пор€дке, в котором они произошли
      while ExceptionStack.Count > 0 do
      begin
        E := ExceptionStack[ExceptionStack.Count - 1];
        ExceptionStack.Delete(ExceptionStack.Count - 1);
        ShowException(E, ExceptAddr);
        E.Free;
      end;
    finally
      ExceptionStack.Free;
    end;
 
    // финализируем все что осталось
    Halt(1);
  end;
end.