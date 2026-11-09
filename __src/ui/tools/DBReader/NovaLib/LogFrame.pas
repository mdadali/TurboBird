{-----------------------------------------------------------------------------
Интерактивный журнал событий

(C) Sergey Bodrov (serbod@gmail.com)
MIT license
-----------------------------------------------------------------------------}
unit LogFrame;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, 
  Logger, Contnrs, StdCtrls, ExtCtrls, ComCtrls, ActnList, Menus, Buttons,
  ImgList, SyncObjs;

type
  TLogItem = class(TObject)
  public
    DateTime: TDateTime;
    LogLevel: TLogLevel;
    Obj: TObject;
    Text: string;
  end;

  TLogItemList = class(TObject)
  private
    FMaxCount: Integer;
    FList: TObjectList;
    FFreezeList: TObjectList;
    FFilteredList: TList;
    FFilterStr: string;
    FFilterObj: TObject;
    FFilterLogLevel: Integer;
    FFilterEnabled: Boolean;
    FFreezeEnabled: Boolean;
    // признак запрета удаления элементов списка, для избежания проблем с визуальным отображением
    FIsUpdating: Boolean;
    FLock: TCriticalSection;
    function GetItem(Index: Integer): TLogItem;
    function GetCount: Integer;
    procedure SetFilterObj(const Value: TObject);
    procedure SetFilterStr(const Value: string);
    { True, если элемент подходит под фильтр или фильтр отключен }
    function CheckFilter(AItem: TLogItem): Boolean;
    procedure SetFreeze(const Value: Boolean);
    procedure SetFilterLogLevel(const Value: Integer);
  public
    constructor Create(AMaxCount: integer);
    destructor Destroy(); override;
    property Items[Index: Integer]: TLogItem read GetItem; default;
    procedure AddItem(const AText: string; ALogLevel: TLogLevel; AObj: TObject = nil);
    procedure Clear();
    { Обновление фильтрованного списка }
    procedure UpdateFilteredList();
    { сброс фильтров и остановки прокрутки }
    procedure ResetSettings();

    procedure BeginUpdate();
    procedure EndUpdate();

    { Число элементов с учетом фильтра }
    property Count: Integer read GetCount;
    { Максимальное число элементов }
    property MaxCount: Integer read FMaxCount write FMaxCount;
    { Фильтр по строке }
    property FilterStr: string read FFilterStr write SetFilterStr;
    { Фильтр по объекту }
    property FilterObj: TObject read FFilterObj write SetFilterObj;
    { Фильтр по типу (уровню серьезности) TLogLevel, -1 - по всем }
    property FilterLogLevel: Integer read FFilterLogLevel write SetFilterLogLevel;
    { Заморозка, отображается сохраненный список событий }
    property Freeze: Boolean read FFreezeEnabled write SetFreeze;
  end;

  TFrameLog = class(TFrame)
    panLogFilter: TPanel;
    lbLogFilterText: TLabel;
    lbLogFilterObj: TLabel;
    edLogFilter: TEdit;
    cbLogFilterObj: TComboBox;
    btnLogFilterClear: TButton;
    lvLog: TListView;
    alLog: TActionList;
    actFreezeLog: TAction;
    actClearLogFilter: TAction;
    actFilterSelectedObj: TAction;
    pmLog: TPopupMenu;
    mniFreezeLog: TMenuItem;
    mniFilterSelectedObj: TMenuItem;
    mniClearLogFilter: TMenuItem;
    actClearLog: TAction;
    mniClearLog: TMenuItem;
    actEdit: TAction;
    mniEdit: TMenuItem;
    imgListLog: TImageList;
    btnPause: TSpeedButton;
    actTimeDiff: TAction;
    mniTimeDiff: TMenuItem;
    procedure lvLogCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure edLogFilterChange(Sender: TObject);
    procedure actFilterSelectedObjExecute(Sender: TObject);
    procedure actClearLogFilterExecute(Sender: TObject);
    procedure actFreezeLogExecute(Sender: TObject);
    procedure actClearLogExecute(Sender: TObject);
    procedure lvLogData(Sender: TObject; Item: TListItem);
    procedure actEditExecute(Sender: TObject);
    procedure actTimeDiffExecute(Sender: TObject);
  private
    { Private declarations }
    FLogItemsList: TLogItemList;
    FIsLogItemListExternal: Boolean;
    FTimeDiffEnabled: Boolean;
    procedure UpdateLogListItem(ListItem: TListItem; Item: TLogItem);
    procedure SetLogItemsList(const Value: TLogItemList);
  public
    { Public declarations }
    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;
    procedure UpdateLogList();
    { сброс фильтров и остановки прокрутки }
    procedure ResetSettings();
    property LogItemsList: TLogItemList read FLogItemsList write SetLogItemsList;
  end;

implementation

{$ifdef FPC}
 {$R *.lfm}
{$else}
 {$R *.dfm}
{$endif}

{ TLogItemList }

constructor TLogItemList.Create(AMaxCount: integer);
begin
  //inherited;
  FLock := TCriticalSection.Create();
  FList := TObjectList.Create(True);
  FFreezeList := TObjectList.Create(True);
  FFilteredList := TList.Create();
  FMaxCount := AMaxCount;
  FFilterStr := '';
  FFilterObj := nil;
  FFilterEnabled := False;
end;

destructor TLogItemList.Destroy;
begin
  Clear();
  FreeAndNil(FList);
  FreeAndNil(FFreezeList);
  FreeAndNil(FFilteredList);
  FreeAndNil(FLock);
  inherited;
end;

procedure TLogItemList.AddItem(const AText: string; ALogLevel: TLogLevel;
  AObj: TObject);
var
  Item: TLogItem;
begin
  Item := TLogItem.Create();
  Item.Text := AText;
  Item.LogLevel := ALogLevel;
  Item.DateTime := Now();
  Item.Obj := AObj;

  FLock.Acquire();
  try
    // добавление в списки
    FList.Add(Item);
    if FFilterEnabled and (not FFreezeEnabled) and CheckFilter(Item) then
      FFilteredList.Add(Item);

    // очистка лишних
    if (FList.Count > (MaxCount + (MaxCount div 5))) and (not FIsUpdating) then
    begin
      while FFilteredList.Count > MaxCount do
        FFilteredList.Delete(0);
        
      while FList.Count > MaxCount do
      begin
        FFilteredList.Remove(FList[0]);
        FList.Delete(0);
      end;
    end;
  finally
    FLock.Release();
  end;
end;

procedure TLogItemList.Clear();
begin
  FLock.Acquire();
  try
    FList.Clear();
    FFilteredList.Clear();
  finally
    FLock.Release();
  end;
end;

function TLogItemList.GetCount: Integer;
begin
  if FFilterEnabled then
    Result := FFilteredList.Count
  else if FFreezeEnabled then
    Result := FFreezeList.Count
  else
    Result := FList.Count;
end;

function TLogItemList.GetItem(Index: Integer): TLogItem;
begin
  if FFilterEnabled then
    Result := TLogItem(FFilteredList.Items[Index])
  else if FFreezeEnabled then
    Result := TLogItem(FFreezeList.Items[Index])
  else
    Result := TLogItem(FList.Items[Index]);
end;

procedure TLogItemList.SetFilterObj(const Value: TObject);
begin
  FFilterObj := Value;
  FFilterEnabled := (Length(FFilterStr) > 0) or Assigned(FFilterObj);
  UpdateFilteredList();
end;

procedure TLogItemList.SetFilterStr(const Value: string);
begin
  FFilterStr := AnsiLowerCase(Trim(Value));
  FFilterEnabled := (Length(FFilterStr) > 0) or Assigned(FFilterObj);
  UpdateFilteredList();
end;

procedure TLogItemList.SetFilterLogLevel(const Value: Integer);
begin
  FFilterLogLevel := Value;
end;

function TLogItemList.CheckFilter(AItem: TLogItem): Boolean;
begin
  Result := True;
  if FFilterEnabled and Assigned(AItem) then
  begin
    if Assigned(FFilterObj) and (FFilterObj <> AItem.Obj) then
      Result := False
    else if (FFilterStr <> '')
    and (Pos(FFilterStr, AnsiLowerCase(AItem.Text)) = 0)
    and (Pos(FFilterStr, FormatDateTime('hh:nn:ss.zzz', AItem.DateTime)) = 0)
    then
      Result := False;
  end;
end;

procedure TLogItemList.UpdateFilteredList();
var
  i: Integer;
  Item: TLogItem;
begin
  FLock.Acquire();
  try
    FFilteredList.Clear();
    if FFilterEnabled then
    begin
      if FFreezeEnabled then
      begin
        for i := 0 to FFreezeList.Count-1 do
        begin
          Item := TLogItem(FFreezeList.Items[i]);
          if CheckFilter(Item) then
            FFilteredList.Add(Item);
        end;
      end
      else
      begin
        for i := 0 to FList.Count-1 do
        begin
          Item := TLogItem(FList.Items[i]);
          if CheckFilter(Item) then
            FFilteredList.Add(Item);
        end;
      end;
    end
  finally
    FLock.Release();
  end;
end;

procedure TLogItemList.SetFreeze(const Value: Boolean);
var
  i: Integer;
  Item, NewItem: TLogItem;
begin
  FLock.Acquire();
  try
    FFreezeEnabled := Value;
    if FFreezeEnabled then
    begin
      FFreezeList.Clear();
      // копирование элементов в список заморозки
      for i := 0 to FList.Count-1 do
      begin
        Item := TLogItem(FList.Items[i]);
        NewItem := TLogItem.Create();
        NewItem.Text := Item.Text;
        NewItem.LogLevel := Item.LogLevel;
        NewItem.DateTime := Item.DateTime;
        NewItem.Obj := Item.Obj;
        FFreezeList.Add(NewItem);
      end;
    end
    else
    begin
      FFreezeList.Clear();
    end;
  finally
    FLock.Release();
  end;
  UpdateFilteredList();
end;

procedure TLogItemList.BeginUpdate();
begin
  FIsUpdating := True;
  FLock.Acquire();
end;

procedure TLogItemList.EndUpdate();
begin
  FIsUpdating := False;
  FLock.Release();
end;

procedure TLogItemList.ResetSettings();
begin
  FilterObj := nil;
  FilterStr := '';
  FilterLogLevel := -1;
  Freeze := False;
end;

{ TFrameLog }

procedure TFrameLog.AfterConstruction;
begin
  inherited;
  FLogItemsList := TLogItemList.Create(10000);
  FIsLogItemListExternal := False;
  lvLog.DoubleBuffered := True;
  FTimeDiffEnabled := False;
end;

procedure TFrameLog.BeforeDestruction;
begin
  if not FIsLogItemListExternal then
    FreeAndNil(FLogItemsList);
  inherited;
end;

procedure TFrameLog.lvLogCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  LogItem: TLogItem;
  n: Integer;
begin
  if Assigned(FLogItemsList) and Assigned(Item) and Assigned(Item.Owner) and Assigned(Item.Owner.Owner) then
  begin
    FLogItemsList.BeginUpdate();
    try
      n := FLogItemsList.Count - Item.Index - 1;
      if (n >= 0) and (n < FLogItemsList.Count) then
      begin
        LogItem := FLogItemsList.GetItem(n);
        case LogItem.LogLevel of
           llError  : Sender.Canvas.Brush.Color := clRed;     // Error
           llWarning: Sender.Canvas.Brush.Color := clYellow;  // Warning
           llNotify : Sender.Canvas.Brush.Color := clLime;    // Notify
           llInfo   : Sender.Canvas.Brush.Color := clWindow;  // Info
           llDebug  : Sender.Canvas.Brush.Color := clWindow;  // Debug
           //llDebug  : Sender.Canvas.Brush.Color := clSkyBlue; // Debug
        else
          Sender.Canvas.Brush.Color := clSkyBlue;  // ?
        end;
      end;
    finally
      FLogItemsList.EndUpdate();
    end;
  end;
end;

procedure TFrameLog.lvLogData(Sender: TObject; Item: TListItem);
var
  LogItem: TLogItem;
  n: Integer;
begin
  //LogItem := nil;
  if Assigned(FLogItemsList) and Assigned(Item) and Assigned(Item.Owner) and Assigned(Item.Owner.Owner) then
  begin
    FLogItemsList.BeginUpdate();
    try
      n := FLogItemsList.Count - Item.Index - 1;
      if (n >= 0) and (n < FLogItemsList.Count) then
      begin
        LogItem := FLogItemsList.GetItem(n);
        UpdateLogListItem(Item, LogItem);
      end
      else
      begin
        Item.Caption := '';
        Item.SubItems.Clear();
      end;
    finally
      FLogItemsList.EndUpdate();
    end;
  end;
end;

procedure TFrameLog.UpdateLogListItem(ListItem: TListItem; Item: TLogItem);
var
  n: Integer;
  TmpItem: TLogItem;
begin
  if Assigned(Item) and Assigned(ListItem) then
  begin
    ListItem.Data := Item;
    // Время
    if not FTimeDiffEnabled then
      ListItem.Caption := FormatDateTime('hh:nn:ss.zzz', Item.DateTime)
    else
    begin
      // разница времени
      n := FLogItemsList.Count - ListItem.Index - 1;
      if n > 0 then
      begin
        TmpItem := FLogItemsList.GetItem(n-1);
        ListItem.Caption := FormatDateTime('hh:nn:ss.zzz', Item.DateTime - TmpItem.DateTime);
      end
      else
        ListItem.Caption := FormatDateTime('hh:nn:ss.zzz', Item.DateTime);
    end;
    // Сообщение
    ListItem.SubItems.Append(Copy(Item.Text, 1, MaxInt));
  end;
end;

procedure TFrameLog.UpdateLogList();
begin
  if Assigned(FLogItemsList) then
  begin
    FLogItemsList.BeginUpdate();
    try
      if (lvLog.Items.Count <> FLogItemsList.Count) then
        lvLog.Items.Count := FLogItemsList.Count;
      lvLog.Invalidate();
    finally
      FLogItemsList.EndUpdate();
    end;
  end
  else
  begin
    lvLog.Items.Count := 0;
    lvLog.Invalidate();
  end;
end;

procedure TFrameLog.edLogFilterChange(Sender: TObject);
begin
  if Assigned(FLogItemsList) then
  begin
    FLogItemsList.FilterStr := Trim(edLogFilter.Text);
    UpdateLogList();
  end;
end;

procedure TFrameLog.actFilterSelectedObjExecute(Sender: TObject);
var
  LogItem: TLogItem;
begin
  if Assigned(FLogItemsList) and Assigned(lvLog.Selected) and Assigned(lvLog.Selected.Data) then
  begin
    LogItem := TLogItem(lvLog.Selected.Data);
    FLogItemsList.FilterObj := LogItem.Obj;
    cbLogFilterObj.Text := 'Фильтр';
    UpdateLogList();
  end;
end;

procedure TFrameLog.actClearLogFilterExecute(Sender: TObject);
begin
  if Assigned(FLogItemsList) then
  begin
    FLogItemsList.FilterObj := nil;
    cbLogFilterObj.Text := '';
    UpdateLogList();
  end;

end;

procedure TFrameLog.actFreezeLogExecute(Sender: TObject);
begin
  if Assigned(FLogItemsList) then
  begin
    FLogItemsList.Freeze := not FLogItemsList.Freeze;
    actFreezeLog.Checked := FLogItemsList.Freeze;
    if FLogItemsList.Freeze then
    begin
      lvLog.Hint := 'Двойной щелчок - включить прокрутку';
      actFreezeLog.ImageIndex := 1;
      actFreezeLog.Caption := 'Возобновить прокрутку';
    end
    else
    begin
      lvLog.Hint := 'Двойной щелчок - приостановить прокрутку';
      actFreezeLog.ImageIndex := 0;
      actFreezeLog.Caption := 'Остановить прокрутку';
    end;

    btnPause.Glyph := nil;
    imgListLog.GetBitmap(actFreezeLog.ImageIndex, btnPause.Glyph);

    UpdateLogList();
  end;
end;

procedure TFrameLog.actClearLogExecute(Sender: TObject);
begin
  if Assigned(FLogItemsList) then
  begin
    FLogItemsList.Clear();
    FLogItemsList.Freeze := False;
    actFreezeLog.Checked := FLogItemsList.Freeze;
    UpdateLogList();
  end;
end;

procedure TFrameLog.actEditExecute(Sender: TObject);
var
  LogItem: TLogItem;
  i: Integer;
  TmpForm: TForm;
  TmpMemo: TMemo;
begin
  if Assigned(FLogItemsList) and Assigned(lvLog.Selected) and Assigned(lvLog.Selected.Data) then
  begin
    LogItem := TLogItem(lvLog.Selected.Data);

    // создаем временную форму
    TmpForm := TForm.Create(Self);
    try
      TmpForm.Height := 200;
      TmpForm.Width := 700;
      TmpForm.Caption := 'Выделенный фрагмент';
      TmpForm.Position := poScreenCenter;

      TmpMemo := TMemo.Create(TmpForm);
      TmpMemo.Align := alClient;
      TmpMemo.Parent := TmpForm;
      TmpMemo.ScrollBars := ssBoth;

      TmpMemo.Lines.Clear();

      // помещаем выделенные элементы в текст
      if lvLog.SelCount > 1 then
      begin
        // отбор выделенных
        for i := lvLog.Items.Count-1 downto 0 do
        begin
          if lvLog.Items[i].Selected then
          begin
            LogItem := TLogItem(lvLog.Items[i].Data);
            TmpMemo.Lines.Append(FormatDateTime('YYYY-MM-DD HH:NN:SS.ZZZ ', LogItem.DateTime)+' '+LogItem.Text);
          end;
        end;
      end
      else
      begin
        TmpMemo.Lines.Append(FormatDateTime('YYYY-MM-DD HH:NN:SS.ZZZ ', LogItem.DateTime)+' '+LogItem.Text);
      end;

      TmpForm.ShowModal();
    finally
      TmpForm.Free();
    end;
  end;
end;

procedure TFrameLog.ResetSettings();
begin
  edLogFilter.Text := '';
  actClearLogFilter.Execute();
  if actFreezeLog.Checked then
    actFreezeLog.Execute();
end;

procedure TFrameLog.actTimeDiffExecute(Sender: TObject);
begin
  FTimeDiffEnabled := not FTimeDiffEnabled;
  actTimeDiff.Checked := FTimeDiffEnabled;
  UpdateLogList();
end;

procedure TFrameLog.SetLogItemsList(const Value: TLogItemList);
begin
  if (not FIsLogItemListExternal) then
    FreeAndNil(FLogItemsList);
  FLogItemsList := Value;
  if not Assigned(FLogItemsList) then
  begin
    FLogItemsList := TLogItemList.Create(10000);
    FIsLogItemListExternal := False;
  end
  else
    FIsLogItemListExternal := True;
end;

end.
