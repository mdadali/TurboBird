unit RVItems;

{$IFDEF FPC}
  {$mode Delphi}
{$ENDIF}

interface

{$I RV_Defs.inc}

uses
  SysUtils, Classes, Controls;

type

  TRVItem = class
  public
    Left: Integer;
    Top: Integer;
    Width: Integer;
    Height: Integer;
    ItemIndex: Integer;     // index in Items[]
    StyleNo: Integer;
    // text item
    JumpId: Integer;
    Alignment: TAlignment;
    TextOffs: Integer;      // text part offset, for subitems. In original item always 1
    IsSubItem: Boolean;     // item in VisItems only, parent is ItemIndex
    FromNewLine: Boolean;
    Text: string;
  end;

  TRVItemList = class(TList)
  protected
    FSubItemCount: Integer;
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    { free item when item deleted }
    OwnItems: Boolean;
    constructor Create(AOwnItems: Boolean);
    function GetItem(Index: Integer): TRVItem;
    property Items[Index: Integer]: TRVItem read GetItem; default;
    { Add item to list. If SubItemCount less than Count, return last subitem }
    function AddSubItem(): TRVItem;
    { Do not actually delete items, only reset SubItemCount }
    procedure ClearSubitems();
    { Remove item and following subitems }
    procedure RemoveItem(AItem: TRVItem);
    property SubItemCount: Integer read FSubItemCount;
  end;

  {------------------------------------------------------------------}
  { Images, Controls, Drawables }
  TRVVisualItem = class(TRVItem)
  public
    imgNo: Integer; { for rvsJump# used as jump id }
    gr: TPersistent;
    Shared: Boolean;
    DataPtr: Pointer;
  end;

  {------------------------------------------------------------------}
  { Checkpoint }
  TCPInfo = class
  public
    Y: Integer;
    ItemIndex: Integer;
    Text: string;
  end;

  { TCPInfoList }

  TCPInfoList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function GetItem(Index: Integer): TCPInfo;
    property Items[Index: Integer]: TCPInfo read GetItem; default;
  end;
  {------------------------------------------------------------------}
  TJumpInfo = class
  public
    l,t,w,h: Integer;
    id: Integer;
    VisibleItemId: Integer;
    Text: string;
  end;

  { TJumpInfoList }

  TJumpInfoList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function GetItem(Index: Integer): TJumpInfo;
    property Items[Index: Integer]: TJumpInfo read GetItem; default;
  end;

  TRVTableColInfo = class
  public
    TableItemId: Integer;
    ColNum: Integer;
    l, t, w, h: Integer;
  end;

implementation

{ TJumpInfoList }

function TJumpInfoList.GetItem(Index: Integer): TJumpInfo;
begin
  Result := TJumpInfo(Get(Index));
end;

procedure TJumpInfoList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Action = lnDeleted then
    TJumpInfo(Ptr).Free();
end;

{ TCPInfoList }

function TCPInfoList.GetItem(Index: Integer): TCPInfo;
begin
  Result := TCPInfo(Get(Index));
end;

procedure TCPInfoList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Action = lnDeleted then
    TCPInfo(Ptr).Free();
end;

{ TRVItemList }

function TRVItemList.AddSubItem(): TRVItem;
begin
  if FSubItemCount < Count then
  begin
    Result := Items[FSubItemCount];
    {Result.Left := 0;
    Result.Top := 0;
    Result.Width := 0;
    Result.Height := 0;
    Result.ItemIndex := 0;
    Result.Text := '';  }
  end
  else
  begin
    Result := TRVItem.Create();
    Add(Result);
  end;
  Inc(FSubItemCount);
end;

procedure TRVItemList.ClearSubitems();
begin
  FSubItemCount := 0;
end;

constructor TRVItemList.Create(AOwnItems: Boolean);
begin
  inherited Create();
  OwnItems := AOwnItems;
  FSubItemCount := 0;
end;

function TRVItemList.GetItem(Index: Integer): TRVItem;
begin
  Result := TRVItem(Get(Index));
end;

procedure TRVItemList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if (Action = lnDeleted) and OwnItems then
    TRVItem(Ptr).Free();
end;

procedure TRVItemList.RemoveItem(AItem: TRVItem);
var
  n: Integer;
  IsRemoved: Boolean;
begin
  n := IndexOf(AItem);
  IsRemoved := False;
  while (n > 0) and (n < Count) do
  begin
    if Items[n].IsSubItem then
      Delete(n)
    else
    begin
      if (not IsRemoved) then
      begin
        Delete(n);
        IsRemoved := True;
      end
      else
        Break;
    end;
    Inc(n);
  end;
end;

end.