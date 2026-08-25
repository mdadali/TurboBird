unit RVScroll;

interface

uses
  {$IFDEF FPC}
  RVLazIntf, LCLType, LCLIntf,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Forms, Controls, Graphics;

type
  { TRVScroller }

  TRVScroller = class(TCustomControl)
  private
    FTracking: Boolean;
    FFullRedraw: Boolean;
    FVScrollVisible: Boolean;
    FOnVScrolled: TNotifyEvent;
    function GetVScrollPos: Integer;
    procedure SetVScrollPos(Pos: Integer);
    function GetVScrollMax: Integer;
    procedure SetVScrollVisible(vis: Boolean);
  protected
    SmallStep, HPos, VPos, XSize, YSize: Integer;
    procedure CreateParams(var Params: TCreateParams); //override;
    procedure CreateWnd; override;
    procedure UpdateScrollBars(XS, YS: Integer);
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure SetVPos(p: Integer);
    procedure SetHPos(p: Integer);
    procedure Paint; override;
    procedure ScrollChildren(dx, dy: Integer);
    procedure UpdateChildren;
    property FullRedraw: Boolean read FFullRedraw write FFullRedraw;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);override;
    procedure EraseBackground(DC: HDC); override;
    procedure ScrollTo(y: Integer);
    property VScrollPos: Integer read GetVScrollPos write SetVScrollPos;
    property VScrollMax: Integer read GetVScrollMax;
  published
    { Published declarations }
    property Visible;
    property TabStop;
    property TabOrder;
    property Align;
    property HelpContext;
    property Tracking: Boolean read FTracking write FTracking;
    property VScrollVisible: Boolean read FVScrollVisible write SetVScrollVisible;
    property OnVScrolled: TNotifyEvent read FOnVScrolled write FOnVScrolled;
  end;

procedure Tag2Y(AControl: TControl);

implementation
{------------------------------------------------------}
procedure Tag2Y(AControl: TControl);
begin
    if AControl.Tag>10000 then
     AControl.Top := 10000
    else
       if AControl.Tag<-10000 then
         AControl.Top := -10000
       else
         AControl.Top := AControl.Tag;
end;
{------------------------------------------------------}
constructor TRVScroller.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 TabStop := True;
 FTracking := True;
 FFullRedraw := False;
 FVScrollVisible := True;
end;

procedure TRVScroller.EraseBackground(DC: HDC);
begin

end;

{------------------------------------------------------}
procedure TRVScroller.CreateParams(var Params: TCreateParams);
begin
  //inherited   CreateParams(Params);   //CreateWindow
  Params.Style := Params.Style or WS_CLIPCHILDREN or WS_HSCROLL or WS_VSCROLL;
end;
{------------------------------------------------------}
procedure  TRVScroller.CreateWnd;
begin
  inherited CreateWnd;
  SmallStep := 10;
  VPos := 0;
  HPos := 0;
  UpdateScrollBars(ClientWidth, (ClientHeight div SmallStep));
end;
{------------------------------------------------------}
procedure TRVScroller.UpdateScrollBars(XS, YS: Integer);
var
  ScrollInfo: TScrollInfo;
begin
  XSize := XS;
  YSize := YS;
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.fMask := SIF_ALL;
  ScrollInfo.nMin := 0;
  ScrollInfo.nPage := ClientHeight div SmallStep;
  ScrollInfo.nMax := YSize;
  ScrollInfo.nPos := VPos;
  ScrollInfo.nTrackPos := 0;
  SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
  if not FVScrollVisible then
    ShowScrollBar(Handle, SB_VERT, FVScrollVisible);

  ScrollInfo.fMask := SIF_ALL;
  ScrollInfo.nMin := 0;
  ScrollInfo.nMax := XSize-1;
  ScrollInfo.nPage := ClientWidth;
  ScrollInfo.nPos := VPos;
  ScrollInfo.nTrackPos := 0;
  SetScrollInfo(Handle, SB_HORZ, ScrollInfo, True);
  //UpdateChildren;
end;
{------------------------------------------------------}
procedure TRVScroller.UpdateChildren;
var i: Integer;
begin
    for i:=0 to ControlCount-1 do
      Tag2Y(Controls[i]);
end;
{------------------------------------------------------}
procedure TRVScroller.ScrollChildren(dx, dy: Integer);
var i: Integer;
begin
  if (dx=0) and (dy=0) then exit;
  for i:=0 to ControlCount-1 do begin
   if dy<>0 then begin
    Controls[i].Tag := Controls[i].Tag+dy;
    Tag2Y(Controls[i]);
   end;
   if dx<>0 then Controls[i].Left := Controls[i].Left + dx;
  end
end;
{------------------------------------------------------}
procedure TRVScroller.WMHScroll(var Message: TWMHScroll);
begin
  with Message do
    case ScrollCode of
      SB_LINEUP: SetHPos(HPos - SmallStep);
      SB_LINEDOWN: SetHPos(HPos + SmallStep);
      SB_PAGEUP: SetHPos(HPos-10*SmallStep);
      SB_PAGEDOWN: SetHPos(HPos+10*SmallStep);
      SB_THUMBPOSITION: SetHPos(Pos);
      SB_THUMBTRACK: if FTracking then SetHPos(Pos);
      SB_TOP: SetHPos(0);
      SB_BOTTOM: SetHPos(XSize);
    end;

end;
{------------------------------------------------------}
procedure TRVScroller.WMVScroll(var Message: TWMVScroll);
begin
  with Message do
    case ScrollCode of
      SB_LINEUP: SetVPos(VPos - 1);
      SB_LINEDOWN: SetVPos(VPos + 1);
      SB_PAGEUP: SetVPos(VPos-10);
      SB_PAGEDOWN: SetVPos(VPos+10);
      SB_THUMBPOSITION: SetVPos(Pos);
      SB_THUMBTRACK: if FTracking then SetVPos(Pos);
      SB_TOP: SetVPos(0);
      SB_BOTTOM: SetVPos(YSize);
    end;

end;
{------------------------------------------------------}
procedure TRVScroller.WMKeyDown(var Message: TWMKeyDown);
var vScrollNotify, hScrollNotify: Integer;
begin
  vScrollNotify := -1;
  hScrollNotify := -1;
  with Message do
    case CharCode of
        VK_UP:
            vScrollNotify := SB_LINEUP;
        VK_PRIOR:
            vScrollNotify := SB_PAGEUP;
        VK_NEXT:
            vScrollNotify := SB_PAGEDOWN;
        VK_DOWN:
            vScrollNotify := SB_LINEDOWN;
        VK_HOME:
            vScrollNotify := SB_TOP;
        VK_END:
            vScrollNotify := SB_BOTTOM;
        VK_LEFT:
            hScrollNotify := SB_LINELEFT;
        VK_RIGHT:
            hScrollNotify := SB_LINERIGHT;
    end;
  if (vScrollNotify <> -1) then
        Perform(WM_VSCROLL, vScrollNotify, 0);
  if (hScrollNotify <> -1) then
        Perform(WM_HSCROLL, hScrollNotify, 0);
  {$IFDEF FPC}
  inherited WMKeyDown(Message);
  {$ELSE}
  inherited;
  {$ENDIF}
end;
{------------------------------------------------------}
procedure TRVScroller.SetVPos(p: Integer);
var   ScrollInfo: TScrollInfo;
      oldPos: Integer;
      r: TRect;
begin
  OldPos := VPos;
  VPos := p;
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.nPos := VPos;
  ScrollInfo.fMask := SIF_POS;
  SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
  GetScrollInfo(Handle, SB_VERT, ScrollInfo);
  VPos := ScrollInfo.nPos;
  r := ClientRect;
  if OldPos-VPos <> 0 then begin
   if FFullRedraw then begin
         ScrollChildren(0, (OldPos-VPos)*SmallStep);
         Refresh;
       end
   else begin
         {$IFDEF MSWINDOWS}
         ScrollWindowEx(Handle, 0, (OldPos-VPos)*SmallStep, nil, @r, 0, nil, SW_INVALIDATE {or
                   SW_SCROLLCHILDREN});
         {$ELSE}
         Invalidate;
         {$ENDIF}
         ScrollChildren(0, (OldPos-VPos)*SmallStep);
       end;
   if Assigned(FOnVScrolled) then  FOnVScrolled(Self);
  end;
end;
{------------------------------------------------------}
procedure TRVScroller.SetHPos(p: Integer);
var   ScrollInfo: TScrollInfo;
      oldPos: Integer;
      r: TRect;
begin
  OldPos := HPos;
  HPos := p;
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.nPos := HPos;
  ScrollInfo.fMask := SIF_POS;
  SetScrollInfo(Handle, SB_HORZ, ScrollInfo, True);
  GetScrollInfo(Handle, SB_HORZ, ScrollInfo);
  HPos := ScrollInfo.nPos;
  r := ClientRect;
  if OldPos-HPos <> 0 then begin
   if FFullRedraw then begin
         ScrollChildren((OldPos-HPos), 0);
         Refresh;
       end
   else begin
         ScrollWindowEx(Handle, (OldPos-HPos), 0,  nil, @r, 0, nil, SW_INVALIDATE{or
                   SW_SCROLLCHILDREN});
         ScrollChildren((OldPos-HPos), 0);
       end;
  end;
end;
{------------------------------------------------------}
procedure TRVScroller.Paint;
var i: Integer;
begin
 Canvas.Font.Color := clRed;
 Canvas.Font.Size := 2;
 Canvas.FillRect(Canvas.ClipRect);
 for i := Canvas.ClipRect.Top div SmallStep -1  to Canvas.ClipRect.Bottom div SmallStep +1 do
   Canvas.TextOut(-HPos, i*SmallStep, IntToStr(i+VPos));
end;
{------------------------------------------------------}
procedure TRVScroller.ScrollTo(y: Integer);
begin
    SetVPos(y div SmallStep);
end;
{-------------------------------------------------------}
function TRVScroller.GetVScrollPos: Integer;
begin
  GetVScrollPos := VPos;
end;
{-------------------------------------------------------}
procedure TRVScroller.SetVScrollPos(Pos: Integer);
begin
   SetVPos(Pos);
end;
{-------------------------------------------------------}
function TRVScroller.GetVScrollMax: Integer;
var ScrollInfo: TScrollInfo;
begin
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.nPos := HPos;
  ScrollInfo.fMask := SIF_RANGE or SIF_PAGE;
  GetScrollInfo(Handle, SB_VERT, ScrollInfo);
  GetVScrollMax := ScrollInfo.nMax - Integer(ScrollInfo.nPage-1);
end;
{-------------------------------------------------------}
procedure TRVScroller.SetVScrollVisible(vis: Boolean);
begin
    FVScrollVisible := vis;
    ShowScrollBar(Handle, SB_VERT, vis);
end;
{-------------------------------------------------------}
procedure TRVScroller.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;
end.
