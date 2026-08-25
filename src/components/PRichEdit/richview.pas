unit RichView;

{$mode Delphi}

interface
{$I RV_Defs.inc}
uses
  {$IFDEF FPC}
  RVLazIntf, LCLType, LCLIntf,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms,
  RVStyle, RVScroll, ClipBrd,
  {$IFDEF RICHVIEWDEF4}
  ImgList,
  {$ENDIF}
  ExtCtrls;
  {------------------------------------------------------------------}



const
  rvsBreak      = -1;
  rvsCheckPoint = -2;
  rvsPicture    = -3;
  rvsHotSpot    = -4;
  rvsComponent  = -5;
  rvsBullet     = -6;
type

  TRichView = class;
  TRVSaveFormat = (rvsfText,
                   rvsfHTML,
                   rvsfRTF, //<---not yet implemented
                   rvsfRVF  //<---not yet implemented
                   );
  TRVSaveOption = (rvsoOverrideImages);
  TRVSaveOptions = set of TRVSaveOption;
  {------------------------------------------------------------------}
  TDrawLineInfo = class
     Left, Top, Width, Height: Integer;
     LineNo, Offs: Integer;
     FromNewLine: Boolean;
  end;
  {------------------------------------------------------------------}
  TLineInfo = class
     StyleNo: Integer;
     SameAsPrev: Boolean;
     Center: Boolean;
     imgNo: Integer; { for rvsJump# used as jump id }
     gr: TPersistent;
  end;
  {------------------------------------------------------------------}
  TCPInfo = class
    public
     Y, LineNo: Integer;
  end;
  {------------------------------------------------------------------}
  TJumpInfo = class
    public
     l,t,w,h: Integer;
     id, idx: Integer;
  end;
  {------------------------------------------------------------------}
  TJumpEvent = procedure (Sender: TObject; id: Integer) of object;
  TRVMouseMoveEvent = procedure (Sender: TObject; id: Integer) of object;
  TRVSaveComponentToFileEvent = procedure (Sender: TRichView; Path: String; SaveMe: TPersistent; SaveFormat: TRVSaveFormat; var OutStr:String) of object;
  TRVURLNeededEvent = procedure (Sender: TRichView; id: Integer; var url:String) of object;
  TRVDblClickEvent = procedure  (Sender: TRichView; ClickedWord: String; Style: Integer) of object;
  TRVRightClickEvent = procedure  (Sender: TRichView; ClickedWord: String; Style, X, Y: Integer) of object;
  {------------------------------------------------------------------}
  TBackgroundStyle = (bsNoBitmap, bsStretched, bsTiled, bsTiledAndScrolled);
  {------------------------------------------------------------------}
  TRVDisplayOption = (rvdoImages, rvdoComponents, rvdoBullets);
  TRVDisplayOptions = set of TRVDisplayOption;
  {------------------------------------------------------------------}
  TScreenAndDevice = record
       ppixScreen, ppiyScreen, ppixDevice, ppiyDevice: Integer;
       LeftMargin: Integer;
   end;
  {------------------------------------------------------------------}
  TRVInteger2 = class
   public
    val: Integer;
  end;
  {------------------------------------------------------------------}
  TRichView = class(TRVScroller)
  private
    { Private declarations }
    ScrollDelta: Integer;
    ScrollTimer: TTimer;
    FAllowSelection, FSingleClick: Boolean;
    FDelimiters: String;
    DrawHover, Selection: Boolean;
    FOnJump: TJumpEvent;
    FOnRVMouseMove: TRVMouseMoveEvent;
    FOnSaveComponentToFile: TRVSaveComponentToFileEvent;
    FOnURLNeeded: TRVURLNeededEvent;
    FOnRVDblClick: TRVDblClickEvent;
    FOnRVRightClick: TRVRightClickEvent;
    FOnSelect, FOnResized: TNotifyEvent;
    FFirstJumpNo, FMaxTextWidth, FMinTextWidth, FLeftMargin, FRightMargin: Integer;
    FBackBitmap: TBitmap;
    FBackgroundStyle: TBackgroundStyle;
    OldWidth, OldHeight: Integer;
    FSelStartNo, FSelEndNo, FSelStartOffs, FSelEndOffs: Integer;
    procedure InvalidateJumpRect(no: Integer);
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    function FindItemAtPos(X,Y: Integer): Integer;
    procedure FindItemForSel(X,Y: Integer; var No, Offs: Integer);
    function GetLineCount: Integer;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure GetSelBounds(var StartNo, EndNo, StartOffs, EndOffs: Integer);
    procedure StoreSelBounds(var StartNo, EndNo, StartOffs, EndOffs: Integer);
    procedure RestoreSelBounds(StartNo, EndNo, StartOffs, EndOffs: Integer);
  protected
    { Protected declarations }
    drawlines:TStringList;
    checkpoints: TStringList;
    jumps: TStringList;
    FStyle: TRVStyle;
    nJmps: Integer;

    skipformatting: Boolean;

    TextWidth, TextHeight: Integer;

    LastJumpMovedAbove, LastLineFormatted: Integer;
    LastJumpDowned, XClicked, YClicked, XMouse, YMouse: Integer;

    imgSavePrefix: String;
    imgSaveNo: Integer;
    SaveOptions: TRVSaveOptions;

    ShareContents: Boolean;

    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
    procedure Click; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);override;
    procedure DblClick; override;    
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure FormatLine(no: Integer; var x,baseline,prevdesc,prevabove:Integer; Canvas: TCanvas;
                         var sad: TScreenAndDevice);
    procedure AdjustJumpsCoords;
    procedure AdjustChildrenCoords;
    procedure ClearTemporal;
    function GetFirstVisible(TopLine: Integer): Integer;
    function GetFirstLineVisible: Integer;
    function GetLastLineVisible: Integer;
    function GetDrawLineNo(BoundLine: Integer; Option: Integer): Integer;
    procedure Format_(OnlyResized:Boolean; depth: Integer; Canvas: TCanvas; OnlyTail: Boolean);
    procedure SetBackBitmap(Value: TBitmap);
    procedure DrawBack(DC: HDC; Rect: TRect; Width,Height:Integer);
    procedure SetBackgroundStyle(Value: TBackgroundStyle);
    procedure SetVSmallStep(Value: Integer);
    function GetNextFileName(Path: String): String; virtual;
    procedure ShareLinesFrom(Source: TRichView);
    function FindClickedWord(var clickedword: String; var StyleNo: Integer): Boolean;
    procedure OnScrollTimer(Sender: TObject);
    procedure Loaded; override;    
  public
    { Public declarations }
    lines:TStringList;
    DisplayOptions: TRVDisplayOptions;
    FClientTextWidth: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure AddFromNewLine(s: String;StyleNo:Integer);
    procedure Add(s: String;StyleNo:Integer);
    procedure AddCenterLine(s: String;StyleNo:Integer);
    procedure AddText(s: String;StyleNo:Integer);
    procedure AddTextFromNewLine(s: String;StyleNo:Integer);
    procedure AddBreak;
    function AddCheckPoint: Integer; { returns cp # }
    function AddNamedCheckPoint(CpName: String): Integer; { returns cp # }
    function GetCheckPointY(no: Integer): Integer;
    function GetJumpPointY(no: Integer): Integer;
    procedure AddPicture(gr: TGraphic);
    procedure AddHotSpot(imgNo: Integer; lst: TImageList; fromnewline: Boolean);
    procedure AddBullet (imgNo: Integer; lst: TImageList; fromnewline: Boolean);
    procedure AddControl(ctrl: TControl; center: Boolean);

    function GetMaxPictureWidth: Integer;
    procedure Clear;
    procedure Format;
    procedure FormatTail;

    procedure AppendFrom(Source: TRichView);
    function GetLastCP: Integer;
    property VSmallStep: Integer read SmallStep write SetVSmallStep;
    function SaveHTML(FileName, Title, ImagesPrefix: String; Options: TRVSaveOptions):Boolean;
    function SaveText(FileName: String; LineWidth: Integer):Boolean;

    procedure DeleteSection(CpName: String);
    procedure DeleteLines(FirstLine, Count: Integer);

    //use this only inside OnSaveComponentToFile event handler:
    function SavePicture(DocumentSaveFormat: TRVSaveFormat; Path: String; gr: TGraphic): String; virtual;

    procedure CopyText;
    function GetSelText: String;
    function SelectionExists: Boolean;
    procedure Deselect;
    procedure SelectAll;

    property LineCount: Integer read GetLineCount;
    property FirstLineVisible: Integer read GetFirstLineVisible;
    property LastLineVisible: Integer read GetLastLineVisible;
  published
    { Published declarations }
    property PopupMenu;
    property OnClick;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;            
    property FirstJumpNo: Integer read FFirstJumpNo write FFirstJumpNo;
    property OnJump: TJumpEvent read FOnJump write FOnJump;
    property OnRVMouseMove: TRVMouseMoveEvent read FOnRVMouseMove write FOnRVMouseMove;
    property OnSaveComponentToFile: TRVSaveComponentToFileEvent read FOnSaveComponentToFile write FOnSaveComponentToFile;
    property OnURLNeeded: TRVURLNeededEvent read FOnURLNeeded write FOnURLNeeded;
    property OnRVDblClick: TRVDblClickEvent read FOnRVDblClick write FOnRVDblClick;
    property OnRVRightClick: TRVRightClickEvent read FOnRVRightClick write FOnRVRightClick;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
    property OnResized: TNotifyEvent read FOnResized write FOnResized;
    property Style: TRVStyle read FStyle write FStyle;
    property MaxTextWidth:Integer read FMaxTextWidth write FMaxTextWidth;
    property MinTextWidth:Integer read FMinTextWidth write FMinTextWidth;
    property LeftMargin: Integer read FLeftMargin write FLeftMargin;
    property RightMargin: Integer read FRightMargin write FRightMargin;
    property BackgroundBitmap: TBitmap read FBackBitmap write SetBackBitmap;
    property BackgroundStyle: TBackgroundStyle read FBackgroundStyle write SetBackgroundStyle;
    property Delimiters: String read FDelimiters write FDelimiters;
    property AllowSelection: Boolean read FAllowSelection write FAllowSelection;
    property SingleClick: Boolean read FSingleClick write FSingleClick;
  end;


  TRichEdit = class(TRichView)
    procedure FCursorTimerTimer(Sender: TObject);
  private
    FCursorTimer : TTimer;
    FCursorVisible : Boolean;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Cursor default crIBeam;
  end;


procedure InfoAboutSaD(var sad:TScreenAndDevice; Canvas: TCanvas);

implementation
{$IFDEF FPC}
uses Printers;

{-------------------------------------}
procedure InfoAboutSaD(var sad:TScreenAndDevice; Canvas: TCanvas);
var screenDC: HDC;
begin
  if Canvas is TPrinterCanvas then begin
    sad.ppixDevice := Printer.XDPI;
    sad.ppiyDevice := Printer.YDPI;
  end else begin
    sad.ppixDevice := GetDeviceCaps(Canvas.Handle, LOGPIXELSX);
    sad.ppiyDevice := GetDeviceCaps(Canvas.Handle, LOGPIXELSY);
  end;
  screenDc := CreateCompatibleDC(0);
  sad.ppixScreen := GetDeviceCaps(screenDC, LOGPIXELSY);
  sad.ppiyScreen := GetDeviceCaps(screenDC, LOGPIXELSY);
  DeleteDC(screenDC);
end;

procedure TRichEdit.FCursorTimerTimer(Sender: TObject);
begin
  FCursorVisible:=not FCursorVisible;
  Repaint;
end;

procedure TRichEdit.Paint;
var
  aObj: TDrawLineInfo;
  aTopObj: TDrawLineInfo;
begin
  inherited Paint;
  if FCursorVisible then
    begin
      if FSelEndNo<0 then exit;
      aObj := TDrawLineInfo(DrawLines.Objects[FSelEndNo]);
      aTopObj := TDrawLineInfo(DrawLines.Objects[GetFirstLineVisible]);
      Canvas.Rectangle(aObj.Left,aObj.Top-aTopObj.Top,aObj.Left+3,(aObj.Top-aTopObj.Top)+aObj.Height);
    end;
end;

constructor TRichEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCursorTimer:=TTimer.Create(Self);
  FCursorTimer.OnTimer:=FCursorTimerTimer;
end;

destructor TRichEdit.Destroy;
begin
  FreeAndNil(FCursorTimer);
  inherited Destroy;
end;

{$ELSE}
{-------------------------------------}
procedure InfoAboutSaD(var sad:TScreenAndDevice; Canvas: TCanvas);
var screenDC: HDC;
begin
     sad.ppixDevice := GetDeviceCaps(Canvas.Handle, LOGPIXELSX);
     sad.ppiyDevice := GetDeviceCaps(Canvas.Handle, LOGPIXELSY);
     screenDc := CreateCompatibleDC(0);
     sad.ppixScreen := GetDeviceCaps(screenDC, LOGPIXELSX);
     sad.ppiyScreen := GetDeviceCaps(screenDC, LOGPIXELSY);
     DeleteDC(screenDC);
end;
{$ENDIF}
{==================================================================}
constructor TRichView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClientTextWidth := False;
  FLeftMargin    := 5;
  FRightMargin   := 5;
  FMaxTextWidth  := 0;
  FMinTextWidth  := 0;
  TextWidth      := -1;
  TextHeight     := 0;
  LastJumpMovedAbove := -1; 
  FStyle         := nil;
  LastJumpDowned := -1;
  drawlines      := TStringList.Create;
  lines          := TStringList.Create;
  checkpoints    := TStringList.Create;
  jumps          := TStringList.Create;
  FBackBitmap    := TBitmap.Create;
  FBackGroundStyle := bsNoBitmap;
  nJmps          :=0;
  FirstJumpNo    :=0;
  skipformatting := False;
  OldWidth       := 0;
  OldHeight      := 0;
  Width          := 100;
  Height         := 40;
  DisplayOptions := [rvdoImages, rvdoComponents, rvdoBullets];
  ShareContents  := False;
  FDelimiters    := ' .;,:(){}"';
  DrawHover      := False;
  FSelStartNo    := -1;
  FSelEndNo      := -1;
  FSelStartOffs  := 0;
  FSelEndOffs    := 0;
  Selection      := False;
  FAllowSelection:= True;
  LastLineFormatted := -1;
  ScrollTimer    := nil;
  //Format_(False,0, Canvas, False);
end;
{-------------------------------------}
destructor TRichView.Destroy;
begin
  FBackBitmap.Free;
  Clear;
  drawlines.Free;
  checkpoints.Free;
  jumps.Free;
  if not ShareContents then lines.Free;
  inherited Destroy;
end;
{-------------------------------------}
procedure TRichView.WMSize(var Message: TWMSize);
begin
  Format_(True, 0, Canvas, False);
  if Assigned(FOnResized) then FOnResized(Self);
end;
{-------------------------------------}
procedure TRichView.Format;
begin
  Format_(False, 0, Canvas, False);
end;
{-------------------------------------}
procedure TRichView.FormatTail;
begin
  Format_(False, 0, Canvas, True);
end;
{-------------------------------------}
procedure TRichView.ClearTemporal;
var i: Integer;
begin
  if ScrollTimer<>nil then begin
     ScrollTimer.Free;
     ScrollTimer := nil;
  end;
  drawlines.BeginUpdate;
  for i:=0 to drawlines.Count-1 do begin
    TDrawLineInfo(drawlines.objects[i]).Free;
    drawlines.objects[i] := nil;
  end;
  drawlines.Clear;
  drawlines.EndUpdate;
  checkpoints.BeginUpdate;
  for i:=0 to checkpoints.Count-1 do begin
    TCPInfo(checkpoints.objects[i]).Free;
    checkpoints.objects[i] := nil;
  end;
  checkpoints.Clear;
  checkpoints.EndUpdate;
  jumps.BeginUpdate;
  for i:=0 to jumps.Count-1 do begin
    TJumpInfo(jumps.objects[i]).Free;
    jumps.objects[i] := nil;
  end;
  jumps.Clear;
  jumps.EndUpdate;
  nJmps :=0;
end;
{-------------------------------------}
procedure TRichView.Deselect;
begin
  Selection := False;
  FSelStartNo := -1;
  FSelEndNo := -1;
  FSelStartOffs := 0;
  FSelEndOffs := 0;
  if Assigned(FOnSelect) then OnSelect(Self);  
end;
{-------------------------------------}
procedure TRichView.SelectAll;
begin
  FSelStartNo := 0;
  FSelEndNo := DrawLines.Count-1;
  FSelStartOffs := 0;
  FSelEndOffs := 0;
  if TLineInfo(Lines.Objects[TDrawLineInfo(DrawLines.Objects[FSelEndNo]).LineNo]).StyleNo>=0 then
    FSelEndOffs := Length(DrawLines[FSelEndNo])+1;
  if Assigned(FOnSelect) then OnSelect(Self);
end;
{-------------------------------------}
procedure TRichView.Clear;
var i: Integer;
begin
  Deselect;
  if not ShareContents then begin
      lines.BeginUpdate;
      for i:=0 to lines.Count-1 do begin
        if TLineInfo(lines.objects[i]).StyleNo = -3 then { image}
          begin
            TLineInfo(lines.objects[i]).gr.Free;
            TLineInfo(lines.objects[i]).gr := nil;
          end;
        if TLineInfo(lines.objects[i]).StyleNo = -5 then {control}
          begin
            RemoveControl(TControl(TLineInfo(lines.objects[i]).gr));
            TLineInfo(lines.objects[i]).gr.Free;
            TLineInfo(lines.objects[i]).gr := nil;
          end;
        TLineInfo(lines.objects[i]).Free;
        lines.objects[i] := nil;
      end;
      lines.Clear;
      lines.EndUpdate;
  end;
  ClearTemporal;
end;
{-------------------------------------}
procedure TRichView.AddFromNewLine(s: String; StyleNo:Integer);
var info: TLineInfo;
begin
  info := TLineInfo.Create;
  info.StyleNo := StyleNo;
  info.SameAsPrev := False;
  info.Center := False;
  lines.AddObject(s, info);
end;
{-------------------------------------}
procedure TRichView.Add(s: String; StyleNo:Integer);
var info: TLineInfo;
begin
  info := TLineInfo.Create;
  info.StyleNo := StyleNo;
  info.SameAsPrev := (lines.Count<>0);
  info.Center := False;
  lines.AddObject(s, info);
end;
{-------------------------------------}
procedure TRichView.AddText(s: String;StyleNo:Integer);
var p: Integer;
begin
   {$IFDEF FPC}
   s:=AdjustLineBreaks(s, tlbsCRLF);
   {$ELSE}
   s:=AdjustLineBreaks(s);
   {$ENDIF}
   p := Pos(chr(13)+chr(10),s);
   if p=0 then begin
     if s<>'' then Add(s,StyleNo);
     exit;
   end;
   Add(Copy(s,1,p-1), StyleNo);
   Delete(s,1, p+1);
   while s<>'' do begin
     p := Pos(chr(13)+chr(10),s);
     if p=0 then begin
        AddFromNewLine(s,StyleNo);
        break;
     end;
     AddFromNewLine(Copy(s,1,p-1), StyleNo);
     Delete(s,1, p+1);
   end;
end;
{-------------------------------------}
procedure TRichView.AddTextFromNewLine(s: String;StyleNo:Integer);
var p: Integer;
begin
   {$IFDEF FPC}
   s:=AdjustLineBreaks(s, tlbsCRLF);
   {$ELSE}
   s:=AdjustLineBreaks(s);
   {$ENDIF}
   p := Pos(chr(13)+chr(10),s);
   if p=0 then begin
     AddFromNewLine(s,StyleNo);
     exit;
   end;
   while s<>'' do begin
     p := Pos(chr(13)+chr(10),s);
     if p=0 then begin
        AddFromNewLine(s,StyleNo);
        break;
     end;
     AddFromNewLine(Copy(s,1,p-1), StyleNo);
     Delete(s,1, p+1);
   end;
end;
{-------------------------------------}
procedure TRichView.AddCenterLine(s: String;StyleNo:Integer);
var info: TLineInfo;
begin
  info := TLineInfo.Create;
  info.StyleNo := StyleNo;
  info.SameAsPrev := False;
  info.Center := True;
  lines.AddObject(s, info);
end;
{-------------------------------------}
procedure TRichView.AddBreak;
var info: TLineInfo;
begin
  info := TLineInfo.Create;
  info.StyleNo := -1;
  lines.AddObject('', info);
end;
{-------------------------------------}
function TRichView.AddNamedCheckPoint(CpName: String): Integer;
var info: TLineInfo;
    cpinfo: TCPInfo;
begin
  info := TLineInfo.Create;
  info.StyleNo := -2;
  lines.AddObject(CpName, info);
  cpInfo := TCPInfo.Create;
  cpInfo.Y := 0;
  checkpoints.AddObject(CpName,cpinfo);
  AddNamedCheckPoint := checkpoints.Count-1;
end;
{-------------------------------------}
function TRichView.AddCheckPoint: Integer;
begin
  AddCheckPoint := AddNamedCheckPoint('');
end;
{-------------------------------------}
function TRichView.GetCheckPointY(no: Integer): Integer;
begin
  GetCheckPointY := TCPInfo(checkpoints.Objects[no]).Y;
end;
{-------------------------------------}
function TRichView.GetJumpPointY(no: Integer): Integer;
var i: Integer;
begin
  GetJumpPointY := 0;
  for i:=0 to Jumps.Count-1 do
   if  TJumpInfo(jumps.objects[i]).id = no-FirstJumpNo then begin
     GetJumpPointY := TJumpInfo(jumps.objects[i]).t;
     exit;
   end;
end;
{-------------------------------------}
procedure TRichView.AddPicture(gr: TGraphic); { gr not copied, do not free it!}
var info: TLineInfo;
begin
  info := TLineInfo.Create;
  info.StyleNo := -3;
  info.gr := gr;
  info.SameAsPrev := False;
  info.Center := True;
  lines.AddObject('', info);
end;
{-------------------------------------}
procedure TRichView.AddHotSpot(imgNo: Integer; lst: TImageList; fromnewline: Boolean);
var info: TLineInfo;
begin
  info := TLineInfo.Create;
  info.StyleNo := -4;
  info.gr := lst;
  info.imgNo := imgNo;
  info.SameAsPrev := not FromNewLine;
  lines.AddObject('', info);
end;
{-------------------------------------}
procedure TRichView.AddBullet(imgNo: Integer; lst: TImageList; fromnewline: Boolean);
var info: TLineInfo;
begin
  info := TLineInfo.Create;
  info.StyleNo := -6;
  info.gr := lst;
  info.imgNo := imgNo;
  info.SameAsPrev := not FromNewLine;
  lines.AddObject('', info);
end;
{-------------------------------------}
procedure TRichView.AddControl(ctrl: TControl; center: Boolean); { do not free ctrl! }
var info: TLineInfo;
begin
  info := TLineInfo.Create;
  info.StyleNo := -5;
  info.gr := ctrl;
  info.SameAsPrev := False;
  info.Center := center;
  lines.AddObject('', info);
  InsertControl(ctrl);
end;
{-------------------------------------}
function TRichView.GetMaxPictureWidth: Integer;
var i,m: Integer;
begin
  m := 0;
  for i:=0 to lines.Count-1 do  begin
   if TLineInfo(lines.objects[i]).StyleNo = -3 then
       if  m<TGraphic(TLineInfo(lines.objects[i]).gr).Width then
          m := TGraphic(TLineInfo(lines.objects[i]).gr).Width;
       if TLineInfo(lines.objects[i]).StyleNo = -5 then
       if  m<TControl(TLineInfo(lines.objects[i]).gr).Width then
          m := TControl(TLineInfo(lines.objects[i]).gr).Width;
   end;
 GetMaxPictureWidth := m;
end;
{-------------------------------------}
function max(a,b: Integer): Integer;
begin
  if a>b then
    max := a
  else
    max := b;
end;
{-------------------------------------}
procedure TRichView.Format_(OnlyResized:Boolean; depth: Integer; Canvas: TCanvas;
          OnlyTail: Boolean);
var i: Integer;
    x,b,d,a: Integer;
    mx : Integer;
    oldy, oldtextwidth, cw, ch: Integer;
    sad: TScreenAndDevice;
    StyleNo: Integer;
    StartLine: Integer;
    StartNo, EndNo, StartOffs, EndOffs: Integer;
begin
   if smallstep = 0 then exit;
   if (csDesigning in ComponentState) or
      not Assigned(FStyle) or
      skipformatting or
      (depth>1)
   then exit;
   skipformatting := True;

   if depth=0 then StoreSelBounds(StartNo, EndNo, StartOffs, EndOffs);

   OldY := VPos*SmallStep;

   oldtextwidth := TextWidth;

   mx := max(ClientWidth-(FLeftMargin+FRightMargin), GetMaxPictureWidth);
   if mx<FMinTextWidth then mx := FMinTextWidth;
   if FClientTextWidth then begin { widths of pictures and maxtextwidth are ignored }
     TextWidth := ClientWidth-(FLeftMargin+FRightMargin);
     if TextWidth<FMinTextWidth then TextWidth := FMinTextWidth;
     end
   else begin
      if (mx > FMaxTextWidth) and (FMaxTextWidth>0) then
        TextWidth := FMaxTextWidth
      else
        TextWidth := mx;
   end;
   if not (OnlyResized and (TextWidth=OldTextWidth)) then begin
     if OnlyTail then begin
        StartLine := LastLineFormatted+1;
        b:= TextHeight;
        end
     else begin
        StartLine := 0;
        b := 0;
        ClearTemporal;
     end;
     x:=0;
     d:=0;
     InfoAboutSaD(sad, Canvas);
     sad.LeftMargin := MulDiv(FLeftMargin,  sad.ppixDevice, sad.ppixScreen);
     for i:=StartLine to lines.Count-1 do begin
       StyleNo := TLineInfo(Lines.Objects[i]).StyleNo;
       if not (((StyleNo = rvsPicture) and (not (rvdoImages in DisplayOptions))) or
          ((StyleNo = rvsComponent)and(not (rvdoComponents in DisplayOptions))) or
          (((StyleNo = rvsBullet) or (StyleNo = rvsHotspot))and(not (rvdoBullets in DisplayOptions)))) then
         FormatLine(i,x,b, d,a, Canvas, sad);
     end;
     TextHeight := b+d+1;
     if TextHeight div SmallStep > 30000 then
       SmallStep := TextHeight div 30000;
     AdjustJumpsCoords;
     end
   else
     AdjustChildrenCoords;
   HPos := 0;
   VPos := 0;
   cw := ClientWidth;
   ch := ClientHeight;
   UpdateScrollBars(mx+FLeftMargin+FRightMargin, TextHeight div SmallStep);
   if (cw<>ClientWidth) or (ch<>ClientHeight) then begin
     skipformatting := False;
     ScrollTo(OldY);
     Format_(OnlyResized, depth+1, Canvas, False);
     end;
   if OnlyResized then ScrollTo(OldY);
   if OnlyTail then ScrollTo(TextHeight);
   if depth=0 then RestoreSelBounds(StartNo, EndNo, StartOffs, EndOffs);
   skipformatting := False;
   LastLineFormatted := Lines.Count-1;
end;
{-------------------------------------}
procedure TRichView.AdjustChildrenCoords;
var i: Integer;
    dli: TDrawLineInfo;
    li : TLineInfo;
begin
  for i:=0 to drawlines.Count-1 do begin
   dli := TDrawLineInfo(drawlines.Objects[i]);
   li := TLineInfo(lines.Objects[dli.LineNo]);
   if li.StyleNo = -5 then {control}
   begin
        TControl(li.gr).Left := dli.Left;
        TControl(li.gr).Tag := dli.Top;
        Tag2Y(TControl(li.gr));
   end;
  end;
end;
{-------------------------------------}
procedure TRichView.FormatLine(no: Integer; var x,baseline,prevdesc,prevabove:Integer; Canvas: TCanvas;
                         var sad: TScreenAndDevice);
var sourceStrPtr, strForAdd, strSpacePos: PChar;
    sourceStrPtrLen: Integer;
    sz: TSIZE;
    max,j, y, ctrlw, ctrlh : Integer;
{$IFNDEF RICHVIEWDEF4}
    arr: array[0..1000] of integer;
{$ENDIF}
    str: array[0..1000] of char;
    info: TDrawLineInfo;
    metr: TTextMetric;
    StyleNo: Integer;
    newline, center:Boolean;
    cpinfo: TCPInfo;
    jmpinfo: TJumpInfo;
    width, y5, Offs : Integer;
begin
  width := TextWidth;
  case TLineInfo(lines.Objects[no]).StyleNo of
   -5: { Control }
     begin
       ctrlw       := TControl(TLineInfo(lines.Objects[no]).gr).Width;
       ctrlh       := TControl(TLineInfo(lines.Objects[no]).gr).Height;
       ctrlw       := MulDiv(ctrlw, sad.ppixDevice, sad.ppixScreen);
       ctrlh       := MulDiv(ctrlh, sad.ppiyDevice, sad.ppiyScreen);
       info        := TDrawLineInfo.Create;
       info.LineNo := no;
       info.Top    := baseline + prevdesc + 1;
       info.Width  := ctrlw;
       info.Height := ctrlh+1;
       if TLineInfo(lines.Objects[no]).Center then begin
          info.Left   := (width-ctrlw) div 2;
          if info.Left<0 then info.Left := 0;
          inc(info.Left,sad.LeftMargin);
          end
       else
          info.Left := sad.LeftMargin;
       drawlines.AddObject('',info);
       TControl(TLineInfo(lines.Objects[no]).gr).Left := info.Left;
       TControl(TLineInfo(lines.Objects[no]).gr).Tag := info.Top;
       Tag2Y(TControl(TLineInfo(lines.Objects[no]).gr));
       inc (baseline,prevdesc+ctrlh+1);
       prevdesc :=1;
       prevabove := ctrlh+1;
     end;
   -4, -6: { hotSpot or Bullet }
     begin
       ctrlw       := TImageList(TLineInfo(lines.Objects[no]).gr).Width;
       ctrlh       := TImageList(TLineInfo(lines.Objects[no]).gr).Height;
       ctrlw       := MulDiv(ctrlw, sad.ppixDevice, sad.ppixScreen);
       ctrlh       := MulDiv(ctrlh, sad.ppiyDevice, sad.ppiyScreen);
       info := TDrawLineInfo.Create;
       info.Width  := ctrlw+1;
       info.Height := ctrlh+1;
       if  not TLineInfo(lines.Objects[no]).SameAsPrev or (x+ctrlw+2 > width) then begin
          x:=0;
          y:=baseline + prevdesc;
          inc (baseline,prevdesc+ctrlh+1);
          prevdesc :=1;
          prevabove := ctrlh+1;
          end
       else begin
          if prevabove < ctrlh+1 then begin
              j := drawlines.Count-1;
              if j>=0 then
                  repeat
                    inc(TDrawLineInfo(drawlines.Objects[j]).Top,ctrlh+1-prevabove);
                    dec(j);
                  until  TDrawLineInfo(drawlines.Objects[j+1]).FromNewLine;
              inc(baseline,ctrlh+1-prevabove);
              prevabove := ctrlh+1;
          end;
          y := baseline - (ctrlh+1);
       end;
       if TLineInfo(lines.Objects[no]).StyleNo = -4 then begin{HotSpot}
          jmpinfo     := TJumpInfo.Create;
          jmpinfo.l   := x+1+sad.LeftMargin;;
          jmpinfo.t   := y+1;
          jmpinfo.w   := ctrlw;
          jmpinfo.h   := ctrlh;
          jmpinfo.id  := nJmps;
          jmpinfo.idx := drawlines.Count;
          jumps.AddObject('',jmpinfo);
          inc(nJmps);
       end;
       info.Left := x+1+sad.LeftMargin;;
       inc(x, ctrlw+2);
       info.Top :=  y+1;
       info.LineNo := no;
       info.FromNewLine := not TLineInfo(lines.Objects[no]).SameAsPrev;
       drawlines.AddObject('',info);
     end;
   -3:  { graphics}
     begin
       ctrlw       := TGraphic(TLineInfo(lines.Objects[no]).gr).Width;
       ctrlh       := TGraphic(TLineInfo(lines.Objects[no]).gr).Height;
       ctrlw       := MulDiv(ctrlw, sad.ppixDevice, sad.ppixScreen);
       ctrlh       := MulDiv(ctrlh, sad.ppiyDevice, sad.ppiyScreen);
       info        := TDrawLineInfo.Create;
       info.Width  := ctrlw;
       info.Height := ctrlh+1;
       info.Left   := (width-ctrlw) div 2;
       if info.Left<0 then info.Left := 0;
       inc(info.Left,sad.LeftMargin);
       info.Top    := baseline + prevdesc + 1;
       info.LineNo := no;
       drawlines.AddObject('',info);
       inc (baseline,prevdesc+ctrlh+1);
       prevdesc    :=1;
       prevabove   := ctrlh+1;
    end;
   -2: { check point}
    begin
       cpinfo   := TCPInfo.Create;
       cpinfo.Y := baseline + prevDesc;
       cpinfo.LineNo := no;
       checkpoints.AddObject(lines[no], cpinfo);
    end;
   -1: { break line}
    begin
       y5          := MulDiv(5, sad.ppiyDevice, sad.ppiyScreen);
       info        := TDrawLineInfo.Create;
       info.Left   := sad.LeftMargin;
       info.Top    := baseline + prevdesc;
       info.LineNo := no;
       info.Width  := Width;
       info.Height := y5+y5+1;
       drawlines.AddObject(Lines[no],info);
       inc (baseline,prevdesc+y5+y5+1);
       prevdesc  := y5;
       prevabove := y5;
    end;
   else begin
       sourceStrPtr := PChar(lines.Strings[no]);
       strForAdd := str;
       sourceStrPtrLen := StrLen(sourceStrPtr);

       StyleNo := TLineInfo(lines.Objects[no]).StyleNo;
       with FStyle.TextStyles[StyleNo] do begin
         Canvas.Font.Style := Style;
         Canvas.Font.Size  := Size;
         Canvas.Font.Name  := FontName;
         {$IFDEF RICHVIEWDEF3}
         Canvas.Font.CharSet  := CharSet;
         {$ENDIF}
       end;
       GetTextMetrics(Canvas.Handle,metr);
       newline := not TLineInfo(lines.Objects[no]).SameAsPrev;
       Center := TLineInfo(lines.Objects[no]).Center;
       while sourceStrPtrLen>0 do begin
         if newline then  x:=0;
         {$IFDEF FPC}
         MyGetTextExtentExPoint(Canvas.Handle,  sourceStrPtr,  sourceStrPtrLen, Width-x,
         {$ELSE}
         GetTextExtentExPoint(Canvas.Handle,  sourceStrPtr,  sourceStrPtrLen, Width-x,
         {$ENDIF}
                              {$IFDEF RICHVIEWDEF4}
                                @max, nil,
                              {$ELSE}
                                max, arr[0],
                              {$ENDIF}
                              sz);
         if max=0 then max := 1;
         StrLCopy(strForAdd, sourceStrPtr,max);
         if max<sourceStrPtrLen then
           {if  sourceStrPtr[max]<>' ' then } begin
             StrLCopy(strForAdd, sourceStrPtr,max);
             strSpacePos := StrRScan(strForAdd,' ');
             if strSpacePos<>nil then begin
               max := strSpacePos-strForAdd;
               StrLCopy(strForAdd, sourceStrPtr,max);
               inc(max);
               end
             else
               if not newline then begin
                 x:=0;
                 newline := true;
                 continue;
               end;
           end;
         Offs := sourceStrPtr - PChar(Lines.Strings[no])+1;
         sourceStrPtr := @(sourceStrPtr[max]);
         info := TDrawLineInfo.Create;
         info.LineNo := no;
         info.Offs := Offs;
         {$IFDEF FPC}
         MyGetTextExtentExPoint(Canvas.Handle,  strForAdd,  StrLen(strForAdd), Width-x,
         {$ELSE}
         GetTextExtentExPoint(Canvas.Handle,  strForAdd,  StrLen(strForAdd), Width-x,
         {$ENDIF}
           {$IFDEF RICHVIEWDEF4}
           @max, nil,
           {$ELSE}
           max,arr[0],
           {$ENDIF}
           sz);
         if not newline then begin {continue line}
           if prevabove < metr.tmExternalLeading+metr.tmAscent then begin
             j := drawlines.Count-1;
             if j>=0 then
               repeat
                 inc(TDrawLineInfo(drawlines.Objects[j]).Top,metr.tmExternalLeading+metr.tmAscent-prevabove);
                 dec(j);
               until  TDrawLineInfo(drawlines.Objects[j+1]).FromNewLine;
             inc(baseline,metr.tmExternalLeading+metr.tmAscent-prevabove);
             prevabove := metr.tmExternalLeading+metr.tmAscent;
           end;
           y := baseline - metr.tmAscent;
           info.FromNewLine := False;
           end
         else  begin { new line }
           info.FromNewLine := True;
           if Center then
             x := (Width - sz.cx) div 2
           else
             x :=0;
           y := baseline+prevDesc+metr.tmExternalLeading;
           inc(baseline, prevDesc+metr.tmExternalLeading+metr.tmAscent);
           prevabove := metr.tmExternalLeading+metr.tmAscent;
         end;
         info.Left   :=x+sad.LeftMargin;;
         info.Top    := y;
         info.Width  := sz.cx;
         info.Height := sz.cy;
         drawlines.AddObject(strForAdd,info);
         if (StyleNo=rvsJump1) or (StyleNo=rvsJump2) then begin
           jmpinfo := TJumpInfo.Create;
           jmpinfo.l := x+sad.LeftMargin;
           jmpinfo.t := y;
           jmpinfo.w := sz.cx;
           jmpinfo.h := sz.cy;
           jmpinfo.id := nJmps;
           jmpinfo.idx := drawlines.Count-1;
           TLineInfo(lines.Objects[no]).imgNo := nJmps;
           jumps.AddObject('',jmpinfo);
         end;
         sourceStrPtrLen := StrLen(sourceStrPtr);
         if newline or (prevDesc < metr.tmDescent) then prevDesc := metr.tmDescent;
         inc(x,sz.cx);
         newline := True;
       end;
       if (StyleNo=rvsJump1) or (StyleNo=rvsJump2) then inc(nJmps);
   end;
  end;
end;
{-------------------------------------}
procedure TRichView.AdjustJumpsCoords;
var i: Integer;
begin
  for i:=0 to jumps.Count-1 do begin
    TJumpInfo(jumps.Objects[i]).l :=
    TDrawLineInfo(drawlines.Objects[TJumpInfo(jumps.Objects[i]).idx]).left;
    TJumpInfo(jumps.Objects[i]).t :=
    TDrawLineInfo(drawlines.Objects[TJumpInfo(jumps.Objects[i]).idx]).top;
  end;
end;
{-------------------------------------}
const gdlnFirstVisible =1;
const gdlnLastCompleteVisible =2;
const gdlnLastVisible =3;
{-------------------------------------}
function TRichView.GetFirstVisible(TopLine: Integer): Integer;
begin
   Result := GetDrawLineNo(TopLine,gdlnFirstVisible);
end;
{-------------------------------------}
function TRichView.GetFirstLineVisible: Integer;
var v: Integer;
begin
   v := GetDrawLineNo(VPos*SmallStep, gdlnFirstVisible);
   if v>=DrawLines.Count then v := DrawLines.Count-1;
   if v<0 then
     Result := -1
   else
     Result := TDrawLineInfo(DrawLines.Objects[v]).LineNo;
end;
{-------------------------------------}
function TRichView.GetLastLineVisible: Integer;
var v: Integer;
begin
   v := GetDrawLineNo(VPos*SmallStep+ClientHeight, gdlnLastVisible);
   if v>=DrawLines.Count then v := DrawLines.Count-1;
   if v<0 then
     Result := -1
   else
     Result := TDrawLineInfo(DrawLines.Objects[v]).LineNo;
end;
{-------------------------------------}
function TRichView.GetDrawLineNo(BoundLine: Integer; Option: Integer): Integer;
var
    a,b,mid: Integer;
begin
  if DrawLines.Count = 0 then begin
     GetDrawLineNo := 0;
     exit;
  end;
  if TDrawLineInfo(drawlines.Objects[0]).Top>=BoundLine then begin
     GetDrawLineNo := 0;
     exit;
  end;
  if (Option=gdlnLastVisible) and (TDrawLineInfo(drawlines.Objects[DrawLines.Count-1]).Top<BoundLine) then begin
     GetDrawLineNo := DrawLines.Count-1;
     exit;
  end;
  a := 1;
  b := DrawLines.Count-1;
  mid := a;
  if Option = gdlnLastCompleteVisible then begin
  {
    while (b-a)>1 do begin
      mid := (a+b) div 2;
      if (TDrawLineInfo(drawlines.Objects[mid]).Top+TDrawLineInfo(drawlines.Objects[mid]).Height>BoundLine) then
          b := mid
      else
          a := mid;
    end;
    if mid>= DrawLines.Count then mid := DrawLines.Count-1;
    while (mid>0) and (TDrawLineInfo(drawlines.Objects[mid]).Top+TDrawLineInfo(drawlines.Objects[mid]).Height>BoundLine) do dec(mid);

      if (mid>0) then dec(mid);
      while (mid>0) and not TDrawLineInfo(drawlines.Objects[mid]).FromNewLine do dec(mid);
      if (mid>0) then dec(mid);
    end
    }
  end
  else begin
    while (b-a)>1 do begin
      mid := (a+b) div 2;
      if (TDrawLineInfo(drawlines.Objects[mid]).Top>=BoundLine) then begin
          if (TDrawLineInfo(drawlines.Objects[mid-1]).Top<BoundLine) then break;
          b := mid;
        end
      else
        a := mid;
    end;
    if mid>= DrawLines.Count then mid := DrawLines.Count-1;
    if Option = gdlnFirstVisible then begin
      while (mid>0) and not TDrawLineInfo(drawlines.Objects[mid]).FromNewLine do dec(mid);
      if (mid>0) then dec(mid);
      while (mid>0) and not TDrawLineInfo(drawlines.Objects[mid]).FromNewLine do dec(mid);
      if (mid>0) then dec(mid);
      end
    else
      while TDrawLineInfo(drawlines.Objects[mid]).Top<BoundLine do inc(mid);
  end;
  GetDrawLineNo := mid;
end;
{
function TRichView.GetFirstVisible(TopLine: Integer): Integer;
var
    a,b,mid: Integer;
begin
  if DrawLines.Count = 0 then begin
     GetFirstVisible := 0;
     exit;
  end;
  if TDrawLineInfo(drawlines.Objects[0]).Top>=TopLine then begin
     GetFirstVisible := 0;
     exit;
  end;
  a := 1;
  b := DrawLines.Count-1;
  mid := a;
  while (b-a)>1 do begin
    mid := (a+b) div 2;
    if (TDrawLineInfo(drawlines.Objects[mid]).Top>=TopLine) then begin
      if (TDrawLineInfo(drawlines.Objects[mid-1]).Top<TopLine) then break;
      b := mid;
      end
    else
      a := mid;
  end;
  dec(mid);
  while (mid>=2) and
        (TDrawLineInfo(drawlines.Objects[mid]).Left>
         TDrawLineInfo(drawlines.Objects[mid-1]).Left) do dec(mid);
  if mid=0 then begin
    GetFirstVisible := mid;
    exit;
  end;
  dec(mid);
  while (mid>=1) and
        (TDrawLineInfo(drawlines.Objects[mid]).Left>
         TDrawLineInfo(drawlines.Objects[mid-1]).Left) do dec(mid);
  GetFirstVisible := mid;
end;
}
{$IFDEF FPC}
procedure TxtOut(Canvas: Tcanvas; X,Y: Integer; Text: String);
var
  Sz: TSize;
  R: TRect;
  ts: TTextStyle;
begin
  Sz := Canvas.TextExtent(Text);
  R := Bounds(X,Y,Sz.cx, Sz.cy);
  ts := Canvas.TextStyle;
  ts.Opaque := Canvas.Brush.Style <> bsClear;
  Canvas.TextRect(R, R.Left, R.Top, Text, ts);
end;
{$ENDIF}

{-------------------------------------}
procedure TRichView.Paint;
var i,no, yshift, xshift: Integer;
    cl, textcolor: TColor;
    dli:TDrawLineInfo;
    li: TLineInfo;
    lastline, hovernow: Boolean;
    r :TRect;
    buffer: TBitmap;
    canv: TCanvas;
    s, s1: String;
    StartNo, EndNo, StartOffs, EndOffs: Integer;
    {$IFDEF FPC}
    St: string;
    {$ENDIF}
begin
 if (csDesigning in ComponentState) or
    not Assigned(FStyle)
 then begin
    cl := Canvas.Brush.Color;
    if Assigned(FStyle) then
        Canvas.Brush.Color := FStyle.Color
    else
        Canvas.Brush.Color := clWindow;
    Canvas.Brush.Style := bsSolid;
    Canvas.Pen.Color := clWindowText;
    Canvas.Font.Color := clWindowText;
    Canvas.Font.Name := 'MS Sans Serif';
    Canvas.Font.Size := 8;
    Canvas.Font.Style := [];
    Canvas.FillRect(Canvas.ClipRect);
    if (csDesigning in ComponentState) then
      Canvas.TextOut(ClientRect.Left+1, ClientRect.Top+1, 'RichView v0.5.1 (www.trichview.com)')
    else
      Canvas.TextOut(ClientRect.Left+1, ClientRect.Top+1, 'Error: style is not assigned');
    Canvas.Brush.Color := clWindowText;
    Canvas.FrameRect(ClientRect);
    Canvas.Brush.Color := cl;
    exit;
 end;
 GetSelBounds(StartNo, EndNo, StartOffs, EndOffs);
 lastline := False;
 r := Canvas.ClipRect;
 buffer := TBitmap.Create;
 buffer.Width := r.Right-r.Left+1;
 buffer.Height := r.Bottom-r.Top+1;
 canv := buffer.Canvas;
 DrawBack(canv.Handle, Canvas.ClipRect, ClientWidth, ClientHeight);
 yshift := VPos*SmallStep;
 inc(r.Top, yshift);
 inc(r.Bottom, yshift);
 inc(yshift, Canvas.ClipRect.Top);
 xshift := HPos + Canvas.ClipRect.Left;
 canv.Brush.Style := bsClear;

 for i:= GetFirstVisible(r.Top) to drawlines.Count-1 do begin
   dli := TDrawLineInfo(drawlines.Objects[i]);
   if lastline and (dli.Left<=TDrawLineInfo(drawlines.Objects[i-1]).left) then break;
   if dli.Top>r.Bottom then lastline := True;
   li := TLineInfo(lines.Objects[dli.LineNo]);
   no := li.StyleNo;
   if no>=0 then begin { text }
     canv.Font.Style := FStyle.TextStyles[no].Style;
     canv.Font.Size := FStyle.TextStyles[no].Size;
     canv.Font.Name := FStyle.TextStyles[no].FontName;
     {$IFDEF RICHVIEWDEF3}
     canv.Font.CharSet := FStyle.TextStyles[no].CharSet;
     {$ENDIF}

     if not ((no in [rvsJump1, rvsJump2]) and DrawHover and
        (LastJumpMovedAbove<>-1) and
        (li.ImgNo = LastJumpMovedAbove)) then begin
       textcolor := FStyle.TextStyles[no].Color;
       hovernow := False;
       end
     else begin
       textcolor := FStyle.HoverColor;
       hovernow := True;
       canv.Font.Color := textcolor;
     end;
     
     if (StartNo>i) or (EndNo<i) then begin
       canv.Font.Color := textcolor;
       canv.TextOut(dli.Left-xshift, dli.Top-yshift, drawlines.Strings[i])
       end
     else if ((StartNo<i) and (EndNo>i)) or
         ((StartNo=i) and (EndNo<>i) and (StartOffs<=1)) or
         ((StartNo<>i) and (EndNo=i) and (EndOffs>Length(drawlines.Strings[i])))
     then begin
       canv.Brush.Style := bsSolid;
       canv.Brush.Color := FStyle.SelColor;
       if not hovernow then canv.Font.Color := FStyle.SelTextColor;
       {$IFDEF FPC}
       TxtOut(canv, dli.Left-xshift, dli.Top-yshift, drawlines.Strings[i]);
       {$ELSE}
       canv.TextOut(dli.Left-xshift, dli.Top-yshift, drawlines.Strings[i]);
       {$ENDIF}
       canv.Brush.Style := bsClear;
       end
     else if (StartNo=i) then begin
       canv.Font.Color := textcolor;
       s := Copy(drawlines.Strings[i], 1, StartOffs-1);
       canv.TextOut(dli.Left-xshift, dli.Top-yshift, s);
       canv.Brush.Style := bsSolid;
       canv.Brush.Color := FStyle.SelColor;
       if not hovernow then canv.Font.Color := FStyle.SelTextColor;
       if (i<>EndNo) or (EndOffs>Length(DrawLines[i])) then begin
         {$IFDEF FPC}
         St := Copy(drawlines.Strings[i], StartOffs, Length(drawlines.Strings[i]));
         TxtOut(canv, dli.Left-xshift+canv.TextWidth(s), dli.Top-yshift,st);
         {$ELSE}
         canv.TextOut(dli.Left-xshift+canv.TextWidth(s), dli.Top-yshift,
            Copy(drawlines.Strings[i], StartOffs, Length(drawlines.Strings[i])));
         {$ENDIF}
         canv.Brush.Style := bsClear;
         end
       else begin
         s1 := Copy(drawlines.Strings[i], StartOffs, EndOffs-StartOffs);
         {$IFDEF FPC}
         TxtOut(canv, dli.Left-xshift+canv.TextWidth(s), dli.Top-yshift, s1);
         {$ELSE}
         canv.TextOut(dli.Left-xshift+canv.TextWidth(s), dli.Top-yshift, s1);
         {$ENDIF}
         canv.Font.Color := textcolor;
         canv.Brush.Style := bsClear;
         canv.TextOut(dli.Left-xshift+canv.TextWidth(s+s1), dli.Top-yshift,
           Copy(drawlines.Strings[i], EndOffs, Length(DrawLines[i])));
       end;
       end else
     if (EndNo=i) then begin
       s := Copy(drawlines.Strings[i], 1, EndOffs-1);
       canv.Brush.Style := bsSolid;
       canv.Brush.Color := FStyle.SelColor;
       if not hovernow then canv.Font.Color := FStyle.SelTextColor;
       {$IFDEF FPC}
       TxtOut(canv, dli.Left-xshift, dli.Top-yshift, s);
       {$ELSE}
       canv.TextOut(dli.Left-xshift, dli.Top-yshift, s);
       {$ENDIF}
       canv.Brush.Style := bsClear;
       canv.Font.Color := textcolor;
       canv.TextOut(dli.Left-xshift+canv.TextWidth(s), dli.Top-yshift,
             Copy(drawlines.Strings[i], EndOffs, Length(drawlines.Strings[i])));
       end;
     continue;
   end;
   if (no = -3)  then begin { graphics }
     canv.Draw(dli.Left-xshift, dli.Top-yshift, TGraphic(li.gr));
     continue;
   end;
   if (no = -4) or (no = -6)  then begin { hotspots and bullets }
     if (StartNo<=i) and (EndNo>=i) and
        not ((EndNo=i) and (EndOffs=0)) and
        not ((StartNo=i) and (StartOffs=2))         
     then begin
        TImageList(li.gr).BlendColor := FStyle.SelColor;
        TImageList(li.gr).DrawingStyle := dsSelected;
     end;
     TImageList(li.gr).Draw(canv, dli.Left-xshift, dli.Top-yshift, li.imgNo);
     TImageList(li.gr).DrawingStyle := ImgList.dsNormal;
     continue;
   end;
   if no = -2 then continue; { check point }
   if no = -1 then begin {break line}
      canv.Pen.Color := FStyle.TextStyles[0].Color;
      canv.MoveTo(dli.Left+5-xshift, dli.Top+5-yshift);
      canv.LineTo(XSize-5-xshift-FRightMargin, dli.Top+5-yshift);
   end;
   { controls ignored }
 end;
 Canvas.Draw(Canvas.ClipRect.Left, Canvas.ClipRect.Top, buffer);
 buffer.Free;
end;
{------------------------------------------------------------------}
procedure TRichView.InvalidateJumpRect(no: Integer);
var rec: TRect;
    i, id : Integer;
begin
   if Style.FullRedraw then
     Invalidate
   else begin
     id := no;
     for i:=0 to Jumps.Count -1 do
      if id = TJumpInfo(jumps.objects[i]).id then
       with TJumpInfo(jumps.objects[i]) do begin
         rec.Left := l-Hpos-5;
         rec.Top  := t-VPos*SmallStep-5;
         rec.Right := l+w-Hpos+5;
         rec.Bottom := t+h-VPos*SmallStep+5;
         InvalidateRect(Handle, @rec, False);
       end;
   end;
   Update;
end;
  {------------------------------------------------------------------}
procedure TRichView.CMMouseLeave(var Message: TMessage);
begin
   if DrawHover and (LastJumpMovedAbove<>-1) then begin
     DrawHover := False;
     InvalidateJumpRect(LastJumpMovedAbove);
   end;
   if Assigned(FOnRVMouseMove) and
      (LastJumpMovedAbove<>-1) then begin
      LastJumpMovedAbove := -1;
      OnRVMouseMove(Self,-1);
   end;
end;
  {------------------------------------------------------------------}
procedure TRichView.MouseMove(Shift: TShiftState; X, Y: Integer);
var i, no, offs,ys: Integer;
  OldCur: TCursor;
begin
    ScrollDelta := 0;
    if Y<0 then ScrollDelta := -1;
    if Y<-20 then ScrollDelta := -10;
    if Y>ClientHeight then ScrollDelta := 1;
    if Y>ClientHeight+20 then ScrollDelta := 10;
    inherited MouseMove(Shift, X, Y);
    if Selection  then begin
      XMouse := x;
      YMouse := y;
      ys := y;
      if ys<0 then y:=0;
      if ys>ClientHeight then ys:=ClientHeight;
      FindItemForSel(X+HPos, ys+VPos*SmallStep, no, offs);
      FSelEndNo   := no;
      FselEndOffs    := offs;
      Invalidate;
    end;
    OldCur := Cursor;
    for i:=0 to jumps.Count-1 do
      if (X>=TJumpInfo(jumps.objects[i]).l-HPos) and
         (X<=TJumpInfo(jumps.objects[i]).l+TJumpInfo(jumps.objects[i]).w-HPos) and
         (Y>=TJumpInfo(jumps.objects[i]).t-VPos*SmallStep) and
         (Y<=TJumpInfo(jumps.objects[i]).t+TJumpInfo(jumps.objects[i]).h-VPos*SmallStep) then
       begin
         Cursor :=  FStyle.JumpCursor;
         if Assigned(FOnRVMouseMove) and
            (LastJumpMovedAbove<>TJumpInfo(jumps.objects[i]).id) then begin
            OnRVMouseMove(Self,TJumpInfo(jumps.objects[i]).id+FirstJumpNo);
         end;
         if DrawHover and (LastJumpMovedAbove<>-1) and
            (LastJumpMovedAbove<>TJumpInfo(jumps.objects[i]).id) then begin
           DrawHover := False;
           InvalidateJumpRect(LastJumpMovedAbove);
         end;
         LastJumpMovedAbove := TJumpInfo(jumps.objects[i]).id;
         if (Style<>nil) and (Style.HoverColor<>clNone) and not DrawHover then begin
           DrawHover := True;
           InvalidateJumpRect(LastJumpMovedAbove);
         end;
         exit;
       end;
   Cursor :=  OldCur;
   if DrawHover and (LastJumpMovedAbove<>-1) then begin
     DrawHover := False;
     InvalidateJumpRect(LastJumpMovedAbove);
   end;
   if Assigned(FOnRVMouseMove) and
      (LastJumpMovedAbove<>-1) then begin
      LastJumpMovedAbove := -1;
      OnRVMouseMove(Self,-1);
   end;
   if Selection then Invalidate;
end;
{-------------------------------------}
procedure TRichView.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i, StyleNo, no, offs, ys: Integer;
    clickedword: String;
    p: TPoint;
begin
    if ScrollTimer<> nil then begin
      ScrollTimer.Free;
      ScrollTimer := nil;
    end;
    XClicked := X;
    YClicked := Y;
    if Selection and (Button = mbLeft) then begin
      ys := y;
      if ys<0 then y:=0;
      if ys>ClientHeight then ys:=ClientHeight;
      FindItemForSel(XClicked+HPos, ys+VPos*SmallStep, no, offs);
      FSelEndNo   := no;
      FselEndOffs    := offs;
      Selection   := False;
      Invalidate;
      if Assigned(FOnSelect) then FOnSelect(Self);
    end;
    if Button = mbRight then begin
      inherited MouseUp(Button, Shift, X, Y);
      if not Assigned(FOnRVRightClick) then exit;
      p := ClientToScreen(Point(X,Y));
      if FindClickedWord(clickedword, StyleNo) then
        FOnRVRightClick(Self, clickedword, StyleNo,p.X,p.Y);
      exit;
    end;
    if Button <> mbLeft then exit;
    if (LastJumpDowned=-1) or not Assigned(FOnJump) then begin
      exit;
    end;
    for i:=0 to jumps.Count-1 do
    with jumps.objects[i] as TJumpInfo do
      if (LastJumpDowned=id) and
         (X>=l-HPos) and
         (X<=l+w-HPos) and
         (Y>=t-VPos*SmallStep) and
         (Y<=t+h-VPos*SmallStep) then
          begin
            OnJump(Self,id+FirstJumpNo);
            break;
          end;
    LastJumpDowned:=-1;
    inherited MouseUp(Button, Shift, X, Y);
end;
{-------------------------------------}
procedure TRichView.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i,no, StyleNo: Integer;
    clickedword: String;
begin
  if Button <> mbLeft then exit;
  XClicked := X;
  YClicked := Y;
  //if Assigned(FOnJump) then begin
    LastJumpDowned := -1;
    for i:=0 to jumps.Count-1 do
     with jumps.objects[i] as TJumpInfo do
      if (X>=l-HPos) and
         (X<=l+w-HPos) and
         (Y>=t-VPos*SmallStep) and
         (Y<=t+h-VPos*SmallStep) then
           begin
             LastJumpDowned := id;
             break;
           end;
    if {LastJumpDowned=-1} AllowSelection then begin
      FindItemForSel(XClicked+HPos, YClicked+VPos*SmallStep, no, FSelStartOffs);
      FSelStartNo := no;
      FSelEndNo   := no;
      Selection   := (no<>-1);
      FSelEndOffs := FSelStartOffs;
      Invalidate;
      if ScrollTimer = nil then begin
        ScrollTimer := TTimer.Create(nil);
        ScrollTimer.OnTimer := OnScrollTimer;
        ScrollTimer.Interval := 100;
      end;

    end;
    if SingleClick and Assigned(FOnRVDblClick) and FindClickedWord(clickedword, StyleNo) then
       FOnRVDblClick(Self, clickedword, StyleNo);
    inherited MouseDown(Button, Shift, X, Y);
end;
{-------------------------------------}
procedure TRichView.AppendFrom(Source: TRichView);
var i: Integer;
    gr: TGraphic;
    grclass: TGraphicClass;
    li: TLineInfo;
begin
  ClearTemporal;
  for i:=0 to Source.Lines.Count-1 do begin
    li := TLineInfo(Source.Lines.Objects[i]);
    case li.StyleNo of
      -1: AddBreak;
      -2: AddCheckPoint;
      -3: begin
           grclass := TGraphicClass(li.gr.ClassType);
           gr := grclass.Create;
           gr.Assign(li.gr);
           AddPicture(gr);
        end;
      -4: AddHotSpot(li.imgNo, TImageList(li.gr), not li.SameAsPrev);
      -5: ;
       {
       begin
           if li.gr is
           ctrlclass := TControlClass(li.gr.ClassType);
           ctrl := ctrlclass.Create(Self);
           ctrl.Assign(li.gr);
           AddControl(ctrl, li.Center);
        end;
        }
      -6: AddBullet(li.imgNo, TImageList(li.gr), not li.SameAsPrev);
      else
        begin
          if li.Center then
               AddCenterLine(Source.Lines[i], li.StyleNo)
          else
             if li.SameAsPrev then
                Add(Source.Lines[i], li.StyleNo)
             else
                AddFromNewLine(Source.Lines[i], li.StyleNo)
        end;
    end;
  end;
end;
{-------------------------------------}
function TRichView.GetLastCP: Integer;
begin
  GetLastCP := CheckPoints.Count-1;
end;
{-------------------------------------}
procedure TRichView.SetBackBitmap(Value: TBitmap);
begin
  FBackBitmap.Assign(Value);
  if (Value=nil) or (Value.Empty) then
     FullRedraw := False
  else
     case FBackgroundStyle of
       bsNoBitmap, bsTiledAndScrolled:
               FullRedraw := False;
       bsStretched, bsTiled:
               FullRedraw := True;
     end;
end;
{-------------------------------------}
procedure TRichView.SetBackgroundStyle(Value: TBackgroundStyle);
begin
  FBackgroundStyle := Value;
  if FBackBitmap.Empty then
     FullRedraw := False
  else
     case FBackgroundStyle of
       bsNoBitmap, bsTiledAndScrolled:
               FullRedraw := False;
       bsStretched, bsTiled:
               FullRedraw := True;
     end;
end;
{-------------------------------------}
procedure TRichView.DrawBack(DC: HDC; Rect: TRect; Width,Height:Integer);
var i, j: Integer;
    hbr: HBRUSH;
begin
 if FStyle = nil then exit; 
 if FBackBitmap.Empty or (FBackgroundStyle=bsNoBitmap) then begin
   hbr := CreateSolidBrush(ColorToRGB(FStyle.Color));
   dec(Rect.Bottom, Rect.Top);
   dec(Rect.Right, Rect.Left);
   Rect.Left := 0;
   Rect.Top := 0;
   FillRect(DC, Rect, hbr);
   DeleteObject(hbr);
  end
 else
   case FBackgroundStyle of
     bsTiled:
      for i:= Rect.Top div FBackBitmap.Height to Rect.Bottom div FBackBitmap.Height do
        for j:= Rect.Left div FBackBitmap.Width to Rect.Right div FBackBitmap.Width do
          BitBlt(DC, j*FBackBitmap.Width-Rect.Left,i*FBackBitmap.Height-Rect.Top, FBackBitmap.Width,
                 FBackBitmap.Height, FBackBitmap.Canvas.Handle, 0, 0, SRCCOPY);
     bsStretched:
          StretchBlt(DC, -Rect.Left, -Rect.Top, Width, Height,
                     FBackBitmap.Canvas.Handle, 0, 0, FBackBitmap.Width, FBackBitmap.Height,
                     SRCCOPY);
     bsTiledAndScrolled:
      for i:= (Rect.Top+VPos*SmallStep) div FBackBitmap.Height to
              (Rect.Bottom+VPos*SmallStep) div FBackBitmap.Height do
        for j:= (Rect.Left+HPos) div FBackBitmap.Width to
                (Rect.Right+HPos) div FBackBitmap.Width do
          BitBlt(DC, j*FBackBitmap.Width-HPos-Rect.Left,i*FBackBitmap.Height-VPos*SmallStep-Rect.Top, FBackBitmap.Width,
                 FBackBitmap.Height, FBackBitmap.Canvas.Handle, 0, 0, SRCCOPY);
   end
end;
{-------------------------------------}
procedure TRichView.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var r1: TRect;
begin
  if (csDesigning in ComponentState) then exit;
  Message.Result := 1;
  if (OldWidth<ClientWidth) or (OldHeight<ClientHeight) then begin
      {$IFDEF FPC}
      GetClipBox(Message.DC, @r1);
      {$ELSE}
      GetClipBox(Message.DC, r1);
      {$ENDIF}
      DrawBack(Message.DC, r1, ClientWidth, ClientHeight);
  end;
  OldWidth := ClientWidth;
  OldHeight := ClientHeight;
end;
{-------------------------------------}
procedure TRichView.SetVSmallStep(Value: Integer);
begin
   if (Value<=0) or (TextHeight div Value > 30000) then exit;
   SmallStep := Value;
end;
{-------------------------------------}
procedure TRichView.ShareLinesFrom(Source: TRichView);
begin
   if ShareContents then begin
     Clear;
     lines := Source.Lines;
   end;
end;
{-------------------------------------}
function TRichView.FindItemAtPos(X,Y: Integer): Integer;
var
    i, a,b,mid, midtop: Integer;
    dli: TDrawLineInfo;

begin
  if DrawLines.Count = 0 then begin
     FindItemAtPos := -1;
     exit;
  end;
  dli := TDrawLineInfo(drawlines.Objects[0]);
  if (dli.Top<=Y) and (dli.Top+dli.Height>Y) and
     (dli.Left<=X) and (dli.Left+dli.Width>X) then begin
     FindItemAtPos := 0;
     exit;
  end;
  a := 1;
  b := DrawLines.Count-1;
  while (b-a)>1 do begin
    mid := (a+b) div 2;
    if (TDrawLineInfo(drawlines.Objects[mid]).Top<=Y) then
      a := mid
    else
      b := mid;
  end;
  mid := a;
  midtop := TDrawLineInfo(drawlines.Objects[mid]).Top;
  while (mid>=1) and
         (TDrawLineInfo(drawlines.Objects[mid-1]).Top+
         TDrawLineInfo(drawlines.Objects[mid-1]).Height>midtop) do dec(mid);
  for i:=1 to 2 do begin
    if mid = DrawLines.Count then break;
    midtop := TDrawLineInfo(drawlines.Objects[mid]).Top+
            TDrawLineInfo(drawlines.Objects[mid]).Height-1;
    while (mid<drawlines.Count) do begin
     dli := TDrawLineInfo(drawlines.Objects[mid]);
     if (dli.Top>midtop) then break;
     if (dli.Top<=Y) and (dli.Top+dli.Height>Y) and
     (dli.Left<=X) and (dli.Left+dli.Width>X) then begin
        FindItemAtPos := mid;
        exit;
     end;
     inc(mid);
    end;
  end;
  FindItemAtPos := -1;
end;
  {------------------------------------------------------------------}
procedure TRichView.FindItemForSel(X,Y: Integer; var No, Offs: Integer);
var
    styleno,i, a,b,mid, midtop, midbottom, midleft, midright, beginline, endline: Integer;
    dli: TDrawLineInfo;
    {$IFNDEF RICHVIEWDEF4}
    arr: array[0..1000] of integer;
    {$ENDIF}
    sz: TSIZE;    
begin
  if DrawLines.Count = 0 then begin
     No := -1;
     exit;
  end;
  dli := TDrawLineInfo(drawlines.Objects[0]);
  if {(dli.Top<=Y) and }(dli.Top+dli.Height>Y) {and
     (dli.Left<=X) and (dli.Left+dli.Width>X)} then
     mid := 0
  else begin
    a := 1;
    b := DrawLines.Count-1;
    while (b-a)>1 do begin
      mid := (a+b) div 2;
      if (TDrawLineInfo(drawlines.Objects[mid]).Top<=Y) then
        a := mid
      else
        b := mid;
    end;
    mid := a;
    if TDrawLineInfo(drawlines.Objects[b]).Top<=Y then mid := b;
  end;
  midtop := TDrawLineInfo(drawlines.Objects[mid]).Top;
  midbottom := midtop + TDrawLineInfo(drawlines.Objects[mid]).Height;
  // searching beginning of line "mid" belong to
  beginline := mid;
  while (beginline>=1) and
         (TDrawLineInfo(drawlines.Objects[beginline-1]).Top+
         TDrawLineInfo(drawlines.Objects[beginline-1]).Height>midtop) do dec(beginline);
  // searching end of line "mid" belong to
  endline := mid;
  while (endline<DrawLines.Count-1) and
         (TDrawLineInfo(drawlines.Objects[endline+1]).Top<midbottom) do inc(endline);
  // calculating line bounds
  midleft := TDrawLineInfo(drawlines.Objects[mid]).Left;
  midright := midleft+TDrawLineInfo(drawlines.Objects[mid]).Width;
  for i:= beginline to endline do begin
    dli := TDrawLineInfo(drawlines.Objects[i]);
    if dli.Top < midtop then midtop := dli.Top;
    if dli.Top + dli.Height > midbottom then midbottom := dli.Top + dli.Height;
    if dli.Left < midleft then midleft := dli.Left;
    if dli.Left + dli.Width > midright then midright := dli.Left + dli.Width;
  end;
  if (Y<midtop) or (X<midleft) then begin
  {
     No := beginline-1;
     if No<0 then begin
       No := 0;
       Offs := 1;
       end
     else begin
       if TLineInfo(Lines.Objects[TDrawLineInfo(DrawLines.Objects[No]).LineNo]).StyleNo<0 then
         Offs := 2
       else
         Offs := Length(DrawLines[No])+1;
     end;
     exit;
  }
     No := beginline;
     if TLineInfo(Lines.Objects[TDrawLineInfo(DrawLines.Objects[No]).LineNo]).StyleNo<0 then
         Offs := 0
       else
         Offs := 1;
     exit;
  end;
  if (Y>midbottom) or (X>midright) then begin
     No := endline+1;
     Offs := 1;
     if No>=DrawLines.Count then begin
       No := DrawLines.Count-1;
       Offs := Length(DrawLines[No])+1;
       end
     else begin
       if TLineInfo(Lines.Objects[TDrawLineInfo(DrawLines.Objects[No]).LineNo]).StyleNo<0 then
         Offs := 0;
     end;
     exit;
  end;
  for i:= beginline to endline do begin
    dli := TDrawLineInfo(drawlines.Objects[i]);
    if (dli.Left<=X) and (dli.Left+dli.Width>=X) then begin
      styleno := TLineInfo(lines.Objects[dli.LineNo]).StyleNo;
      No := i;
      Offs := 0;
      if styleno>=0 then begin
        with FStyle.TextStyles[StyleNo] do begin
         Canvas.Font.Style := Style;
         Canvas.Font.Size  := Size;
         Canvas.Font.Name  := FontName;
         {$IFDEF RICHVIEWDEF3}
         Canvas.Font.CharSet  := CharSet;
         {$ENDIF}
       end;
       {$IFDEF FPC}
       MyGetTextExtentExPoint(Canvas.Handle,  PChar(DrawLines[i]),  Length(DrawLines[i]),
       {$ELSE}
       GetTextExtentExPoint(Canvas.Handle,  PChar(DrawLines[i]),  Length(DrawLines[i]),
       {$ENDIF}
                            X-dli.Left,
                            {$IFDEF RICHVIEWDEF4}
                            @Offs, nil,
                            {$ELSE}
                            Offs, arr[0],
                            {$ENDIF}
                             sz);
       inc(Offs);
       if Offs>Length(DrawLines[i]) then Offs := Length(DrawLines[i]);
       if (Offs < 1) and (Length(DrawLines[i])>0) then Offs := 1;
       end
      else
       Offs := 1;
    end;
  end;
end;
  {------------------------------------------------------------------}
function TRichView.FindClickedWord(var clickedword: String; var StyleNo: Integer): Boolean;
var no, lno: Integer;
{$IFNDEF RICHVIEWDEF4}
    arr: array[0..1000] of integer;
{$ENDIF}
    sz: TSIZE;
    max,first,len: Integer;
begin
  FindClickedWord := False;
  no := FindItemAtPos(XClicked+HPos, YClicked+VPos*SmallStep);
  if no<>-1 then begin
     lno := TDrawLineInfo(drawlines.Objects[no]).LineNo;
     clickedword := drawlines[no];
     styleno := TLineInfo(lines.Objects[lno]).StyleNo;
     if styleno>=0 then begin
        with FStyle.TextStyles[StyleNo] do begin
         Canvas.Font.Style := Style;
         Canvas.Font.Size  := Size;
         Canvas.Font.Name  := FontName;
         {$IFDEF RICHVIEWDEF3}
         Canvas.Font.CharSet  := CharSet;
         {$ENDIF}
       end;
       {$IFDEF FPC}
       MyGetTextExtentExPoint(Canvas.Handle,PChar(clickedword),Length(clickedword),
       {$ELSE}
       GetTextExtentExPoint(Canvas.Handle,  PChar(clickedword),  Length(clickedword),
       {$ENDIF}
                            XClicked+HPos-TDrawLineInfo(drawlines.Objects[no]).Left,
                            {$IFDEF RICHVIEWDEF4}
                            @max, nil,
                            {$ELSE}
                            max, arr[0],
                            {$ENDIF}
                            sz);
       inc(max);
       if max>Length(clickedword) then max := Length(clickedword);
       first := max;
       if (Pos(clickedword[first], Delimiters)<>0) then begin
         ClickedWord := '';
         FindClickedWord := True;
         exit;
       end;
       while (first>1) and (Pos(clickedword[first-1], Delimiters)=0) do
         dec(first);
       len := max-first+1;
       while (first+len-1<Length(clickedword)) and (Pos(clickedword[first+len], Delimiters)=0) do
         inc(len);
       clickedword := copy(clickedword, first, len);
     end;
     FindClickedWord := True;
  end;

end;
  {------------------------------------------------------------------}
procedure TRichView.DblClick;
var
    StyleNo: Integer;
    clickedword: String;
begin
  inherited DblClick;
  if SingleClick or (not Assigned(FOnRVDblClick)) then exit;
  if FindClickedWord(clickedword, StyleNo) then
     FOnRVDblClick(Self, clickedword, StyleNo);
end;
  {------------------------------------------------------------------}
procedure TRichView.DeleteSection(CpName: String);
var i,j, startno, endno: Integer;
begin
   if ShareContents then exit;
   for i:=0 to checkpoints.Count-1 do
     if checkpoints[i]=CpName then begin
       startno := TCPInfo(checkpoints.Objects[i]).LineNo;
       endno := Lines.Count-1;
       for j := i+1 to checkpoints.Count-1 do
         if checkpoints[j]<>'' then
         begin
           endno := TCPInfo(checkpoints.Objects[j]).LineNo-1;
           break;
         end;
       DeleteLines(startno, endno-startno+1);
       exit;
     end;
end;
  {------------------------------------------------------------------}
procedure TRichView.DeleteLines(FirstLine, Count: Integer);
var i: Integer;
begin
  if ShareContents then exit;
  if FirstLine>=lines.Count then exit;
  Deselect;
  if FirstLine+Count>lines.Count then Count := lines.Count-firstline;
  lines.BeginUpdate;
  for i:=FirstLine to FirstLine+Count-1 do begin
    if TLineInfo(lines.objects[i]).StyleNo = -3 then { image}
      begin
        TLineInfo(lines.objects[i]).gr.Free;
        TLineInfo(lines.objects[i]).gr := nil;
      end;
    if TLineInfo(lines.objects[i]).StyleNo = -5 then {control}
      begin
        RemoveControl(TControl(TLineInfo(lines.objects[i]).gr));
        TLineInfo(lines.objects[i]).gr.Free;
        TLineInfo(lines.objects[i]).gr := nil;
      end;
    TLineInfo(lines.objects[i]).Free;
    lines.objects[i] := nil;
  end;
  for i:=1 to Count do lines.Delete(FirstLine);
  lines.EndUpdate;
end;
  {------------------------------------------------------------------}
procedure TRichView.GetSelBounds(var StartNo, EndNo, StartOffs, EndOffs: Integer);
begin
   if FSelStartNo <= FSelEndNo then begin
     StartNo := FSelStartNo;
     EndNo   := FSelEndNo;
     if not ((StartNo=EndNo) and (FSelStartOffs>FSelEndOffs)) then begin
         StartOffs := FSelStartOffs;
         EndOffs   := FSelEndOffs;
       end
     else begin
         StartOffs := FSelEndOffs;
         EndOffs   := FSelStartOffs;
       end;
     end
   else begin
     StartNo := FSelEndNo;
     EndNo   := FSelStartNo;
     StartOffs := FSelEndOffs;
     EndOffs   := FSelStartOffs;
   end;
end;
  {------------------------------------------------------------------}
procedure TRichView.StoreSelBounds(var StartNo, EndNo, StartOffs, EndOffs: Integer);
var dli: TDrawLineInfo;
begin
  GetSelBounds(StartNo, EndNo, StartOffs, EndOffs);
  if StartNo<>-1 then begin
    dli := TDrawLineInfo(DrawLines.Objects[StartNo]);
    if TLineInfo(Lines.Objects[dli.LineNo]).StyleNo>=0 then
        inc(StartOffs, dli.Offs-1);
    StartNo := dli.LineNo;
    dli := TDrawLineInfo(DrawLines.Objects[EndNo]);
    if TLineInfo(Lines.Objects[dli.LineNo]).StyleNo>=0 then
        inc(EndOffs, dli.Offs-1);
    EndNo := dli.LineNo;
  end;
end;
  {------------------------------------------------------------------}
procedure TRichView.RestoreSelBounds(StartNo, EndNo, StartOffs, EndOffs: Integer);
var i: Integer;
    dli, dli2, dli3: TDrawLineInfo;
begin
  if StartNo = -1 then exit;
  for i :=0 to DrawLines.Count-1 do begin
    dli := TDrawLineInfo(DrawLines.Objects[i]);
    if dli.LineNo = StartNo then
      if TLineInfo(Lines.Objects[dli.LineNo]).StyleNo<0 then begin
        FSelStartNo := i;
        FSelStartOffs := StartOffs;
        end
      else begin
        if i<>DrawLines.Count-1 then
          dli2 := TDrawLineInfo(DrawLines.Objects[i+1])
        else
          dli2 := nil;
        if i<>0 then
          dli3 := TDrawLineInfo(DrawLines.Objects[i-1])
        else
          dli3 := nil;
        if
          ((dli.Offs<=StartOffs) and (Length(DrawLines[i])+dli.Offs>StartOffs)) or
          ((StartOffs>Length(Lines[dli.LineNo])) and ((dli2=nil)or(dli2.LineNo<>dli.LineNo))) or
          ((dli.Offs>StartOffs) and ((dli3=nil)or(dli3.LineNo<>dli.LineNo)))
        then begin
          FSelStartNo := i;
          FSelStartOffs := StartOffs-dli.Offs+1;
          if FSelStartOffs<0 then FSelStartOffs := 0;
          if FSelStartOffs>dli.Offs+Length(DrawLines[i]) then FSelStartOffs := dli.Offs+Length(DrawLines[i]);
        end;
      end;
    if dli.LineNo = EndNo then
      if TLineInfo(Lines.Objects[dli.LineNo]).StyleNo<0 then begin
        FSelEndNo := i;
        FSelEndOffs := EndOffs;
        end
      else begin
        if i<>DrawLines.Count-1 then
          dli2 := TDrawLineInfo(DrawLines.Objects[i+1])
        else
          dli2 := nil;
        if i<>0 then
          dli3 := TDrawLineInfo(DrawLines.Objects[i-1])
        else
          dli3 := nil;
        if
          ((dli.Offs<=EndOffs) and (Length(DrawLines[i])+dli.Offs>EndOffs)) or
          ((EndOffs>Length(Lines[dli.LineNo])) and ((dli2=nil)or(dli2.LineNo<>dli.LineNo))) or
          ((dli.Offs>EndOffs) and ((dli3=nil)or(dli3.LineNo<>dli.LineNo)))
        then begin
          FSelEndNo := i;
          FSelEndOffs := EndOffs-dli.Offs+1;
          if FSelEndOffs<0 then FSelEndOffs := 0;
          if FSelEndOffs>dli.Offs+Length(DrawLines[i]) then FSelEndOffs := dli.Offs+Length(DrawLines[i]);
        end;
      end;
  end;
end;
  {------------------------------------------------------------------}
function TRichView.GetLineCount: Integer;
begin
  GetLineCount := lines.Count;
end;
  {------------------------------------------------------------------}
function TRichView.SelectionExists: Boolean;
var StartNo, EndNo, StartOffs, EndOffs: Integer;
begin
  GetSelBounds(StartNo, EndNo, StartOffs, EndOffs);
  if (StartNo = -1) or (EndNo = -1) or ((StartNo=EndNo) and (StartOffs=EndOffs)) then
    Result := False
  else
    Result := True;
end;
  {------------------------------------------------------------------}
function TRichView.GetSelText: String;
var StartNo, EndNo, StartOffs, EndOffs, i: Integer;
    s : String;
    li : TLineInfo;
begin
  Result := '';
  if not SelectionExists then exit;
  { getting selection as Lines indices }
  StoreSelBounds(StartNo, EndNo, StartOffs, EndOffs);
  if StartNo = EndNo then begin
    li := TLineInfo(Lines.Objects[StartNo]);
    if li.StyleNo < 0 then exit;
    Result := Copy(Lines[StartNo], StartOffs, EndOffs-StartOffs);
    exit;
    end
  else begin
    li := TLineInfo(Lines.Objects[StartNo]);
    if li.StyleNo < 0 then
      s := ''
    else
      s := Copy(Lines[StartNo], StartOffs, Length(Lines[StartNo]));
    for i := StartNo+1 to EndNo do begin
      li := TLineInfo(Lines.Objects[i]);
      if (li.StyleNo<>rvsCheckpoint) and not li.SameAsPrev then
          s := s+chr(13);
      if li.StyleNo >= 0 then
        if i<>EndNo then
          s := s + Lines[i]
        else
          s := s + Copy(Lines[i], 1, EndOffs-1);
    end;
    {$IFDEF FPC}
    Result := AdjustLineBreaks(s, tlbsCRLF);
    {$ELSE}
    Result := AdjustLineBreaks(s);
    {$ENDIF}
    exit;
  end;
end;
  {------------------------------------------------------------------}
procedure TRichView.CopyText;
begin
  if SelectionExists then begin
    ClipBoard.Clear;
    Clipboard.SetTextBuf(PChar(GetSelText));
  end;
end;
  {------------------------------------------------------------------}
procedure TRichView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if SelectionExists and (ssCtrl in Shift) then begin
    if (Key = ord('C')) or (Key = VK_INSERT) then CopyText;
    end
  else
    inherited KeyDown(Key,Shift)
end;
  {------------------------------------------------------------------}
procedure TRichView.OnScrollTimer(Sender: TObject);
begin
  if ScrollDelta<>0 then begin
    VScrollPos := VScrollPos+ScrollDelta;
    MouseMove([], XMouse, YMouse);
  end;
end;
  {------------------------------------------------------------------}
procedure TRichView.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (AComponent=FStyle) then begin
      Style := nil;
  end;
end;
  {------------------------------------------------------------------}
procedure TRichView.Click;
begin
  SetFocus;
  inherited;
end;
  {------------------------------------------------------------------}
procedure TRichView.Loaded;
begin
  inherited Loaded;
  Format;
end;
  {------------------------------------------------------------------}
{$I RV_Save.inc}
  {------------------------------------------------------------------}

end.
