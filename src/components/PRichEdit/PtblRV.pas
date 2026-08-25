unit PtblRV;

{$mode Delphi}

interface

uses
  {$IFDEF FPC}
  LCLType,LclIntf,OsPrinters,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  RVScroll, RichView, Printers{, CommDlg};

{$I RV_Defs.inc}

type
  {------------------------------------------------------------}
  TRVPrintingStep = (rvpsStarting, rvpsProceeding, rvpsFinished);
  TRVPrintingEvent = procedure (Sender: TRichView; PageCompleted: Integer; Step:TRVPrintingStep) of object;
  {------------------------------------------------------------}
  TRVPageInfo = class (TCollectionItem)
    public
      StartY, StartLineNo : Integer;
      procedure Assign(Source: TPersistent); override;
  end;
  {------------------------------------------------------------}
  EInvalidPageNo = class(Exception);
  EStyleNotAssigned = class(Exception);
  {------------------------------------------------------------}
  TPrintableRV = class(TRichView)
  private
    { Private declarations }
    FOnFormatting, FOnPrinting: TRVPrintingEvent;
    pagescoll: TCollection;
    FLeftMarginMM, FRightMarginMM, FTopMarginMM, FBottomMarginMM: Integer;
    TmpLM, TmpTM, TmpRM, TmpBM: Integer;
    PrinterSad: TScreenAndDevice;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FormatPages: Integer;
    procedure DrawPage(pgNo: Integer; Canvas: TCanvas);
    procedure PrintPages(firstPgNo, lastPgNo: Integer; Title: String;
                         Copies: Integer; Collate: Boolean);
    procedure Print(Title: String;
                    Copies: Integer; Collate: Boolean);
  published
    { Published declarations }
    property OnFormatting: TRVPrintingEvent read FOnFormatting write FOnFormatting;
    property OnSendingToPrinter: TRVPrintingEvent read FOnPrinting write FOnPrinting;
  end;
  {------------------------------------------------------------}
  TRVPrint = class(TComponent)
  private
    { Private declarations }
    FOnFormatting, FOnPrinting: TRVPrintingEvent;
    function GetLM: Integer;
    function GetRM: Integer;
    function GetTM: Integer;
    function GetBM: Integer;
    procedure SetLM(mm: Integer);
    procedure SetRM(mm: Integer);
    procedure SetTM(mm: Integer);
    procedure SetBM(mm: Integer);
    function GetPagesCount: Integer;
  protected
    { Protected declarations }
  public
    { Public declarations }
    rv: TPrintableRV;
    constructor Create(AOwner: TComponent); override;
    procedure AssignSource(PrintMe: TRichView);
    procedure Clear;
    function FormatPages(PrintOptions:TRVDisplayOptions): Integer;
    procedure PrintPages(firstPgNo, lastPgNo: Integer; Title: String;
                         Copies: Integer; Collate: Boolean);
    procedure Print(Title: String; Copies: Integer; Collate: Boolean);
    procedure MakePreview(pgNo: Integer; bmp: TBitmap);

  published
    { Published declarations }
    property PagesCount: Integer read GetPagesCount;
    property LeftMarginMM:  Integer read GetLM write SetLM;
    property RightMarginMM: Integer read GetRM write SetRM;
    property TopMarginMM:   Integer read GetTM write SetTM;
    property BottomMarginMM:Integer read GetBM write SetBM;
    property OnFormatting: TRVPrintingEvent read FOnFormatting write FOnFormatting;
    property OnSendingToPrinter: TRVPrintingEvent read FOnPrinting write FOnPrinting;
  end;
function GetPrinterDC: HDC;
implementation
{==================================================================}
procedure TRVPageInfo.Assign(Source: TPersistent);
begin
  if Source is TRVPageInfo then begin
    StartY := TRVPageInfo(Source).StartY;
    StartLineNo := TRVPageInfo(Source).StartLineNo;    
    end
  else
    inherited Assign(Source);
end;
{==================================================================}
type
  TPrinterDevice = class
    Driver, Device, Port: String;
  end;
{$IFDEF FPC}
function GetPrinterDC: HDC;
begin
  result := 0; // not used in lazarus
end;
{$ELSE}
function GetPrinterDC: HDC;
var ADevice, ADriver, APort: array[0..79] of Char;
    ADeviceMode: THandle;
    DevMode: PDeviceMode;
begin
  //Printer.GetPrinter(ADevice,ADriver,APort,ADeviceMode);
  if ADeviceMode<>0 then
    DevMode := PDeviceMode(GlobalLock(ADeviceMode))
  else
    DevMode := nil;
  Result := CreateDC(ADriver, ADevice, APort, DevMode);
  if ADeviceMode<>0 then
    GlobalUnlock(ADeviceMode);
end;
{$ENDIF}
constructor TRVPrint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  rv := TPrintableRV.Create(Self);
  if not (csDesigning in ComponentState) then rv.Parent := TWinControl(Self.Owner);
  LeftMarginMM   := 20;
  RightMarginMM  := 20;
  TopMarginMM    := 20;
  BottomMarginMM := 20;
end;
{------------------------------------------------------------------}
function  TRVPrint.GetLM: Integer;
begin
   GetLM := rv.FLeftMarginMM;
end;
{------------------------------------------------------------------}
function  TRVPrint.GetRM: Integer;
begin
   GetRM := rv.FRightMarginMM;
end;
{------------------------------------------------------------------}
function  TRVPrint.GetTM: Integer;
begin
   GetTM := rv.FTopMarginMM;
end;
{------------------------------------------------------------------}
function  TRVPrint.GetBM: Integer;
begin
   GetBM := rv.FBottomMarginMM;
end;
{------------------------------------------------------------------}
procedure TRVPrint.SetLM(mm: Integer);
begin
   rv.FLeftMarginMM := mm;
end;
{------------------------------------------------------------------}
procedure TRVPrint.SetRM(mm: Integer);
begin
   rv.FRightMarginMM := mm;
end;
{------------------------------------------------------------------}
procedure TRVPrint.SetTM(mm: Integer);
begin
   rv.FTopMarginMM := mm;
end;
{------------------------------------------------------------------}
procedure TRVPrint.SetBM(mm: Integer);
begin
   rv.FBottomMarginMM := mm;
end;
{------------------------------------------------------------------}
function TRVPrint.FormatPages(PrintOptions:TRVDisplayOptions): Integer;
begin
  rv.DisplayOptions := PrintOptions;
  rv.FOnFormatting := FOnFormatting;
  FormatPages := rv.FormatPages;
end;
{------------------------------------------------------------------}
procedure TRVPrint.Print(Title: String; Copies: Integer; Collate: Boolean);
begin
  rv.FOnPrinting := FOnPrinting;
  rv.Print(Title, Copies, Collate);
end;
{------------------------------------------------------------------}
procedure TRVPrint.PrintPages(firstPgNo, lastPgNo: Integer; Title: String;
                              Copies: Integer; Collate: Boolean);
begin
  rv.FOnPrinting := FOnPrinting;
  rv.PrintPages(firstPgNo, lastPgNo, Title, Copies, Collate);
end;
{------------------------------------------------------------------}
procedure TRVPrint.AssignSource(PrintMe: TRichView);
begin
  rv.ShareLinesFrom(PrintMe);
  rv.Style := PrintMe.Style;
  rv.BackgroundBitmap := PrintMe.BackgroundBitmap;
  rv.BackgroundStyle := PrintMe.BackgroundStyle;
end;
{------------------------------------------------------------------}
procedure TRVPrint.Clear;
begin
  rv.Clear;
end;
{------------------------------------------------------------------}
procedure TRVPrint.MakePreview(pgNo: Integer; bmp: TBitmap);
var w,h: Integer;
begin
   w :=
     MulDiv(rv.Width+rv.TmpLM+rv.TmpRM, rv.Printersad.ppixScreen, rv.Printersad.ppixDevice);
   h :=
     MulDiv(rv.Height+rv.TmpTM+rv.TmpBM, rv.Printersad.ppiyScreen, rv.Printersad.ppiyDevice);
   if bmp.Width <> w then bmp.Width := w;
   if bmp.Height <> h then bmp.Height := h;
   bmp.Canvas.Brush.Color := clWhite;
   bmp.Canvas.Pen.Color := clWhite;
   bmp.Canvas.FillRect(Rect(0,0,bmp.Width, bmp.Height));
   rv.DrawPage(pgNo, bmp.Canvas);
end;
{------------------------------------------------------------------}
function TRVPrint.GetPagesCount: Integer;
begin
   GetPagesCount := rv.pagescoll.Count;
end;
{==================================================================}
constructor TPrintableRV.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  pagescoll := TCollection.Create(TRVPageInfo);
  Visible := False;
  lines.Free;
  lines := nil;
  ShareContents := True;
end;
{------------------------------------------------------------------}
destructor TPrintableRV.Destroy;
begin
  pagescoll.Free;
  inherited Destroy;
end;
{------------------------------------------------------------------}
function TPrintableRV.FormatPages: Integer;
var i,j       : Integer;
    dli, dli2, dli3 :TDrawLineInfo;
    nextnewline  : Integer;
    rvpi       : TRVPageInfo;
    nPages    : Integer;
    PrinterCanvas : TCanvas;
    PHDC: HDC;
    lpy, lpx, StartY : Integer;

begin
   if Assigned(FOnFormatting) then FOnFormatting(Self,0, rvpsStarting);
   VScrollVisible := False;
   {$IFDEF FPC}
   lpy := Printer.YDPI;
   lpx := Printer.XDPI;
   Width := Printer.PageWidth -MulDiv(FLeftMarginMM+FRightMarginMM, 5, 127)*lpx;
   Height:= Printer.PageHeight - MulDiv(FTopMarginMM+FBottomMarginMM, 5, 127)*lpy;
   with Printer.PaperSize.PaperRect do begin
    lpx := MulDiv(PhysicalRect.Right-PhysicalRect.Left, 127, 5*lpx);
    lpy := MulDiv(PhysicalRect.Bottom-PhysicalRect.Top, 127, 5*lpy);
   end;
   TmpLM := MulDiv(FLeftMarginMM, Printer.PageWidth, lpx);
   TmpTM := MulDiv(FTopMarginMM, Printer.PageHeight, lpy);
   TmpRM := MulDiv(FRightMarginMM, Printer.PageWidth, lpx);
   TmpBM := MulDiv(FBottomMarginMM, Printer.PageHeight, lpy);
   Format_(False, 0, Printer.Canvas, False);
   InfoAboutSaD(PrinterSaD, Printer.Canvas);
   {$ELSE}
   PrinterCanvas := TCanvas.Create;
   PHDC := GetPrinterDC;
   PrinterCanvas.Handle := PHDC;
   lpy := GetDeviceCaps(PHDC, LOGPIXELSY);
   lpx := GetDeviceCaps(PHDC, LOGPIXELSX);
   PrinterCanvas.Font.PixelsPerInch := lpy;
   Width := Printer.PageWidth -MulDiv(FLeftMarginMM+FRightMarginMM, 5, 127)*lpx;
   Height:= Printer.PageHeight - MulDiv(FTopMarginMM+FBottomMarginMM, 5, 127)*lpy;
   lpx := GetDeviceCaps(PHDC, HORZSIZE);
   lpy := GetDeviceCaps(PHDC, VERTSIZE);
   TmpLM := MulDiv(FLeftMarginMM, Printer.PageWidth, lpx);
   TmpTM := MulDiv(FTopMarginMM, Printer.PageHeight, lpy);
   TmpRM := MulDiv(FRightMarginMM, Printer.PageWidth, lpx);
   TmpBM := MulDiv(FBottomMarginMM, Printer.PageHeight, lpy);
   Format_(False, 0, PrinterCanvas, False);
   InfoAboutSaD(PrinterSaD, PrinterCanvas);
   PrinterCanvas.Handle := 0;
   PrinterCanvas.Free;
   DeleteDC(PHDC);
   {$ENDIF}

   PagesColl.Clear;
   FormatPages := 0;
   if DrawLines.Count = 0 then exit;
   nPages := 1;
   rvpi     := TRVPageInfo(PagesColl.Add);
   rvpi.StartY := 0;
   rvpi.StartLineNo := 0;
   StartY := 0;
   i := 0;
   if Assigned(FOnFormatting) then FOnFormatting(Self,0, rvpsProceeding);
   while i<DrawLines.Count do
   begin
      dli := TDrawLineInfo(DrawLines.Objects[i]);
      if dli.Top+dli.Height>StartY+Height then begin { i-th item does not fit in page }
        nextnewline := i;
        { searching first item in first last in new page }
        for j:=i downto 0 do
        begin
          dli2 := TDrawLineInfo(DrawLines.Objects[j]);
          if (j<>i) and (dli2.Top + dli2.Height <= dli.Top) then break;
          nextnewline := j;
        end;
        { page must contain one item at least}
        if nextnewline = TRVPageInfo(PagesColl.Items[nPages-1]).StartLineNo then
           inc(nextnewline);
        if nextnewline<>DrawLines.Count then begin
           { searching min y of first line in new page }
           dli2 := TDrawLineInfo(DrawLines.Objects[nextnewline]);
           StartY := dli2.Top;
           for j := nextnewline+1 to DrawLines.Count-1 do begin
             dli3 := TDrawLineInfo(DrawLines.Objects[j]);
             if (dli3.Top >= dli2.Top + dli2.Height) then break;
             if dli3.Top<StartY then StartY := dli3.Top;
           end;
           rvpi             := TRVPageInfo(PagesColl.Add);
           rvpi.StartLineNo := nextnewline;
           rvpi.StartY      := StartY;
           if Assigned(FOnFormatting) then FOnFormatting(Self,nPages, rvpsProceeding);
           inc(nPages);           
        end;
        i := nextnewline;
        end
      else
        inc(i);
   end;
   if Assigned(FOnFormatting) then FOnFormatting(Self,nPages,rvpsProceeding);
   FormatPages := nPages;
   if Assigned(FOnFormatting) then FOnFormatting(Self,nPages, rvpsFinished);
end;
{------------------------------------------------------------------}

procedure DrawOnDevice(Canvas: TCanvas; x,y: Integer; sad:TScreenAndDevice; gr: TGraphic);
var
  Info: PBitmapInfo;
  InfoSize: DWORD;
  Image: Pointer;
  ImageSize: DWORD;
  Bits: HBITMAP;
  DIBWidth, DIBHeight: Longint;
  PrintWidth, PrintHeight: Longint;
begin
 if gr is TBitmap then begin
     Bits := TBitmap(gr).Handle;
     //GetDIBSizes(Bits, InfoSize, ImageSize);
     Info := AllocMem(InfoSize);
     try
        Image := AllocMem(ImageSize);
        try
          //GetDIB(Bits, 0, Info^, Image^);
          with Info^.bmiHeader do
            begin
              DIBWidth := biWidth;
              DIBHeight := biHeight;
            end;
            PrintWidth := MulDiv(DIBWidth, sad.ppixDevice, sad.ppixScreen);
            PrintHeight:= MulDiv(DIBHeight, sad.ppiyDevice, sad.ppiyScreen);
            StretchDIBits(Canvas.Handle, x, y, PrintWidth, PrintHeight, 0, 0,
              DIBWidth, DIBHeight, Image, Info^, DIB_RGB_COLORS, SRCCOPY);
        finally
          FreeMem(Image, ImageSize);
        end;
     finally
        FreeMem(Info, InfoSize);
     end;
 end;
end;
{------------------------------------------------------------------}
procedure TPrintableRV.DrawPage(pgNo: Integer; Canvas: TCanvas);
var i,no: Integer;
    dli:TDrawLineInfo;
    li: TLineInfo;
    zerocoord: Integer;
    first, last: Integer;
    sad:TScreenAndDevice;
    background, tmpbmp : TBitmap;
    BackWidth, BackHeight: Integer;
    //wmf: TMetafile;
begin
 if not Assigned(FStyle) then begin
  raise EStyleNotAssigned.Create('Style of printable TRichView component is not assigned');
  exit;
 end;
 if (pgNo<1) or (pgNo>PagesColl.Count) then begin
  raise EInvalidPageNo.Create('Invalid page number is specified for printing');
  exit;
 end;
 first := TRVPageInfo(PagesColl.Items[pgNo-1]).StartLineNo;
 if pgNo=PagesColl.Count then
   last := DrawLines.Count-1
 else
   last := TRVPageInfo(PagesColl.Items[pgNo]).StartLineNo-1;
 zerocoord := TRVPageInfo(PagesColl.Items[pgNo-1]).StartY-TmpTM;
 Canvas.Brush.Style := bsClear;
 InfoAboutSaD(sad, Canvas);
 BackWidth  := MulDiv(Width,  printersad.ppixScreen, printersad.ppixDevice);
 BackHeight := MulDiv(Height, printersad.ppiyScreen, printersad.ppiyDevice);
 if (BackGroundStyle <> bsNoBitmap) and (BackGroundBitmap<>nil) then begin
    if BackGroundStyle=bsTiledAndScrolled then BackGroundStyle:=bsTiled;
    background := TBitmap.Create;
    background.Width := BackWidth;
    background.Height := BackHeight;
    DrawBack(background.Canvas.Handle, Rect(0,0, BackWidth, BackHeight),
             BackWidth, BackHeight);
    DrawOnDevice(Canvas,
          MulDiv(TmpLM, sad.ppixDevice, Printersad.ppixDevice),
          MulDiv(TmpTM, sad.ppiyDevice, Printersad.ppiyDevice),
          sad, background);
    end
 else begin
    background := nil;
    Canvas.Pen.Color := Style.Color;
    Canvas.Brush.Color := Style.Color;
    Canvas.FillRect(
      Rect(
        MulDiv(TmpLM, sad.ppixDevice, Printersad.ppixDevice),
        MulDiv(TmpTM, sad.ppiyDevice, Printersad.ppiyDevice),
        MulDiv(TmpLM, sad.ppixDevice, Printersad.ppixDevice)+
        MulDiv(BackWidth,  sad.ppixDevice, sad.ppixScreen),
        MulDiv(TmpTM, sad.ppiyDevice, Printersad.ppiyDevice)+
        MulDiv(BackHeight, sad.ppiyScreen, sad.ppiyScreen)));
 end;
 tmpbmp := TBitmap.Create;
try
 for i:=first to last do begin
   dli := TDrawLineInfo(drawlines.Objects[i]);
   li := TLineInfo(lines.Objects[dli.LineNo]);
   no := li.StyleNo;
   if no>=0 then { text }
     with FStyle.TextStyles[no] do begin
       Canvas.Font.Color := Color;
       Canvas.Font.Style := Style;
       Canvas.Font.Size  := Size;
       Canvas.Font.Name  := FontName;
       {$IFDEF RICHVIEWDEF3}
       Canvas.Font.CharSet  := CharSet;
       {$ENDIF}
       Canvas.TextOut(
          MulDiv(dli.Left+TmpLM, sad.ppixDevice, Printersad.ppixDevice),
          MulDiv( dli.Top-zerocoord, sad.ppiyDevice, Printersad.ppiyDevice),
          drawlines.Strings[i]);
       continue;
     end;
   case no of
     -3,-4,-6:{ graphics } { hotspots and bullets }
       begin
         //if li.gr is TMetafile then begin
         //    wmf := TMetafile.Create;
         //    try
         //       wmf.Assign(li.gr);
         //       wmf.Width  := MulDiv(TMetafile(li.gr).Width, sad.ppixDevice, sad.ppixScreen);
         //       wmf.Height := MulDiv(TMetafile(li.gr).Height, sad.ppiyDevice, sad.ppiyScreen);
         //       Canvas.Draw(
         //         MulDiv(dli.Left+TmpLM, sad.ppixDevice, Printersad.ppixDevice),
         //         MulDiv( dli.Top-zerocoord, sad.ppiyDevice, Printersad.ppiyDevice),
         //         wmf);
         //    finally
         //      wmf.free;
         //    end;
         // end
         {else} begin
          if no = rvsPicture then begin
            tmpbmp.Width  := TGraphic(li.gr).Width;
            tmpbmp.Height := TGraphic(li.gr).Height;
            end
          else begin
            tmpbmp.Width  := TImageList(li.gr).Width;
            tmpbmp.Height := TImageList(li.gr).Height;
          end;
          if background<>nil then
            tmpbmp.Canvas.CopyRect(Rect(0,0, tmpbmp.Width, tmpbmp.Height),
                 background.Canvas,
                 Rect(
                     MulDiv(dli.Left,          Printersad.ppixScreen, Printersad.ppixDevice),
                     MulDiv(dli.Top-(zerocoord+TmpTM), Printersad.ppiyScreen, Printersad.ppiyDevice),
                     MulDiv(dli.Left,          Printersad.ppixScreen, Printersad.ppixDevice)+tmpbmp.Width,
                     MulDiv(dli.Top-(zerocoord+TmpTM), Printersad.ppiyScreen, Printersad.ppiyDevice)+tmpbmp.Height
                     )
                 )
          else begin
            tmpbmp.Canvas.Pen.Color := Style.Color;
            tmpbmp.Canvas.Brush.Color := Style.Color;
            tmpbmp.Canvas.FillRect(Rect(0,0, tmpbmp.Width, tmpbmp.Height));
          end;
          if no = rvsPicture then
             tmpbmp.Canvas.Draw(0,0, TGraphic(li.gr))
          else
             TImageList(li.gr).Draw(tmpbmp.Canvas,0,0,li.imgNo);
          DrawOnDevice(Canvas,
             MulDiv(dli.Left+TmpLM, sad.ppixDevice, Printersad.ppixDevice),
             MulDiv( dli.Top-zerocoord, sad.ppiyDevice, Printersad.ppiyDevice),
             sad, tmpbmp);
         end;
       end;
     -1: {break line}
       begin
         Canvas.Pen.Color := FStyle.TextStyles[0].Color;
         Canvas.MoveTo(
             MulDiv(dli.Left+TmpLM+MulDiv(5, printersad.ppixDevice, printersad.ppixScreen),
                   sad.ppixDevice, Printersad.ppixDevice),
             MulDiv(dli.Top-zerocoord+MulDiv(5, printersad.ppiyDevice, printersad.ppiyScreen),
                   sad.ppiyDevice, Printersad.ppiyDevice));
         Canvas.LineTo(
             MulDiv(Width+TmpLM-MulDiv(5+RightMargin, printersad.ppixDevice, printersad.ppixScreen),
                   sad.ppixDevice, Printersad.ppixDevice),
             MulDiv(dli.Top-zerocoord+MulDiv(5, printersad.ppiyDevice, printersad.ppiyScreen),
                   sad.ppiyDevice, Printersad.ppiyDevice));
       end;
     { controls is not supported yet }
   end;
 end;
finally
  background.Free;
  tmpbmp.Free;
end;
end;
{------------------------------------------------------------------}
procedure TPrintableRV.PrintPages(firstPgNo, lastPgNo: Integer; Title: String;
                                  Copies: Integer; Collate: Boolean);
var i,copyno: Integer;
    PrinterCopies: Integer;
begin
   if Assigned(FOnPrinting) then FOnPrinting(Self,0, rvpsStarting);
   Printer.Title := Title;   
   PrinterCopies := Printer.Copies; { storing }
   //if pcCopies in Printer.Capabilities then
   //  begin
   //    Printer.Copies := Copies;
   //                              // Printer can make copies and collation if needed
   //    Copies := 1;              // TRichView need not support copies and collation itself
   //  end
   //else
     Printer.Copies := 1;        // TRichView will provide copies and collation itself
   Printer.BeginDoc;
   if Collate then
     for copyno:= 1 to Copies do
       for i := firstPgNo to lastPgNo do
       begin
         DrawPage(i, Printer.Canvas);
         if Assigned(FOnPrinting) then FOnPrinting(Self,i, rvpsProceeding);
         if not ((i=lastPgNo) and (copyno=Copies)) then Printer.NewPage;
       end
   else
     for i := firstPgNo to lastPgNo do
       for copyno:= 1 to Copies do
       begin
         DrawPage(i, Printer.Canvas);
         if Assigned(FOnPrinting) then FOnPrinting(Self,i, rvpsProceeding);
         if not ((i=lastPgNo) and (copyno=Copies)) then Printer.NewPage;         
       end;
   Printer.EndDoc;
   Printer.Copies := PrinterCopies; { restoring }
   if Assigned(FOnPrinting) then FOnPrinting(Self,0, rvpsFinished);
end;
{------------------------------------------------------------------}
procedure TPrintableRV.Print(Title: String; Copies: Integer; Collate: Boolean);
begin
   PrintPages(1, PagesColl.Count, Title, Copies, Collate);
end;

end.
