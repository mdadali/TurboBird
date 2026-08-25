unit PtblRV;

{$IFDEF FPC}
  {$mode Delphi}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
  LCLType, LclIntf, OsPrinters,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  RVItems, RVScroll, RichView, Printers{, CommDlg};

{$I RV_Defs.inc}

type
  {------------------------------------------------------------}
  TRVPrintingStep = (rvpsStarting, rvpsProceeding, rvpsFinished);
  TRVPrintingEvent = procedure (Sender: TRichView; PageCompleted: Integer; Step: TRVPrintingStep) of object;
  {------------------------------------------------------------}
  TRVPageInfo = class (TCollectionItem)
  public
    StartY: Integer;
    StartLineNo: Integer;
    procedure Assign(Source: TPersistent); override;
  end;
  {------------------------------------------------------------}
  EInvalidPageNo = class(Exception);
  EStyleNotAssigned = class(Exception);
  {------------------------------------------------------------}
  TPrintableRV = class(TRichView)
  private
    { Private declarations }
    FPagesList: TCollection;
    FOnFormatting: TRVPrintingEvent;
    FOnPrinting: TRVPrintingEvent;
    FLeftMarginMM: Integer;
    FRightMarginMM: Integer;
    FTopMarginMM: Integer;
    FBottomMarginMM: Integer;
    TmpLM, TmpTM, TmpRM, TmpBM: Integer;
    PrinterSad: TScreenAndDevice;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FormatPages(): Integer;
    procedure DrawPage(pgNo: Integer; Canvas: TCanvas);
    procedure PrintPages(firstPgNo, lastPgNo: Integer; Title: string;
                         Copies: Integer; Collate: Boolean);
    procedure Print(Title: string; Copies: Integer; Collate: Boolean);

    property PagesList: TCollection read FPagesList;
  published
    { Published declarations }
    property OnFormatting: TRVPrintingEvent read FOnFormatting write FOnFormatting;
    property OnSendingToPrinter: TRVPrintingEvent read FOnPrinting write FOnPrinting;
  end;
  {------------------------------------------------------------}
  TRVPrint = class(TComponent)
  private
    { Private declarations }
    Frv: TPrintableRV;
    FOnFormatting: TRVPrintingEvent;
    FOnPrinting: TRVPrintingEvent;
    function GetLM(): Integer;
    function GetRM(): Integer;
    function GetTM(): Integer;
    function GetBM(): Integer;
    procedure SetLM(mm: Integer);
    procedure SetRM(mm: Integer);
    procedure SetTM(mm: Integer);
    procedure SetBM(mm: Integer);
    function GetPagesCount(): Integer;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure AssignSource(PrintMe: TRichView);
    procedure Clear();
    function FormatPages(PrintOptions: TRVDisplayOptions): Integer;
    procedure PrintPages(firstPgNo, lastPgNo: Integer; Title: String;
                         Copies: Integer; Collate: Boolean);
    procedure Print(Title: String; Copies: Integer; Collate: Boolean);
    procedure MakePreview(pgNo: Integer; bmp: TBitmap);

    property rv: TPrintableRV read Frv;

  published
    { Published declarations }
    property PagesCount: Integer read GetPagesCount;
    property LeftMarginMM: Integer read GetLM write SetLM;
    property RightMarginMM: Integer read GetRM write SetRM;
    property TopMarginMM: Integer read GetTM write SetTM;
    property BottomMarginMM: Integer read GetBM write SetBM;
    property OnFormatting: TRVPrintingEvent read FOnFormatting write FOnFormatting;
    property OnSendingToPrinter: TRVPrintingEvent read FOnPrinting write FOnPrinting;
  end;

  function GetPrinterDC: HDC;

implementation

uses
  RVStyle;

{==================================================================}
procedure TRVPageInfo.Assign(Source: TPersistent);
begin
  if Source is TRVPageInfo then
  begin
    StartY := TRVPageInfo(Source).StartY;
    StartLineNo := TRVPageInfo(Source).StartLineNo;    
  end
  else
    inherited Assign(Source);
end;
{==================================================================}
type
  TPrinterDevice = class
  public
    Driver: string;
    Device: string;
    Port: string;
  end;
{$IFDEF FPC}
function GetPrinterDC: HDC;
begin
  result := 0; // not used in lazarus
end;
{$ELSE}
function GetPrinterDC: HDC;
var
  ADevice, ADriver, APort: array[0..79] of Char;
  ADeviceMode: THandle;
  DevMode: PDeviceMode;
begin
  Printer.GetPrinter(ADevice, ADriver, APort, ADeviceMode);
  if ADeviceMode <> 0 then
    DevMode := PDeviceMode(GlobalLock(ADeviceMode))
  else
    DevMode := nil;
  Result := CreateDC(ADriver, ADevice, APort, DevMode);
  if ADeviceMode <> 0 then
    GlobalUnlock(ADeviceMode);
end;
{$ENDIF}
constructor TRVPrint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Frv := TPrintableRV.Create(Self);
  if not (csDesigning in ComponentState) then
    rv.Parent := TWinControl(Self.Owner);
  LeftMarginMM   := 20;
  RightMarginMM  := 20;
  TopMarginMM    := 20;
  BottomMarginMM := 20;
end;
{------------------------------------------------------------------}
function  TRVPrint.GetLM(): Integer;
begin
  Result := rv.FLeftMarginMM;
end;
{------------------------------------------------------------------}
function  TRVPrint.GetRM(): Integer;
begin
  Result := rv.FRightMarginMM;
end;
{------------------------------------------------------------------}
function  TRVPrint.GetTM(): Integer;
begin
  Result := rv.FTopMarginMM;
end;
{------------------------------------------------------------------}
function  TRVPrint.GetBM(): Integer;
begin
  Result := rv.FBottomMarginMM;
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
function TRVPrint.FormatPages(PrintOptions: TRVDisplayOptions): Integer;
begin
  rv.DisplayOptions := PrintOptions;
  rv.FOnFormatting := FOnFormatting;
  Result := rv.FormatPages;
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
procedure TRVPrint.Clear();
begin
  rv.Clear();
end;
{------------------------------------------------------------------}
procedure TRVPrint.MakePreview(pgNo: Integer; bmp: TBitmap);
var
  w, h: Integer;
begin
  w := MulDiv(rv.Width + rv.TmpLM + rv.TmpRM, rv.Printersad.ppixScreen, rv.Printersad.ppixDevice);
  h := MulDiv(rv.Height + rv.TmpTM + rv.TmpBM, rv.Printersad.ppiyScreen, rv.Printersad.ppiyDevice);
  if bmp.Width <> w then
    bmp.Width := w;
  if bmp.Height <> h then
    bmp.Height := h;
  bmp.Canvas.Brush.Color := clWhite;
  bmp.Canvas.Pen.Color := clWhite;
  bmp.Canvas.FillRect(Rect(0, 0, bmp.Width, bmp.Height));
  rv.DrawPage(pgNo, bmp.Canvas);
end;
{------------------------------------------------------------------}
function TRVPrint.GetPagesCount(): Integer;
begin
  GetPagesCount := rv.PagesList.Count;
end;
{==================================================================}
constructor TPrintableRV.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPagesList := TCollection.Create(TRVPageInfo);
  Visible := False;
end;
{------------------------------------------------------------------}
destructor TPrintableRV.Destroy();
begin
  FreeAndNil(FPagesList);
  inherited Destroy;
end;
{------------------------------------------------------------------}
function TPrintableRV.FormatPages(): Integer;
var
  i, j: Integer;
  Item, Item2, Item3: TRVItem;
  nextnewline: Integer;
  rvpi: TRVPageInfo;
  nPages: Integer;
  lpy, lpx, StartY: Integer;
  {$IFDEF FPC}
  {$ELSE}
  PrinterCanvas: TCanvas;
  PHDC: HDC;
  {$ENDIF}

begin
  if Assigned(FOnFormatting) then
    FOnFormatting(Self, 0, rvpsStarting);
  VScrollVisible := False;

  {$IFDEF FPC}
  lpy := Printer.YDPI;
  lpx := Printer.XDPI;
  Width := Printer.PageWidth - MulDiv(FLeftMarginMM + FRightMarginMM, 5, 127) * lpx;
  Height := Printer.PageHeight - MulDiv(FTopMarginMM + FBottomMarginMM, 5, 127) * lpy;
  with Printer.PaperSize.PaperRect do
  begin
    lpx := MulDiv(PhysicalRect.Right - PhysicalRect.Left, 127, 5 * lpx);
    lpy := MulDiv(PhysicalRect.Bottom - PhysicalRect.Top, 127, 5 * lpy);
  end;
  TmpLM := MulDiv(FLeftMarginMM, Printer.PageWidth, lpx);
  TmpTM := MulDiv(FTopMarginMM, Printer.PageHeight, lpy);
  TmpRM := MulDiv(FRightMarginMM, Printer.PageWidth, lpx);
  TmpBM := MulDiv(FBottomMarginMM, Printer.PageHeight, lpy);
  Format_(False, 0, Printer.Canvas, False);
  InfoAboutSaD(PrinterSaD, Printer.Canvas);

  {$ELSE}
  PrinterCanvas := TCanvas.Create();
  PHDC := GetPrinterDC;
  try
    PrinterCanvas.Handle := PHDC;
    lpy := GetDeviceCaps(PHDC, LOGPIXELSY);
    lpx := GetDeviceCaps(PHDC, LOGPIXELSX);
    PrinterCanvas.Font.PixelsPerInch := lpy;
    Width := Printer.PageWidth - MulDiv(FLeftMarginMM + FRightMarginMM, 5, 127) * lpx;
    Height:= Printer.PageHeight - MulDiv(FTopMarginMM + FBottomMarginMM, 5, 127) * lpy;
    lpx := GetDeviceCaps(PHDC, HORZSIZE);
    lpy := GetDeviceCaps(PHDC, VERTSIZE);
    TmpLM := MulDiv(FLeftMarginMM, Printer.PageWidth, lpx);
    TmpTM := MulDiv(FTopMarginMM, Printer.PageHeight, lpy);
    TmpRM := MulDiv(FRightMarginMM, Printer.PageWidth, lpx);
    TmpBM := MulDiv(FBottomMarginMM, Printer.PageHeight, lpy);
    Format_(False, 0, PrinterCanvas, False);
    InfoAboutSaD(PrinterSaD, PrinterCanvas);
    PrinterCanvas.Handle := 0;
  finally
    PrinterCanvas.Free;
    DeleteDC(PHDC);
  end;
  {$ENDIF}

  PagesList.Clear();
  FormatPages := 0;
  if VisItems.Count = 0 then
    Exit;
  nPages := 1;
  rvpi := TRVPageInfo(PagesList.Add());
  rvpi.StartY := 0;
  rvpi.StartLineNo := 0;
  StartY := 0;
  i := 0;
  if Assigned(FOnFormatting) then FOnFormatting(Self, 0, rvpsProceeding);
  while i < VisItems.Count do
  begin
    Item := VisItems[i];
    if Item.Top + Item.Height > StartY + Height then
    begin { i-th item does not fit in page }
      nextnewline := i;
      { searching first item in first last in new page }
      for j:=i downto 0 do
      begin
        Item2 := VisItems[j];
        if (j <> i) and (Item2.Top + Item2.Height <= Item.Top) then
          Break;

        nextnewline := j;
      end;
      { page must contain one item at least}
      if nextnewline = TRVPageInfo(PagesList.Items[nPages-1]).StartLineNo then
        Inc(nextnewline);

      if nextnewline <> VisItems.Count then
      begin
        { searching min y of first line in new page }
        Item2 := VisItems[nextnewline];
        StartY := Item2.Top;
        for j := nextnewline + 1 to VisItems.Count - 1 do
        begin
          Item3 := VisItems[j];
          if (Item3.Top >= Item2.Top + Item2.Height) then
            Break;
          if Item3.Top < StartY then
            StartY := Item3.Top;
        end;
        rvpi := TRVPageInfo(PagesList.Add);
        rvpi.StartLineNo := nextnewline;
        rvpi.StartY := StartY;
        if Assigned(FOnFormatting) then FOnFormatting(Self, nPages, rvpsProceeding);
        Inc(nPages);
      end;
      i := nextnewline;
    end
    else
      Inc(i);
  end;
  if Assigned(FOnFormatting) then FOnFormatting(Self, nPages, rvpsProceeding);
  FormatPages := nPages;
  if Assigned(FOnFormatting) then FOnFormatting(Self, nPages, rvpsFinished);
end;
{------------------------------------------------------------------}

procedure DrawOnDevice(Canvas: TCanvas; x, y: Integer; sad: TScreenAndDevice; gr: TGraphic);
var
  bmp: TBitmap;
  PrintWidth, PrintHeight: Longint;
  {Info: PBitmapInfo;
  InfoSize: DWORD;
  Image: Pointer;
  ImageSize: DWORD;
  Bits: HBITMAP;
  DIBWidth, DIBHeight: Longint;  }
begin
  if (gr is TBitmap) then
  begin
    bmp := (gr as TBitmap);

    PrintWidth := MulDiv(bmp.Width, sad.ppixDevice, sad.ppixScreen);
    PrintHeight := MulDiv(bmp.Height, sad.ppiyDevice, sad.ppiyScreen);

    Canvas.StretchDraw(Rect(x, y, x + PrintWidth, y + PrintHeight), bmp);

    (*
    Bits := TBitmap(gr).Handle;
    GetDIBSizes(Bits, InfoSize, ImageSize);
    Info := AllocMem(InfoSize);
    try
        Image := AllocMem(ImageSize);
        try
          GetDIB(Bits, 0, Info^, Image^);
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
    *)
  end;
end;
{------------------------------------------------------------------}
procedure TPrintableRV.DrawPage(pgNo: Integer; Canvas: TCanvas);
var
  i, no: Integer;
  Item: TRVItem;
  VisItem: TRVVisualItem;
  zerocoord: Integer;
  first, last: Integer;
  sad: TScreenAndDevice;
  background, tmpbmp: TBitmap;
  BackWidth, BackHeight: Integer;
  FontInf: TFontInfo;
  psad: TScreenAndDevice;
  //wmf: TMetafile;
begin
  if not Assigned(FStyle) then
  begin
    raise EStyleNotAssigned.Create('Style of printable TRichView component is not assigned');
    Exit;
  end;
  if (pgNo < 1) or (pgNo > PagesList.Count) then
  begin
    raise EInvalidPageNo.Create('Invalid page number is specified for printing');
    Exit;
  end;

  first := TRVPageInfo(PagesList.Items[pgNo-1]).StartLineNo;
  if pgNo = PagesList.Count then
    last := VisItems.Count-1
  else
    last := TRVPageInfo(PagesList.Items[pgNo]).StartLineNo-1;

  psad := PrinterSad;
  zerocoord := TRVPageInfo(PagesList.Items[pgNo-1]).StartY-TmpTM;
  Canvas.Brush.Style := bsClear;
  InfoAboutSaD(sad, Canvas);

  BackWidth  := MulDiv(Width,  psad.ppixScreen, psad.ppixDevice);
  BackHeight := MulDiv(Height, psad.ppiyScreen, psad.ppiyDevice);
  if (BackGroundStyle <> bsNoBitmap) and (BackGroundBitmap <> nil) then
  begin
    if BackGroundStyle = bsTiledAndScrolled then
      BackGroundStyle := bsTiled;
    background := TBitmap.Create();
    background.Width := BackWidth;
    background.Height := BackHeight;
    DrawBack(background.Canvas.Handle, Rect(0,0, BackWidth, BackHeight),
             BackWidth, BackHeight);
    DrawOnDevice(Canvas,
                 MulDiv(TmpLM, sad.ppixDevice, psad.ppixDevice),
                 MulDiv(TmpTM, sad.ppiyDevice, psad.ppiyDevice),
                 sad, background);
  end
  else
  begin
    background := nil;
    Canvas.Pen.Color := Style.Color;
    Canvas.Brush.Color := Style.Color;
    Canvas.FillRect(
        Rect(
          MulDiv(TmpLM, sad.ppixDevice, psad.ppixDevice),
          MulDiv(TmpTM, sad.ppiyDevice, psad.ppiyDevice),
          MulDiv(TmpLM, sad.ppixDevice, psad.ppixDevice)+
          MulDiv(BackWidth,  sad.ppixDevice, sad.ppixScreen),
        MulDiv(TmpTM, sad.ppiyDevice, psad.ppiyDevice)+
          MulDiv(BackHeight, sad.ppiyScreen, sad.ppiyScreen)));
  end;

  tmpbmp := TBitmap.Create();
  try
    for i := first to last do
    begin
      Item := VisItems[i];
      no := Item.StyleNo;
      if no >= 0 then { text }
      begin
        FontInf := FStyle.TextStyles[no];
        Canvas.Font.Color := FontInf.Color;
        Canvas.Font.Style := FontInf.Style;
        Canvas.Font.Size  := FontInf.Size;
        Canvas.Font.Name  := FontInf.FontName;
        {$IFDEF RICHVIEWDEF3}
        Canvas.Font.CharSet := FontInf.CharSet;
        {$ENDIF}
        Canvas.TextOut(
          MulDiv(Item.Left + TmpLM, sad.ppixDevice, psad.ppixDevice),
          MulDiv(Item.Top - zerocoord, sad.ppiyDevice, psad.ppiyDevice),
          Item.Text);
        Continue;
      end;

      case no of
        rvsPicture, rvsHotSpot, rvsBullet:
        begin
          //if li.gr is TMetafile then begin
          //    wmf := TMetafile.Create;
          //    try
          //       wmf.Assign(li.gr);
          //       wmf.Width  := MulDiv(TMetafile(li.gr).Width, sad.ppixDevice, sad.ppixScreen);
          //       wmf.Height := MulDiv(TMetafile(li.gr).Height, sad.ppiyDevice, sad.ppiyScreen);
          //       Canvas.Draw(
          //         MulDiv(dli.Left+TmpLM, sad.ppixDevice, psad.ppixDevice),
          //         MulDiv( dli.Top-zerocoord, sad.ppiyDevice, psad.ppiyDevice),
          //         wmf);
          //    finally
          //      wmf.free;
          //    end;
          // end
          {else}
          begin
            VisItem := (Item as TRVVisualItem);
            if no = rvsPicture then
            begin
              tmpbmp.Width  := TGraphic(VisItem.gr).Width;
              tmpbmp.Height := TGraphic(VisItem.gr).Height;
            end
            else
            begin
              tmpbmp.Width  := TImageList(VisItem.gr).Width;
              tmpbmp.Height := TImageList(VisItem.gr).Height;
            end;
            if background <> nil then
              tmpbmp.Canvas.CopyRect(
                 Rect(0, 0, tmpbmp.Width, tmpbmp.Height),
                 background.Canvas,
                 Rect(
                     MulDiv(Item.Left, psad.ppixScreen, psad.ppixDevice),
                     MulDiv(Item.Top - (zerocoord + TmpTM), psad.ppiyScreen, psad.ppiyDevice),
                     MulDiv(Item.Left, psad.ppixScreen, psad.ppixDevice) + tmpbmp.Width,
                     MulDiv(Item.Top - (zerocoord + TmpTM), psad.ppiyScreen, psad.ppiyDevice) + tmpbmp.Height
                     )
                )
            else
            begin
              tmpbmp.Canvas.Pen.Color := Style.Color;
              tmpbmp.Canvas.Brush.Color := Style.Color;
              tmpbmp.Canvas.FillRect(Rect(0, 0, tmpbmp.Width, tmpbmp.Height));
            end;

            if no = rvsPicture then
              tmpbmp.Canvas.Draw(0, 0, TGraphic(VisItem.gr))
            else
              TImageList(VisItem.gr).Draw(tmpbmp.Canvas, 0, 0, VisItem.imgNo);

            DrawOnDevice(Canvas,
              MulDiv(Item.Left + TmpLM, sad.ppixDevice, psad.ppixDevice),
              MulDiv(Item.Top - zerocoord, sad.ppiyDevice, psad.ppiyDevice),
              sad, tmpbmp);
          end;
        end;

        rvsBreak: {break line}
        begin
          Canvas.Pen.Color := FStyle.TextStyles[0].Color;
          Canvas.MoveTo(
             MulDiv(Item.Left + TmpLM + MulDiv(5, psad.ppixDevice, psad.ppixScreen),
                   sad.ppixDevice, psad.ppixDevice),
             MulDiv(Item.Top - zerocoord + MulDiv(5, psad.ppiyDevice, psad.ppiyScreen),
                   sad.ppiyDevice, psad.ppiyDevice));
          Canvas.LineTo(
             MulDiv(Width + TmpLM - MulDiv(5 + RightMargin, psad.ppixDevice, psad.ppixScreen),
                   sad.ppixDevice, psad.ppixDevice),
             MulDiv(Item.Top - zerocoord + MulDiv(5, psad.ppiyDevice, psad.ppiyScreen),
                   sad.ppiyDevice, psad.ppiyDevice));
        end;

        { controls is not supported yet }
      end;
    end;
  finally
    background.Free();
    tmpbmp.Free();
  end;
end;
{------------------------------------------------------------------}
procedure TPrintableRV.PrintPages(firstPgNo, lastPgNo: Integer; Title: String;
                                  Copies: Integer; Collate: Boolean);
var
  i, copyno: Integer;
  PrinterCopies: Integer;
begin
  if Assigned(FOnPrinting) then FOnPrinting(Self, 0, rvpsStarting);
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
  Printer.BeginDoc();

  if Collate then
  begin
    for copyno:= 1 to Copies do
    begin
      for i := firstPgNo to lastPgNo do
      begin
        DrawPage(i, Printer.Canvas);
        if Assigned(FOnPrinting) then FOnPrinting(Self, i, rvpsProceeding);
        if not ((i = lastPgNo) and (copyno = Copies)) then
          Printer.NewPage();
      end
    end
  end
  else
  begin
    for i := firstPgNo to lastPgNo do
    begin
      for copyno:= 1 to Copies do
      begin
        DrawPage(i, Printer.Canvas);
        if Assigned(FOnPrinting) then FOnPrinting(Self, i, rvpsProceeding);
        if not ((i = lastPgNo) and (copyno = Copies)) then
          Printer.NewPage();
      end;
    end;
  end;

  Printer.EndDoc();
  Printer.Copies := PrinterCopies; { restoring }
  if Assigned(FOnPrinting) then FOnPrinting(Self, 0, rvpsFinished);
end;
{------------------------------------------------------------------}
procedure TPrintableRV.Print(Title: String; Copies: Integer; Collate: Boolean);
begin
  PrintPages(1, PagesList.Count, Title, Copies, Collate);
end;

end.
