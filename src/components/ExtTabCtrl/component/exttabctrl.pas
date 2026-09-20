unit ExtTabCtrl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, FPImage, GraphType, Graphics, Buttons, LCLType, Types, Math,
  LResources, LCLIntf, GraphUtil, ImgList, LMessages, Forms, Menus,
  IntfGraphics{$IFDEF LCLDesign}, PropEdits{$ENDIF};

type
  TTabPosition = (tpTop, tpBottom, tpLeft, tpRight);

  TTabStyle = (tsFlat, tsButton, tsDelphi, tsChrome, tsMacOS);

  TExtTabOption = (toActivateNewTab, toShowCloseButton, toShowAddButton,
                   toCloseOnMiddleClick, toAllowDragReorder,
                   toRotateTabImages, toRotateAddImage, toGetFocus,
                   toShowFocusRect, toActiveBold, toActiveItalic);
  TExtTabOptions = set of TExtTabOption;

  TExtTab = class;

  TTabCreatingEvent = procedure(Sender: TObject; var Caption: String; var Data: TObject; var Allow: Boolean) of object;
  TTabIndexAllowEvent = procedure(Sender: TObject; Index: Integer; var Allow: Boolean) of object;
  TTabChangedEvent = procedure(Sender: TObject; NewIndex: Integer) of object;
  TTabChangingEvent = procedure(Sender: TObject; OldIndex, NewIndex: Integer; var Allow: Boolean) of object;
  TTabReorderedEvent = procedure(Sender: TObject; OldIndex, NewIndex: Integer) of object;
  TTabReorderingEvent = procedure(Sender: TObject; OldIndex, NewIndex: Integer; var Allow: Boolean) of object;
  TTabClickEvent = procedure(Sender: TObject; Index: Integer) of object;
  TTabImportEvent = procedure(Sender: TObject; Tab: TExtTab; AObject: TObject) of object;
  TButtonClickEvent = procedure(Sender: TObject) of object;
  TTabMouseEvent = procedure(Sender: TObject; Index: Integer) of object;
  TTabDrawEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRect; IsActive, IsHover: Boolean; var FontColor: TColor; var Indent: Integer) of object;

  TExtTabCtrl = class;

  TButtonImages = class(TPersistent)
  private
    FOwnerCtrl: TExtTabCtrl;
    FImgIndex: array[0..3] of TImageIndex;
    FSavedIndex: Array[0..3] of TImageIndex;
    FOnChange: TNotifyEvent;
    function GetIndex(Index: Integer): TImageIndex;
    procedure SetIndex(Index: Integer; Value: TImageIndex);
  public
    constructor Create(AOwner: TExtTabCtrl);
    procedure Assign(Source: TPersistent); override;
    procedure Save;
    procedure Restore;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    function GetOwner: TExtTabCtrl;

    property PrevIndex: TImageIndex index 0 read GetIndex write SetIndex default -1;
    property NextIndex: TImageIndex index 1 read GetIndex write SetIndex default -1;
    property AddIndex: TImageIndex index 2 read GetIndex write SetIndex default -1;
    property CloseIndex: TImageIndex index 3 read GetIndex write SetIndex default -1;
  end;

  TButtonHints = class(TPersistent)
  private
    FAddHint: String;
    FScrollPrevHint: String;
    FScrollNextHint: String;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property AddHint: String read FAddHint write FAddHint;
    property ScrollPrevHint: String read FScrollPrevHint write FScrollPrevHint;
    property ScrollNextHint: String read FScrollNextHint write FScrollNextHint;
  end;

  TExtFontOptions = class(TPersistent)
  private
    FFontSize: Integer;
    FFontColor: TColor;
    FFontStyles: TFontStyles;
    FOnRedraw: TNotifyEvent;

    procedure SetFontSize(AValue: Integer);
    procedure SetFontColor(AValue: TColor);
    procedure SetFontStyles(AValue: TFontStyles);
  protected
    procedure Changed;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    property OnRedraw: TNotifyEvent read FOnRedraw write FOnRedraw;
  published
    property FontSize: Integer read FFontSize write SetFontSize default 0;
    property FontColor: TColor read FFontColor write SetFontColor default clNone;
    property FontStyles: TFontStyles read FFontStyles write SetFontStyles default [];
  end;

  TImagesWidth = class(TPersistent)
  private
    FPrevWidth: Integer;
    FNextWidth: Integer;
    FAddWidth: Integer;
    FCloseWidth: Integer;
    FTabWidth: Integer;

    FOnChange: TNotifyEvent;
    procedure SetWidth(Index, Value: Integer);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property PrevWidth: Integer index 0 read FPrevWidth write SetWidth default 0;
    property NextWidth: Integer index 1 read FNextWidth write SetWidth default 0;
    property AddWidth: Integer index 2 read FAddWidth write SetWidth default 0;
    property CloseWidth: Integer index 3 read FCloseWidth write SetWidth default 0;
    property TabsWidth: Integer index 4 read FTabWidth write SetWidth default 0;
  end;

  TExtTab = class(TCollectionItem)
  private
    FCaption: TCaption;
    FColor: TColor;
    FStripeColor: TColor;
    FVisible: Boolean;
    FValue: String;
    FData: TObject;
    FFontOptions: TExtFontOptions;
    FOwnerCtrl: TExtTabCtrl;
    FImage: TBitmap;
    FImageIndex: TImageIndex;
    FHint: String;
    FShowCloseButton: Boolean;
    FTextWidth: Integer;
    FTextHeight: Integer;
    FCachedTabImage: TBitmap;
    FCachedImageRotation: Integer;
    procedure SetCaption(AValue: TCaption);
    procedure SetColor(AValue: TColor);
    procedure SetStripeColor(AValue: TColor);
    procedure SetVisible(AValue: Boolean);
    procedure SetImage(AValue: TBitmap);
    procedure SetImageIndex(AValue: TImageIndex);
    function GetImage: TBitmap;
    procedure SetShowCloseButton(AValue: Boolean);
    procedure Redraw(Sender: TObject);
  protected
    FBoundRect: TRect;
    function GetDisplayName: String; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    function GetOwner: TExtTabCtrl;

    property BoundRect: TRect read FBoundRect;
  published
    property Caption: TCaption read FCaption write SetCaption;
    property Color: TColor read FColor write SetColor default clNone;
    property StripeColor: TColor read FStripeColor write SetStripeColor default clNone;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Value: String read FValue write FValue;
    property Data: TObject read FData write FData;
    property FontOptions: TExtFontOptions read FFontOptions;
    property Image: TBitmap read GetImage write SetImage;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property Hint: String read FHint write FHint;
    property ShowCloseButton: Boolean read FShowCloseButton write SetShowCloseButton default True;
  end;

  TExtTabs = class(TCollection)
  private
    FOwnerCtrl: TExtTabCtrl;
    function GetItems(Index: Integer): TExtTab;
  protected
    procedure Update(Item: TCollectionItem); override;
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TExtTabCtrl);
    function Add(const ACaption: String): TExtTab;
    property Items[Index: Integer]: TExtTab read GetItems; default;
  end;

  TExtTabCtrl = class(TCustomControl)
  private

  private
    FUpdateCount: Integer;
    FLayoutDirty: Boolean;
    FTabSize: Integer;
    FTotalTabsSize: Integer;
    FTabs: TExtTabs;
    FTabIndex: Integer;
    FTabStyle: TTabStyle;
    FTabOptions: TExtTabOptions;
    FBtnAdd: TSpeedButton;

    FImages: TCustomImageList;
    FInternalBtnImages: TCustomImageList;    // Imagelist with button images
    FInternalTabImages: TCustomImageList;    // Copy of FImages with rotated images
    FButtonImages: TButtonImages;
    FButtonHints: TButtonHints;
    FImagesWidth: TImagesWidth;

    FDragging: Boolean;
    FDragIndex, FDragTargetIndex: Integer;
    FMouseDownPos: TPoint;
    FMouseDownIndex: Integer;

    FTabPosition: TTabPosition;
    FOnTabCreating: TTabCreatingEvent;
    FOnTabCreated: TNotifyEvent;
    FOnTabDeleting: TTabIndexAllowEvent;
    FOnTabDeleted: TNotifyEvent;
    FOnTabChanging: TTabChangingEvent;
    FOnTabChanged: TTabChangedEvent;
    FOnTabReordering: TTabReorderingEvent;
    FOnTabReordered: TTabReorderedEvent;
    FOnTabClick: TTabClickEvent;
    FOnTabDblClick: TTabClickEvent;
    FOnImportTab: TTabImportEvent;
    FOnAddButtonClick: TButtonClickEvent;
    FOnGetFocus: TNotifyEvent;
    FOnLostFocus: TNotifyEvent;
    FOnMouseEnterTab: TTabMouseEvent;
    FOnMouseLeaveTab: TTabMouseEvent;
    FOnDrawTab: TTabDrawEvent;

    FScrollOffset: Integer;
    FHoverTab, FHoverCloseTab: Integer;
    FBtnScrollPrev, FBtnScrollNext: TSpeedButton;

    FAddTabCounter: Integer;
    FImportActive: Boolean;
    FInternalChange: Integer;
    FMinCaptionLen: Integer;
    FMaxCaptionLen: Integer;

    procedure BeginInternalChange;
    procedure EndInternalChange;
    procedure NormalizeState;
    procedure CancelDrag;
    procedure InvalidateTabImageCaches;

    procedure SetTabIndex(AValue: Integer);
    procedure SetTabSize(AValue: Integer);
    function IsStoredTabSize: Boolean;
    procedure AddBtnClick(Sender: TObject);
    procedure ScrollPrev(Sender: TObject);
    procedure ScrollNext(Sender: TObject);

    procedure SetTabStyle(AValue: TTabStyle);
    procedure SetTabPosition(AValue: TTabPosition);
    procedure SetTabOptions(AValue: TExtTabOptions);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetButtonImageIndexes(AValue: TButtonImages);
    procedure SetButtonHints(AValue: TButtonHints);
    procedure SetImagesWidth(AValue: TImagesWidth);
    procedure SetTabs(AValue: TExtTabs);
    procedure SetMinCaptionLen(AValue: Integer);
    procedure SetMaxCaptionLen(AValue: Integer);

    procedure SetAddMenu(AValue: TPopupMenu);
    function GetAddMenu: TPopupMenu;

    procedure SetOnDrawTab(AValue: TTabDrawEvent);

    procedure ButtonImagesChanged(Sender: TObject);
    procedure ImagesWidthChanged(Sender: TObject);
    function TabsViewportRect: TRect;
    procedure AnchorButtons;
    function GetDisplayCaption(Tab: TExtTab): String;
    function CloseButtonRect(Tab: TExtTab): TRect;
    function TabAtPos(X, Y: Integer): Integer;
    procedure LoadBitmapFromLRS(const ResName: String; DestBitmap: TBitmap);
    procedure LoadBitmapFromRes(const ResName: String; DestImg: TCustomBitmap);
    function MaxScrollOffset: Integer;
    procedure EnsureTabVisible(Index: Integer);
    procedure UpdateScrollButtons;
    function GetScale(Value: Integer): Integer;
    procedure DrawTabTextAndImage(ACanvas: TCanvas; const R: TRect; Tab: TExtTab; IsActive: Boolean; DefaultFontColor: TColor);
    procedure DrawCloseButton(ACanvas: TCanvas; const R: TRect; Tab: TExtTab; IsActive: Boolean);
    procedure DrawColorStripe(ACanvas: TCanvas; const R: TRect; Tab: TExtTab; Indent: Integer);
    procedure DrawStripLine(ACanvas: TCanvas; const View: TRect);
    function ResolveColor(AColor: TColor): TColor;

    procedure DrawTabImage(ACanvas: TCanvas; Tab: TExtTab; X, Y: Integer);
    procedure DrawRotatedText(ACanvas: TCanvas; const S: String; const R: TRect; Degrees: Integer);
    function GetRotationForPosition: Integer;
    function HasAnyImage(Tab: TExtTab): Boolean;
    function GetTabImageWidth(Tab: TExtTab): Integer;
    function GetTabImageHeight(Tab: TExtTab): Integer;
    function GetTabTextBounds(ACanvas: TCanvas; const R: TRect; Tab: TExtTab): TRect;
    procedure GetBaseTabBitmap(Tab: TExtTab; Dest: TBitmap);

    function GetAddButtonImages: TCustomImageList;
    function GetCloseButtonImages: TCustomImageList;
    function GetScrollButtonImages(Which: Integer): TCustomImageList;
  protected
  const
    cDefaultTabSize = 26;

    cContentIndent = 6;
    cTabOverlap = 2;
    cImageSpacing = 6;
    cDragThreshold = 6;

    cPrevIndex = 0;    // Image indices into FInternalImages
    cNextIndex = 1;
    cAddIndex = 2;
    cCloseIndex = 3;

    class function GetControlClassDefaultSize: TSize; override;

    procedure Paint; override;
    procedure Resize; override;
    procedure CalcLayout;

    procedure DrawFlatTab(ACanvas: TCanvas; var R: TRect; IsActive: Boolean; Tab: TExtTab; var FontColor: TColor; var Indent: Integer);
    procedure DrawButtonTab(ACanvas: TCanvas; var R: TRect; IsActive: Boolean; Tab: TExtTab; var FontColor: TColor; var Indent: Integer);
    procedure DrawDelphiTab(ACanvas: TCanvas; var R: TRect; IsActive: Boolean; Tab: TExtTab; var FontColor: TColor; var Indent: Integer);
    procedure DrawChromeTab(ACanvas: TCanvas; var R: TRect; IsActive: Boolean; Tab: TExtTab; var FontColor: TColor; var Indent: Integer);
    procedure DrawMacOSTab(ACanvas: TCanvas; var R: TRect; IsActive: Boolean; Tab: TExtTab; var FontColor: TColor; var Indent: Integer);
    procedure DrawTab(ACanvas: TCanvas; Index: Integer; ARect: TRect; IsActive: Boolean);

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure DblClick; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    procedure DoEnter; override;
    procedure DoExit; override;

    procedure WMLMGetDlgCode(var Message: TLMessage); message LM_GETDLGCODE;
    procedure CMDesignHitTest(var Message: TLMessage); message CM_DESIGNHITTEST;

    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer; WithImplicitConstraints: Boolean); override;
    procedure CMShowHintChanged(var Message: TLMessage); message CM_SHOWHINTCHANGED;
    procedure CMFontChanged(var Message: TLMessage); message CM_FONTCHANGED;

    procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
      const AXProportion, AYProportion: Double); override;
    procedure PrepareInternalBtnImages({%H-}ARotation: Integer);
    procedure PrepareInternalTabImages(ARotation: Integer);
    procedure UpdateImages;
    procedure UpdateBtnImages;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function IsVertical: Boolean;
    function IsHorizontal: Boolean;
    procedure InvalidateLayout;
    function NextVisibleTab(FromIndex: Integer): Integer;
    function PrevVisibleTab(FromIndex: Integer): Integer;
    function AddTab(const ACaption: String; AData: TObject = nil): TExtTab;
    procedure DeleteTab(Index: Integer);
    procedure ImportFromStrings(Source: TStrings; ClearExisting: Boolean = True);
    procedure SetDesignTabIndex(AValue: Integer);
  published
    property Align;
    property AutoSize;
    property BorderSpacing;
    property Color default clForm;
    property DoubleBuffered;
    property Tabs: TExtTabs read FTabs write SetTabs;
    property TabIndex: Integer read FTabIndex write SetTabIndex default -1;
    property TabSize: Integer read FTabSize write SetTabSize stored IsStoredTabSize;
    property TabStyle: TTabStyle read FTabStyle write SetTabStyle default tsFlat;
    property TabOptions: TExtTabOptions read FTabOptions write SetTabOptions
                           default [toActivateNewTab, toShowCloseButton,
                                    toShowAddButton, toCloseOnMiddleClick,
                                    toAllowDragReorder, toGetFocus, toShowFocusRect];
    property TabPosition: TTabPosition read FTabPosition write SetTabPosition default tpTop;

    property ShowHint default True;
    property Font;
    property ParentFont;
    property ParentColor;

    property Images: TCustomImageList read FImages write SetImages;
    property ButtonImages: TButtonImages read FButtonImages write SetButtonImageIndexes;
    property ImagesWidth: TImagesWidth read FImagesWidth write SetImagesWidth;
    property ButtonHints: TButtonHints read FButtonHints write SetButtonHints;

    property AddMenu: TPopupMenu read GetAddMenu write SetAddMenu;

    property MinCaptionLen: Integer read FMinCaptionLen write SetMinCaptionLen default 5;
    property MaxCaptionLen: Integer read FMaxCaptionLen write SetMaxCaptionLen default 25;

    property OnTabReordering: TTabReorderingEvent read FOnTabReordering write FOnTabReordering;
    property OnTabReordered: TTabReorderedEvent read FOnTabReordered write FOnTabReordered;
    property OnTabCreating: TTabCreatingEvent read FOnTabCreating write FOnTabCreating;
    property OnTabCreated: TNotifyEvent read FOnTabCreated write FOnTabCreated;
    property OnTabDeleting: TTabIndexAllowEvent read FOnTabDeleting write FOnTabDeleting;
    property OnTabDeleted: TNotifyEvent read FOnTabDeleted write FOnTabDeleted;
    property OnTabChanging: TTabChangingEvent read FOnTabChanging write FOnTabChanging;
    property OnTabChanged: TTabChangedEvent read FOnTabChanged write FOnTabChanged;
    property OnTabClick: TTabClickEvent read FOnTabClick write FOnTabClick;
    property OnTabDblClick: TTabClickEvent read FOnTabDblClick write FOnTabDblClick;
    property OnImportTab: TTabImportEvent read FOnImportTab write FOnImportTab;
    property OnAddButtonClick: TButtonClickEvent read FOnAddButtonClick write FOnAddButtonClick;
    property OnGetFocus: TNotifyEvent read FOnGetFocus write FOnGetFocus;
    property OnLostFocus: TNotifyEvent read FOnLostFocus write FOnLostFocus;
    property OnMouseEnterTab: TTabMouseEvent read FOnMouseEnterTab write FOnMouseEnterTab;
    property OnMouseLeaveTab: TTabMouseEvent read FOnMouseLeaveTab write FOnMouseLeaveTab;
    property OnDrawTab: TTabDrawEvent read FOnDrawTab write SetOnDrawTab;
  end;

function IsDarkMode: Boolean;

implementation

{$R exttabctrl_images.res}

{ Global Helpers }
procedure SwapIntegers(var A, B: Integer);
var
  Temp: Integer;
begin
  Temp := A;
  A := B;
  B := Temp;
end;

function BlendColors(C1, C2: TColor; Ratio: Single): TColor;
var
  R1, G1, B1, R2, G2, B2: Byte;
begin
  C1 := ColorToRGB(C1);
  C2 := ColorToRGB(C2);
  R1 := GetRValue(C1);
  G1 := GetGValue(C1);
  B1 := GetBValue(C1);
  R2 := GetRValue(C2);
  G2 := GetGValue(C2);
  B2 := GetBValue(C2);
  Result := RGB(Round(R1*(1 - Ratio) + R2*Ratio),
                Round(G1*(1 - Ratio) + G2*Ratio),
                Round(B1*(1 - Ratio) + B2*Ratio));
end;

function IsDarkMode: Boolean;
var
  bkClr, txtClr: TColor;
begin
  bkClr := ColorToRGB(clWindow);
  txtClr := ColorToRGB(clWindowText);
  Result := ColorToGray(txtClr) > ColorToGray(bkClr);
end;

procedure RotateBitmap(Source, Dest: TCustomBitmap; Degrees: Integer);
var
  SrcIntf, DestIntf: TLazIntfImage;
  x, y: Integer;
begin
  if Source.Empty then Exit;
  SrcIntf := Source.CreateIntfImage;
  DestIntf := TLazIntfImage.Create(0, 0);
  try
    DestIntf.DataDescription := SrcIntf.DataDescription;
    if (Degrees = 90) or (Degrees = 270) then
      DestIntf.SetSize(SrcIntf.Height, SrcIntf.Width)
    else
      DestIntf.SetSize(SrcIntf.Width, SrcIntf.Height);

    case Degrees of
      90: // 90° CCW: src(x,y) --> dest(Height-1-y, x)
        for y := 0 to SrcIntf.Height - 1 do
          for x := 0 to SrcIntf.Width - 1 do
            DestIntf.Colors[SrcIntf.Height - 1 - y, x] := SrcIntf.Colors[x, y];
      180:
        for y := 0 to SrcIntf.Height - 1 do
          for x := 0 to SrcIntf.Width - 1 do
            DestIntf.Colors[SrcIntf.Width - 1 - x, SrcIntf.Height - 1 - y] := SrcIntf.Colors[x, y];
      270: // 270° CCW: src(x,y) --> dest(y, Width-1-x)
        for y := 0 to SrcIntf.Height - 1 do
          for x := 0 to SrcIntf.Width - 1 do
            DestIntf.Colors[y, SrcIntf.Width - 1 - x] := SrcIntf.Colors[x, y];
    else
      DestIntf.Assign(SrcIntf);
    end;
    Dest.LoadFromIntfImage(DestIntf);
  finally
    SrcIntf.Free;
    DestIntf.Free;
  end;
end;

// Degrees are counter-clockwise
procedure RotateImage(Img: TCustomBitmap; Degrees: Integer);
var
  SrcIntf, DestIntf: TLazIntfImage;
  x, y: Integer;
begin
  if Img.Empty then Exit;
  SrcIntf := Img.CreateIntfImage;
  DestIntf := TLazIntfImage.Create(0, 0);
  try
    DestIntf.DataDescription := SrcIntf.DataDescription;
    if (Degrees = 90) or (Degrees = 270) then
      DestIntf.SetSize(SrcIntf.Height, SrcIntf.Width)
    else
      DestIntf.SetSize(SrcIntf.Width, SrcIntf.Height);

    case Degrees of
      270: //90: // 90° clockwise: src(x,y) --> dest(Height-1-y, x)
        for y := 0 to SrcIntf.Height - 1 do
          for x := 0 to SrcIntf.Width - 1 do
            DestIntf.Colors[SrcIntf.Height - 1 - y, x] := SrcIntf.Colors[x, y];
      180:
        for y := 0 to SrcIntf.Height - 1 do
          for x := 0 to SrcIntf.Width - 1 do
            DestIntf.Colors[SrcIntf.Width - 1 - x, SrcIntf.Height - 1 - y] := SrcIntf.Colors[x, y];
      90: //270: // 270° clockwise (= 90° CCW): src(x,y) --> dest(y, Width-1-x)
        for y := 0 to SrcIntf.Height - 1 do
          for x := 0 to SrcIntf.Width - 1 do
            DestIntf.Colors[y, SrcIntf.Width - 1 - x] := SrcIntf.Colors[x, y];
    else
      DestIntf.Assign(SrcIntf);
    end;
    Img.LoadFromIntfImage(DestIntf);
  finally
    SrcIntf.Free;
    DestIntf.Free;
  end;
end;

procedure RecolorImage(Img: TCustomBitmap; AColor: TColor);
var
  intfImg: TLazIntfImage;
  x, y: Integer;
  clr, tmpClr: TFPColor;
begin
  if Img = nil then
    exit;

  clr := TColorToFPColor(ColorToRGB(AColor));
  intfImg := Img.CreateIntfImage;
  try
    intfImg.BeginUpdate;
    try
      for y := 0 to intfImg.Height - 1 do
        for x := 0 to intfImg.Width - 1 do
        begin
          tmpClr := intfImg.Colors[x, y];
          if tmpClr.Alpha > 0 then
          begin
            tmpClr.Red := clr.Red;
            tmpClr.Green := clr.Green;
            tmpClr.Blue := clr.Blue;
            intfImg.Colors[x, y] := tmpClr;
          end;
        end;
    finally
      intfImg.EndUpdate;
    end;
    img.LoadFromIntfImage(intfImg);
  finally
    intfImg.Free;
  end;
end;

function GetScalePercent: Integer;
begin
  if ScreenInfo.PixelsPerInchX <= 120 then
    Result := 100 // 100-125% (96-120 DPI): no scaling
  else
  if ScreenInfo.PixelsPerInchX <= 168 then
    Result := 150 // 126%-175% (144-168 DPI): 150% scaling
  else
    Result := Round(ScreenInfo.PixelsPerInchX/96) * 100; // 200, 300, 400, ...
end;

{ TButtonImages }
constructor TButtonImages.Create(AOwner: TExtTabCtrl);
begin
  FOwnerCtrl := AOwner;
  FillChar(FImgIndex, SizeOf(FImgIndex), $FF);
  FillChar(FSavedIndex, SizeOf(FSavedIndex), $FF);
end;

procedure TButtonImages.Assign(Source: TPersistent);
begin
  if Source is TButtonImages then
  begin
    FImgIndex := TButtonImages(Source).FImgIndex;
    if Assigned(FOnChange) then FOnChange(Self);
  end
  else
    inherited Assign(Source);
end;

function TButtonImages.GetIndex(Index: Integer): TImageIndex;
begin
  Result := FImgIndex[Index];
end;

procedure TButtonImages.Restore;
var
  i: Integer;
begin
  for i := Low(FImgIndex)to High(FImgIndex) do
    FImgIndex[i] := FSavedIndex[i];
end;

procedure TButtonImages.Save;
var
  i: Integer;
begin
  for i := Low(FImgIndex)to High(FImgIndex) do
    FSavedIndex[i] := FImgIndex[i];
end;

procedure TButtonImages.SetIndex(Index: Integer; Value: TImageIndex);
var
  Ptr: ^Integer;
begin
  if GetIndex(Index) <> Value then
  begin
    FImgIndex[index] := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

function TButtonImages.GetOwner: TExtTabCtrl;
begin
  Result := FOwnerCtrl;
end;

{ TButtonHints }
procedure TButtonHints.Assign(Source: TPersistent);
begin
  if Source is TButtonHints then
  begin
    FAddHint := TButtonHints(Source).AddHint;
    FScrollPrevHint := TButtonHints(Source).ScrollPrevHint;
    FScrollNextHint := TButtonHints(Source).ScrollNextHint;
  end
  else
    inherited Assign(Source);
end;

{ TExtFontOptions }
procedure TExtFontOptions.SetFontSize(AValue: Integer);
begin
  if FFontSize <> AValue then
  begin
    FFontSize := AValue;
    Changed;
  end;
end;

procedure TExtFontOptions.SetFontColor(AValue: TColor);
begin
  if FFontColor <> AValue then
  begin
    FFontColor := AValue;
    Changed;
  end;
end;

procedure TExtFontOptions.SetFontStyles(AValue: TFontStyles);
begin
  if FFontStyles <> AValue then
  begin
    FFontStyles := AValue;
    Changed;
  end;
end;

procedure TExtFontOptions.Changed;
begin
  if Assigned(FOnRedraw) then FOnRedraw(Self);
end;

constructor TExtFontOptions.Create;
begin
  FFontSize := 0;
  FFontColor := clNone;
  FFontStyles := [];
end;

procedure TExtFontOptions.Assign(Source: TPersistent);
begin
  if Source is TExtFontOptions then
  begin
    FFontSize := TExtFontOptions(Source).FontSize;
    FFontColor := TExtFontOptions(Source).FontColor;
    FFontStyles := TExtFontOptions(Source).FontStyles;
    Changed;
  end
  else
    inherited Assign(Source);
end;

{ TImagesWidth }
constructor TImagesWidth.Create;
begin
  FPrevWidth := 0;
  FNextWidth := 0;
  FAddWidth := 0;
  FCloseWidth := 0;
  FTabWidth := 0;
end;

procedure TImagesWidth.Assign(Source: TPersistent);
begin
  if Source is TImagesWidth then
  begin
    FPrevWidth := TImagesWidth(Source).PrevWidth;
    FNextWidth := TImagesWidth(Source).NextWidth;
    FAddWidth := TImagesWidth(Source).AddWidth;
    FCloseWidth := TImagesWidth(Source).CloseWidth;
    FTabWidth := TImagesWidth(Source).TabsWidth;
    if Assigned(FOnChange) then FOnChange(Self);
  end
  else
    inherited Assign(Source);
end;

procedure TImagesWidth.SetWidth(Index, Value: Integer);
var
  Ptr: ^Integer;
begin
  Ptr := nil;
  case Index of
    0: Ptr := @FPrevWidth;
    1: Ptr := @FNextWidth;
    2: Ptr := @FAddWidth;
    3: Ptr := @FCloseWidth;
    4: Ptr := @FTabWidth;
  end;
  if Ptr = nil then Exit;
  if Ptr^ <> Value then
  begin
    Ptr^ := Value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

{ TExtTab }
procedure TExtTab.SetCaption(AValue: TCaption);
begin
  if FCaption <> AValue then
  begin
    FCaption := AValue;
    FTextWidth := -1;
    FTextHeight := -1;
    Changed(False);
    Redraw(Self);
  end;
end;

procedure TExtTab.SetColor(AValue: TColor);
begin
  if FColor = AValue then Exit;
  FColor := AValue;
  if Assigned(FOwnerCtrl) then
    FOwnerCtrl.Invalidate;
end;

procedure TExtTab.SetStripeColor(AValue: TColor);
begin
  if FStripeColor = AValue then Exit;
  FStripeColor := AValue;
  if Assigned(FOwnerCtrl) then
    FOwnerCtrl.Invalidate;
end;

procedure TExtTab.SetVisible(AValue: Boolean);
var
  WasActive: Boolean;
  OldIndex, Candidate: Integer;
  Allow: Boolean;
begin
  if FVisible = AValue then Exit;
  if not Assigned(FOwnerCtrl) then
  begin
    FVisible := AValue;
    Exit;
  end;

  // If we just hid the active tab, route through SetTabIndex so that
  // OnTabChanging and OnTabChange fire normally
  WasActive := (not AValue) and (FOwnerCtrl.TabIndex = Self.Index);
  if WasActive then
  begin
    OldIndex := Self.Index;
    Candidate := FOwnerCtrl.NextVisibleTab(OldIndex);
    if Candidate = -1 then Candidate := FOwnerCtrl.PrevVisibleTab(OldIndex);
    // Fire OnTabChanging; if vetoed, keep the tab visible and abort
    Allow := True;
    if Assigned(FOwnerCtrl.FOnTabChanging) then
      FOwnerCtrl.FOnTabChanging(FOwnerCtrl, OldIndex, Candidate, Allow);
    if not Allow then Exit;
    FVisible := AValue;
    FOwnerCtrl.FTabIndex := Candidate;
    FOwnerCtrl.InvalidateLayout;
    if Assigned(FOwnerCtrl.FOnTabChanged) then
      FOwnerCtrl.FOnTabChanged(FOwnerCtrl, Candidate);
  end
  else
  begin
    FVisible := AValue;
    FOwnerCtrl.InvalidateLayout;
  end;
end;

function TExtTab.GetImage: TBitmap;
begin
  if FImage = nil then
    FImage := TBitmap.Create;
  Result := FImage;
end;

procedure TExtTab.SetShowCloseButton(AValue: Boolean);
begin
  if FShowCloseButton = AValue then Exit;
  FShowCloseButton := AValue;
  if Assigned(FOwnerCtrl) then
    FOwnerCtrl.InvalidateLayout;
end;

procedure TExtTab.SetImage(AValue: TBitmap);
begin
  if FImage = AValue then Exit;
  FreeAndNil(FImage);
  FreeAndNil(FCachedTabImage);
  FImage := AValue;
  FTextWidth := -1;
  Redraw(Self);
end;

procedure TExtTab.SetImageIndex(AValue: TImageIndex);
begin
  if FImageIndex = AValue then Exit;
  FImageIndex := AValue;
  FreeAndNil(FCachedTabImage);
  FTextWidth := -1;
  FTextHeight := -1;
  Redraw(Self);
end;

procedure TExtTab.Redraw(Sender: TObject);
begin
  if Sender = FFontOptions then
  begin
    FTextWidth := -1;
    FTextHeight := -1;
  end;
  if Assigned(FOwnerCtrl) then FOwnerCtrl.InvalidateLayout;
end;

function TExtTab.GetDisplayName: String;
begin
  Result := FCaption;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

constructor TExtTab.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);

  FOwnerCtrl := TExtTabs(ACollection).FOwnerCtrl;
  FFontOptions := TExtFontOptions.Create;
  FFontOptions.OnRedraw := @Redraw;
  FVisible := True;
  FColor := clNone;
  FStripeColor := clNone;
  FImageIndex := -1;
  FTextWidth := -1;
  FTextHeight := -1;
  FCachedTabImage := nil;
  FCachedImageRotation := -1;
  FShowCloseButton := True;
  // Provide a default caption so new tabs are never empty
  FCaption := 'New Tab ' + IntToStr(Index + 1);
end;

destructor TExtTab.Destroy;
begin
  FImage.Free;
  FCachedTabImage.Free;
  FFontOptions.Free;
  inherited Destroy;
end;

function TExtTab.GetOwner: TExtTabCtrl;
begin
  Result := FOwnerCtrl;
end;

{ TExtTabs }
function TExtTabs.GetItems(Index: Integer): TExtTab;
begin
  Result := TExtTab(inherited Items[Index]);
end;

procedure TExtTabs.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOwnerCtrl) then
  begin
    FOwnerCtrl.NormalizeState;
    FOwnerCtrl.InvalidateLayout;

    // Notify the IDE that the internal structure changed so it updates the Object Inspector
    {$IFDEF LCLDesign}
    if (csDesigning in FOwnerCtrl.ComponentState) and Assigned(GlobalDesignHook) then
    begin
      GlobalDesignHook.Modified(FOwnerCtrl);
      GlobalDesignHook.RefreshPropertyValues;
    end;
    {$ENDIF}
  end;
end;

function TExtTabs.GetOwner: TPersistent;
begin
  Result := FOwnerCtrl;
end;

constructor TExtTabs.Create(AOwner: TExtTabCtrl);
begin
  inherited Create(TExtTab);
  FOwnerCtrl := AOwner;
end;

function TExtTabs.Add(const ACaption: String): TExtTab;
begin
  Result := inherited Add as TExtTab;
  Result.Caption := ACaption;
end;

{ TExtTabCtrl - Private Methods }

procedure TExtTabCtrl.BeginInternalChange;
begin
  Inc(FInternalChange);
end;

procedure TExtTabCtrl.EndInternalChange;
begin
  if FInternalChange > 0 then Dec(FInternalChange);
end;

procedure TExtTabCtrl.CancelDrag;
begin
  if FDragging then
  begin
    FDragging := False;
    FDragIndex := -1;
    FDragTargetIndex := -1;
    FMouseDownIndex := -1;
    MouseCapture := False;
    Invalidate;
  end;
end;

procedure TExtTabCtrl.NormalizeState;
var
  Candidate: Integer;
begin
  // Clamp or fix FTabIndex
  if FTabs.Count = 0 then
    FTabIndex := -1
  else
  begin
    if FTabIndex >= FTabs.Count then
      FTabIndex := FTabs.Count - 1;
    // If active tab became invisible, move to nearest visible one
    if (FTabIndex >= 0) and not FTabs[FTabIndex].Visible then
    begin
      Candidate := NextVisibleTab(FTabIndex);
      if Candidate = -1 then Candidate := PrevVisibleTab(FTabIndex);
      FTabIndex := Candidate; // -1 if none visible at all
    end;
  end;

  // Clamp scroll offset
  if HandleAllocated then
    FScrollOffset := Max(0, Min(FScrollOffset, MaxScrollOffset))
  else
    FScrollOffset := Max(0, FScrollOffset);

  // Reset stale hover state
  if (FHoverTab >= FTabs.Count) then FHoverTab := -1;
  if (FHoverCloseTab >= FTabs.Count) then FHoverCloseTab := -1;

  // Cancel drag if indexes are now out of range
  if FDragging and
     ((FDragIndex < 0) or (FDragIndex >= FTabs.Count) or
      (FDragTargetIndex > FTabs.Count)) then
    CancelDrag;
end;

procedure TExtTabCtrl.InvalidateTabImageCaches;
var
  i: Integer;
begin
  for i := 0 to FTabs.Count - 1 do
  begin
    FreeAndNil(FTabs[i].FCachedTabImage);
    FTabs[i].FCachedImageRotation := -1;
  end;
end;

// Set an initial size when dropped onto a form by click
class function TExtTabCtrl.GetControlClassDefaultSize: TSize;
begin
  Result.cx := 300;
  Result.cy := 30;
end;

function TExtTabCtrl.NextVisibleTab(FromIndex: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := FromIndex + 1 to FTabs.Count - 1 do
    if FTabs[i].Visible then Exit(i);
end;

function TExtTabCtrl.PrevVisibleTab(FromIndex: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := FromIndex - 1 downto 0 do
    if FTabs[i].Visible then Exit(i);
end;

procedure TExtTabCtrl.SetTabIndex(AValue: Integer);
var
  Allow: Boolean;
  Candidate: Integer;
begin
  if FInternalChange > 0 then Exit;
  if (AValue < -1) or (AValue >= FTabs.Count) then
    AValue := -1;

  // If target tab is invisible, find the nearest visible one
  if (AValue >= 0) and not FTabs[AValue].Visible then
  begin
    Candidate := NextVisibleTab(AValue);
    if Candidate = -1 then
      Candidate := PrevVisibleTab(AValue);
    AValue := Candidate;
  end;

  if FTabIndex = AValue then Exit;

  Allow := True;
  if Assigned(FOnTabChanging) then
  begin
    FOnTabChanging(Self, FTabIndex, AValue, Allow);
    if csDestroying in ComponentState then Exit;
    // Re-validate: user callback may have changed the collection
    if (AValue >= FTabs.Count) then AValue := -1;
  end;
  if not Allow then Exit;

  BeginInternalChange;
  try
    // If toActiveBold/Italic is on, the old and new active tabs are
    // measured with different font styles
    if (toActiveBold in FTabOptions) or (toActiveItalic in FTabOptions) then
    begin
      if (FTabIndex >= 0) and (FTabIndex < FTabs.Count) then
      begin
        FTabs[FTabIndex].FTextWidth := -1;
        FTabs[FTabIndex].FTextHeight := -1;
      end;
      if (AValue >= 0) and (AValue < FTabs.Count) then
      begin
        FTabs[AValue].FTextWidth := -1;
        FTabs[AValue].FTextHeight := -1;
      end;
    end;
    FTabIndex := AValue;
    if FTabIndex <> -1 then
      EnsureTabVisible(FTabIndex);
  finally
    EndInternalChange;
  end;

  if Assigned(FOnTabChanged) then
  begin
    FOnTabChanged(Self, FTabIndex);
    if csDestroying in ComponentState then Exit;
  end;

  Invalidate;
end;

procedure TExtTabCtrl.SetTabSize(AValue: Integer);
begin
  if FTabSize <> AValue then
  begin
    FTabSize := AValue;
    InvalidateLayout;
  end;
end;

function TExtTabCtrl.IsStoredTabSize: Boolean;
begin
  Result := FTabSize <> Scale96ToFont(cDefaultTabSize);
end;

procedure TExtTabCtrl.AddBtnClick(Sender: TObject);
var
  P: TPoint;
begin
  // If the event has been assigned just call it and let the user add the tab
  if Assigned(FOnAddButtonClick) then
  begin
    FOnAddButtonClick(Self);
  end
  // If a menu is assigned, show it on left-click
  else if Assigned(FBtnAdd.PopupMenu) then
  begin
    P := FBtnAdd.ClientToScreen(Point(0, FBtnAdd.Height));
    FBtnAdd.PopupMenu.PopUp(P.X, P.Y);
  end
  else // Just add the tab
  begin
    // Reset the counter when the strip is empty
    if FTabs.Count = 0 then
      FAddTabCounter := 0;
    Inc(FAddTabCounter);
    AddTab('New Tab ' + IntToStr(FAddTabCounter));
  end;
end;

procedure TExtTabCtrl.ScrollPrev(Sender: TObject);
var
  i: Integer;
  VisibleStart: Integer;
begin
  if FScrollOffset = 0 then Exit;

  // Find the first tab that starts before the current scroll position and scroll to show it
  VisibleStart := FScrollOffset;

  for i := FTabs.Count - 1 downto 0 do
  begin
    if not FTabs[i].Visible then Continue;
    if IsHorizontal then
    begin
      if FTabs[i].FBoundRect.Left < VisibleStart then
      begin
        FScrollOffset := FTabs[i].FBoundRect.Left;
        Break;
      end;
    end
    else
    begin
      if FTabs[i].FBoundRect.Top < VisibleStart then
      begin
        FScrollOffset := FTabs[i].FBoundRect.Top;
        Break;
      end;
    end;
  end;
  if FScrollOffset < 0 then FScrollOffset := 0;
  UpdateScrollButtons;
  Invalidate;
end;

procedure TExtTabCtrl.ScrollNext(Sender: TObject);
var
  i: Integer;
  V: TRect;
  ViewSize, VisibleEnd: Integer;
begin
  V := TabsViewportRect;
  if IsHorizontal then
  begin
    ViewSize := V.Width;
    VisibleEnd := FScrollOffset + ViewSize;
    for i := 0 to FTabs.Count - 1 do
    begin
      if not FTabs[i].Visible then Continue;
      if FTabs[i].FBoundRect.Right > VisibleEnd then
      begin
        FScrollOffset := FTabs[i].FBoundRect.Left;
        Break;
      end;
    end;
  end
  else
  begin
    ViewSize := V.Height;
    VisibleEnd := FScrollOffset + ViewSize;
    for i := 0 to FTabs.Count - 1 do
    begin
      if not FTabs[i].Visible then Continue;
      if FTabs[i].FBoundRect.Bottom > VisibleEnd then
      begin
        FScrollOffset := FTabs[i].FBoundRect.Top;
        Break;
      end;
    end;
  end;
  FScrollOffset := Min(MaxScrollOffset, FScrollOffset);
  UpdateScrollButtons;
  Invalidate;
end;

procedure TExtTabCtrl.SetTabStyle(AValue: TTabStyle);
begin
  if FTabStyle <> AValue then
  begin
    FTabStyle := AValue;
    InvalidateLayout;
  end;
end;

procedure TExtTabCtrl.SetTabPosition(AValue: TTabPosition);
var
  WasVertical, WillBeVertical: Boolean;
  W, H: Integer;
begin
  if FTabPosition <> AValue then
  begin
    WasVertical := FTabPosition in [tpLeft, tpRight];
    WillBeVertical := AValue in [tpLeft, tpRight];

    BeginUpdate;
    try
      FTabPosition := AValue;
      FScrollOffset := 0;

      // Rotation angle changed --> per-tab image caches are stale
      InvalidateTabImageCaches;

      // When crossing between horizontal and vertical swap Width and Height
      // so the tab strip keeps the same thickness in the new orientation.
      if WasVertical <> WillBeVertical then
      begin
        W := Width;
        H := Height;
        SetBounds(Left, Top, H, W);
      end;

      UpdateBtnImages;
      AnchorButtons;

      // Mark the internal layout (tab rects) as dirty
      InvalidateLayout;

      // Tell the LCL autosize engine the preferred size has changed
      InvalidatePreferredSize;
    finally
      EndUpdate;
      if AutoSize then AdjustSize;
      Invalidate; // Force a full repaint of the control
    end;
  end;
end;

procedure TExtTabCtrl.SetTabOptions(AValue: TExtTabOptions);
var
  i: Integer;
  btnChanged: Boolean;
  tabChanged: Boolean;
begin
  if FTabOptions = AValue then Exit;
  btnChanged := [toRotateAddImage]*AValue <> [toRotateAddImage]*FTabOptions;
  tabChanged := [toRotateTabImages]*AValue <> [toRotateTabImages]*FTabOptions;
  FTabOptions := AValue;
  FLayoutDirty := True;
  // toActiveBold/Italic affects text measurements, reset all caches
  for i := 0 to FTabs.Count - 1 do
  begin
    FTabs[i].FTextWidth := -1;
    FTabs[i].FTextHeight := -1;
  end;
  if btnChanged or tabChanged then
  begin
    PrepareInternalBtnImages(GetRotationForPosition);
    PrepareInternalTabImages(GetRotationForPosition);
  end;
  AnchorButtons;
  Invalidate;
end;

procedure TExtTabCtrl.SetImages(AValue: TCustomImageList);
begin
  if FImages <> AValue then
  begin
    FImages := AValue;
    if FImages <> nil then
//      FImages.FreeNotification(Self)
    else
      FButtonImages.Save;
    UpdateBtnImages;
    InvalidateTabImageCaches;
    InvalidateLayout;
  end;
end;

procedure TExtTabCtrl.SetButtonImageIndexes(AValue: TButtonImages);
begin
  FButtonImages.Assign(AValue);
end;

procedure TExtTabCtrl.SetImagesWidth(AValue: TImagesWidth);
begin
  FImagesWidth.Assign(AValue);
end;

procedure TExtTabCtrl.SetMinCaptionLen(AValue: Integer);
var
  i: Integer;
begin
  if AValue < 0 then AValue := 0;
  if FMinCaptionLen = AValue then Exit;
  FMinCaptionLen := AValue;
  // Display text may change length — bust all text-width caches
  for i := 0 to FTabs.Count - 1 do
  begin
    FTabs[i].FTextWidth  := -1;
    FTabs[i].FTextHeight := -1;
  end;
  InvalidateLayout;
end;

procedure TExtTabCtrl.SetMaxCaptionLen(AValue: Integer);
var
  i: Integer;
begin
  if AValue < 0 then AValue := 0;
  if FMaxCaptionLen = AValue then Exit;
  FMaxCaptionLen := AValue;
  for i := 0 to FTabs.Count - 1 do
  begin
    FTabs[i].FTextWidth  := -1;
    FTabs[i].FTextHeight := -1;
  end;
  InvalidateLayout;
end;

procedure TExtTabCtrl.SetButtonHints(AValue: TButtonHints);
begin
  FButtonHints.Assign(AValue);

  if not (csLoading in ComponentState) then
    AnchorButtons;
end;

procedure TExtTabCtrl.SetTabs(AValue: TExtTabs);
begin
  FTabs.Assign(AValue);
end;

function TExtTabCtrl.GetAddMenu: TPopupMenu;
begin
  Result := FBtnAdd.PopupMenu;
end;

procedure TExtTabCtrl.SetOnDrawTab(AValue: TTabDrawEvent);
begin
  if (AValue <> FOnDrawTab) then
  begin
    FOnDrawTab := AValue;
    Invalidate;
  end;
end;

procedure TExtTabCtrl.SetAddMenu(AValue: TPopupMenu);
begin
  FBtnAdd.PopupMenu := AValue;
end;

procedure TExtTabCtrl.ButtonImagesChanged(Sender: TObject);
begin
  UpdateBtnImages;
end;

procedure TExtTabCtrl.ImagesWidthChanged(Sender: TObject);
begin
  // Drop the cached tab images
  InvalidateTabImageCaches;
  // Re-trigger the button image extraction
  UpdateBtnImages;
end;

function TExtTabCtrl.TabsViewportRect: TRect;
begin
  Result := ClientRect;

  case FTabPosition of
    tpTop: Result.Bottom := Result.Top + FTabSize;
    tpBottom: Result.Top := Result.Bottom - FTabSize;
    tpLeft: Result.Right := Result.Left + FTabSize;
    tpRight: Result.Left := Result.Right - FTabSize;
  end;

  // At design time all buttons are always visible
  if csDesigning in ComponentState then
  begin
    if IsHorizontal then
    begin
      Inc(Result.Left, FBtnScrollPrev.Width);
      Dec(Result.Right, FBtnScrollNext.Width + FBtnAdd.Width);
    end
    else
    begin
      Inc(Result.Top, FBtnScrollPrev.Height);
      Dec(Result.Bottom, FBtnScrollNext.Height + FBtnAdd.Height);
    end;
  end
  // At runtime, only include visible buttons
  else
  begin
    if IsHorizontal then
    begin
      if FBtnScrollPrev.Visible then Inc(Result.Left, FBtnScrollPrev.Width);
      if FBtnScrollNext.Visible then Dec(Result.Right, FBtnScrollNext.Width);
      if FBtnAdd.Visible then Dec(Result.Right, FBtnAdd.Width);
    end
    else
    begin
      if FBtnScrollPrev.Visible then Inc(Result.Top, FBtnScrollPrev.Height);
      if FBtnScrollNext.Visible then Dec(Result.Bottom, FBtnScrollNext.Height);
      if FBtnAdd.Visible then Dec(Result.Bottom, FBtnAdd.Height);
    end;
  end;
end;

// Sets Anchors on the three buttons so the LCL layout engine keeps them
// correctly positioned automatically on every resize
procedure TExtTabCtrl.AnchorButtons;
var
  ScrollPrevW, ScrollPrevH, ScrollNextW, ScrollNextH, AddW, AddH, ppi: Integer;
  ShowAdd: Boolean;
  NextLeft, NextTop: Integer;
  AddLeft, AddTop: Integer;
  MinStrip: Integer;
  imgBorder: Integer;
begin
  if (csDestroying in ComponentState) or not HandleAllocated then Exit;

  ppi := Font.PixelsPerInch;
  imgBorder := Scale96ToFont(2);
  with GetScrollButtonImages(0) do
  begin
    ScrollPrevW := WidthForPPI[FImagesWidth.PrevWidth, ppi];
    ScrollPrevH := HeightForPPI[FImagesWidth.PrevWidth, ppi];
  end;
  with GetScrollButtonImages(1) do
  begin
    ScrollNextW := WidthForPPI[FImagesWidth.NextWidth, ppi];
    ScrollNextH := HeightForPPI[FImagesWidth.NextWidth, ppi];
  end;
  with GetAddButtonImages do
  begin
    AddW := WidthForPPI[FImagesWidth.AddWidth, ppi];
    AddH := HeightForPPI[FImagesWidth.AddWidth, ppi];
  end;

  (*  THIS CAUSES A HIGH-DPI CRASH --> move to somewhere else (SetTabSize, SetImages)
  // Populate the cache first so the size variables below reflect the
  // already-rotated glyphs that will actually be drawn, not the raw
  // source bitmaps
  RefreshGlyphCache;

  ScaledTabSize := GetScale(FTabSize);
  ScrollPrevW := FCachedScrollGlyphs[0].Width;
  ScrollPrevH := FCachedScrollGlyphs[0].Height;
  ScrollNextW := FCachedScrollGlyphs[1].Width;
  ScrollNextH := FCachedScrollGlyphs[1].Height;
  AddW := FCachedAddGlyph.Width;
  AddH := FCachedAddGlyph.Height;

  // If any glyph is larger than the current tab strip, expand it to fit
  if IsHorizontal then
  begin
    MinStrip := Max(Max(ScrollPrevH, ScrollNextH), AddH);
    if FTabSize < MinStrip then
    begin
      FTabSize := MinStrip div GetScale(1);   // (wp) ???
      InvalidatePreferredSize;
      if AutoSize then AdjustSize;
    end;
  end
  else
  begin
    MinStrip := Max(Max(ScrollPrevW, ScrollNextW), AddW);
    if FTabSize < MinStrip then
    begin
      FTabSize := MinStrip div GetScale(1);    // (wp) ???
      InvalidatePreferredSize;
      if AutoSize then AdjustSize;
    end;
  end;
  *)

  // The Add button is always visible at design time
  ShowAdd := (toShowAddButton in FTabOptions) or (csDesigning in ComponentState);
  FBtnAdd.Visible := ShowAdd;

  // *** Horizontal orientation ***
  if IsHorizontal then
  begin
    // Scroll-Prev: left edge, full strip height
    if (FTabPosition = tpTop) then
      FBtnScrollPrev.Anchors := [akLeft, akTop]
    else
      FBtnScrollPrev.Anchors := [akLeft, akBottom];
    FBtnScrollPrev.AnchorSide[akLeft].Control := Self;
    FBtnScrollPrev.AnchorSide[akLeft].Side := asrLeft;
    if (FTabPosition = tpTop) then
    begin
      FBtnScrollPrev.AnchorSide[akTop].Control := Self;
      FBtnScrollPrev.AnchorSide[akTop].Side := asrTop;
      FBtnScrollPrev.Anchors := [akLeft, akTop];
    end else
    begin
      FBtnScrollPrev.AnchorSide[akBottom].Control := Self;
      FBtnScrollPrev.AnchorSide[akBottom].Side := asrBottom;
      FBtnScrollPrev.Anchors := [akLeft, akBottom];
    end;
    FBtnScrollPrev.Constraints.MinHeight := FTabSize;
    FBtnScrollPrev.Constraints.MinWidth := 0;
    // Due to anchoring the button is positioned automatically, no need to
    // specify Left and Top.
    FBtnScrollPrev.SetBounds(0, 0, ScrollPrevW, FTabSize);

    // Add: right edge, full strip height
    if (FTabPosition = tpTop) then
      FBtnAdd.Anchors := [akRight, akTop]
    else
      FBtnAdd.Anchors := [akRight, akBottom];
    FBtnAdd.AnchorSide[akRight].Control := Self;
    FBtnAdd.AnchorSide[akRight].Side := asrRight;
    if (FTabPosition = tpTop) then
    begin
      FBtnAdd.AnchorSide[akTop].Control := Self;
      FBtnAdd.AnchorSide[akTop].Side := asrTop;
      FBtnAdd.Anchors := [akRight, akTop];
    end else
    begin
      FBtnAdd.AnchorSide[akBottom].Control := Self;
      FBtnAdd.AnchorSide[akBottom].Side := asrBottom;
      FBtnAdd.Anchors := [akRight, akBottom];
    end;
    FBtnAdd.Constraints.MinHeight := FTabSize;
    FBtnAdd.Constraints.MinWidth := 0;
    inc(AddW, 2*imgBorder);                    // Hmmm... Sometimes there may be cases when this is not wanted
    // Due to anchoring the button is positioned automatically, no need to
    // specify Left and Top.
    FBtnAdd.SetBounds(0, 0, AddW, FTabSize);

    // Scroll-Next: just left of Add, full strip height
    if (FTabPosition = tpTop) then
    begin
      FBtnScrollNext.AnchorSide[akTop].Control := Self;
      FBtnScrollNext.AnchorSide[akTop].Side := asrTop;
      FBtnScrollNext.Anchors := [akRight, akTop];
    end else
    begin
      FBtnScrollNext.AnchorSide[akBottom].Control := Self;
      FBtnScrollNext.AnchorSide[akBottom].Side := asrBottom;
      FBtnScrollNext.Anchors := [akRight, akBottom];
    end;
    FBtnScrollNext.Constraints.MinHeight := FTabSize;
    FBtnScrollNext.Constraints.MinWidth := 0;

    if ShowAdd then
    begin
      FBtnScrollNext.AnchorSide[akRight].Control := FBtnAdd;
      FBtnScrollNext.AnchorSide[akRight].Side := asrLeft;
    end else
    begin
      FBtnScrollNext.AnchorSide[akRight].Control := Self;
      FBtnScrollNext.AnchorSide[akRight].Side := asrRight;
    end;

    // Due to anchoring the button is positioned automatically, no need to
    // specify Left and Top.
    FBtnScrollNext.SetBounds(0, 0, ScrollNextW, FTabSize);
  end
  else
  // *** Vertical orientation ***
  begin
    // Scroll-Prev: top of the strip, full strip width
    if (FTabPosition = tpLeft) then
      FBtnScrollPrev.Anchors := [akLeft, akTop]
    else
      FBtnScrollPrev.Anchors := [akRight, akTop];
    FBtnScrollPrev.AnchorSide[akTop].Control := Self;
    FBtnScrollPrev.AnchorSide[akTop].Side := asrTop;
    if (FTabPosition = tpLeft) then
    begin
      FBtnScrollPrev.AnchorSide[akLeft].Control := Self;
      FBtnScrollPrev.AnchorSide[akLeft].Side := asrLeft;
      FBtnScrollPrev.Anchors := [akLeft, akTop];
    end else
    begin
      FBtnScrollPrev.AnchorSide[akRight].Control := Self;
      FBtnScrollPrev.AnchorSide[akRight].Side := asrRight;
      FBtnScrollPrev.Anchors := [akRight, akTop];
    end;
    FBtnScrollPrev.Constraints.MinWidth := FTabSize;
    FBtnScrollPrev.Constraints.MinHeight := 0;
    FBtnScrollPrev.SetBounds(0, 0, FTabSize, ScrollPrevH);

    // Add: bottom of the strip, full strip width
    if (FTabPosition = tpLeft) then
      FBtnAdd.Anchors := [akLeft, akBottom]
    else
      FBtnAdd.Anchors := [akRight, akBottom];
    FBtnAdd.AnchorSide[akBottom].Control := Self;
    FBtnAdd.AnchorSide[akBottom].Side := asrBottom;
    if (FTabPosition = tpLeft) then
    begin
      FBtnAdd.AnchorSide[akLeft].Control := Self;
      FBtnAdd.AnchorSide[akLeft].Side := asrLeft;
      FBtnAdd.Anchors := [akLeft, akBottom];
    end else
    begin
      FBtnAdd.AnchorSide[akRight].Control := Self;
      FBtnAdd.AnchorSide[akRight].Side := asrRight;
      FBtnAdd.Anchors := [akRight, akBottom];
    end;
    FBtnAdd.Constraints.MinHeight := 0;
    FBtnAdd.Constraints.MinWidth := FTabSize;
    FBtnAdd.SetBounds(0, 0, FTabSize, AddH + 2*imgBorder);

    // Scroll-Next: just above Add, full strip width
    if (FTabPosition = tpLeft) then
    begin
      FBtnScrollNext.AnchorSide[akLeft].Control := Self;
      FBtnScrollNext.AnchorSide[akLeft].Side := asrLeft;
      FBtnScrollNext.Anchors := [akLeft, akBottom];
    end else
    begin
      FBtnScrollNext.AnchorSide[akRight].Control := Self;
      FBtnScrollNext.AnchorSide[akRight].Side := asrRight;
      FBtnScrollNext.Anchors := [akRight, akBottom];
    end;
    FBtnScrollNext.Constraints.MinHeight := 0;
    FBtnScrollNext.Constraints.MinWidth := FTabSize;

    if ShowAdd then
    begin
      FBtnScrollNext.AnchorSide[akBottom].Control := FBtnAdd;
      FBtnScrollNext.AnchorSide[akBottom].Side := asrTop;
    end else
    begin
      FBtnScrollNext.AnchorSide[akBottom].Control := Self;
      FBtnScrollNext.AnchorSide[akBottom].Side := asrBottom;
    end;

    FBtnScrollNext.SetBounds(0, 0, FTabSize, ScrollNextH);
  end;

  //FBtnScrollPrev.Glyph.Assign(FCachedScrollGlyphs[0]);
  //FBtnScrollNext.Glyph.Assign(FCachedScrollGlyphs[1]);
  //FBtnAdd.Glyph.Assign(FCachedAddGlyph);

  FLayoutDirty := True;
end;

function TExtTabCtrl.IsVertical: Boolean;
begin
  Result := FTabPosition in [tpLeft, tpRight];
end;

function TExtTabCtrl.IsHorizontal: Boolean;
begin
  Result := not IsVertical;
end;

function TExtTabCtrl.GetAddButtonImages: TCustomImageList;
begin
  if Assigned(FImages) and (FButtonImages.AddIndex > -1) then
    Result := FInternalTabImages
  else
    Result := FInternalBtnImages;
end;

function TExtTabCtrl.GetCloseButtonImages: TCustomImageList;
begin
  if Assigned(FImages) and (FButtonImages.CloseIndex > -1) then
    Result := FInternalTabImages
  else
    Result := FInternalBtnImages;
end;

function TExtTabCtrl.GetScrollButtonImages(Which: Integer): TCustomImageList;
var
  imgIndex: Integer;
begin
  case Which of
    0: imgIndex := FButtonImages.PrevIndex;
    1: imgIndex := FButtonImages.NextIndex;
  end;
  if Assigned(FImages) and (imgIndex > -1) then
    Result := FInternalTabImages
  else
    Result := FInternalBtnImages;
end;

function TExtTabCtrl.CloseButtonRect(Tab: TExtTab): TRect;
var
  CloseW, CloseH, M, ppi: Integer;
begin
  ppi := Font.PixelsPerInch;

  // Determine dynamic Close Button size
  CloseW := GetCloseButtonImages.WidthForPPI[FImagesWidth.CloseWidth, ppi];
  CloseH := CloseW;

  M := GetScale(cContentIndent);

  if IsHorizontal then
  begin
    // Horizontal: Positioned at the right end of the tab, vertically centered
    Result.Left := Tab.FBoundRect.Width - CloseW - M;
    Result.Top := (Tab.FBoundRect.Height - CloseH) div 2;
    Result.Right := Tab.FBoundRect.Width - M;
    Result.Bottom := Result.Top + CloseH;
  end
  else
  begin
    // Vertical: Horizontally centered within the tab strip
    Result.Left := (Tab.FBoundRect.Width - CloseW) div 2;
    Result.Right := Result.Left + CloseW;

    if FTabPosition = tpLeft then
    begin
      // tpLeft: Close button at the top
      Result.Top := M;
      Result.Bottom := M + CloseH;
    end
    else
    begin
      // tpRight: Close button at the bottom
      Result.Bottom := Tab.FBoundRect.Height - M;
      Result.Top := Result.Bottom - CloseH;
    end;
  end;
end;

function TExtTabCtrl.TabAtPos(X, Y: Integer): Integer;
var
  i: Integer;
  P: TPoint;
  V: TRect;
begin
  Result := -1;
  V := TabsViewportRect;
  if not PtInRect(V, Point(X, Y)) then Exit;
  if IsHorizontal then
    P := Point(X - V.Left + FScrollOffset, Y - V.Top)
  else
    P := Point(X - V.Left, Y - V.Top + FScrollOffset);
  for i := 0 to FTabs.Count - 1 do
    if PtInRect(FTabs[i].FBoundRect, P) then Exit(i);
end;

procedure TExtTabCtrl.LoadBitmapFromRes(const ResName: String; DestImg: TCustomBitmap);
begin
  Assert(DestImg <> nil, 'DestImg cannot be nil in LoadBitmapFromRes');
  case GetScalePercent of
    100: DestImg.LoadFromResourceName(HInstance, ResName);
    150: DestImg.LoadFromResourceName(HInstance, ResName + '_150');
    else DestImg.LoadFromResourceName(HInstance, ResName + '_200');
  end;
end;

procedure TExtTabCtrl.LoadBitmapFromLRS(const ResName: String; DestBitmap: TBitmap);
var
  PNG: TPortableNetworkGraphic;
begin
  if LazarusResources.Find(ResName) = nil then Exit;
  PNG := TPortableNetworkGraphic.Create;
  try
    PNG.LoadFromLazarusResource(ResName);
    DestBitmap.Assign(PNG);
  finally
    PNG.Free;
  end;
end;

function TExtTabCtrl.MaxScrollOffset: Integer;
var
  V: TRect;
begin
  V := TabsViewportRect;
  if IsHorizontal then
    Result := Max(0, FTotalTabsSize - V.Width)
  else
    Result := Max(0, FTotalTabsSize - V.Height);
end;

procedure TExtTabCtrl.EnsureTabVisible(Index: Integer);
var
  R, V: TRect;
begin
  if (Index < 0) or (Index >= FTabs.Count) then Exit;

  // Force buttons to update state so we know exactly how much
  // viewable space is actually "eaten" by the scroll buttons
  UpdateScrollButtons;
  CalcLayout;

  R := FTabs[Index].FBoundRect;
  V := TabsViewportRect;

  if IsHorizontal then
  begin
    // Check if the tab is hidden by the 'Next' button (Right side)
    if FBtnScrollNext.Visible and (R.Right > FScrollOffset + V.Width - FBtnScrollNext.Width) then
      FScrollOffset := R.Right - (V.Width - FBtnScrollNext.Width)
    // Check if the tab is hidden by the 'Previous' button (Left side)
    else if R.Left < FScrollOffset then
      FScrollOffset := R.Left;

    // Final check: if the Previous button is visible, ensure it doesn't overlap R.Left
    if FBtnScrollPrev.Visible and (R.Left < FScrollOffset + FBtnScrollPrev.Width) then
      FScrollOffset := R.Left - FBtnScrollPrev.Width;
  end
  else
  begin
    // Check if the tab is hidden by the 'Next' button (Bottom side)
    if FBtnScrollNext.Visible and (R.Bottom > FScrollOffset + V.Height - FBtnScrollNext.Height) then
      FScrollOffset := R.Bottom - (V.Height - FBtnScrollNext.Height)
    // Check if the tab is hidden by the 'Previous' button (Top side)
    else if R.Top < FScrollOffset then
      FScrollOffset := R.Top;

    // Final check: if the Previous button is visible, ensure it doesn't overlap R.Top
    if FBtnScrollPrev.Visible and (R.Top < FScrollOffset + FBtnScrollPrev.Height) then
      FScrollOffset := R.Top - FBtnScrollPrev.Height;
  end;

  // Final safety bounds: clamp BEFORE UpdateScrollButtons uses the value
  if FScrollOffset < 0 then FScrollOffset := 0;

  UpdateScrollButtons;
  Invalidate;
end;

procedure TExtTabCtrl.UpdateScrollButtons;
var
  Avail: Integer;
  Can: Boolean;
  NewPrevVis, NewNextVis: Boolean;
  HasChanged: Boolean;
begin
  if (FUpdateCount > 0) or not HandleAllocated then Exit;

  // At design time all buttons are always shown, so Avail must still account
  // for the Add button footprint even though FBtnAdd.Visible = False.
  if IsHorizontal then
  begin
    Avail := ClientWidth;
    if FBtnAdd.Visible or (csDesigning in ComponentState) then
      Avail := Avail - FBtnAdd.Width;
  end
  else
  begin
    Avail := ClientHeight;
    if FBtnAdd.Visible or (csDesigning in ComponentState) then
      Avail := Avail - FBtnAdd.Height;
  end;

  Can := FTotalTabsSize > Avail;
  NewPrevVis := Can and (FScrollOffset > 0);
  NewNextVis := Can and (FScrollOffset < MaxScrollOffset);

  HasChanged := (FBtnScrollPrev.Visible <> NewPrevVis) or
                (FBtnScrollNext.Visible <> NewNextVis);

  if HasChanged then
  begin
    FBtnScrollPrev.Visible := NewPrevVis;
    FBtnScrollNext.Visible := NewNextVis;
    if not Can then FScrollOffset := 0;
    FLayoutDirty := True;
    Invalidate;
  end;
end;

function TExtTabCtrl.GetScale(Value: Integer): Integer;
begin
  Result := Scale96ToFont(Value);
end;

procedure TExtTabCtrl.DrawTabTextAndImage(ACanvas: TCanvas; const R: TRect; Tab: TExtTab; IsActive: Boolean; DefaultFontColor: TColor);
var
  TextRect: TRect;
  ImgPos: TPoint;
  ImgW, ImgH: Integer;
  ActiveExtra: TFontStyles;
begin
  ACanvas.Font.Assign(Font);

  // Apply per-tab font overrides
  if Tab.FFontOptions.FontSize > 0 then
    ACanvas.Font.Size := Tab.FFontOptions.FontSize;
  if Tab.FFontOptions.FontStyles <> [] then
    ACanvas.Font.Style := Tab.FFontOptions.FontStyles;

  // Optionally render the active tab in bold and/or italic
  if IsActive then
  begin
    ActiveExtra := [];
    if toActiveBold in FTabOptions then Include(ActiveExtra, fsBold);
    if toActiveItalic in FTabOptions then Include(ActiveExtra, fsItalic);
    if ActiveExtra <> [] then
      ACanvas.Font.Style := ACanvas.Font.Style + ActiveExtra;
  end;

  if Tab.FFontOptions.FontColor <> clNone then
    ACanvas.Font.Color := Tab.FFontOptions.FontColor
  else
    ACanvas.Font.Color := DefaultFontColor;

  ACanvas.Brush.Style := bsClear;

  TextRect := GetTabTextBounds(ACanvas, R, Tab);

  if HasAnyImage(Tab) then
  begin
    ImgW := GetTabImageWidth(Tab);
    ImgH := GetTabImageHeight(Tab);

    if IsHorizontal then
      ImgPos := Point(TextRect.Left - GetScale(cImageSpacing) - ImgW, R.Top + (R.Height - ImgH) div 2)
    else
      // Vertical logic: adjust Y based on whether it's tpLeft or tpRight
      ImgPos := Point(R.Left + (R.Width - ImgW) div 2,
                  IfThen(FTabPosition = tpLeft, TextRect.Bottom + GetScale(cImageSpacing),
                                                TextRect.Top - GetScale(cImageSpacing) - ImgH));

    DrawTabImage(ACanvas, Tab, ImgPos.X, ImgPos.Y);
  end;

  if IsHorizontal then
    DrawText(ACanvas.Handle, PChar(GetDisplayCaption(Tab)), -1, TextRect,
             DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX)
  else
    DrawRotatedText(ACanvas, GetDisplayCaption(Tab), TextRect, GetRotationForPosition);
end;

procedure TExtTabCtrl.DrawTabImage(ACanvas: TCanvas; Tab: TExtTab; X, Y: Integer);
var
  bmp: TBitmap;
begin
  if Assigned(FImages) and (Tab.ImageIndex > -1) then
  begin
    bmp := TBitmap.Create;
    GetBaseTabBitmap(Tab, bmp);
    ACanvas.Draw(X, Y, bmp);
  end;
end;

procedure TExtTabCtrl.DrawRotatedText(ACanvas: TCanvas; const S: String; const R: TRect; Degrees: Integer);
var
  SavedOrientation: Integer;
begin
  SavedOrientation := ACanvas.Font.Orientation;
  ACanvas.Font.Orientation := Degrees*10;

  if Degrees = 90 then
    // 90°: anchor is Bottom-Left
    ACanvas.TextOut(R.Left, R.Bottom, S)
  else if Degrees = 270 then
    // 270°: anchor is Top-Right
    ACanvas.TextOut(R.Right, R.Top, S)
  else
    ACanvas.TextOut(R.Left, R.Top, S);

  ACanvas.Font.Orientation := SavedOrientation;
end;

function TExtTabCtrl.GetRotationForPosition: Integer;
begin
  case FTabPosition of
    tpLeft: Result := 90;
    tpRight: Result := 270;
    else Result := 0;
  end;
end;

function TExtTabCtrl.HasAnyImage(Tab: TExtTab): Boolean;
begin
  Result := (Assigned(FImages) and (Tab.ImageIndex >= 0)) or
            (Assigned(Tab.FImage) and not Tab.FImage.Empty);
end;

function TExtTabCtrl.GetTabImageWidth(Tab: TExtTab): Integer;
begin
  if Assigned(FImages) and (Tab.ImageIndex >= 0) then
    // Use the DPI-aware width
    Result := FInternalTabImages.WidthForPPI[FImagesWidth.TabsWidth, Font.PixelsPerInch]
  else if Assigned(Tab.FImage) and not Tab.FImage.Empty then
    Result := Tab.FImage.Width
  else
    Result := 0;
end;

function TExtTabCtrl.GetTabImageHeight(Tab: TExtTab): Integer;
begin
  if Assigned(FImages) and (Tab.ImageIndex >= 0) then
    // Use the DPI-aware height
    Result := FInternalTabImages.HeightForPPI[FImagesWidth.TabsWidth, Font.PixelsPerInch]
  else if Assigned(Tab.FImage) and not Tab.FImage.Empty then
    Result := Tab.FImage.Height
  else
    Result := 0;
end;

function TExtTabCtrl.GetTabTextBounds(ACanvas: TCanvas; const R: TRect; Tab: TExtTab): TRect;
var
  Indent, Spacing, CloseW, CloseH, ImgH, ppi: Integer;
  TextSize: TSize;
  TxtRect: TRect;
  CX, CY: Integer;
begin
  Indent := GetScale(cContentIndent);
  Spacing := GetScale(cImageSpacing);
  ppi := Font.PixelsPerInch;

  // Reuse the cached dimensions measured in CalcLayout, except when the cache is stale
  if Tab.FTextWidth >= 0 then
  begin
    TextSize.cx := Tab.FTextWidth;
    TextSize.cy := Tab.FTextHeight;
  end
  else
    TextSize := ACanvas.TextExtent(GetDisplayCaption(Tab));

  Result := R;

  if IsHorizontal then
  begin
    if Assigned(FImages) and (FButtonImages.CloseIndex > -1) then
      CloseW := FInternalTabImages.WidthForPPI[FImagesWidth.CloseWidth, ppi]
    else
      CloseW := FInternalBtnImages.WidthForPPI[0, ppi];

    TxtRect := R;
    TxtRect.Left := R.Left + Indent;

    Dec(TxtRect.Right, Indent);

    // Account for Image
    if HasAnyImage(Tab) then
      Inc(TxtRect.Left, GetTabImageWidth(Tab) + Spacing);

    // Account for Close Button
    if (toShowCloseButton in FTabOptions) and Tab.ShowCloseButton then
      Dec(TxtRect.Right, CloseW + Indent);

    Result.Left := TxtRect.Left + (TxtRect.Width - TextSize.cx) div 2;
    Result.Right := Result.Left + TextSize.cx;
    Result.Top := TxtRect.Top + (TxtRect.Height - TextSize.cy) div 2;
    Result.Bottom := Result.Top + TextSize.cy;
  end
  else
  begin
    // Mirroring DrawVerticalTab logic
    if Assigned(FImages) and (FButtonImages.CloseIndex > -1) then
      CloseH := FInternalTabImages.HeightForPPI[FImagesWidth.CloseWidth, ppi]
    else
      CloseH := FInternalBtnImages.HeightForPPI[0, ppi];
    TxtRect := R;
    InflateRect(TxtRect, -Indent, -Indent);

    if (toShowCloseButton in FTabOptions) and Tab.ShowCloseButton then
    begin
      if FTabPosition = tpLeft then
        Inc(TxtRect.Top, CloseH + Indent)
      else
        Dec(TxtRect.Bottom, CloseH + Indent);
    end;

    CX := (TxtRect.Left + TxtRect.Right) div 2;
    if FTabPosition = tpLeft then CY := TxtRect.Bottom else CY := TxtRect.Top;

    if HasAnyImage(Tab) then
    begin
      ImgH := GetTabImageHeight(Tab);
      if FTabPosition = tpLeft then
        CY := TxtRect.Bottom - ImgH - Spacing
      else
        CY := TxtRect.Top + ImgH + Spacing;
    end;

    if FTabPosition = tpLeft then
    begin
      // 90°: TextOut anchor is Bottom-Left
      Result.Left := CX - (TextSize.cy div 2);
      Result.Right := Result.Left + TextSize.cy;
      Result.Bottom := CY;
      Result.Top := CY - TextSize.cx;
    end
    else
    begin
      // 270°: TextOut anchor is Top-Right
      Result.Left := CX - (TextSize.cy div 2);
      Result.Right := Result.Left + TextSize.cy;
      Result.Top := CY;
      Result.Bottom := CY + TextSize.cx;
    end;
  end;
end;

{ Get the best matching resolution for the current DPI }
procedure TExtTabCtrl.GetBaseTabBitmap(Tab: TExtTab; Dest: TBitmap);
begin
  if Assigned(FImages) and (Tab.ImageIndex >= 0) then
    //FImages.ResolutionForPPI[FImagesWidth.TabsWidth, Font.PixelsPerInch, GetCanvasScaleFactor].GetBitmap(Tab.ImageIndex, Dest)
    FInternalTabImages.ResolutionForPPI[FImagesWidth.TabsWidth, Font.PixelsPerInch, 1].GetBitmap(Tab.ImageIndex, Dest)
  else if Assigned(Tab.FImage) and not Tab.FImage.Empty then
    Dest.Assign(Tab.FImage)
  else
    Dest.Clear;
end;

procedure TExtTabCtrl.DrawCloseButton(ACanvas: TCanvas; const R: TRect; Tab: TExtTab; IsActive: Boolean);
var
  CloseR: TRect;
  imgRes: TScaledImageListResolution;
  scale: Integer = 1;
  ppi: Integer;
  effect: TGraphicsDrawEffect;
begin
  if not (toShowCloseButton in FTabOptions) or not Tab.ShowCloseButton then Exit;

  ppi := Font.PixelsPerInch;
  CloseR := CloseButtonRect(Tab);
  Types.OffsetRect(CloseR, R.Left, R.Top);

  if FHoverCloseTab = Tab.Index then
  begin
//    ACanvas.Brush.Color := clSilver;
//    ACanvas.FillRect(CloseR);
    effect := gdeHighlighted;
  end else
    effect := gdeNormal;

  if Assigned(FImages) and (FButtonImages.CloseIndex >= 0) then
  begin
    imgRes := FInternalTabImages.ResolutionForPPI[FImagesWidth.CloseWidth, ppi, scale];
    imgRes.Draw(ACanvas,
      CloseR.Left + (CloseR.Width - imgRes.Width) div 2,
      CloseR.Top + (CloseR.Height - imgRes.Height) div 2,
      FButtonImages.CloseIndex,
      effect
    );
  end else
  if Assigned(FInternalBtnImages) then
  begin
    imgRes := FInternalBtnImages.ResolutionForPPI[0, ppi, scale];
    imgRes.Draw(ACanvas,
      CloseR.Left + (CloseR.Width - imgRes.Width) div 2,
      CloseR.Top + (CloseR.Height - imgRes.Height) div 2,
      cCloseIndex,
      effect
    );
  end else
  begin
    // Vector fallback: draw a simple x using the pen
    ACanvas.Pen.Color := IfThen(FHoverCloseTab = Tab.Index, clBlack, clGray);
    ACanvas.Pen.Width := GetScale(1);
    ACanvas.Line(CloseR.Left + GetScale(3), CloseR.Top + GetScale(3),
                 CloseR.Right - GetScale(3), CloseR.Bottom - GetScale(3));
    ACanvas.Line(CloseR.Right - GetScale(3), CloseR.Top + GetScale(3),
                 CloseR.Left + GetScale(3), CloseR.Bottom - GetScale(3));
  end;
end;

procedure TExtTabCtrl.DrawColorStripe(ACanvas: TCanvas; const R: TRect; Tab: TExtTab; Indent: Integer);
var
  IndicatorRect: TRect;
  Thick: Integer;
begin
  if (Tab.StripeColor = clNone) then Exit;

  ACanvas.Brush.Color := Tab.StripeColor;
  ACanvas.Pen.Style := psClear;
  Thick := GetScale(3);

  // Safety catch for extremely narrow tabs
  if (R.Width <= Indent*2) or (R.Height <= Indent*2) then
    Indent := 0;

  case FTabPosition of
    tpTop: IndicatorRect := Rect(R.Left + Indent, R.Top + 1, R.Right - Indent, R.Top + 1 + Thick);
    tpBottom: IndicatorRect := Rect(R.Left + Indent, R.Bottom - 1 - Thick, R.Right - Indent, R.Bottom - 1);
    tpLeft: IndicatorRect := Rect(R.Left + 1, R.Top + Indent, R.Left + 1 + Thick, R.Bottom - Indent);
    tpRight: IndicatorRect := Rect(R.Right - 1 - Thick, R.Top + Indent, R.Right - 1, R.Bottom - Indent);
  end;

  ACanvas.FillRect(IndicatorRect);
  ACanvas.Pen.Style := psSolid;
end;

{ Drawing Handlers }

procedure TExtTabCtrl.DrawFlatTab(ACanvas: TCanvas; var R: TRect; IsActive: Boolean; Tab: TExtTab; var FontColor: TColor; var Indent: Integer);
var
  P: array[0..3] of TPoint;
  BaseClr: TColor;
begin
  Indent := 2;
  FontColor := IfThen(IsActive, Font.Color, clGrayText);

  // Draw Background
  if IsActive then
  begin
    if Tab.Index = FHoverTab then
      ACanvas.Brush.Color := BlendColors(ResolveColor(Color), clHighlight, 0.3)
    else
      ACanvas.Brush.Color := Color;
  end
  else
  begin
    if (Tab.Color <> clNone) then
      BaseClr := ResolveColor(Tab.Color)
    else
      BaseClr := clBtnFace;

    if (Tab.Index = FHoverTab) then
      ACanvas.Brush.Color := BlendColors(BaseClr, clHighlight, 0.2)
    else
      ACanvas.Brush.Color := BaseClr;
  end;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.FillRect(R);

  // Border Logic
  ACanvas.Pen.Color := clBtnShadow;
  case FTabPosition of
    tpTop: begin
      P[0] := Point(R.Left, R.Bottom - 1);
      P[1] := Point(R.Left, R.Top);
      P[2] := Point(R.Right - 1, R.Top);
      P[3] := Point(R.Right - 1, R.Bottom - 1);
    end;
    tpBottom: begin
      P[0] := Point(R.Left, R.Top);
      P[1] := Point(R.Left, R.Bottom - 1);
      P[2] := Point(R.Right - 1, R.Bottom - 1);
      P[3] := Point(R.Right - 1, R.Top);
    end;
    tpLeft: begin
      P[0] := Point(R.Right - 1, R.Top);
      P[1] := Point(R.Left, R.Top);
      P[2] := Point(R.Left, R.Bottom - 1);
      P[3] := Point(R.Right - 1, R.Bottom - 1);
    end;
    tpRight: begin
      P[0] := Point(R.Left, R.Top);
      P[1] := Point(R.Right - 1, R.Top);
      P[2] := Point(R.Right - 1, R.Bottom - 1);
      P[3] := Point(R.Left, R.Bottom - 1);
    end;
  end;

  if IsActive then ACanvas.Polyline(P) else ACanvas.Polygon(P);
end;

procedure TExtTabCtrl.DrawButtonTab(ACanvas: TCanvas; var R: TRect; IsActive: Boolean; Tab: TExtTab; var FontColor: TColor; var Indent: Integer);
var
  BaseClr, LightClr, ShadowClr, BackClr: TColor;
begin
  Indent := 2;
  FontColor := IfThen(IsActive, Font.Color, clGrayText);

  if (Tab.Color <> clNone) then
  begin
    BaseClr := ResolveColor(Tab.Color);
    LightClr := BlendColors(BaseClr, clWhite, 0.4);
    ShadowClr := BlendColors(BaseClr, clBlack, 0.4);
  end
  else
  begin
    BaseClr := clBtnFace;
    LightClr := clBtnHighlight;
    ShadowClr := clBtnShadow;
  end;


  // Determine and Draw Background
  if IsActive then
  begin
    if Tab.Index = FHoverTab then
      BackClr := BlendColors(BaseClr, clHighlight, 0.15)
    else
      BackClr := BlendColors(BaseClr, clBlack, 0.05);
  end
  else if Tab.Index = FHoverTab then
    BackClr := BlendColors(BaseClr, clHighlight, 0.3)
  else
    BackClr := BaseClr;

  ACanvas.Brush.Color := BackClr;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.FillRect(R);

  // Draw 3D Frame
  if IsActive then
  begin
    // Inverted 3D frame (Shadow on Top/Left, Light on Bottom/Right)
    ACanvas.Pen.Color := ShadowClr;
    ACanvas.Polyline([Point(R.Left, R.Bottom - 1), Point(R.Left, R.Top),
      Point(R.Right - 1, R.Top)]);
    ACanvas.Pen.Color := LightClr;
    ACanvas.Polyline([Point(R.Right - 1, R.Top), Point(R.Right - 1, R.Bottom - 1),
      Point(R.Left, R.Bottom - 1)]);

    // Adjust Content Position for "Pressed" effect
    Types.OffsetRect(R, GetScale(1), GetScale(1));
  end
  else
  begin
    // Standard 3D frame (Light on Top/Left, Shadow on Bottom/Right)
    ACanvas.Pen.Color := LightClr;
    ACanvas.Polyline([Point(R.Left, R.Bottom - 1), Point(R.Left, R.Top),
      Point(R.Right - 1, R.Top)]);
    ACanvas.Pen.Color := ShadowClr;
    ACanvas.Polyline([Point(R.Right - 1, R.Top), Point(R.Right - 1, R.Bottom - 1),
      Point(R.Left, R.Bottom - 1)]);
  end;
end;

procedure TExtTabCtrl.DrawDelphiTab(ACanvas: TCanvas; var R: TRect; IsActive: Boolean; Tab: TExtTab; var FontColor: TColor; var Indent: Integer);
var
  P: array[0..3] of TPoint;
  S: Integer;
  BaseClr: TColor;
begin
  Indent := 4;
  FontColor := IfThen(IsActive, Font.Color, clGrayText);

  S := GetScale(3); // Angle slant amount

  // Set Colors and draw background
  if IsActive then
  begin
    if Tab.Index = FHoverTab then
      ACanvas.Brush.Color := BlendColors(ResolveColor(Color), clHighlight, 0.3)
    else
      ACanvas.Brush.Color := Color;
  end
  else
  begin
    if (Tab.Color <> clNone) then
      BaseClr := ResolveColor(Tab.Color)
    else
      BaseClr := clBtnFace;

    if Tab.Index = FHoverTab then
      ACanvas.Brush.Color := BlendColors(BaseClr, clHighlight, 0.2)
    else
      ACanvas.Brush.Color := BaseClr;
  end;

  ACanvas.Pen.Color := clBtnShadow;
  ACanvas.Brush.Style := bsSolid;

  // Define Polygon Points for the tab body
  case FTabPosition of
    tpTop: begin
      P[0] := Point(R.Left, R.Bottom);
      P[1] := Point(R.Left + S, R.Top);
      P[2] := Point(R.Right - S, R.Top);
      P[3] := Point(R.Right, R.Bottom);
    end;
    tpBottom: begin
      P[0] := Point(R.Left, R.Top);
      P[1] := Point(R.Left + S, R.Bottom - 1);
      P[2] := Point(R.Right - S, R.Bottom - 1);
      P[3] := Point(R.Right, R.Top);
    end;
    tpLeft: begin
      P[0] := Point(R.Right, R.Top);
      P[1] := Point(R.Left, R.Top + S);
      P[2] := Point(R.Left, R.Bottom - S);
      P[3] := Point(R.Right, R.Bottom);
    end;
    tpRight: begin
      P[0] := Point(R.Left, R.Top);
      P[1] := Point(R.Right - 1, R.Top + S);
      P[2] := Point(R.Right - 1, R.Bottom - S);
      P[3] := Point(R.Left, R.Bottom);
    end;
  end;

  if IsActive then
  begin
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Polygon(P);
    ACanvas.Pen.Color := clBtnShadow;
    ACanvas.Polyline(P);
  end
  else
    ACanvas.Polygon(P);
end;

procedure TExtTabCtrl.DrawChromeTab(ACanvas: TCanvas; var R: TRect; IsActive: Boolean; Tab: TExtTab; var FontColor: TColor; var Indent: Integer);
var
  Radius: Integer;
  StripeBounds: TRect;
  BaseClr: TColor;
begin
  Indent := 5;
  FontColor := IfThen(IsActive, Font.Color, clGrayText);

  Radius := GetScale(8);

  // Background and hover
  if IsActive then
  begin
    BaseClr := ResolveColor(Color);

    if Tab.Index = FHoverTab then
      ACanvas.Brush.Color := BlendColors(BaseClr, clHighlight, 0.2)
    else
      ACanvas.Brush.Color := BaseClr;
    ACanvas.Brush.Style := bsSolid;
  end
  else
  begin
    if (Tab.Color <> clNone) then
      BaseClr := ResolveColor(Tab.Color)
    else
      BaseClr := ResolveColor(Color);

    if Tab.Index = FHoverTab then
    begin
      // On hover, slide from Tab.Color toward the component background
      // blended with a touch of Highlight
      if Tab.Color <> clNone then
        ACanvas.Brush.Color := BlendColors(BlendColors(ResolveColor(Color), clHighlight, 0.12), BaseClr, 0.25)
      else
        ACanvas.Brush.Color := BlendColors(BaseClr, clHighlight, 0.08);
      ACanvas.Brush.Style := bsSolid;
    end
    else if Tab.Color <> clNone then
    begin
      // At rest: draw with the tab's own colour
      ACanvas.Brush.Color := BaseClr;
      ACanvas.Brush.Style := bsSolid;
    end
    else
      ACanvas.Brush.Style := bsClear;
  end;

  // Draw Tab (RoundRect with overlap to square the bottom)
  if IsActive or (Tab.Index = FHoverTab) or
     (not IsActive and (Tab.Color <> clNone)) then
  begin
    ACanvas.Pen.Color := clBtnShadow;
    case FTabPosition of
      tpTop:
        ACanvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom + Radius, Radius, Radius);
      tpBottom:
        ACanvas.RoundRect(R.Left, R.Top - Radius, R.Right, R.Bottom, Radius, Radius);
      tpLeft:
        ACanvas.RoundRect(R.Left, R.Top, R.Right + Radius, R.Bottom, Radius, Radius);
      tpRight:
        ACanvas.RoundRect(R.Left - Radius, R.Top, R.Right, R.Bottom, Radius, Radius);
    end;
  end;

  // Draw the shadow line for all tabs on the side touching the body
  ACanvas.Pen.Color := clBtnShadow;
  case FTabPosition of
    tpTop: ACanvas.Line(R.Left, R.Bottom - 1, R.Right, R.Bottom - 1);
    tpBottom: ACanvas.Line(R.Left, R.Top, R.Right, R.Top);
    tpLeft: ACanvas.Line(R.Right - 1, R.Top, R.Right - 1, R.Bottom);
    tpRight: ACanvas.Line(R.Left, R.Top, R.Left, R.Bottom);
  end;

  // Separators (For inactive non-hovered tabs without their own colour border)
  if not IsActive and (Tab.Index <> FHoverTab) and
     (Tab.Index <> FTabIndex - 1) and (Tab.Color = clNone) then
  begin
    ACanvas.Pen.Color := clBtnShadow;
    if IsHorizontal then
      ACanvas.Line(R.Right - 1, R.Top + GetScale(6), R.Right - 1, R.Bottom - GetScale(6))
    else
      ACanvas.Line(R.Left + GetScale(6), R.Bottom - 1, R.Right - GetScale(6), R.Bottom - 1);
  end;

  // Active Tab: Accent Line and "Open" Connection
  if IsActive then
  begin
    // Erase the strip-line segment under the active tab using the active fill
    ACanvas.Pen.Color := BaseClr;
    case FTabPosition of
      tpTop: ACanvas.Line(R.Left + 1, R.Bottom - 1, R.Right - 1, R.Bottom - 1);
      tpBottom: ACanvas.Line(R.Left + 1, R.Top, R.Right - 1, R.Top);
      tpLeft: ACanvas.Line(R.Right - 1, R.Top + 1, R.Right - 1, R.Bottom - 1);
      tpRight: ACanvas.Line(R.Left, R.Top + 1, R.Left, R.Bottom - 1);
    end;

    // Accent line: use Tab.Color when set, otherwise fall back to clHighlight
    if (Tab.StripeColor = clNone) then
    begin
      ACanvas.Pen.Color := IfThen(Tab.Color <> clNone,
        ResolveColor(Tab.Color), clHighlight);
      ACanvas.Pen.Width := GetScale(3);

      StripeBounds := R;
      InflateRect(StripeBounds, -GetScale(5), -GetScale(5));

      case FTabPosition of
        tpTop: ACanvas.Line(StripeBounds.Left, R.Top + 1, StripeBounds.Right, R.Top + 1);
        tpBottom: ACanvas.Line(StripeBounds.Left, R.Bottom - 2, StripeBounds.Right, R.Bottom - 2);
        tpLeft: ACanvas.Line(R.Left + 1, StripeBounds.Top, R.Left + 1, StripeBounds.Bottom);
        tpRight: ACanvas.Line(R.Right - 2, StripeBounds.Top, R.Right - 2, StripeBounds.Bottom);
      end;
      ACanvas.Pen.Width := 1;
    end;
  end;
end;

procedure TExtTabCtrl.DrawMacOSTab(ACanvas: TCanvas; var R: TRect; IsActive: Boolean; Tab: TExtTab; var FontColor: TColor; var Indent: Integer);
var
  Radius: Integer;
  DrawR: TRect;
  BaseClr: TColor;
begin
  Indent := 6;
  FontColor := IfThen(IsActive, clWindowText, clGrayText);

  Radius := GetScale(6);
  DrawR := R;

  // Floating segment effect
  InflateRect(DrawR, -GetScale(2), -GetScale(2));

  if IsActive then
  begin
    // Active pill: blend component Color with clWindow for the floating look
    // When Tab.Color is set, tint the base subtly so the color is visible
    // without losing the characteristic macOS translucent-pill appearance
    if Tab.Color <> clNone then
      BaseClr := BlendColors(ResolveColor(Color), ResolveColor(Tab.Color), 0.25)
    else
      BaseClr := ResolveColor(Color);
    if Tab.Index = FHoverTab then
      ACanvas.Brush.Color := BlendColors(BlendColors(BaseClr, clWindow, 0.85), clHighlight, 0.2)
    else
      ACanvas.Brush.Color := BlendColors(BaseClr, clWindow, 0.85);
    ACanvas.Pen.Color := BlendColors(BaseClr, clBtnShadow, 0.15);
    ACanvas.RoundRect(DrawR.Left, DrawR.Top, DrawR.Right, DrawR.Bottom, Radius, Radius);
  end
  else
  begin
    if (Tab.Color <> clNone) then
      BaseClr := ResolveColor(Tab.Color)
    else
      BaseClr := ResolveColor(Color);

    if Tab.Index = FHoverTab then
    begin
      // Hover: blend Tab.Color toward window background
      ACanvas.Brush.Color := BlendColors(BaseClr, clWindow, 0.9);
      ACanvas.Pen.Style := psClear;
      ACanvas.RoundRect(DrawR.Left, DrawR.Top, DrawR.Right, DrawR.Bottom, Radius, Radius);
      ACanvas.Pen.Style := psSolid;
    end
    else if Tab.Color <> clNone then
    begin
      // Non-hover inactive with Tab.Color: draw a subtle tinted pill so the
      // color is visible at rest (more muted than on hover)
      ACanvas.Brush.Color := BlendColors(BaseClr, clWindow, 0.82);
      ACanvas.Pen.Style := psClear;
      ACanvas.RoundRect(DrawR.Left, DrawR.Top, DrawR.Right, DrawR.Bottom, Radius, Radius);
      ACanvas.Pen.Style := psSolid;
    end;

    // Minimalist separators
    if (Tab.Index < FTabs.Count - 1) and (Tab.Index <> FTabIndex) and
      (Tab.Index <> FTabIndex - 1) then
    begin
      ACanvas.Pen.Color := BlendColors(ResolveColor(Color), clBlack, 0.05);
      ACanvas.MoveTo(R.Right - 1, R.Top + GetScale(7));
      ACanvas.LineTo(R.Right - 1, R.Bottom - GetScale(7));
    end;
  end;
end;

// Resolves clDefault and other system colors before mixing
function TExtTabCtrl.ResolveColor(AColor: TColor): TColor;
begin
  if AColor = clDefault then
    Result := ColorToRGB(GetDefaultColor(dctBrush))
  else
    Result := ColorToRGB(AColor);
end;

// Returns the caption as it should appear on the tab:
// 1. Padded with trailing spaces when shorter than FMinCaptionLen
// 2. Truncated to FMaxCaptionLen with an ellipsis in the middle when longer
//    first FMaxCaptionLen - 5 - Len(...) chars + '...' + last 5 chars
// 3. 0 = no limit
// The original caption is never modified, hints always show the full text
function TExtTabCtrl.GetDisplayCaption(Tab: TExtTab): String;
const
  EllipsisStr = '...';
  TailLen = 5;
var
  S: String;
  HeadLen: Integer;
begin
  S := Tab.Caption;

  // Minimum length: pad with spaces
  if (FMinCaptionLen > 0) and (Length(S) < FMinCaptionLen) then
    S := S + StringOfChar(' ', FMinCaptionLen - Length(S));

  // Maximum length: middle ellipsis
  if (FMaxCaptionLen > 0) and (Length(S) > FMaxCaptionLen) then
  begin
    // Head fills everything left after reserving tail + ellipsis
    HeadLen := FMaxCaptionLen - TailLen - Length(EllipsisStr);
    if HeadLen < 1 then HeadLen := 1;
    S := Copy(S, 1, HeadLen) + EllipsisStr + Copy(S, Length(S) - TailLen + 1, TailLen);
  end;

  Result := S;
end;

// Draws the folder-tab separator line along the inner edge of the tab strip
procedure TExtTabCtrl.DrawStripLine(ACanvas: TCanvas; const View: TRect);
var
  ActiveR: TRect;
  StripY, StripX: Integer;
begin
  if FTabStyle = tsMacOS then Exit;

  ACanvas.Pen.Color := clBtnShadow;
  ACanvas.Pen.Width := 1;
  ACanvas.Pen.Style := psSolid;

  if (FTabIndex >= 0) and (FTabIndex < FTabs.Count) then
  begin
    // Compute the screen rect of the active tab
    ActiveR := FTabs[FTabIndex].FBoundRect;
    if IsHorizontal then
      Types.OffsetRect(ActiveR, View.Left - FScrollOffset, View.Top)
    else
      Types.OffsetRect(ActiveR, View.Left, View.Top - FScrollOffset);

    // Draw two segments: before and after the active tab
    case FTabPosition of
      tpTop:
      begin
        StripY := View.Bottom - 1;
        ACanvas.Line(0, StripY, ActiveR.Left, StripY);            // left segment
        ACanvas.Line(ActiveR.Right, StripY, ClientWidth, StripY); // right segment
      end;
      tpBottom:
      begin
        StripY := View.Top;
        ACanvas.Line(0, StripY, ActiveR.Left, StripY);
        ACanvas.Line(ActiveR.Right, StripY, ClientWidth, StripY);
      end;
      tpLeft:
      begin
        StripX := View.Right - 1;
        ACanvas.Line(StripX, 0, StripX, ActiveR.Top);               // top segment
        ACanvas.Line(StripX, ActiveR.Bottom, StripX, ClientHeight); // bottom segment
      end;
      tpRight:
      begin
        StripX := View.Left;
        ACanvas.Line(StripX, 0, StripX, ActiveR.Top);
        ACanvas.Line(StripX, ActiveR.Bottom, StripX, ClientHeight);
      end;
    end;
  end
  else
  begin
    // No active tab: draw the full unbroken line
    case FTabPosition of
      tpTop: ACanvas.Line(0, View.Bottom - 1, ClientWidth, View.Bottom - 1);
      tpBottom: ACanvas.Line(0, View.Top, ClientWidth, View.Top);
      tpLeft: ACanvas.Line(View.Right - 1, 0, View.Right - 1, ClientHeight);
      tpRight: ACanvas.Line(View.Left, 0, View.Left, ClientHeight);
    end;
  end;
end;

procedure TExtTabCtrl.DrawTab(ACanvas: TCanvas; Index: Integer; ARect: TRect; IsActive: Boolean);
var
  IsHover: Boolean;
  FontColor: TColor;
  Indent: Integer;
  Tab: TExtTab;
  TabRect: TRect;
begin
  Tab := FTabs[Index];
  IsHover := (Index = FHoverTab);
  TabRect := ARect;

  // Sensible defaults; the style procedure (or OnDrawTab) may override either.
  FontColor := IfThen(IsActive, Font.Color, clGrayText);
  Indent := 2;

  // If the user has assigned a custom draw event, use it
  // Otherwise, dispatch to the built-in style
  // Either way the background/border is drawn here
  if Assigned(FOnDrawTab) then
    FOnDrawTab(Self, ACanvas, TabRect, IsActive, IsHover, FontColor, Indent)
  else
  begin
    case FTabStyle of
      tsButton: DrawButtonTab(ACanvas, TabRect, IsActive, Tab, FontColor, Indent);
      tsDelphi: DrawDelphiTab(ACanvas, TabRect, IsActive, Tab, FontColor, Indent);
      tsMacOS:  DrawMacOSTab(ACanvas, TabRect, IsActive, Tab, FontColor, Indent);
      tsFlat:   DrawFlatTab(ACanvas, TabRect, IsActive, Tab, FontColor, Indent);
      tsChrome: DrawChromeTab(ACanvas, TabRect, IsActive, Tab, FontColor, Indent);
    end;
  end;

  // Content common to every style: color stripe, image, caption, close button.
  if (Tab.StripeColor <> clNone) then
  begin
    DrawColorStripe(ACanvas, TabRect, Tab, Indent);
    ACanvas.Brush.Style := bsClear;
    ACanvas.Pen.Style := psSolid;
  end;

  DrawTabTextAndImage(ACanvas, TabRect, Tab, IsActive, FontColor);
  DrawCloseButton(ACanvas, TabRect, Tab, IsActive);
end;

procedure TExtTabCtrl.CalcLayout;
var
  i, Pos, TabLen: Integer;
  TxtExtent, ImgExtent,
  CloseExtent, Padding, ppi: Integer;
  ActiveExtra: TFontStyles;
  ImgW, ImgH, MinStrip: Integer;
begin
  if not FLayoutDirty then Exit;
  FLayoutDirty := False;

  //DisableAutoSizing;

  Canvas.Font.Assign(Font);
  Pos := 0;
  Padding := GetScale(cContentIndent)*2;
  ppi := Font.PixelsPerInch;

  for i := 0 to FTabs.Count - 1 do
  begin
    if not FTabs[i].Visible then
    begin
      FTabs[i].FBoundRect := Rect(0, 0, 0, 0);
      Continue;
    end;

    // Apply per-tab FontOptions overrides before measuring
    Canvas.Font.Assign(Font);
    if FTabs[i].FFontOptions.FontSize > 0 then
      Canvas.Font.Size := FTabs[i].FFontOptions.FontSize;
    if FTabs[i].FFontOptions.FontColor <> clNone then
      Canvas.Font.Color := FTabs[i].FFontOptions.FontColor;
    if FTabs[i].FFontOptions.FontStyles <> [] then
      Canvas.Font.Style := FTabs[i].FFontOptions.FontStyles;

    // If the active tab will be rendered bold/italic, measure with those
    // styles applied so the tab is wide enough to hold its caption
    if i = FTabIndex then
    begin
      ActiveExtra := [];
      if toActiveBold in FTabOptions then Include(ActiveExtra, fsBold);
      if toActiveItalic in FTabOptions then Include(ActiveExtra, fsItalic);
      if ActiveExtra <> [] then
        Canvas.Font.Style := Canvas.Font.Style + ActiveExtra;
    end;

    // Use cached text width and height; measure only when stale
    if FTabs[i].FTextWidth < 0 then
    begin
      FTabs[i].FTextWidth  := Canvas.TextWidth(GetDisplayCaption(FTabs[i]));
      FTabs[i].FTextHeight := Canvas.TextHeight(GetDisplayCaption(FTabs[i]));
    end;
    TxtExtent := FTabs[i].FTextWidth;

    ImgExtent := 0;
    ImgW := 0;
    ImgH := 0;

    // Check for ImageList + ImageIndex
    if Assigned(FImages) and (FTabs[i].ImageIndex >= 0) then
    begin
      ImgW := FInternalTabImages.WidthForPPI[FImagesWidth.TabsWidth, Font.PixelsPerInch];
      ImgH := FInternalTabImages.HeightForPPI[FImagesWidth.TabsWidth, Font.PixelsPerInch];
      ImgExtent := ImgW + GetScale(cImageSpacing);
    end
    // Fallback to the standalone TBitmap property
    else if Assigned(FTabs[i].FImage) and not FTabs[i].FImage.Empty then
    begin
      ImgW := FTabs[i].Image.Width;
      ImgH := FTabs[i].Image.Height;
      ImgExtent := ImgW + GetScale(cImageSpacing);
    end;
              (*  THIS CAUSES A HIGH-DPI CRASH --> move to somewhere else (SetTabSize, SetImages)
    // Expand the tab strip if this image is taller (horizontal) or wider
    // (vertical) than the current strip thickness
    MinStrip := IfThen(IsHorizontal, ImgH, ImgW) + GetScale(cContentIndent)*2;
//    if (MinStrip > GetScale(cContentIndent)*2) and (GetScale(FTabSize) < MinStrip) then
    if (MinStrip > GetScale(cContentIndent)*2) and (FTabSize < MinStrip) then
    begin
      FTabSize := MinStrip div GetScale(1);
      InvalidatePreferredSize;
      if AutoSize then AdjustSize;
    end;
             *)

    if (toShowCloseButton in FTabOptions) and FTabs[i].ShowCloseButton then
    begin
      if IsHorizontal then
        CloseExtent := GetCloseButtonImages.WidthForPPI[FImagesWidth.CloseWidth, ppi]
      else
        CloseExtent := GetCloseButtonImages.HeightForPPI[FImagesWidth.CloseWidth, ppi]
    end
    else
      CloseExtent := 0;

    TabLen := Padding + TxtExtent + ImgExtent + CloseExtent;

    if IsHorizontal then
      FTabs[i].FBoundRect := Rect(Pos, 0, Pos + TabLen, FTabSize) //GetScale(FTabSize))
    else
      FTabs[i].FBoundRect := Rect(0, Pos, FTabSize, Pos + TabLen);
      //FTabs[i].FBoundRect := Rect(0, Pos, GetScale(FTabSize), Pos + TabLen);

    Pos := Pos + TabLen - GetScale(cTabOverlap);
  end;

  // Restore control-level font after per-tab override passes
  Canvas.Font.Assign(Font);

  FTotalTabsSize := Pos + GetScale(cTabOverlap);
  UpdateScrollButtons;

  //InvalidatePreferredSize;
  //if AutoSize then AdjustSize;
  //EnableAutoSizing;
  //if AutoSize then AdjustSize;
end;

procedure TExtTabCtrl.Paint;
var
  i, SaveIdx: Integer;
  R, View, Dummy, TabRect: TRect;
  IndicatorPos: Integer;
begin
  if not HandleAllocated then Exit;

  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);

  if FTabs.Count = 0 then Exit;

  CalcLayout;

  View := TabsViewportRect;
  SaveIdx := SaveDC(Canvas.Handle);
  try
    IntersectClipRect(Canvas.Handle, View.Left, View.Top, View.Right, View.Bottom);

    // Draw all tabs in their original places
    for i := 0 to FTabs.Count - 1 do
    begin
      if not FTabs[i].Visible then Continue;
      R := FTabs[i].FBoundRect;
      if IsHorizontal then Types.OffsetRect(R, View.Left - FScrollOffset, View.Top)
      else
        Types.OffsetRect(R, View.Left, View.Top - FScrollOffset);

      if IntersectRect(Dummy, R, View) then
        DrawTab(Canvas, i, R, i = FTabIndex);
    end;

    // Draw the strip separator line across the full viewport edge
    RestoreDC(Canvas.Handle, SaveIdx);
    SaveIdx := SaveDC(Canvas.Handle);
    DrawStripLine(Canvas, View);
    IntersectClipRect(Canvas.Handle, View.Left, View.Top, View.Right, View.Bottom);

    // Draw drop indicator (where the tab will be inserted)
    if FDragging and (FDragTargetIndex <> -1) then
    begin
      Canvas.Pen.Color := clHotLight;
      Canvas.Pen.Width := 3;

      if FDragTargetIndex < FTabs.Count then
        TabRect := FTabs[FDragTargetIndex].FBoundRect
      else
      begin
        // Drop at end
        if FTabs.Count = 0 then
          TabRect := Rect(0, 0, 0, 0)
        else
          TabRect := FTabs[FTabs.Count - 1].FBoundRect;
      end;

      if IsHorizontal then
      begin
        // Calculate logical X position
        if FDragTargetIndex = FTabs.Count then
          IndicatorPos := TabRect.Right
        else
          IndicatorPos := TabRect.Left;

        // Transform Logical X to Visual X: (Pos - Scroll + Offset)
        IndicatorPos := IndicatorPos - FScrollOffset + View.Left;

        Canvas.MoveTo(IndicatorPos, View.Top);
        Canvas.LineTo(IndicatorPos, View.Bottom);
      end
      else
      begin
        // Calculate logical Y position
        if FDragTargetIndex = FTabs.Count then
          IndicatorPos := TabRect.Bottom
        else
          IndicatorPos := TabRect.Top;

        // Transform Logical Y to Visual Y
        IndicatorPos := IndicatorPos - FScrollOffset + View.Top;

        Canvas.MoveTo(View.Left, IndicatorPos);
        Canvas.LineTo(View.Right, IndicatorPos);
      end;

      Canvas.Pen.Width := 1;
    end;

    if Focused and (toShowFocusRect in FTabOptions) and
       (FTabIndex >= 0) and (FTabIndex < FTabs.Count) then
    begin
      R := FTabs[FTabIndex].FBoundRect;
      if IsHorizontal then
        Types.OffsetRect(R, View.Left - FScrollOffset, View.Top)
      else
        Types.OffsetRect(R, View.Left, View.Top - FScrollOffset);
      R := GetTabTextBounds(Canvas, R, FTabs[FTabIndex]);
      InflateRect(R, GetScale(2), GetScale(2));
      DrawFocusRect(Canvas.Handle, R);
    end;

  finally
    RestoreDC(Canvas.Handle, SaveIdx);
  end;
end;

procedure TExtTabCtrl.Resize;
begin
  inherited Resize;
  if not HandleAllocated then Exit;
  FLayoutDirty := True;
  if FUpdateCount = 0 then
    CalcLayout;
  Invalidate;
end;

// Lightweight tab-switch for use at design time and from the component tree
// Bypasses the OnTabChanging/OnTabChanged event chain so that design-time
// selection does not fire user event handlers
procedure TExtTabCtrl.SetDesignTabIndex(AValue: Integer);
begin
  if (AValue < 0) or (AValue >= FTabs.Count) then Exit;
  if FTabIndex = AValue then Exit;

  FTabIndex := AValue;
  EnsureTabVisible(FTabIndex);

  {$IFDEF LCLDesign}
  if Assigned(GlobalDesignHook) then
  begin
    GlobalDesignHook.Modified(Self);
    // Forces the Object Inspector to reload lists
    GlobalDesignHook.RefreshPropertyValues;
  end;
  {$ENDIF}

  Invalidate;
end;

procedure TExtTabCtrl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Idx: Integer;
  V, R, CR: TRect;
begin
  if csDesigning in ComponentState then
  begin
    if Button = mbLeft then
    begin
      if FBtnScrollPrev.Visible and PtInRect(FBtnScrollPrev.BoundsRect, Point(X, Y)) then
      begin
        ScrollPrev(nil);
        Exit;
      end;
      if FBtnScrollNext.Visible and PtInRect(FBtnScrollNext.BoundsRect, Point(X, Y)) then
      begin
        ScrollNext(nil);
        Exit;
      end;

      Idx := TabAtPos(X, Y);
      if Idx <> -1 then
      begin
        SetDesignTabIndex(Idx);
        {$IFDEF LCLDesign}
        if Assigned(GlobalDesignHook) then
          GlobalDesignHook.SelectOnlyThis(Tabs[Idx]);
        {$ENDIF}
      end;
    end;

    Exit; // Bypass runtime mouse tracking actions
  end;

  inherited MouseDown(Button, Shift, X, Y);

  Idx := TabAtPos(X, Y);
  FMouseDownPos := Point(X, Y);
  FMouseDownIndex := Idx;

  if Idx = -1 then Exit;

  // Close button click: only on left mouse button
  if (Button = mbLeft) and (toShowCloseButton in FTabOptions) and
     FTabs[Idx].ShowCloseButton then
  begin
    V := TabsViewportRect;
    CR := CloseButtonRect(FTabs[Idx]);
    R := FTabs[Idx].FBoundRect;

    if IsHorizontal then
      Types.OffsetRect(R, V.Left - FScrollOffset, V.Top)
    else
      Types.OffsetRect(R, V.Left, V.Top - FScrollOffset);

    Types.OffsetRect(CR, R.Left, R.Top);

    if PtInRect(CR, Point(X, Y)) then
    begin
      DeleteTab(Idx);
      Exit;
    end;
  end;

  // Middle click closes tab
  if (Button = mbMiddle) and (toCloseOnMiddleClick in FTabOptions) then
  begin
    DeleteTab(Idx);
    Exit;
  end;

  if Button = mbLeft then
  begin
    if toGetFocus in FTabOptions then SetFocus;
    TabIndex := Idx;
  end;
end;

procedure TExtTabCtrl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  i, NT: Integer;
  TabRect: TRect;
  MousePos: Integer;
  V: TRect;
  P: TPoint;
  OldHint: String;
  Msg: TLMMouse;
begin
  if csDesigning in ComponentState then
  begin
    if FHoverTab <> -1 then
    begin
      FHoverTab := -1;
      Invalidate;
    end;
    Exit;
  end;

  inherited MouseMove(Shift, X, Y);

  NT := TabAtPos(X, Y);
  V := TabsViewportRect;

  // Capture existing hint to detect change
  OldHint := Self.Hint;

  if not FDragging then
  begin
    // Update hover state and dynamic hint
    if FHoverTab <> NT then
    begin
      // Mouse left the old tab
      if (FHoverTab <> -1) and Assigned(FOnMouseLeaveTab) then
        FOnMouseLeaveTab(Self, FHoverTab);

      // Mouse entered the new tab
      if (NT <> -1) and Assigned(FOnMouseEnterTab) then
        FOnMouseEnterTab(Self, NT);

      FHoverTab := NT;

      if ShowHint then
      begin
        if (NT <> -1) then
        begin
          // Set hint to Tab.Hint or fallback to Tab.Text
          if FTabs[NT].Hint <> '' then
            Self.Hint := FTabs[NT].Hint
          else
            Self.Hint := FTabs[NT].Caption;
        end
        else
        begin
          // Button hover fallback logic
          if FBtnAdd.BoundsRect.Contains(Point(X, Y)) then
            Self.Hint := FButtonHints.AddHint
          else if FBtnScrollPrev.BoundsRect.Contains(Point(X, Y)) then
            Self.Hint := FButtonHints.ScrollPrevHint
          else if FBtnScrollNext.BoundsRect.Contains(Point(X, Y)) then
            Self.Hint := FButtonHints.ScrollNextHint
          else
            Self.Hint := '';
        end;

        if (Self.Hint <> OldHint) then
        begin
          // On macOS/Linux, we must cancel the existing timer/window first
          Application.CancelHint;

          if Self.Hint <> '' then
          begin
            // Construct the message to trigger the Hint Manager immediately
            FillChar(Msg, SizeOf(Msg), 0);
            Msg.Msg := LM_MOUSEMOVE;
            Msg.XPos := X;
            Msg.YPos := Y;
            Application.HintMouseMessage(Self, TLMessage(Msg));
          end;
        end;
      end;
      Invalidate;
    end;

    // Close button hover
    if FHoverCloseTab <> -1 then
    begin
      FHoverCloseTab := -1;
      Invalidate;  // clear the button that was previously highlighted
    end;
    if (NT <> -1) and (toShowCloseButton in FTabOptions) and
       FTabs[NT].ShowCloseButton then
    begin
      TabRect := FTabs[NT].FBoundRect;
      if IsHorizontal then
        Types.OffsetRect(TabRect, V.Left - FScrollOffset, V.Top)
      else
        Types.OffsetRect(TabRect, V.Left, V.Top - FScrollOffset);

      P := Point(X, Y);
      if PtInRect(CloseButtonRect(FTabs[NT]), Point(P.X - TabRect.Left,
        P.Y - TabRect.Top)) then
      begin
        FHoverCloseTab := NT;
        Invalidate;
      end;
    end;

    // Drag detection
    if (ssLeft in Shift) and (FMouseDownIndex <> -1) and
      (toAllowDragReorder in FTabOptions) then
    begin
      if (Abs(X - FMouseDownPos.X) > cDragThreshold) or
        (Abs(Y - FMouseDownPos.Y) > cDragThreshold) then
      begin
        FDragging := True;
        FDragIndex := FMouseDownIndex;
        FDragTargetIndex := FDragIndex;
        MouseCapture := True; // retain mouse events even when cursor leaves control
      end;
    end;
  end
  else
  begin
    // Drag reorder
    FDragTargetIndex := FTabs.Count;

    if IsHorizontal then
      MousePos := X - V.Left + FScrollOffset
    else
      MousePos := Y - V.Top + FScrollOffset;

    for i := 0 to FTabs.Count - 1 do
    begin
      if i = FDragIndex then Continue;

      TabRect := FTabs[i].FBoundRect;

      if IsHorizontal then
      begin
        // Check midpoint of the tab in logical space
        if MousePos < (TabRect.Left + TabRect.Right) div 2 then
        begin
          FDragTargetIndex := i;
          Break;
        end;
      end
      else
      begin
        // Check midpoint of the tab in logical space
        if MousePos < (TabRect.Top + TabRect.Bottom) div 2 then
        begin
          FDragTargetIndex := i;
          Break;
        end;
      end;
    end;
    Invalidate;
  end;
end;

procedure TExtTabCtrl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Allow: Boolean;
  Idx: Integer;
begin
  if FDragging then
  begin
    // Handle Reordering
    if (FDragTargetIndex <> -1) and (FDragIndex <> FDragTargetIndex) then
    begin
      if FDragTargetIndex > FDragIndex then
        Dec(FDragTargetIndex);

      Allow := True;
      if Assigned(FOnTabReordering) then
        FOnTabReordering(Self, FDragIndex, FDragTargetIndex, Allow);

      if Allow then
      begin
        Self.BeginUpdate; // Batch the internal layout changes
        try
          FTabs.Items[FDragIndex].Index := FDragTargetIndex;
          FTabIndex := FDragTargetIndex;
          FLayoutDirty := True;
        finally
          Self.EndUpdate;
        end;

        if Assigned(FOnTabReordered) then
          FOnTabReordered(Self, FDragIndex, FDragTargetIndex);
      end;
    end;

    FDragging := False;
    FDragIndex := -1;
    FDragTargetIndex := -1;
    FHoverTab := -1;
    FHoverCloseTab := -1;
    MouseCapture := False; // release capture acquired at drag start
    Invalidate;
  end
  else if (Button = mbLeft) then
  begin
    // Handle Single Click Event
    Idx := TabAtPos(X, Y);
    if (Idx <> -1) and (Idx = FMouseDownIndex) then
    begin
      if Assigned(FOnTabClick) then
        FOnTabClick(Self, Idx);
    end;
  end;

  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TExtTabCtrl.MouseLeave;
begin
  if (FHoverTab <> -1) and Assigned(FOnMouseLeaveTab) then
    FOnMouseLeaveTab(Self, FHoverTab);

  FHoverTab := -1;
  FHoverCloseTab := -1;
  Invalidate;
end;

function TExtTabCtrl.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
var
  Candidate: Integer;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if Result then Exit; // parent already handled it
  if FTabs.Count = 0 then Exit;

  if WheelDelta < 0 then
    Candidate := NextVisibleTab(FTabIndex)
  else
    Candidate := PrevVisibleTab(FTabIndex);

  if Candidate <> -1 then
    TabIndex := Candidate;

  Result := True;
end;

procedure TExtTabCtrl.DoEnter;
begin
  inherited DoEnter;
  Invalidate; // repaint to show focus indicator
  if Assigned(FOnGetFocus) then FOnGetFocus(Self);
end;

procedure TExtTabCtrl.DoExit;
begin
  inherited DoExit;
  Invalidate; // repaint to hide focus indicator
  if Assigned(FOnLostFocus) then FOnLostFocus(Self);
end;

procedure TExtTabCtrl.WMLMGetDlgCode(var Message: TLMessage);
begin
  // Only claim arrow keys when the control is allowed to hold focus
  if toGetFocus in FTabOptions then
    Message.Result := DLGC_WANTARROWS
  else
    Message.Result := 0;
end;

procedure TExtTabCtrl.CMDesignHitTest(var Message: TLMessage);
var
  PCoords: TSmallPoint;
  Pt: TPoint;
begin
  // Extract coordinates from LParam as a TSmallPoint (Lazarus documentation)
  // https://wiki.freepascal.org/Extending_the_IDE#Disabling_the_designer_mouse_handler
  PCoords := TSmallPoint(LongInt(Message.LParam));
  Pt := Point(PCoords.x, PCoords.y);

  // Return Message.Result := 1 if clicking on a visible scroll button or a valid tab
  if (FBtnScrollPrev.Visible and PtInRect(FBtnScrollPrev.BoundsRect, Pt)) or
     (FBtnScrollNext.Visible and PtInRect(FBtnScrollNext.BoundsRect, Pt)) or
     (TabAtPos(Pt.X, Pt.Y) <> -1) then
  begin
    Message.Result := 1; // Overrides the designer mouse handler for this click
  end;

  //inherited;
end;

procedure TExtTabCtrl.DblClick;
var
  Idx: Integer;
begin
  inherited DblClick;
  // Use coordinates from MouseDown to identify which tab was double-clicked
  Idx := TabAtPos(FMouseDownPos.X, FMouseDownPos.Y);
  if (Idx <> -1) and Assigned(FOnTabDblClick) then
    FOnTabDblClick(Self, Idx);
end;

procedure TExtTabCtrl.KeyDown(var Key: Word; Shift: TShiftState);
var
  Candidate: Integer;
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_LEFT, VK_UP:
    begin
      Candidate := PrevVisibleTab(FTabIndex);
      if Candidate <> -1 then TabIndex := Candidate;
    end;
    VK_RIGHT, VK_DOWN:
    begin
      Candidate := NextVisibleTab(FTabIndex);
      if Candidate <> -1 then TabIndex := Candidate;
    end;
    VK_HOME:
    begin
      Candidate := NextVisibleTab(-1);
      if Candidate <> -1 then TabIndex := Candidate;
    end;
    VK_END:
    begin
      Candidate := PrevVisibleTab(FTabs.Count);
      if Candidate <> -1 then TabIndex := Candidate;
    end;
  end;
end;

procedure TExtTabCtrl.CreateWnd;
begin
  inherited CreateWnd;
  AnchorButtons;
end;

procedure TExtTabCtrl.Loaded;
begin
  inherited Loaded;

  if Assigned(FBtnAdd) then
  begin
    FBtnAdd.ShowHint := Self.ShowHint;
    FBtnAdd.Hint := FButtonHints.AddHint;
  end;

  if Assigned(FBtnScrollPrev) then
  begin
    FBtnScrollPrev.ShowHint := ShowHint;
    FBtnScrollPrev.Hint := FButtonHints.ScrollPrevHint;
  end;

  if Assigned(FBtnScrollNext) then
  begin
    FBtnScrollNext.ShowHint := ShowHint;
    FBtnScrollNext.Hint := FButtonHints.ScrollNextHint;
  end;

  FButtonImages.Save;
  UpdateBtnImages;
end;

procedure TExtTabCtrl.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FImages) then
  begin
    FInternalTabImages.Clear;
    FImages := nil;
  end;
end;

procedure TExtTabCtrl.CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer; WithImplicitConstraints: Boolean);
begin
  // Clamp the control to exactly the tab-strip thickness
  // Return 0 for the free dimension so the LCL leaves it alone
  if IsHorizontal then
  begin
    PreferredWidth := 0;            // user controls width freely
    PreferredHeight := FTabSize;    // height = one tab row
  end
  else
  begin
    PreferredWidth := FTabSize;     // width = one tab column
    PreferredHeight := 0;           // user controls height freely
  end;
end;

procedure TExtTabCtrl.CMShowHintChanged(var Message: TLMessage);
begin
  inherited;

  if Assigned(FBtnAdd) then
    FBtnAdd.ShowHint := Self.ShowHint;
  if Assigned(FBtnScrollPrev) then
    FBtnScrollPrev.ShowHint := Self.ShowHint;
  if Assigned(FBtnScrollNext) then
    FBtnScrollNext.ShowHint := Self.ShowHint;
end;

procedure TExtTabCtrl.CMFontChanged(var Message: TLMessage);
begin
  inherited;
  UpdateImages;
end;

procedure TExtTabCtrl.UpdateImages;
var
  i: Integer;
begin
  // Invalidate all cached text metrics, the new font makes them stale
  for i := 0 to FTabs.Count - 1 do
  begin
    FTabs[i].FTextWidth := -1;
    FTabs[i].FTextHeight := -1;
  end;

  // Reset image caches: tab images and button glyphs are sized for the old DPI
  InvalidateTabImageCaches;

  // Reload ButtonImages from FImages at the new DPI if indices are set
  if Assigned(FImages) then
    ButtonImagesChanged(Self);

  InvalidateLayout;
end;

{ Assigns the correct image list and image indices to the buttons. }
procedure TExtTabCtrl.UpdateBtnImages;
begin
  PrepareInternalBtnImages(GetRotationForPosition);
  PrepareInternalTabImages(GetRotationForPosition);

  // Default: use the InternalBtnImages
  FBtnAdd.Images := FInternalBtnImages;
  FBtnScrollPrev.Images := FInternalBtnImages;
  FBtnScrollNext.Images := FInternalBtnImages;
  FBtnAdd.ImageIndex := cAddIndex;
  FBtnScrollPrev.ImageIndex := cPrevIndex;
  FBtnScrollNext.ImageIndex := cNextIndex;
  FBtnAdd.ImageWidth := 0;
  FBtnScrollPrev.ImageWidth := 0;
  FBtnScrollNext.ImageWidth := 0;

  // Use external images instead if available and selected
  if FImages <> nil then
  begin
    FButtonImages.Restore;
    if FButtonImages.AddIndex > -1 then
    begin
      FBtnAdd.Images := FInternalTabImages;
      FBtnAdd.ImageIndex := FButtonImages.AddIndex;
      FBtnAdd.ImageWidth := ImagesWidth.AddWidth;
    end;
    if FButtonImages.PrevIndex > -1 then
    begin
      FBtnScrollPrev.Images := FInternalTabImages;
      FBtnScrollPrev.ImageIndex := FButtonImages.PrevIndex;
      FBtnScrollPrev.ImageWidth := ImagesWidth.PrevWidth;
    end;
    if FButtonImages.NextIndex > -1 then
    begin
      FBtnScrollNext.Images := FInternalTabImages;
      FBtnScrollNext.ImageIndex := FButtonImages.NextIndex;
      FBtnScrollNext.ImageWidth := ImagesWidth.NextWidth;
    end;
  end;
end;

procedure TExtTabCtrl.DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
  const AXProportion, AYProportion: Double);
begin
  inherited;
  if AMode in [lapAutoAdjustWithoutHorizontalScrolling, lapAutoAdjustForDPI] then
  begin
    if IsStoredTabSize then
      FTabSize := round(FTabSize * AXProportion);
    AnchorButtons;
  end;
end;

procedure TExtTabCtrl.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TExtTabCtrl.EndUpdate;
begin
  if FUpdateCount > 0 then Dec(FUpdateCount);
  if FUpdateCount = 0 then InvalidateLayout;
end;

procedure TExtTabCtrl.InvalidateLayout;
begin
  FLayoutDirty := True;
  if (FUpdateCount = 0) and HandleAllocated then Invalidate;
end;

function TExtTabCtrl.AddTab(const ACaption: String; AData: TObject): TExtTab;
var
  Allow: Boolean;
  Cap: String;
  Data: TObject;
  NewIndex: Integer;
begin
  Result := nil;
  if FInternalChange > 0 then Exit;

  Cap := ACaption;
  Data := AData;
  Allow := True;

  if not FImportActive then
  begin
    if Assigned(FOnTabCreating) then
      FOnTabCreating(Self, Cap, Data, Allow);
    // User callback may have destroyed or mutated us --> revalidate
    if csDestroying in ComponentState then Exit;
  end;

  if not Allow then Exit;

  NewIndex := -1;
  BeginInternalChange;
  try
    Result := FTabs.Add(Cap);
    Result.Data := Data;

    // Defer layout: if inside BeginUpdate, CalcLayout will fire in EndUpdate
    InvalidateLayout;

    if toActivateNewTab in FTabOptions then
      NewIndex := Result.Index;
  finally
    EndInternalChange;
  end;

  if NewIndex >= 0 then
    SetTabIndex(NewIndex);

  if not FImportActive then
  begin
    if Assigned(FOnTabCreated) then
    begin
      FOnTabCreated(Self);
      if csDestroying in ComponentState then Exit;
    end;
  end;
end;

procedure TExtTabCtrl.DeleteTab(Index: Integer);
var
  Allow: Boolean;
  OldIndex, NewIndex: Integer;
begin
  if (Index < 0) or (Index >= FTabs.Count) then Exit;
  if FInternalChange > 0 then Exit;

  Allow := True;
  if Assigned(FOnTabDeleting) then
  begin
    FOnTabDeleting(Self, Index, Allow);
    // User callback may have mutated collection: revalidate index
    if csDestroying in ComponentState then Exit;
    if (Index < 0) or (Index >= FTabs.Count) then Exit;
  end;
  if not Allow then Exit;

  OldIndex := FTabIndex;
  NewIndex := FTabIndex;

  if Index = FTabIndex then
  begin
    // Deleting the active tab: find a replacement
    NewIndex := NextVisibleTab(Index);
    if NewIndex = -1 then NewIndex := PrevVisibleTab(Index);
    // Adjust for the upcoming index shift caused by the deletion itself
    if (NewIndex > Index) then Dec(NewIndex);

    // Fire OnTabChanging before the structural change
    Allow := True;
    if Assigned(FOnTabChanging) then
    begin
      FOnTabChanging(Self, OldIndex, NewIndex, Allow);
      if csDestroying in ComponentState then Exit;
      if (Index < 0) or (Index >= FTabs.Count) then Exit;
    end;
    if not Allow then Exit;
  end
  else if Index < FTabIndex then
    Dec(NewIndex); // shift active index down

  BeginInternalChange;
  try
    FTabs.Delete(Index);

    if ((toActiveBold in FTabOptions) or (toActiveItalic in FTabOptions)) and
       (NewIndex >= 0) and (NewIndex < FTabs.Count) then
    begin
      FTabs[NewIndex].FTextWidth := -1;
      FTabs[NewIndex].FTextHeight := -1;
    end;

    FTabIndex := NewIndex;

    // Cancel any in-progress drag: indexes are now stale
    CancelDrag;

    // Reset hover state: indices may now be stale
    FHoverTab := -1;
    FHoverCloseTab := -1;

    NormalizeState;
  finally
    EndInternalChange;
  end;

  // Fire events after the structural change is complete
  if Assigned(FOnTabDeleted) then
  begin
    FOnTabDeleted(Self);
    if csDestroying in ComponentState then Exit;
  end;

  // Fire OnTabChanged if the active tab changed
  if ((NewIndex <> OldIndex) or (Index = OldIndex)) and Assigned(FOnTabChanged) then
    FOnTabChanged(Self, FTabIndex);

  InvalidateLayout;
end;

procedure TExtTabCtrl.ImportFromStrings(Source: TStrings; ClearExisting: Boolean = True);
var
  i: Integer;
  NewTab: TExtTab;
begin
  if not Assigned(Source) then Exit;

  Self.BeginUpdate;
  FImportActive := True;
  try
    if ClearExisting then
      FTabs.Clear;

    for i := 0 to Source.Count - 1 do
    begin
      NewTab := AddTab(Source.Strings[i]);
      if not Assigned(NewTab) then Continue;

      if Assigned(Source.Objects[i]) then
      begin
        if Assigned(FOnImportTab) then
          FOnImportTab(Self, NewTab, Source.Objects[i])
        else
          NewTab.Data := Source.Objects[i];
      end;
    end;

    NormalizeState;
    InvalidateLayout;
  finally
    FImportActive := False;
    Self.EndUpdate;
  end;

  // Fire a single OnTabCreated to signal import is complete
  if Assigned(FOnTabCreated) then
    FOnTabCreated(Self);
end;

procedure TExtTabCtrl.PrepareInternalBtnImages(ARotation: Integer);
const
  DARKMODE_COLOR = clSilver; //clWindowText;
var
  img100, img150, img200: TCustomBitmap;
begin
  FInternalBtnImages.Clear;
  FInternalBtnImages.RegisterResolutions([16, 24, 32]);

  img100 := TPortableNetworkGraphic.Create;
  img150 := TPortableNetworkGraphic.Create;
  img200 := TPortableNetworkGraphic.Create;
  try
    img100.LoadFromResourceName(HInstance, 'tab_prev');
    img150.LoadFromResourceName(HInstance, 'tab_prev_150');
    img200.LoadFromResourceName(HInstance, 'tab_prev_200');
    if IsVertical then
    begin
      RotateImage(img100, 270);
      RotateImage(img150, 270);
      RotateImage(img200, 270);
    end;
    if IsDarkMode then
    begin
      RecolorImage(img100, DARKMODE_COLOR);
      RecolorImage(img150, DARKMODE_COLOR);
      RecolorImage(img200, DARKMODE_COLOR);
    end;
    FInternalBtnImages.AddMultipleResolutions([img100, img150, img200]);

    img100.LoadFromResourceName(HInstance, 'tab_next');
    img150.LoadFromResourceName(HInstance, 'tab_next_150');
    img200.LoadFromResourceName(HInstance, 'tab_next_200');
    if IsVertical then
    begin
      RotateImage(img100, 270);
      RotateImage(img150, 270);
      RotateImage(img200, 270);
    end;
    if IsDarkMode then
    begin
      RecolorImage(img100, DARKMODE_COLOR);
      RecolorImage(img150, DARKMODE_COLOR);
      RecolorImage(img200, DARKMODE_COLOR);
    end;
    FInternalBtnImages.AddMultipleResolutions([img100, img150, img200]);

    img100.LoadFromResourceName(HInstance, 'tab_new');
    img150.LoadFromResourceName(HInstance, 'tab_new_150');
    img200.LoadFromResourceName(HInstance, 'tab_new_200');
    if (toRotateAddImage in FTabOptions) and IsVertical then
    begin
      RotateImage(img100, ARotation);
      RotateImage(img150, ARotation);
      RotateImage(img200, ARotation);
    end;
    if IsDarkMode then
    begin
      RecolorImage(img100, DARKMODE_COLOR);
      RecolorImage(img150, DARKMODE_COLOR);
      RecolorImage(img200, DARKMODE_COLOR);
    end;
    FInternalBtnImages.AddMultipleResolutions([img100, img150, img200]);

    img100.LoadFromResourceName(HInstance, 'cross');
    img150.LoadFromResourceName(HInstance, 'cross_150');
    img200.LoadFromResourceName(HInstance, 'cross_200');
    if IsDarkMode then
    begin
      RecolorImage(img100, DARKMODE_COLOR);
      RecolorImage(img150, DARKMODE_COLOR);
      RecolorImage(img200, DARKMODE_COLOR);
    end;
    // cross is symmetrical --> no need for rotation
    FInternalBtnImages.AddMultipleResolutions([img100, img150, img200]);
  finally
    img200.Free;
    img150.Free;
    img100.Free;
  end;
end;

procedure TExtTabCtrl.PrepareInternalTabImages(ARotation: Integer);
var
  bmp: array of TCustomBitmap = nil;
  widths: Array of Integer = nil;
  i, j: Integer;
  R: TCustomImageListResolution;
begin
  FInternalTabImages.Clear;

  if FImages = nil then
    exit;

  if IsVertical then
  begin
    // Vertical layout
    SetLength(bmp, FImages.ResolutionCount);
    SetLength(widths, FImages.ResolutionCount);
    for i := 0 to FImages.ResolutionCount-1 do
    begin
      bmp[i] := TBitmap.Create;
      widths[i] := FImages.ResolutionByIndex[i].Width;
    end;
    FInternalTabImages.RegisterResolutions(widths);
    try
      for j := 0 to FImages.Count-1 do
      begin
        for i := 0 to High(bmp) do
        begin
          R := FImages.ResolutionByIndex[i];
          R.GetBitmap(j, bmp[i]);
          if (j = FButtonImages.AddIndex) then
          begin
            if (toRotateAddImage in FTabOptions) then
              RotateImage(bmp[i], ARotation);
          end
          else
          if (j = FButtonImages.PrevIndex) or (j = FButtonImages.NextIndex) then
            RotateImage(bmp[i], 270)
          else
          if toRotateTabImages in FTabOptions then
            RotateImage(bmp[i], ARotation);
        end;
        FInternalTabImages.AddMultipleResolutions(bmp);
      end;
    finally
      for i := 0 to High(bmp) do
        bmp[i].Free;
      bmp := nil;
      widths := nil;
    end;
  end else
    // Horizontal layout
    FInternalTabImages.Assign(FImages);
end;

constructor TExtTabCtrl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csClickEvents, csDoubleClicks, csOpaque];
  TabStop := True;

  // Provide a proper initial size when dropped onto a form by a  single click
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);

  ShowHint := True;
  DoubleBuffered := True;
  FTabs := TExtTabs.Create(Self);
  FTabIndex := -1;
  FTabStyle := tsFlat;
  FTabPosition := tpTop;
  FTabSize := Scale96ToFont(cDefaultTabSize);
  FTabOptions := [toActivateNewTab, toShowCloseButton, toShowAddButton,
                  toCloseOnMiddleClick, toAllowDragReorder, toGetFocus,
                  toShowFocusRect];
  FMinCaptionLen := 5;
  FMaxCaptionLen := 25;

  FInternalTabImages := TImageList.Create(self);
  FInternalTabImages.Scaled := true;

  FInternalBtnImages := TImageList.Create(Self);
  FInternalBtnImages.Scaled := True;
  FInternalBtnImages.Width := 16;
  FInternalBtnImages.Height := 16;
  FInternalBtnImages.RegisterResolutions([16, 24, 32]);
  PrepareInternalBtnImages(0);

  FButtonImages := TButtonImages.Create(Self);
  FButtonImages.OnChange := @ButtonImagesChanged;
  FButtonHints := TButtonHints.Create;
  FImagesWidth := TImagesWidth.Create;
  FImagesWidth.OnChange := @ImagesWidthChanged;

  FBtnAdd := TSpeedButton.Create(Self);
  FBtnAdd.Name := 'BtnAdd';  // just for debugging
  FBtnAdd.Parent := Self;
  FBtnAdd.Flat := True;
  FBtnAdd.ParentShowHint := False;
  FBtnAdd.Images := FInternalBtnImages;
  FBtnAdd.ImageIndex := cAddIndex;
  FBtnAdd.OnClick := @AddBtnClick;
  FBtnAdd.BringToFront;

  FBtnScrollPrev := TSpeedButton.Create(Self);
  FBtnScrollPrev.Name := 'BtnScrollPrev';
  FBtnScrollPrev.Parent := Self;
  FBtnScrollPrev.Flat := True;
  FBtnScrollPrev.ParentShowHint := False;
  FBtnScrollPrev.ShowHint := ShowHint;
  FBtnScrollPrev.Images := FInternalBtnImages;
  FBtnScrollPrev.ImageIndex := cPrevIndex;
  FBtnScrollPrev.OnClick := @ScrollPrev;

  FBtnScrollNext := TSpeedButton.Create(Self);
  FBtnScrollNext.Name := 'BtnScrollNext';
  FBtnScrollNext.Parent := Self;
  FBtnScrollNext.Flat := True;
  FBtnScrollNext.ParentShowHint := False;
  FBtnScrollNext.ShowHint := ShowHint;
  FBtnScrollNext.Images := FInternalBtnImages;
  FBtnScrollNext.ImageIndex := cNextIndex;
  FBtnScrollNext.OnClick := @ScrollNext;

  FMouseDownIndex := -1;
  FDragIndex := -1;
  FDragTargetIndex := -1;
  FHoverTab := -1;
  FHoverCloseTab := -1;
  FAddTabCounter := 0;
  FImportActive := False;
  FInternalChange := 0;
end;

destructor TExtTabCtrl.Destroy;
begin
  // Cancel any pending hint window that may reference our handle
  Application.CancelHint;
  ShowHint := False;

  // Explicitly free child buttons before their glyph bitmaps are freed
  FreeAndNil(FBtnAdd);
  FreeAndNil(FBtnScrollPrev);
  FreeAndNil(FBtnScrollNext);

  FreeAndNil(FInternalBtnImages);
  FreeAndNil(FInternalTabImages);
  FreeAndNil(FButtonImages);
  FreeAndNil(FButtonHints);
  FreeAndNil(FImagesWidth);

  FreeAndNil(FTabs);

  inherited Destroy;
end;

initialization
  {$I ExtTabCtrl.lrs}

end.
