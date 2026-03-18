unit HtmlHelpViewer;

interface

uses
  Classes, HelpIntfs;

implementation

uses
  SysUtils, Windows, HtmlHelp, WinHelpViewer, Forms;

resourcestring
  hNoKeyword = 'No help keyword specified.';


var
  ViewerName: String;

type

  THtmlHelpFinder = record
    HelpTitle: PChar;
    HelpWnd: HWnd;
    DialogWnd: HWnd;
    FoundMainWindow: Boolean;
    FoundDialog: Boolean;
  end;

  TWHCommandType = (twhContext, twhCommand, twhContents, twhQuit);

{ THtmlHelpViewer.
  implements the interfaces  ---
  ICustomHelpViewer (required of all Help Viewers),
  IExtendedHelpViewer (Topic and Context),
  ISpecialWinHelpViewer (Winhelp-specific messages }
  THtmlHelpViewer = class(TInterfacedObject, ICustomHelpViewer, IExtendedHelpViewer,
    ISpecialWinHelpViewer, IHelpSelector)
  private
    FViewerID: Integer;
    function MakeHtmlHelpHappy: Boolean;
  public
    FHelpManager: IHelpManager;
    constructor Create;
    destructor Destroy; override;
    function HelpFile(const Name: String): String;
    procedure InternalShutDown;
    { ICustomHelpViewer }
    function GetViewerName: String;
    function UnderstandsKeyword(const HelpString: String): Integer;
    function GetHelpStrings(const HelpString: String): TStringList;
    function CanShowTableOfContents: Boolean;
    procedure ShowTableOfContents;
    procedure ShowHelp(const HelpString: String);
    procedure NotifyID(const ViewerID: Integer);
    procedure SoftShutDown;
    procedure ShutDown;
    { IExtendedHelpViewer }
    function UnderstandsTopic(const Topic: String): Boolean;
    procedure DisplayTopic(const Topic: String);
    function UnderstandsContext(const ContextID: Integer; const HelpFileName: String): Boolean;
    procedure DisplayHelpByContext(const ContextID: Integer; const HelpFileName: String);
    { ISpecialWinHelpViewer }
    function CallWinHelp(Handle: LongInt; const HelpFileName: String; Command: Word; Data: LongInt): Boolean;
    property ViewerID: Integer read FViewerID;
    property HelpManager: IHelpManager read FHelpManager write FHelpManager;
    { IHelpSelector }
    function SelectKeyword(Keywords: TStrings): Integer;
    function TableOfContents(Contents: TStrings): Integer;
  end;


{ global instance of THtmlHelpViewer which HelpIntfs can talk to. }
var
  HelpViewer: THtmlHelpViewer;
  HelpViewerIntf: ICustomHelpViewer;
  HelpSelectorIntf: IHelpSelector;

{ THtmlHelpViewer }
function THtmlHelpViewer.HelpFile(const Name: String): String;
var
  FileName: String;
begin
  Result := '';
  if (Name = '') then FileName := Application.CurrentHelpFile
  else FileName := Name;
  Result := FileName;
end;

{ InternalShut Down is called from unit finalization if the unit is exiting
  and the Help Manager needs to be informed. }

procedure THtmlHelpViewer.InternalShutDown;
begin
  SoftShutDown;
  if Assigned(FHelpManager) then
  begin
    HelpManager.Release(ViewerID);
    HelpManager := nil;
  end;
end;

{---------------------------------------------------------------------------}
{ THtmlHelpViewer - ICustomHelpViewer }

{ GetViewerName returns a string that the Help Manager can use to identify
  this Viewer in a UI element asking users to choose among Viewers. }
function THtmlHelpViewer.GetViewerName: String;
begin
  Result := ViewerName;
end;

{ UnderstandsKeyword is a querying function that the Help Manager calls to
  determine if the Viewer provide helps on a particular keyword string. }
function THtmlHelpViewer.UnderstandsKeyword(const HelpString: String): Integer;
begin
  Result := 1;
end;

{ GetHelpStrings is used by the Help Manager to display a list of keyword
  matches from which an application's user can select one. It assumes
  that the String List is properly allocated, so this function should
  never return nil. }
  
function THtmlHelpViewer.GetHelpStrings(const HelpString: String): TStringList;
begin
  Result := TStringList.Create;
  Result.Add(HelpString);
end;

{ CanShowTableOfContents is a querying function that the Help Manager
  calls to determine if the Viewer supports tables of contents. WinHelp
  and HyperHelp both do. }

function THtmlHelpViewer.CanShowTableOfContents: Boolean;
begin
  Result := True;
end;

{ ShowTableOfContents is a command function that the Help Manager uses
  to direct the Viewer to display a table of contents. It is never
  called without being preceded by a call to CanShowTableOfContents. }
procedure THtmlHelpViewer.ShowTableOfContents;
var
  FileName: String;
begin
  if MakeHtmlHelpHappy then begin
    FileName := HelpFile(HelpManager.GetHelpFile);
    if FileName <> '' then
      HHelp(HelpManager.GetHandle, PChar(FileName), HH_DISPLAY_TOC, 0);
  end;
end;

{ ShowHelp is the function that the Help Manager calls to request that
  a Help Viewer display help for a given keyword. For WinHelp, this is
  done via a complex WinHelp macro. The macro syntax is slightly different
  for HyperHelp than it is for WinHelp; thus this function is IFDEFed
  in its entirety. }

procedure THtmlHelpViewer.ShowHelp(const HelpString: String);
begin
  if HelpString = '' then raise EHelpSystemException.CreateRes(@hNoKeyword)
  else DisplayTopic(HelpString);
end;

{ NotifyID is called by the Help Manager after a successful registration
  to provide the Help Viewer with a cookie which uniquely identifies the
  Viewer to the Manager, and can be used in communications between the two. }

procedure THtmlHelpViewer.NotifyID(const ViewerID: Integer);
begin
  FViewerID := ViewerID;
end;

{ SoftShutDown is called by the help manager to ask the viewer to
  terminate any externally spawned subsystem without shutting itself down. }
procedure THtmlHelpViewer.SoftShutDown;
begin
  HHelp(0, PChar(''), HH_CLOSE_ALL, 0);
end;

procedure THtmlHelpViewer.ShutDown;
begin
  SoftShutDown;
  if Assigned(FHelpManager) then
    HelpManager := nil;
end;

{-----------------------------------------------------------------------------}
{ THtmlHelpViewer --- IExtendedHelpViewer }

{ UnderstandsTopic is called by the Help Manager to ask if the Viewer
  is capable of displaying a topic-based help query for a given topic.
  Its default behavior, like all default behaviors in this file, is
  opposite for Windows than it is for linux. }

function THtmlHelpViewer.UnderstandsTopic(const Topic: String): Boolean;
begin
  Result := True;
end;

{ DisplayTopic is called by the Help Manager if a Help Viewer claims
  in its response to UnderstandsTopic to be able to provide Topic-based
  help for a particular keyword. }

procedure THtmlHelpViewer.DisplayTopic(const Topic: String);
var
  FileName: String;
begin
  FileName := HelpFile('');
  if MakeHtmlHelpHappy then begin
    HHelp(HelpManager.GetHandle, PChar(FileName), HH_DISPLAY_TOPIC, LongInt(PChar(Topic + '.htm')));
  end;
end;

{ UnderstandsContext is a querying function called by the Help Manager
  to determine if an Extended Help Viewer is capable of providing
  help for a particular context-ID. Like all querying functions in
  this file, the default behavior is opposite --- YES for windows,
  NO for linux --- and the Tester is asked, if available. }
function THtmlHelpViewer.UnderstandsContext(const ContextID: Integer;
  const HelpFileName: String): Boolean;
begin
  Result := True;
end;

{ DisplayHelpByContext is used by the Help Manager to request that a 
  Help Viewer display help for a particular Context-ID. It is only
  invoked after a successful call to CanShowContext. }
procedure THtmlHelpViewer.DisplayHelpByContext(const ContextID: Integer; const HelpFileName: String);
var
   FileName: String;
begin
  FileName := HelpFile(HelpFileName);
  if MakeHtmlHelpHappy then begin
    HHelp(HelpManager.GetHandle, PChar(FileName), HH_HELP_CONTEXT, ContextID);
  end;
end;

{----------------------------------------------------------------------------}
{ THtmlHelpViewer --- ISpecialWinHelpViewer }

{ CallWinHelp is called by the Help Manager when it recieves WinHelp-style
  help requests from external systems and is not able to deconstruct them
  into something meaningful for non-winhelp specific viewers. These
  get forwarded on to WinHelp in *all* circumstances. HyperHelp may
  not support some of these functions, and will issue an error complaint
  in that case. }
function THtmlHelpViewer.CallWinHelp(Handle: LongInt; const HelpFileName: String;
  Command: Word; Data: LongInt): Boolean;
begin
  Result := False;
  if MakeHtmlHelpHappy then begin
    Result := HHelp(Handle, PChar(HelpFile(HelpFileName)), Command, Data)<>0;
  end;
end;

{----------------------------------------------------------------------------}
{ THtmlHelpViewer internal support methods }

{ These functions will not work with HyperHelp, because HyperHelp does not
  run under the WINE subsystem. These functions may only be invoked from code
  intended to run under Windows.}
function WindowEnumerationTester(wnd: HWnd; var Info: THtmlHelpFinder): Bool; stdcall;
var
  Buf: array [Byte] of Char;
begin
  GetClassName(wnd, Buf, sizeof(Buf));
  if (StrIComp(Buf, 'MS_WINHELP') = 0) then begin
    Info.HelpWnd := Wnd;
    Info.FoundMainWindow := True;
  end;
  Result := not Info.FoundMainWindow;
end;

function DialogEnumerationTester(wnd: HWnd; var Info: THtmlHelpFinder): Bool; stdcall;
var
  Buf: Array [Byte] of Char;
begin
  GetClassName(wnd, Buf, sizeof(Buf));
  if (StrComp(Buf, '#32770') = 0) then
  begin
    Info.FoundDialog := True;
    Info.DialogWnd := Wnd;
  end;
  Result := not Info.FoundDialog;
end;

function FindHtmlHelp: THtmlHelpFinder;
begin
  Result.FoundMainWindow := False;
  Result.FoundDialog := False;
  Result.HelpWnd := 0;
  EnumWindows(@WindowEnumerationTester, Integer(@Result));
end;

function THtmlHelpViewer.MakeHtmlHelpHappy: Boolean;
begin
  Result := True;
end;

{-----------------------------------------------------------------------------}
{ THtmlHelpViewer --- IHelpSelector }

function THtmlHelpViewer.SelectKeyword(Keywords: TStrings): Integer;
begin
  if Keywords.Count=1 then Result := 0
  else Result := -1;
end;

function THtmlHelpViewer.TableOfContents(Contents: TStrings): Integer;
begin
  if Contents.Count=1 then Result := 0
  else Result := -1;
end;

constructor THtmlHelpViewer.Create;
begin
  inherited Create;
  HelpViewerIntf := Self;
  HelpSelectorIntf := Self;
end;

destructor THtmlHelpViewer.Destroy;
begin
  HelpViewer := nil;
  HelpSelectorIntf := nil;
  inherited Destroy;
end;

type
  { WinHelpTester for off all functions WinHelpViewer }
  TDummyWinHelpTester = class(TInterfacedObject, IWinHelpTester)
  public
    function CanShowALink(const ALink, FileName: String): Boolean;
    function CanShowTopic(const Topic, FileName: String): Boolean;
    function CanShowContext(const Context: Integer; const FileName: String): Boolean;
    function GetHelpStrings(const ALink: String): TStringList;
    function GetHelpPath: String;
    function GetDefaultHelpFile: String;
  end;

{ TDummyWinHelpTester }

function TDummyWinHelpTester.CanShowALink(const ALink, FileName: String): Boolean;
begin
  Result := False;
end;

function TDummyWinHelpTester.CanShowContext(const Context: Integer; const FileName: String): Boolean;
begin
  Result := False;
end;

function TDummyWinHelpTester.CanShowTopic(const Topic, FileName: String): Boolean;
begin
  Result := False;
end;

function TDummyWinHelpTester.GetDefaultHelpFile: String;
begin
  Result := '';
end;

function TDummyWinHelpTester.GetHelpPath: String;
begin
  Result := '';
end;

function TDummyWinHelpTester.GetHelpStrings( const ALink: String): TStringList;
begin
  Result := nil;
end;

var
  DummyWinHelpTester: TDummyWinHelpTester;
  HelpSystem: IHelpSystem;

initialization
  DummyWinHelpTester := TDummyWinHelpTester.Create;
  WinHelpTester := DummyWinHelpTester;
  HelpViewer := THtmlHelpViewer.Create;
  HelpIntfs.RegisterViewer(HelpViewerIntf, HelpViewer.FHelpManager);
  GetHelpSystem(HelpSystem);
  HelpSystem.AssignHelpSelector(HelpSelectorIntf);
  HelpSystem := nil;

finalization
  WinHelpTester := nil;
  if Assigned(HelpViewer.FHelpManager) then HelpViewer.InternalShutDown;
  HelpViewerIntf := nil;

end.
