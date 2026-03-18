unit HtmlHelp;

interface

uses
  Windows, SysUtils;

var
  // procedural type representing HtmlHelpA from hhctrl.ocx
  HHelp: function(hwndCaller: HWND; pszFile: PChar; uCommand: UInt; dwData: DWORD): HWND; stdcall;


type
  PHHPopup = ^THHPopup;
  tagHH_POPUP = packed record
    cbStruct:      Integer;     // sizeof this structure
    hinst:         HInst;       // instance handle for string resource
    idString:      Cardinal;    // string resource id, or text id if pszFile is specified in HtmlHelp call
    pszText:       PChar;       // used if idString is zero
    pt:            TPoint;      // top center of popup window
    clrForeground: COLORREF;    // use -1 for default
    clrBackground: COLORREF;    // use -1 for default
    rcMargins:     TRect;       // amount of space between edges of window and text, -1 for each member to ignore
    pszFont:       PChar;       // facename, point size, char set, BOLD ITALIC UNDERLINE
  end;
  HH_POPUP = tagHH_POPUP;
  THHPopup = tagHH_POPUP;

const
  HH_FTS_DEFAULT_PROXIMITY = (-1);

type
  PHHAkLink = ^THHAkLink;
  // tagHH_AKLINK, HH_ALINK_LOOKUP
  tagHH_AKLINK = packed record
    cbStruct: Integer;  // Sizeof of the structure in bytes.
    fReserved: Bool;    // This parameter must be set to FALSE.
    pszKeywords: PChar; // One or more ALink names or KLink keywords to look up. Multiple entries are delimited by a semicolon.
    pszUrl: PChar;      // Topic file to navigate to if the lookup fails. pszURL refers to a valid topic within the specified compiled help (.chm) file and does not support Internet protocols that point to an HTML file.
    pszMsgText: PChar;  // Text to display in a message box if the lookup fails and fIndexOnFail is FALSE and pszURL is NULL.
    pszMsgTitle: PChar; // Caption of the message box in which the pszMsgText parameter appears.
    pszWindow: PChar;   // Name of the window type in which to display one of the following:
    fIndexOnFail: Bool; // Whether to display the keyword in the Index tab
  end;
  THHAkLink = tagHH_AKLINK;

type
  PHHFtsQuery = ^THHFtsQuery;
  // tagHH_FTS_QUERY, HH_FTS_QUERY
  tagHH_FTS_QUERY = packed record
    cbStruct:          Integer;      // Sizeof structure in bytes.
    fUniCodeStrings:   Bool;         // TRUE if all strings are unicode.
    pszSearchQuery:    PChar;        // String containing the search query.
    iProximity:        LongInt;      // Word proximity.
    fStemmedSearch:    Bool;         // TRUE for StemmedSearch only.
    fTitleOnly:        Bool;         // TRUE for Title search only.
    fExecute:          Bool;         // TRUE to initiate the search.
    pszWindow:         PChar;        // Window to display in
  end;
  THHFtsQuery = tagHH_FTS_QUERY;


const
  HH_DISPLAY_TOPIC        = $0000;
  HH_HELP_FINDER          = $0000;  // WinHelp equivalent
  HH_DISPLAY_TOC          = $0001;  // not currently implemented
  HH_DISPLAY_INDEX        = $0002;  // not currently implemented
  HH_DISPLAY_SEARCH       = $0003;  // not currently implemented
  HH_SET_WIN_TYPE         = $0004;
  HH_GET_WIN_TYPE         = $0005;
  HH_GET_WIN_HANDLE       = $0006;
  HH_ENUM_INFO_TYPE       = $0007;  // Get Info type name, call repeatedly to enumerate, -1 at end
  HH_SET_INFO_TYPE        = $0008;  // Add Info type to filter.
  HH_SYNC                 = $0009;
  HH_RESERVED1            = $000A;
  HH_RESERVED2            = $000B;
  HH_RESERVED3            = $000C;
  HH_KEYWORD_LOOKUP       = $000D;
  HH_DISPLAY_TEXT_POPUP   = $000E;  // display string resource id or text in a popup window
  HH_HELP_CONTEXT         = $000F;  // display mapped numeric value in dwData
  HH_TP_HELP_CONTEXTMENU  = $0010;  // text popup help, same as WinHelp HELP_CONTEXTMENU
  HH_TP_HELP_WM_HELP      = $0011;  // text popup help, same as WinHelp HELP_WM_HELP
  HH_CLOSE_ALL            = $0012;  // close all windows opened directly or indirectly by the caller
  HH_ALINK_LOOKUP         = $0013;  // ALink version of HH_KEYWORD_LOOKUP
  HH_GET_LAST_ERROR       = $0014;  // not currently implemented // See HHERROR.h
  HH_ENUM_CATEGORY        = $0015;  // Get category name, call repeatedly to enumerate, -1 at end
  HH_ENUM_CATEGORY_IT     = $0016;  // Get category info type members, call repeatedly to enumerate, -1 at end
  HH_RESET_IT_FILTER      = $0017;  // Clear the info type filter of all info types.
  HH_SET_INCLUSIVE_FILTER = $0018;  // set inclusive filtering method for untyped topics to be included in display
  HH_SET_EXCLUSIVE_FILTER = $0019;  // set exclusive filtering method for untyped topics to be excluded from display
  HH_INITIALIZE           = $001C;  // Initializes the help system.
  HH_UNINITIALIZE         = $001D;  // Uninitializes the help system.
  HH_PRETRANSLATEMESSAGE  = $00fd;  // Pumps messages. (NULL, NULL, MSG*).
  HH_SET_GLOBAL_PROPERTY  = $00fc;  // Set a global property. (NULL, NULL, HH_GPROP)


implementation

uses
  Forms;

var
  // handle for hhctrl.ocx
  OCXHandle: THandle = 0;

procedure LoadOCX;
var
  OCXPath: string;
  Buffer: Array[0..255] of Char;
  BuffSize: DWORD;
begin
  BuffSize := SizeOf(Buffer);
  GetSystemDirectory(@Buffer, BuffSize);
  OCXPath := string(Buffer)+'\HHCTRL.OCX';
  if OCXHandle = 0 then begin
    if (OCXPath <> '') and FileExists(OCXPath) then begin
      OCXHandle := LoadLibrary(PChar(OcxPath));
      if OCXHandle > HINSTANCE_ERROR then @HHelp := GetProcAddress(OCXHandle, 'HtmlHelpA')
      else OCXHandle := 0;
    end;
  end;
end;

procedure FreeOCX;
begin
  if OCXHandle <> 0 then begin
    FreeLibrary(OCXHandle);
    @HHelp := nil;
    OCXHandle := 0;
  end;
end;

initialization
  LoadOCX;

finalization
  FreeOCX;

end.

