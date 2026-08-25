{$mode objfpc}{$H+}

unit Unit1;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  RVStyle, RichView, ExtCtrls, StdCtrls, ComCtrls, RVScroll, PtblRV, Menus,
  ImgList, Buttons, LResources, LCLType;

type
  TForm1 = class(TForm)
    RVStyle1: TRVStyle;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    ImageList1: TImageList;
    StatusBar1: TStatusBar;
    RichView1: TRichView;
    Image4: TImage;
    RVPrint1: TRVPrint;
    SaveDialog1: TSaveDialog;
    PopupMenu1: TPopupMenu;
    ClickedWord: TMenuItem;
    N1: TMenuItem;
    mitCopy: TMenuItem;
    mitSelectAll: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure RichView1Jump(Sender: TObject; id: Integer);
    procedure RichView1RVMouseMove(Sender: TObject; id: Integer);
    procedure RVPrint1Formatting(Sender: TRichView; PageCompleted: Integer;
      Step: TRVPrintingStep);
    procedure RVPrint1SendingToPrinter(Sender: TRichView;
      PageCompleted: Integer; Step: TRVPrintingStep);
    procedure RichView1URLNeeded(Sender: TRichView; id: Integer;
      var url: string);
    procedure RichView1SaveComponentToFile(Sender: TRichView; Path: string;
      SaveMe: TPersistent; SaveFormat: TRVSaveFormat; var OutStr: string);
    procedure RichView1RVDblClick(Sender: TRichView; AClickedWord: string;
      Style: Integer);
    procedure RichView1RVRightClick(Sender: TRichView; AClickedWord: string;
      Style, X, Y: Integer);
    procedure StatusBar1Click(Sender: TObject);
    procedure mitSelectAllClick(Sender: TObject);
    procedure mitCopyClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses BackStyl, PrintFrm;
{.$R *.DFM}
{------------------------------------------------------------------}
type
  TDemoButton = class (TButton)
     public
       constructor Create(AOwner: TComponent); override;
       procedure Click; override;
  end;
constructor TDemoButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 200;
  Height := 20;
  Caption := 'Click me!';
  TabStop := False;
end;
procedure TDemoButton.Click;
begin
  Application.MessageBox('Yes, it works!','Demo', MB_OK or MB_ICONEXCLAMATION);
  Form1.RichView1.SetFocus;  
end;
{------------------------------------------------------------------}
procedure TForm1.FormCreate(Sender: TObject);
const crlf:String = chr(13)+chr(10);
      rvsProgram = LAST_DEFAULT_STYLE_NO+1;
      rvsLetter = LAST_DEFAULT_STYLE_NO+2;      
var QuakeHead: TIcon;
    btn: TDemoButton;
begin
  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Form1.Color := RVStyle1.Color;
  RichView1.FirstJumpNo := 100; {you can use this property to set
         base value of hypertext link indices. It will allow you use
         sole handlers of OnJump and OnRVMouseMove events for
         several TRichView controls. In this program FirstJumpNo=100
         for no particular reason}
  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  with RichView1 do begin
     AddCenterLine('TRichView Demo and Help Program', rvsHeading);
     Self.RemoveControl(Image1);
     AddControl(Image1, True);
     AddCenterLine('(Copyright(c) 1997,98 by Tkachenko S.V.)', rvsNormal);
     AddCenterLine('Contents', rvsSubHeading);
     AddBullet(0, ImageList1, True);
     Add('    1.   ', rvsNormal);  Add('Introduction', rvsJump1); // jump#0
     AddBullet(0, ImageList1, True);
     Add('    2.   ', rvsNormal); Add('Step1: Text Styles', rvsJump1); // jump#1
     AddBullet(0, ImageList1, True);
     Add('    3.   ', rvsNormal); Add('Step2: Adding Text', rvsJump1); // jump#2
     AddBullet(0, ImageList1, True);
     Add('    4.   ', rvsNormal); Add('Step3: Adding Pictures', rvsJump1); // jump#3
     AddBullet(0, ImageList1, True);
     Add('    5.   ', rvsNormal); Add('Step4: Adding Delphi Controls', rvsJump1); // jump#4
     AddBullet(0, ImageList1, True);
     Add('    6.   ', rvsNormal); Add('Step5: Hypertext', rvsJump1); // jump#5
     AddBullet(0, ImageList1, True);
     Add('    7.   ', rvsNormal); Add('Step6: Other Properties', rvsJump1); // jump#6
     AddBullet(0, ImageList1, True);
     Add('    8.   ', rvsNormal); Add('New in version 0.3', rvsJump1); // jump#7
     AddBullet(0, ImageList1, True);
     Add('    9.   ', rvsNormal); Add('New in version 0.4 - Printing', rvsJump1); // jump#8
     AddBullet(0, ImageList1, True);
     Add('   10.   ', rvsNormal); Add('New in version 0.4 - Saving', rvsJump1); // jump#9
     AddBullet(0, ImageList1, True);
     Add('   11.   ', rvsNormal); Add('New in version 0.4 - Other features', rvsJump1); // jump#10
     AddBullet(0, ImageList1, True);
     Add('   12.   ', rvsNormal); Add('New in version 0.5', rvsJump1); // jump#11
     AddBullet(2, ImageList1, True);
     Add('Appendix.   ', rvsNormal); Add('Shareware versions', rvsJump1); // jump#12
    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
     AddBreak;
     AddNamedCheckPoint('introduction'); // cp# 0
     AddCenterLine('Introduction', rvsSubheading);
     AddTextFromNewLine('    TRichView controls can contain:'+ crlf+
       '    - text with various fonts'+crlf+
       '    - hypertext'+crlf+
       '    - pictures'+crlf+
       '    - pictures from Image Lists'+crlf+
       '    - any Delphi controls'+crlf+
       '    First program with TRichView:'+crlf+
       '    1) at design time create TRVStyle control (RVStyle1)'+crlf+
       '    2) at design time create TRichView control (RichView1)'+crlf+
       '    3) at design time in Object Inspector set RichView1.Style := RVStyle1'+crlf+
       '    5) in FormCreate event handler write: "RichView1.AddFromNewLine(''Hello world!'', 0); RichView1.Format;"',
       rvsNormal);
    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
     AddBreak;
     AddNamedCheckPoint('step1'); // cp# 1
     AddCenterLine('Step1: TextStyles', rvsSubheading);
     AddCenterLine('(TRVStyle Control)', rvsNormal);
     Self.RemoveControl(Image2);
     AddControl(Image2, True);
     AddTextFromNewLine('    In order to use TRichView control, you need to define '+
       'text styles for it. All properties for customizing TRichView control are '+
       'concentrated in TRVStyle control. You should create such control '+
       '(let its Name is RVStyle1) and assign it to Style property of one or more of '+
       'your TRichView controls (at design or run  time):', rvsNormal);
     AddTextFromNewLine('RichView1.Style := RVStyle1', rvsProgram);
     AddTextFromNewLine('   TRichView will display text and graphic only if '+
       'its Style property is assigned. You can create one or more TRVStyle controls.'+crlf+
       'TRVStyle control has properties:', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('Color: TColor', rvsKeyword);
     Add(' - back color of TRichView controls;', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('JumpCursor: TCursor', rvsKeyword);
     Add(' - cursor, which appears when mouse pointer moves above hypertext jumps. '+
       'When created, TRVStyle control adds ''hand'' cursor to application Screen cursors, '+
       'with number crJump:', rvsNormal);
     AddTextFromNewLine('    const crJump = 101', rvsProgram);
     AddFromNewLine('This value is default for this property;', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('SelColor: TColor ', rvsKeyword);
     AddBullet(0, ImageList1, False);
     Add('SelTextColor: TColor', rvsKeyword);
     Add(' - color of background and text of selection for copying to clipboard. '+
         'Default values: clHighlight and clHighlightText.', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('HoverColor: TColor ', rvsKeyword);
     Add(' - color of hypertext link under mouse. Set this property to clNone (default value) to disable effect. '+
         'In this demo HoverColor=clBlack.', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('TextStyles: TFontInfos', rvsKeyword);
     AddText(' - collection of text styles for TRichView controls. You can edit '+
       'this property at design (by special property editor) or run time. '+
       'At run time you can edit text style # i (i from 0 to RVStyle1.TextStyles.Count-1):', rvsNormal);
     AddTextFromNewLine('    RVStyle1.TextStyles[i].FontName := ''Arial'';'+crlf+
       '    RVStyle1.TextStyles[i].Size := 10;'+crlf+
       '    RVStyle1.TextStyles[i].Color := clRed;'+crlf+
       '    RVStyle1.TextStyles[i].Style := [fsBold, fsUnderline];', rvsProgram);
     AddBullet(0, ImageList1, True);
     Add(' RVStyle1.TextStyles[i].CharSet := RUSSIAN_CHARSET; '+
       '{for Delphi3+}', rvsProgram);
    AddFromNewLine('    There are some predefined styles with numbers:', rvsNormal);
    AddTextFromNewLine('const'+crlf+'    rvsNormal = 0;'+crlf+
       '    rvsHeading = 1;'+crlf+'    rvsSubheading = 2;'+crlf+
       '    rvsKeyword = 3;'+crlf+'    rvsJump1 = 4;'+crlf+'    rvsJump2 = 5;', rvsProgram);
    AddTextFromNewLine('    You can edit but can''t delete these styles. '+crlf+
       '    Only rvsJump1, rvsJump2 (see ', rvsNormal);
    Add('hypertext', rvsJump1); // jump#13
    Add(') and  rvsNormal (see ', rvsNormal);
    Add('horizontal lines', rvsJump1); // jump#14
    Add(') have special meanings. In addition, you can add your own styles '+
       '(at design time by special property editor or at run time by method AddTextStyle:', rvsNormal);
    AddBullet(0, ImageList1, True);
    Add('AddTextStyle', rvsKeyword);
    AddTextFromNewLine('function TRVStyle.AddTextStyle: Integer; { returns index of new style }'+
       crlf+'{Example:}'+crlf+'RVStyle1.AddTextStyle;', rvsProgram);
    AddTextFromNewLine('    You can define constants for your styles '+
       'using constant LAST_DEFAULT_STYLE_NO:',rvsNormal);
    AddTextFromNewLine('const rvsProgram = LAST_DEFAULT_STYLE_NO+1 { example from this program}',
       rvsProgram);
    AddTextFromNewLine('    You can delete nonstandard text styles at run time by method DeleteTextStyle:',
       rvsNormal);
    AddBullet(0, ImageList1, True);
    Add('DeleteTextStyle', rvsKeyword);
    AddTextFromNewLine('procedure TRVStyle.DeleteTextStyle(Index: Integer); { removes NONSTANDARD text style}'+
       crlf+'{Example:}'+crlf+'RVStyle1.DeleteTextStyle(rvsProgram);', rvsProgram);
    AddBullet(0, ImageList1, True);
    Add('SaveINI', rvsKeyword);
    AddFromNewLine('procedure TRVStyle.SaveINI(filename, section: String);', rvsProgram);
    AddTextFromNewLine(' - this method saves control in section "section" of '+
       'ini-file "filename". WARNING: all contents of this section erased before saving!!!',
        rvsNormal);
    AddBullet(0, ImageList1, True);
    Add('LoadINI', rvsKeyword);
    AddFromNewLine('procedure TRVStyle.LoadINI(filename, section: String);', rvsProgram);
    AddTextFromNewLine(' - this method loads control from section "section" of '+
       'ini-file "filename".', rvsNormal);
    AddTextFromNewLine('Note: When you change Color property of TRVStyle control, you should call '+
       'Refresh method of all linked TRichView controls in order to redraw them. '+
       'When you change TextStyles property of TRVStyle control, you should call '+
       'Format and Refresh methods of all linked TRichView controls.', rvsNormal);
    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
     AddBreak;
     AddNamedCheckPoint('step2'); // cp# 2
     AddCenterLine('Step2: Adding Text', rvsSubheading);
     AddCenterLine('(the main methods of TRichView control)', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('Method Clear', rvsKeyword);
     Add(' - deletes all text, graphic and other objects from TRichView control;',
       rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('Method Format', rvsKeyword);
     AddText(' - prepares control to display text and graphics. You must call it after'+crlf+
       '- you have added text and graphics (main reason)'+crlf+
       '- you have changed text styles of linked TRVStyle control'+crlf+
       '- you have modified LeftMargin, RightMargin, MaxTextWidth or MinTextWidth properties'+crlf+
       '- you have resized controls inserted in this TRichView control'+crlf+
       '    Method is called automatically when TRichView control is resized.',
       rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('Methods AddFromNewLine, Add, AddCenterLine, AddText, AddTextFromNewLine', rvsKeyword);
     Add(' - adds text to TRichView control. String should not contain TAB characters. String should not be empty.', rvsNormal);
     AddTextFromNewLine('    {Add one new line of text (line must not contain CR-LF characters)}'+crlf+
       'procedure AddFromNewLine(s: String;StyleNo:Integer);'+crlf+
       '    {Append text to the end of last line(line must not contain CR-LF characters)}'+crlf+
       'procedure Add(s: String;StyleNo:Integer);'+crlf+
       '    {Add one new line of text with center alignment(line must not contain CR-LF characters)}'+crlf+
       'procedure AddCenterLine(s: String;StyleNo:Integer);'+crlf+
       '    {Add one or more lines. First line appends to the end of previous text}'+crlf+
       'procedure AddText(s: String;StyleNo:Integer);'+crlf+
       '    {Add one or more lines}'+crlf+
       'procedure AddTextFromNewLine(s: String;StyleNo:Integer);', rvsProgram);
     AddTextFromNewLine('    First argument of these methods - text string to add, '+
       'second - its style # (in text styles of linked TRVStyle control)', rvsNormal);
     AddCheckPoint; // cp#3
     AddBullet(0, ImageList1, True);
     Add('Method AddBreak', rvsKeyword);
     Add(' - adds horizontal line with color of text style # rvsNormal',
       rvsNormal);
    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
     AddBreak;
     AddNamedCheckPoint('step3'); // cp#4
     AddCenterLine('Step3: Adding Pictures', rvsSubheading);
     AddCenterLine('(methods of TRichView control)', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('Method AddPicture', rvsKeyword);
     AddText(' - adds picture with center alignment. This method DOES NOT COPY picture from '+
       'argument, only makes pointer to it. Memory is released when you call method Clear '+
       'or when control is destroyed. Do not destroy this picture yourself!'+crlf+
       '    TRichView control provides flicker-free scrolling of pictures.', rvsNormal);
     AddTextFromNewLine('procedure AddPicture(gr: TGraphic);'+crlf+
       '{example from this program:}'+crlf+
       'var QuakeHead: TIcon'+crlf+'...'+crlf+
       'QuakeHead := TIcon.Create; { you can create Icon, Bitmap or Metafile}'+crlf+
       'QuakeHead.Assign(Image3.Picture.Icon);'+crlf+
       'RichView1.AddPicture(QuakeHead);'+crlf+'...(see result below)', rvsProgram);
     QuakeHead := TIcon.Create;
     QuakeHead.Assign(Image3.Picture.Icon);
     AddPicture(QuakeHead);
     AddBullet(0, ImageList1, True);
     Add('Method AddBullet', rvsKeyword);
     AddFromNewLine('procedure TRichView.AddBullet(imgNo: Integer; lst: TImageList;'+
       ' fromnewline: Boolean);', rvsProgram);
     AddFromNewLine(' - adds picture # imgNo (1st argument) from Image List '+
       '(2nd argument) from the new line or not (3d argument). See example in this '+
       'program code. Nothing is copied and nothing is destroyed.', rvsNormal);
    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
     AddBreak;
     AddNamedCheckPoint('step4'); // cp# 5
     AddCenterLine('Step4: Adding Delphi Controls', rvsSubheading);
     AddCenterLine('(methods of TRichView control)', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('Method AddControl', rvsKeyword);
     AddTextFromNewLine('procedure AddControl(ctrl: TControl; center: Boolean);', rvsProgram);
     AddText(' - adds ANY visible Delphi control (first argument) with center '+
       '(second argument = True) or left (second argument = False) alignment.'+crlf+
       '    Unlike previous methods this method adds element, which is drawed and '+
       'processed by Windows, not by my control. It works the same way as if it '+
       'was inserted in any other Delphi control.'+crlf+
       '    (1) You can insert this control at design time in form, and then '+
       'remove it from form and insert in TRichView control:', rvsNormal);
     AddTextFromNewLine(
       '{example from this program (see pictures of TRVStyle and TRichView controls. '+
       'They have popup hints.)}'+crlf+
       'Self.RemoveControl(Image1);'+crlf+
       'AddControl(Image1, True);', rvsProgram);
     AddTextFromNewLine(
       '    (2) You can create control at run time:', rvsNormal);
     AddTextFromNewLine(
       '{example from this program (see result below)}'+crlf+
       'var btn: TDemoButton;'+crlf+'...'+crlf+
       'btn := TDemoButton.Create(RichView1);'+crlf+
       'RichView1.AddControl(btn, False);', rvsProgram);
     AddTextFromNewLine('    These componets will be destroyed when you call method Clear '+
       'or when TRichView control is destroyed. Do not destroy them yourself!'+crlf+
       'Warning: This version of TRichView uses Tag property of inserted components. '+
       'You should not use it yourself. But you can use Tag property of components, inserted '+
       'in components, inserted in TRichView.', rvsNormal);
     btn := TDemoButton.Create(RichView1);
     AddControl(btn, False);
     AddBreak;
     AddBullet(0, ImageList1, True);     
     Add('    I''ve received a mail from Cub Lea:', rvsNormal);
     AddTextFromNewLine('TIP: When using Delphi controls in the body of the RichView canvas, remember to '+
       'reset focus to the RichView control after the user performs an action which '+
       'sets focus to the embedded control.  If you do not do this, the RichView '+
       'will not have keyboard focus and up-down scrolling using the arrow keys '+
       '(which most expert users use instead of the scrollbar) will be lost. '+
       'Here''s an example that processes a button click and returns keyboard input '+
       'focus to the RichView component so that arrow-key scrolling can continue: '+
       crlf+' '+
       'procedure TDemoButton.Click;'+crlf+
       'begin'+crlf+
       '  Application.MessageBox(''Yes, it works!'',''Demo'', MB_OK or MB_ICONEXCLAMATION);'+crlf+
       '  Form1.RichView1.SetFocus;'+crlf+
       'end;'+crlf+
       '    Alternately, you can set the form''s KeyPreview to True and handle up/down '+
       'keystrokes yourself in the OnKeyPress event, but in many situations this '+
       'will either be too clumsy or too inconsistent. ',rvsLetter);
    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
     AddBreak;
     AddNamedCheckPoint('step5'); // cp# 6
     AddCenterLine('Step5: Hypertext', rvsSubheading);
     AddCenterLine('(methods of TRichView control)', rvsNormal);
     AddTextFromNewLine('    TRichView control does not provide any action when '+
       'user clicks at link. It only sends the event OnJump.'+crlf+
       '   The hypertext link is created '+
       'then you add string by methods Add, AddCenterLine, AddFromNewLine with '+
       'styleNo = rvsJump1 or rvsJump2, or when you add picture by method AddHotspot. '+
       ' This link has index =(FirstJumpNo+''index of previous link''+1)', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('FirstJumpNo', rvsKeyword);
     AddText(' is a property of TRichView control, 0 by default.'+crlf+
       '    So, first link has number FirstJumpNo, second FirstJumpNo+1 ...', rvsNormal);
     AddBullet(0, ImageList1, True);
     AddText('OnJump:TJumpEvent;'+crlf+
             'TJumpEvent = procedure (Sender: TObject; id: Integer) of object;', rvsKeyword);
     AddText(' - event of TRichView control. It is sent then user clicks at hypertext link. '+crlf+
       '    First argument - TRichView control which sent this '+
       'event. Second - index of link.', rvsNormal);
     AddBullet(0, ImageList1, True);
     AddText('property OnRVMouseMove: TRVMouseMoveEvent;'+crlf+
             'TRVMouseMoveEvent = procedure (Sender: TObject; id: Integer) of object;', rvsKeyword);
     AddText(' - event of TRichView control. It is sent then mouse pointer moves above control. '+crlf+
       '    First argument - TRichView control which sent this '+
       'event. Second - index of link (-1 if mouse pointer is not above any link). '+crlf+
       'This event is not sent twice with equal id one after another', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('AddHotSpot', rvsKeyword);
     AddFromNewLine('procedure AddHotSpot(imgNo: Integer; lst: TImageList; fromnewline: Boolean);', rvsProgram);
     AddTextFromNewLine(' - adds image-hypertext link. Parameters are the same '+
       ' as in the method AddBullet. (See hotspot example at the end of this text)',
       rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('ScrollTo', rvsKeyword);
     AddFromNewLine('procedure ScrollTo(y: Integer);', rvsProgram);
     AddTextFromNewLine(' scrolls TRichView control to vertical '+
       'coordinate = y pixels from the top of scrolled area. So RichView1.ScrollTo(0) scrolls '+
       'to the top of document. You can use this method with methods GetCheckPointY '+
       'and GetJumpPointY.', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('AddCheckPoint', rvsKeyword); 
     AddFromNewLine('function AddCheckPoint: Integer;', rvsProgram);
     AddFromNewLine(' - adds invisible label (checkpoint). Method returns index of checkpoint '+
       '(First checkpoint has index 0, second- 1, ...). '+
       'You can get Y coordinate of checkpoint by method GetCheckPointY.', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('GetCheckPointY, GetJumpPointY', rvsKeyword);
     AddTextFromNewLine('function GetCheckPointY(no: Integer): Integer;'+crlf+
       'function GetJumpPointY(no: Integer): Integer;', rvsProgram);
     AddText(' - returns Y coordinate (in pixels) of checkpoint (argument - checkpoint index) '+
       'and hypertext link (argument - hypertext link index). You can use this '+
       'methods for scrolling.'+crlf+
       'Note: method ScrollTo does not scroll exactly to specified coordinate, '+
       'but can scroll slightly highter due to my scrolling technique. '+crlf+
       'This technique provides what height of scrolled area < max value of 31bit integer '+
        '(not 15bit like Delphi scrolled controls), but can not provide pixel to '+
        'pixel precision. It scrolls in logical units (= 10 pixels by default), but '+
        'unit can be automaticaly increased if height of scrolled area in this logical '+
        'units exceeds 30000.', rvsNormal);
    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
     AddBreak;
     AddNamedCheckPoint('step6'); // cp# 7
     AddCenterLine('Step6: Other Properties of TRichView', rvsSubheading);
     AddTextFromNewLine('    TRichView has standard properties: Align, TabStop, '+
       'TabOrder, HelpContext and other. ', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('Tracking: Boolean', rvsKeyword);
     Add(' - the Tracking property determines whether the control will scroll '+
       'when the scroll bar thumb tab is being dragged or will wait for the tab'+
       ' to be dropped. Default value = True.', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('LeftMargin, RightMargin: Integer', rvsKeyword);
     Add(' - margins in pixels. Default values = 5 pixels.', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('MinTextWidth, MaxTextWidth: Integer', rvsKeyword);
     AddText(' - limitation of text width in pixels. Default values = 0. '+
       'If MaxTextWidth=0 this property is ignored. '+
       'Width of scrolled area = max(ClientWidth, widths of inserted pictures and '+
       'controls, MinTextWidth)+LeftMargin+RightMargin. If you have inserted very wide '+
       'control or picture, this value will be large. For example, if you want to limit '+
       'text width by screen width, write "RichView1.MaxTextWidth := Screen.Width". '+
       'MaxTextWidth property does not affect the width of scrolled area. '+
       'MinTextWidth not only limits text width, but can also increase the speed of resizing. '+
       'If text width was not changed after resizing, reformatting will be MUCH more '+
       'faster (if text is large). You can see it: if you resize width of this window to values < '+
       'width of button "Click me", the horizontal scrollbar will appear, and redrawing will '+
       'be processed quickly. You also can try to set MinTextWidth=600 in this program and see '+
       'result.'+crlf+'Tip: if you want supress flicker effect during resizing, you should set '+
       'background color of parent control to the same value as the color of TRichView control.'
       , rvsNormal);
    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
     AddBreak;
     AddNamedCheckPoint('new in v0.3'); // cp# 8
     AddCenterLine('New in version 0.3', rvsSubheading);
     AddTextFromNewLine('    New properties of TRichView:', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('BackgroundBitmap: TBitmap', rvsKeyword);
     Add(' - background image appeared according to BackgroundStyle property', rvsNormal);
     AddBullet(0, ImageList1, True);
     AddText('BackgroundStyle: TBackgroundStyle'+crlf+
       'TBackgroundStyle = (bsNoBitmap {bitmap is ignored},'+crlf+
       '    bsStretched {bitmap is stretched to fit component size and do not scrolled},'+crlf+
       '    bsTiled {bitmap is tiled and do not scrolled},'+crlf+
       '    bsTiledAndScrolled {bitmap is tiled and scrolls with other contents of component});'  ,
       rvsKeyword);
     AddFromNewLine('    You can test this property now: ', rvsNormal);
     Add('Select background style', rvsJump2); // jump#15
     Add('.', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('VScrollVisible: Boolean', rvsKeyword);
     AddText(' - hides or shows vertical scrollbar. If VScrollVisible=False vertical '+
       'scrollbar never appears. If VScrollVisible=True vertical '+
       'scrollbar appears when it needed. Horizontal scrollbar appears only if you '+
       'insert pictures or components wider when width of TRichView component, or if '+
       'you set large MinTextWidth property.'
       , rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('VScrollMax: Integer (run-time and read-only)', rvsKeyword);
     AddText(' - maximum value of VScrollPos property.', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('VScrollPos: Integer (run-time)', rvsKeyword);
     AddText(' - vertical scrolling position, from 0 to VScrollMax inclusively.'+crlf+
       'It is measured in my scrolling units (MSU). By default 1 MSU =10 pixels.'+crlf+
       'You can change it by', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('VSmallStep: Integer (run-time)', rvsKeyword);
     AddText(' - number of pixels in 1 MSU. Important: if you change it when TRichView '+
       'component is already displayed, you must call Format and Refresh methods after.'+crlf+
       '    If you assign too small value to this property (and scrolling range becomes > 30000 MSU) '+
       'assignment is ignored. Format method may increase this property value because of the same case.'+crlf+
       'Note:  "VScrollPos:=#" is equivalent to "ScrollTo(#*VSmallStep)". The second demo '+
       'illustrates using of these properties.'+crlf+
       '    Other changes:'+crlf+
       '    - Intermediate class TScroller changed to TRVScroller (file Scroller.pas '+
       'renamed to RVScroll.pas) to avoid conflict of names with other VCL.'+crlf+
       '    - Little inaccuracy in demo program was corrected: last hyperlink should be pointed at '+
       'first hyperlink. Line "ScrollTo(GetJumpPointY(0))" was corrected to '+
       '"ScrollTo(GetJumpPointY(FirstJumpNo))"',
       rvsNormal);
    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
     AddBreak;
     AddNamedCheckPoint('printing'); // cp# 9
     AddCenterLine('New in version 0.4 - Printing', rvsSubheading);
     AddFromNewLine('(this version can not print inserted components, only text and pictures)', rvsNormal);
     AddFromNewLine(' In order to print contents of RichView you should create '+
       'TRVPrint component:', rvsNormal);
     Self.RemoveControl(Image4);
     AddControl(Image4, True);
     AddFromNewLine(' This is invisible at run-time component. It has properties:', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('LeftMarginMM, RightMarginMM, TopMarginMM, BottomMarginMM: Integer', rvsKeyword);
     AddText(' - margins, mm.'+crlf+'    Printing steps:'+crlf+
       '    1) set margins (in TRVPrint);'+crlf+
       '    2) call AssignSource method of TRVPrint;'+crlf+
       '    3) call FormatPages method of TRVPrint;'+crlf+
       '    4) now you can show print preview (MakePreview method of TRVPrint)'+crlf+
       '    5) print whole document (Print method of TRVPrint) or some pages (PrintPages '+
       'method of TRVPrint);'+crlf+
       '    6) when finished you can free some temporary allocated memory (Clear method of TRVPrint).'+crlf+
       '    Methods of TRVPrint:',
       rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('AssignSource', rvsKeyword);
     AddFromNewLine('procedure TRVPrint.AssignSource(PrintMe: TRichView);',rvsProgram);
     AddTextFromNewLine('    This method specifies TRichView component which will be printed. '+
       '    This method creates a link from TRVPrint to TRichView. Internally, TRVPrint contains '+
       'component of special type - TPrintableRV, descendant of TRichView. In this version, it creates this component at '+
       'run-time, setting its Visible property to false. SO YOU CAN NOT CREATE TRVPRINT IN DATAMODULE (in this version), '+
       'ONLY IN FORM. After calling TRVPrint.AssignSource, this TPrintableRV component contains '+
       'copy of background picture (if exists), has the same background style, and SHARES (does not contain copy) contents '+
       'with TRichView component from argument of method. In this version this TPrintableRV '+
       'has name rv and it is a public member of TRVPrint, so you can change its '+
       'background after calling AssignSource method (but only background!!!).', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('FormatPages', rvsKeyword);
     AddTextFromNewLine('function FormatPages(PrintOptions:TRVDisplayOptions): Integer;'+crlf+
       'type'+crlf+'  TRVDisplayOption = (rvdoImages, rvdoComponents, rvdoBullets);'+crlf+
       '  TRVDisplayOptions = set of TRVDisplayOption;',rvsProgram);
     AddTextFromNewLine('    This method prepares document for printing. Please call it with argument '+
       'PrintOptions=[rvdoImages, rvdoBullets]. Other values for future versions. '+crlf+
       ' You can display process of formatting by handling OnFormatting event. '+
       'This function returns number of pages. You can also after calling this function '+
       'use PagesCount property.', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('MakePreview', rvsKeyword);
     AddFromNewLine('procedure TRVPrint.MakePreview(pgNo: Integer; bmp: TBitmap);',rvsProgram);
     AddFromNewLine('    This method: (1) sets Width and Height of bmp according to printer values, '+
       '(2) draws page with pgNo number into bmp. First page has pgNo=1.', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('Print, PrintPages', rvsKeyword);
     AddTextFromNewLine('procedure Print(Title: String; Copies: Integer; Collate: Boolean);'+crlf+
       'procedure PrintPages(firstPgNo, lastPgNo: Integer; Title: String; '+
       ' Copies: Integer; Collate: Boolean);',rvsProgram);
     AddTextFromNewLine('    These methods print whole document or some diapason of pages. '+
       'Calling of Print equivalent to calling PrintPages with firstPgNo=1, lastPgNo=PagesCount.'+crlf+
       '    Title determines the text that appears listed in the Print Manager.'+crlf+
       '    As for Copies and Collate arguments, I have a problem. Now I do following things. '+
       'If printer can handle several copies (if pcCopies in Printer.Capabilities then...) '+
       'I send only one copy of document to printer and set Printer.Copies := Copies '+
       '(you can change collate mode with TPrintDialog). Else I send several copies to printer '+
       'according to Collate and Copies arguments. If I am wrong, or you can suggest better way, '+
       'please let me know.'+crlf+
       '    You can display process of print spooling by handling OnSendingToPrinter event.'+crlf+
       'WARNING: you should prevent your application closing or source TRichView destroying, '+
       'clearing or changing while these methods work!!!', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('Clear', rvsKeyword);
     AddFromNewLine('procedure TRVPrint.Clear;',rvsProgram);
     AddTextFromNewLine('    This methods frees memory allocated by FormatPages method.', rvsNormal);
     AddText('Events of TRVPrint:', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('OnFormatting, OnSendingToPrinter: TRVPrintingEvent', rvsKeyword);
     AddTextFromNewLine('type'+crlf+
       '  TRVPrintingStep = (rvpsStarting, rvpsProceeding, rvpsFinished);'+crlf+
       '  TRVPrintingEvent = procedure (Sender: TRichView; PageCompleted: Integer; '+
       'Step:TRVPrintingStep) of object;',rvsProgram);
     AddTextFromNewLine('    These methods allow you to display progress of printing. '+
       ' OnFormatting is rised by FormatPages, OnSendingToPrinter by Print or PrintPages. '+crlf+
       ' First they rised with Step=rvpsStarting, next several times with Step=rvpsProceeding, '+
       ' and last time with Step=rvpsFinished. If Step=rvpsProceeding you can use '+
       'PageCompleted argument to determine wich page was formatted or sent to printer.'+crlf+
       ' You can test printing abilities of TRichView now: ', rvsNormal);
       Add('PRINT...', rvsJump2); // jump#16
     {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
     AddBreak;
     AddNamedCheckPoint('saving'); // cp# 10
     AddCenterLine('New in version 0.4 - Saving', rvsSubheading);
     AddTextFromNewLine(' In this version you can save contents of TRichView as'+crlf+
       '- plain text;'+crlf+
       '- html text + several bitmaps;', rvsNormal);
     AddCenterLine('Saving text', rvsKeyword);
     AddBullet(0, ImageList1, True);
     Add('SaveText', rvsKeyword);
     AddFromNewLine('function TRichView.SaveText(FileName: String; LineWidth: Integer):Boolean;',rvsProgram);
     AddTextFromNewLine('    FileName - file name (of course).'+crlf+
       '    LineWidth is used when saving horizontal lines (saved as LineWidth ''-'' characters) '+
       'and when saving centered lines.'+crlf+
       '    Return value - "saving was successful?"'+crlf+
       '    Pictures, bullets, hotspots, hypertext links are not saved. '+
       'But you can provide you own code to save inserted components. '+
       'You can do it by handling OnSaveComponentToFile event.', rvsNormal);
     AddCenterLine('Saving HTML file', rvsKeyword);
     AddTextFromNewLine('    Limitations: images saved as bitmap (not for all browsers!), '+
       'losing of transparent color, losing some font size information.', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('SaveHTML', rvsKeyword);
     AddTextFromNewLine('function TRichView.SaveHTML(FileName,Title,ImagesPrefix: String; Options: TRVSaveOptions):Boolean;'+crlf+
       'type'+crlf+'  TRVSaveOption = (rvsoOverrideImages);'+crlf+
       '  TRVSaveOptions = set of TRVSaveOption;',rvsProgram);
     AddTextFromNewLine('    FileName - file name.'+crlf+
       '    Title - title of document (tag TITLE).'+crlf+
       '    Return value - "saving was successful?"'+crlf+       
       '    ImagesPrefix - first part of image file names. For example, if '+
       'ImagesPrefix=''img'' then images will be saved as img1.bmp, img2.bmp and so on.'+crlf+
       '    If rvsoOverrideImages in Options then all files with same names will be overriden. '+
       'Else component will make unique file names for images.'+crlf+
       'All images saved as bitmaps, so they can be viewed ONLY in browsers which support this format!!!'+crlf+
       '    But function which saves pictures - SavePicture - is public and VIRTUAL, so '+
       'you can override it to save gifs or jpegs.'+crlf+
       '    Saved bitmap is not transparent. When converting to bitmap the transparent '+
       'color of icons or metafiles converted to background color of TRichView.'+crlf+
       '    Checkpoints are saved as <A name=RichViewCheckPoint#></A> where #-index of checkpoint.'+crlf+
       '    By default hypertext links are not saved (saved as regular text). But you can save it by '+
       'handling event OnURLNeeded:', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('OnURLNeeded: TRVURLNeededEvent', rvsKeyword);
     AddFromNewLine('type TRVURLNeededEvent = procedure (Sender: TRichView; id: Integer; var url:String) of object;',
       rvsProgram);
     AddTextFromNewLine('    id - hypertext jump id (from FirstJumpNo)'+crlf+
       '    url - output argument - target of hypertext jump. For example you can write "url := ''#RichViewCheckPoint0''".'+crlf+
       '    Initial value of url=''''.'+crlf+
       '    By default inserted components are not saved. But you can save it by '+
       'handling event OnSaveComponentToFile:', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('OnSaveComponentToFile: TRVSaveComponentToFileEvent', rvsKeyword);
     AddFromNewLine('type   TRVSaveComponentToFileEvent = procedure (Sender: TRichView; Path: String; SaveMe: TPersistent; SaveFormat: TRVSaveFormat; var OutStr:String) of object;',
       rvsProgram);
     AddTextFromNewLine('    Path - path where html file being saved (use it when calling SavePicture method).'+crlf+
       '    SaveMe - component being saved.'+crlf+
       '    SaveFormat = rvsfHTML (use it when calling SavePicture method).'+crlf+
       '    OutStr - output argument - string which will be added to html file.'+crlf+
       '    You should set OutStr and may be save some pictures by SavePicture method',
       rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('SavePicture', rvsKeyword);
     AddFromNewLine('function TRichView.SavePicture(DocumentSaveFormat: TRVSaveFormat; Path: String; gr: TGraphic): String; virtual;',
       rvsProgram);
     AddTextFromNewLine('    You should use this method ONLY inside handlers of '+
       'OnSaveComponentToFile event(!!!). DocumentSaveFormat and Path - arguments '+
       'of OnSaveComponentToFile event (SaveFormat and Path). gr - image to be saved.'+crlf+
       '    This function returns name of image file.'+crlf+
       '    Now you can save this demo as ',rvsNormal);
     Add('text file', rvsJump2); // jump#17
     Add(' or as ',rvsNormal);
     Add('html file', rvsJump2); // jump#18
     Add('.',rvsNormal);
    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
     AddBreak;
     AddNamedCheckPoint('rest of v0.4'); // cp# 11
     AddCenterLine('New in version 0.4 - Other changes', rvsSubheading);
     AddFromNewLine(' Events of TRichView:', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('OnRVDblClick: TRVDblClickEvent', rvsKeyword);
     AddFromNewLine('type TRVDblClickEvent = procedure  (Sender: TRichView; ClickedWord: String; Style: Integer) of object;',rvsProgram);
     AddTextFromNewLine('    This event is rised then user doubleclicks word or image.'+crlf+
       '    ClickedWord - if user clicks text, this is a word. If user clicks image, this argument = ''''.'+crlf+
       '    Style - index of text style (if text), or one of constants rvsBreak, '+
       'rvsPicture, rvsHotSpot or rvsBullet for other contents. You can doubliclick this demo and see text in status bar.', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('SingleClick: Boolean', rvsKeyword);
     Add('    - property of TRichView. If SingleClick=True then OnRVDblClick is '+
       'rised  on click, not on doubleclick. You can use this property to create dialogs like '+
       'Syntax Highlighting Dialog in Delphi.' , rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('OnRVRightClick: TRVRightClickEvent', rvsKeyword);
     AddFromNewLine('type TRVRightClickEvent = procedure  (Sender: TRichView; ClickedWord: String; Style, X, Y: Integer) of object;',rvsProgram);
     AddTextFromNewLine('    This event is rised then user clicks right mouse button.'+crlf+
       '    Arguments ClickedWord and Style - same as in OnRVDblClick.'+crlf+
       '    X,Y - Screen mouse cursor coordinates. You can right click this demo and see popup menu.'+crlf+
       '    What is "word"? It is defined by Delimiters property of TRichView:', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('Delimiters: String', rvsKeyword);
     AddTextFromNewLine('    This is a set of delimiters. Default value is  ''.;,:(){}"''. Word is '+
       'characters between two characters from delimiters set. If user clicks on delemiter character, '+
       'event with ClickedWord='''' is rised.'+crlf+
       '    There are 2 new methods which allow you to remove some lines from TRichView:', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('DeleteLines', rvsKeyword);
     AddFromNewLine('procedure TRichView.DeleteLines(FirstLine, Count: Integer);',rvsProgram);
     AddTextFromNewLine('    This method removes Count lines starting with FirstLine (from 0).'+
       'Before calling this method TRichView may be formatted or not. In both cases method runs '+
       'correctly. This method does not perform any formatting. You should call Format and Refresh '+
       'methods after it. So this method needs reformatting of whole document and works rather slowly.'+crlf+
       '    What are "lines" in this method? One "line" is item added with methods Add, AddFromNewLine, '+
       'AddPicture and so on. AddText and AddTextFromNewLine can add more than one "line". '+
       'You can get count of "lines" with new property:', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('LineCount: Integer', rvsKeyword);
     AddTextFromNewLine('    The second method removes "section" of document. '+
       '"Section" is defined as "lines between two named checkpoints". Named checkpoint is '+
       'a new feature. It can be added by method AddNamedCheckPoint:', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('AddNamedCheckPoint', rvsKeyword);
     AddFromNewLine('function TRichView.AddNamedCheckPoint(CpName: String): Integer;',rvsProgram);
     AddTextFromNewLine('    This method adds new checkpoint with name CpName and '+
       'returns index of added checkpoint. This checkpoint works just as normal one '+
       '(added by method AddCheckPoint), but also defines a section of document. '+
       'In this version the only action you can do with section is removing. But '+
       'in future versions TRichView will allow to print not only whole document or '+
       'range of pages but also selected sections.', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('DeleteSection', rvsKeyword);
     AddFromNewLine('procedure TRichView.DeleteSection(CpName: String);',rvsProgram);
     AddTextFromNewLine('    This method removes section of document starting with '+
       'checkpoint named CpName till the next named checkpoint (if exists) or the '+
       'end of document. Before calling this method TRichView MUST be formatted. '+
       'This method does not perform any formatting. You should call Format and Refresh '+
       'methods after it. So this method needs reformatting of whole document and works rather slowly.'+crlf+
       '    There are two examples in this demo which are placed at the end of FormCreate. '+
       'Remove comments and see result.'+crlf+
       '    The next addition is IRC-style "autoscroll":',
       rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('FormatTail', rvsKeyword);
     AddFromNewLine('procedure TRichView.FormatTail;',rvsProgram);
     AddTextFromNewLine('    This method formats ONLY NEW items added after last '+
       'calling of Format or FormatTail methods. The first of these items should be '+
       'added from new line. This method also scrolls to '+
       'the end of document. This method does not reformat whole document so it '+
       'works quickly. Like Format method, FormatTail does not perform repainting. '+
       'You should call Repaint method of TRichView after it. There is an example '+
       'at the end of source code of this demo (StatusBar1Click event handler). '+
       'Remove comments and see result (click on status bar adds new lines).'+crlf+
       '    And the last additions - scrolling with arrow keys was added and some '+
       'very small memory leak on destroying was eliminated.',
       rvsNormal);
    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
     AddBreak;
     AddNamedCheckPoint('new in v0.5'); // cp# 12
     AddCenterLine('New in version 0.5', rvsSubheading);
     AddFromNewLine('  The main improvement of TRichView in this version is an ability to '+
       'copy text (only text in this version) to clipboard. The color of selection '+
       'is defined by TRVStyle.SelColor and TRVStyle.SelTextColor properties ( see ', rvsNormal);
     Add('Step 1', rvsJump1); // jump#19
     Add(').', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('AllowSelection: Boolean', rvsKeyword);
     Add(' - property of TRichView. It permits  or forbids selecting.',rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('SelectAll', rvsKeyword);
     AddFromNewLine('procedure TRichView.SelectAll;',rvsProgram);
     AddFromNewLine('    This method selects all contents of component. '+
         'This method does not repaint component, so you should call Refresh after it.', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('Deselect', rvsKeyword);
     AddFromNewLine('procedure TRichView.Deselect;',rvsProgram);
     AddFromNewLine('    This method clears selection. '+
         'This method does not repaint component, so you should call Refresh after it.', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('SelectionExists', rvsKeyword);
     AddFromNewLine('    function TRichView.SelectionExists: Boolean;',rvsProgram);
     AddFromNewLine('    - "Are some contents of component selected?"', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('GetSelText', rvsKeyword);
     AddFromNewLine('    function TRichView.GetSelText: String;',rvsProgram);
     AddFromNewLine('    Function returns selected text as string. You have no needs to '+
         'use this function because you can copy selected text to clipboard by CopyText.', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('CopyText', rvsKeyword);
     AddFromNewLine('    procedure TRichView.CopyText;',rvsProgram);
     AddFromNewLine('    This function copies selection to clipboard as text. If nothing is selected, '+
         'this function does nothing.', rvsNormal);
     AddFromNewLine('    In this demo you can Copy and SelectAll with popup menu. '+
         'This menu is popped up when you rightclick on some word in component '+
         '(this menu was designed for OnRVRightClick demo; if you want to create menu '+
         'which is displayed even if you rightclick on empty space, you should use PopupMenu property). ' +
         'TRichView handles Ctrl-C and Ctrl-Ins keys.', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('OnSelect: TNotifyEvent', rvsKeyword);
     AddFromNewLine('    This event is rised then selection is made (user releases '+
         'mouse button, or SelectAll or Deselect is called.', rvsNormal);
     AddBreak;
     AddFromNewLine('    Next improvement is highlighting of hypertext links under mouse '+
         'with color of TRVStyle.HoverColor. Next - CharSet property of styles (for Delphi3+). (see ', rvsNormal);
     Add('Step 1', rvsJump1); // jump#20
     Add(').', rvsNormal);
     AddBullet(0, ImageList1, True);
     Add('SingleClick: Boolean', rvsKeyword);
     Add('    - property of TRichView. If SingleClick=True then OnRVDblClick is '+
       'rised  on click, not on doubleclick. You can use this property to create dialogs like '+
       'Syntax Highlighting Dialog in Delphi.' , rvsNormal);
     AddBreak;
    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
     AddNamedCheckPoint('shareware'); // cp# 13
     AddCenterLine('Shareware version', rvsSubheading);
     AddTextFromNewLine('  The development of freeware version was stopped in 1999.'+crlf+
       '  This update was made for Delphi 5 and C++Builder 5 compatibility.'+crlf+
       '  Newer (shareware) version includes editor, data-aware versions, component for print '+
       'preview, works with Unicode, supports HTML-style tables. Contents can have much more '+
       'complicated formatting - left, center, right and justify alignments, subscripts/'+
       'superscripts, paragraph backgrounds and more.'+crlf+
       '  Literally all actions of free version are performed in much faster and convenient '+
       'way with shareware version.'+crlf+
       '  Please visit www.trichview.com for additional information.', rvsNormal);
    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
     AddBreak;
     AddHotSpot(1, ImageList1, True); // jump#21
     Add(' Contents (go to first hypertext link)', rvsNormal);
  end;
  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  RichView1.Format;
  {
   //Example of line removing (uncomment this):
    RichView1.DeleteLines(1,RichView1.LineCount-2);
    RichView1.Format;
  }
  {
   //Example of section removing (uncomment this):
    RichView1.DeleteSection('step1');
    RichView1.Format;
    RichView1.DeleteSection('rest of v0.4');
    RichView1.Format;
   }
end;
{------------------------------------------------------------------}
// id0 - jump Id (from 0, not from RichView1.FirstJumpNo)
// Returns the checkpoint index
function JumpToCPNo(id0: Integer): Integer;
begin
  case id0 of
    0: Result := 0; // Introduction
    1,19,20: Result := 1; // Step1
    2: Result := 2; // Step2
    3: Result := 4; // Step3
    4: Result := 5; // Step4
    5: Result := 6; // Step5
    6: Result := 7; // Step6
    7: Result := 8; // New in v0.3
    8: Result := 9; // New in v0.4 - Printing
    9: Result := 10;// New in v0.4 - Saving
   10: Result := 11;// New in v0.4 - Others
   11: Result := 12;// New in v0.5
   12: Result := 13; // shareware
   13: Result := 6; // Step5
   14: Result := 3; // Horz lines
  end;
end;
{------------------------------------------------------------------}
// Event Handler
// Actions on hypertext clicks
procedure TForm1.RichView1Jump(Sender: TObject; id: Integer);
begin
  case id-RichView1.FirstJumpNo of
    0..14, 19, 20:
      RichView1.ScrollTo(RichView1.GetCheckPointY(JumpToCPNo(id-RichView1.FirstJumpNo)));

    15:
    begin
      frmBackStyle.ShowModal();
      RichView1.Refresh();
    end;

    16:
    begin
      RVPrint1.AssignSource(RichView1);
      RVPrint1.FormatPages([rvdoImages, rvdoBullets]);
      RVPrint1.MakePreview(1, frmPrint.Image1.Picture.Bitmap);
      if frmPrint.ShowModal = mrOk then
        RVPrint1.Print('TRichView Demo', 1, False);
    end;

    17:
    begin;
      SaveDialog1.DefaultExt := 'txt';
      SaveDialog1.Title := 'Save text file';
      SaveDialog1.Filter := 'Text Files|*.txt';
      if SaveDialog1.Execute then
        RichView1.SaveText(SaveDialog1.FileName, 65);
    end;

    18:
    begin
      SaveDialog1.DefaultExt := 'htm';
      SaveDialog1.Title := 'Save HTML File';
      SaveDialog1.Filter := 'HTML files|*.htm;*.html';
      if SaveDialog1.Execute then
        RichView1.SaveHTML(SaveDialog1.FileName,'TRichView Help', 'rvimg', [rvsoOverrideImages]);
    end;

    21:
      RichView1.ScrollTo(RichView1.GetJumpPointY(RichView1.FirstJumpNo));
  end;
end;
{------------------------------------------------------------------}
// Event Handler
// Actions on mouse moving over RichView1
procedure TForm1.RichView1RVMouseMove(Sender: TObject; id: Integer);
begin
  if id = -1 then
  begin
    StatusBar1.SimpleText := ''; // mouse pointer is not above link
    Exit;
  end;

  case id-RichView1.FirstJumpNo of
    0: StatusBar1.SimpleText := 'Go to Introduction';
    1,19,20: StatusBar1.SimpleText := 'Go to Text Styles';
    2: StatusBar1.SimpleText := 'Go to Adding Text';
    3: StatusBar1.SimpleText := 'Go to Adding Pictures';
    4: StatusBar1.SimpleText := 'Go to Adding Controls';
    5, 13: StatusBar1.SimpleText := 'Go to Hypertext';
    6: StatusBar1.SimpleText := 'Go to Other Properties';
    7: StatusBar1.SimpleText := 'Go to New in version 0.3';
    8: StatusBar1.SimpleText := 'Go to Printing (v0.4)';
    9: StatusBar1.SimpleText := 'Go to Saving (v0.4)';
   10: StatusBar1.SimpleText := 'Go to New in version 0.4';
   11: StatusBar1.SimpleText := 'Go to New in version 0.5';
   12: StatusBar1.SimpleText := 'Information about further development of this components';
   14: StatusBar1.SimpleText := 'Go to AddBreak method';
   15: StatusBar1.SimpleText := 'Show Background Styles Dialog';
   16: StatusBar1.SimpleText := 'Print this Help';
   17: StatusBar1.SimpleText := 'Save this Help as text file';
   18: StatusBar1.SimpleText := 'Save this Help as html file';
   21: StatusBar1.SimpleText := 'Go to first hypertext link';
  end;
end;
{------------------------------------------------------------------}
// Event Handler
// Displays print page formating progress
procedure TForm1.RVPrint1Formatting(Sender: TRichView;
  PageCompleted: Integer; Step: TRVPrintingStep);
begin
 case Step of
  rvpsStarting:
     StatusBar1.SimpleText := 'Repaginating...';
  rvpsProceeding:
     StatusBar1.SimpleText := 'Repaginating... ('+IntToStr(PageCompleted)+')';
  rvpsFinished:
     StatusBar1.SimpleText := '';
 end;
end;
{------------------------------------------------------------------}
// Event Handler
// Displays print spooling progress
procedure TForm1.RVPrint1SendingToPrinter(Sender: TRichView;
  PageCompleted: Integer; Step: TRVPrintingStep);
begin
 case Step of
  rvpsStarting:
     StatusBar1.SimpleText := 'Start printing...';
  rvpsProceeding:
     StatusBar1.SimpleText := 'Printing page '+IntToStr(PageCompleted)+'...';
  rvpsFinished:
     StatusBar1.SimpleText := '';
 end;
end;
{------------------------------------------------------------------}
// Event Handler
// Asking for target URL
procedure TForm1.RichView1URLNeeded(Sender: TRichView; id: Integer;
  var url: string);
begin
  with RichView1 do
    case id-FirstJumpNo of
      0..13, 18, 19: url := '#RichViewCheckPoint'+intToStr(JumpToCPNo(id-FirstJumpNo));
      { other hypertext links are ignored }
    end;
end;
{------------------------------------------------------------------}
// Event Handler
// Saving inserted components
procedure TForm1.RichView1SaveComponentToFile(Sender: TRichView;
  Path: string; SaveMe: TPersistent; SaveFormat: TRVSaveFormat;
  var OutStr: string);
var bmp: TBitmap;
    imgfilename: String;
begin
  case SaveFormat of
   rvsfText:
      begin
         if SaveMe is TButton then begin
           OutStr := '['+TButton(SaveMe).Caption+']';
           exit;
         end;
         if SaveMe is TImage then begin
           OutStr := '          <Image of '+TImage(SaveMe).Hint+'>';
           exit;
         end;
      end;
   rvsfHTML:
       begin
         if SaveMe is TButton then begin
           OutStr := '<FORM><INPUT type="button" value="     Click me!     " '+
                     'onClick="alert(''Did you think it will not work?'')"></FORM>';
           exit;
         end;
         if SaveMe is TImage then begin
           bmp         := TBitmap.Create;
           bmp.Height  := TImage(SaveMe).Height;
           bmp.Width   := TImage(SaveMe).Width;
           bmp.Canvas.Draw(0,0, TImage(SaveMe).Picture.Bitmap);
           imgfilename := RichView1.SavePicture(SaveFormat,Path,bmp);
           OutStr      := '<CENTER><IMG src="'+imgfilename+'" alt='+
                          TImage(SaveMe).Hint+'></CENTER>';
           bmp.Free;
           exit;
         end;
       end;
   end;
end;
{------------------------------------------------------------------}
// Event Handler
// DoubleClick
procedure TForm1.RichView1RVDblClick(Sender: TRichView;
  AClickedWord: string; Style: Integer);
begin
   StatusBar1.SimpleText :=AClickedWord+', StyleNo='+IntToStr(Style);
end;
{------------------------------------------------------------------}
// Event Handler
// RightClick
procedure TForm1.RichView1RVRightClick(Sender: TRichView;
  AClickedWord: string; Style, X, Y: Integer);
begin
  PopupMenu1.Items[0].Caption := AClickedWord+', StyleNo='+IntToStr(Style);
  mitCopy.Enabled := RichView1.SelectionExists;
  PopupMenu1.Popup(X,Y);
end;
{------------------------------------------------------------------}
{ Popup Menu Action }
procedure TForm1.mitSelectAllClick(Sender: TObject);
begin
  RichView1.SelectAll;
  RichView1.Invalidate;
end;
{------------------------------------------------------------------}
{ Popup Menu Action }
procedure TForm1.mitCopyClick(Sender: TObject);
begin
  RichView1.CopyText;
end;
{------------------------------------------------------------------}
procedure TForm1.StatusBar1Click(Sender: TObject);
begin
{
    RichView1.AddFromNewLine('aaa', random(LAST_DEFAULT_STYLE_NO+2));
    RichView1.Add('aaa', random(LAST_DEFAULT_STYLE_NO+2));
    RichView1.AddBullet(0, ImageList1, Boolean(Random(2)));
    RichView1.FormatTail;
    RichView1.Refresh;
}
end;

initialization
{$I Unit1.lrs}

end.
