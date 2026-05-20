unit u_psstudio;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF Windows} Windows, {$ENDIF} LCLType, LCLIntf, LMessages,
  Math, Classes, Types, SysUtils, FileUtil, RTTIGrids, Forms, Controls, Graphics, Dialogs,
  Menus, StdCtrls, ExtCtrls, JvDesignSurface, JvDesignUtils,
  RTTI, ObjectInspector, PropEdits, PropEditUtils, ComponentEditors,
  ComponentTreeView, TypInfo, GraphPropEdits,



ComCtrls, ActnList, Buttons, AbUnzper, AbZipTyp, AbArcTyp, SynEdit, SynEditTypes, SynHighlighterPas,
SynEditSearch, SynEditMiscClasses, SynEditHighlighter, SynGutterBase,
SynGutterMarks, SynGutterLineNumber, SynGutterChanges, SynGutter,
SynGutterCodeFolding, SynEditMarkupSpecialLine, SynEditRegexSearch,
SynEditMarks, PrintersDlgs,

  uPSDisassembly,

  uCodeGenerator,
  u_consoleide,


  DBCtrls,
  DBGrids,

  uPSI_IBXConnection;

type

  { TfrmPSStudio }

  TfrmPSStudio = class(TForm)
    acDebugBreakPoint: TAction;
    acDebugDecompile: TAction;
    acDebugPause: TAction;
    acDebugReset: TAction;
    acDebugRun: TAction;
    acDebugStepInto: TAction;
    acDebugStepOver: TAction;
    acDebugSyntaxCheck: TAction;
    acEditCopy: TAction;
    acEditCut: TAction;
    acEditPaste: TAction;
    acEditRedo: TAction;
    acEditUndo: TAction;
    acFileExit: TAction;
    acFileNew: TAction;
    acFileOpen: TAction;
    acFilePrint: TAction;
    acFileRecent: TAction;
    acFileSave: TAction;
    acFileSaveAs: TAction;
    ActionList1: TActionList;
    edtFormName: TEdit;
    ImageListClassic: TImageList;
    JvDesignPanel1: TJvDesignPanel;
    Label1: TLabel;
    pgcLeft: TPageControl;
    pgcCenter: TPageControl;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    pnlCodeTree: TPanel;
    pashighlighter: TSynPasSyn;
    pnlInsp: TPanel;
    PopupMenu1: TPopupMenu;
    PropertyGrid: TOIPropertyGrid;
    Active1: TMenuItem;
    RadioGroup1: TRadioGroup;
    SelectButton1: TToolButton;
    SelectButton2: TToolButton;
    SelectButton3: TToolButton;
    SelectButton4: TToolButton;
    tbarDataControls1: TToolBar;
    tbarStandard1: TToolBar;
    tbarDataControls: TToolBar;
    tbtnDBNavigator: TToolButton;
    tbtnDBGrid: TToolButton;
    tbtnDBNavigator1: TToolButton;
    tsDataAccess: TTabSheet;
    tsDataControls: TTabSheet;
    tsDialogs: TTabSheet;
    tbtnMainMenu: TToolButton;
    csDesigning1: TMenuItem;
    DelphiSelector1: TMenuItem;
    File1: TMenuItem;
    Grid1: TMenuItem;
    tbtnEdit: TToolButton;
    ImageList1: TImageList;
    tbtnPopupMenu: TToolButton;
    MainMenu1: TMainMenu;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    OpenDialog: TOpenDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    tbtnLabel: TToolButton;
    Rules1: TMenuItem;
    Save1: TMenuItem;
    SaveDialog: TSaveDialog;
    SelectButton: TToolButton;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    tsStandard: TTabSheet;
    tbarAdditional: TToolBar;
    tsAdditional: TTabSheet;
    tsCommonControls: TTabSheet;
    tsOInspector: TTabSheet;
    tsCodeTree: TTabSheet;
    ToolBar2: TToolBar;
    ToolBar3: TToolBar;
    ToolBar4: TToolBar;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    ToolButton22: TToolButton;
    ToolButton23: TToolButton;
    ToolButton24: TToolButton;
    ToolButton25: TToolButton;
    ToolButton26: TToolButton;
    ToolButton27: TToolButton;
    ToolButton28: TToolButton;
    ToolButton29: TToolButton;
    ToolButton30: TToolButton;
    ToolButton31: TToolButton;
    ToolButton32: TToolButton;
    ToolButton33: TToolButton;
    tsDesign: TTabSheet;
    tsEditor: TTabSheet;
    tbarStandard: TToolBar;
    tbtnButton: TToolButton;
    tbtnRadioGroup: TToolButton;
    tbtnCheckGroup: TToolButton;
    tbtnPanel: TToolButton;
    tbtnFrame: TToolButton;
    tbtnActionList: TToolButton;
    tbtnMemo: TToolButton;
    tbtnToggleBox: TToolButton;
    tbtnCheckBox: TToolButton;
    tbtnRadioButton: TToolButton;
    tbtnListBox: TToolButton;
    tbtnComboBox: TToolButton;
    tbtnScrollBar: TToolButton;
    tbtnGroupBox: TToolButton;
    VSSelector1: TMenuItem;
    WindowProcHook1: TMenuItem;
    procedure acDebugBreakPointExecute(Sender: TObject);
    procedure acDebugDecompileExecute(Sender: TObject);
    procedure acDebugPauseExecute(Sender: TObject);
    procedure acDebugResetExecute(Sender: TObject);
    procedure acDebugRunExecute(Sender: TObject);
    procedure acDebugStepIntoExecute(Sender: TObject);
    procedure acDebugStepOverExecute(Sender: TObject);
    procedure acDebugSyntaxCheckExecute(Sender: TObject);
    procedure acFileNewExecute(Sender: TObject);
    procedure acFileOpenExecute(Sender: TObject);
    procedure acFileSaveAsExecute(Sender: TObject);
    procedure acFileSaveExecute(Sender: TObject);
    procedure Active1Click(Sender: TObject);
    procedure csDesigning1Click(Sender: TObject);
    procedure edtFormNameChange(Sender: TObject);
    procedure edtFormNameExit(Sender: TObject);
    procedure edtFormNameKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure JvDesignPanel1Change(Sender: TObject);
    procedure JvDesignPanel1DblClick(Sender: TObject);
    procedure JvDesignSurface1SelectionChange(Sender: TObject);
    procedure Rules1Click(Sender: TObject);
    procedure JvDesignPanel1GetAddClass(Sender: TObject; var ioClass: String);
    procedure JvDesignPanelPaint(Sender: TObject);
    procedure PaletteButtonClick(Sender: TObject);
    procedure tsCodeTreeContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure TIPropertyGrid1Modified(Sender: TObject);
    procedure ToolButton33Click(Sender: TObject);
    procedure tsEditorShow(Sender: TObject);

  private
    { private declarations }
    FFileName: string;
    procedure SetObjectInspectorRoot(AComponent: TComponent);
  public
    { public declarations }
    FStdFormTemplateFile: string;
    FFormName: string;

    DesignClass: string;
    StickyClass: Boolean;

    TheObjectInspector: TObjectInspectorDlg;
    ThePropertyEditorHook: TPropertyEditorHook;
    Selection: TPersistentSelectionList;

    procedure OnControlDoubleClick(Sender: TObject; AControl: TControl);
    procedure JumpToControlEvent(AControl: TControl; Editor: TSynEdit);
    procedure PropertyGridOnModified(Sender: TObject);

    procedure OpenFileSilent(AFileName: string);

    procedure  TIPropertyGrid1EditingDone(Sender: TObject);
    procedure  TIPropertyGrid1EditorFilter(Sender: TObject;
      aEditor: TPropertyEditor; var aShow: boolean);
  protected
    function GetOwner: TPersistent; override;


  end;

const
  cClasses: array[0..155] of string = ( '',
                                          'TMainMenu', 'TPopupMenu', 'TButton', 'TLabel',
                                          'TEdit', 'TMemo', 'TToggleBox', 'TCheckBox',
                                          'TRadioButton', 'TListBox', 'TComboBox', 'TScrollBar',
                                          'TGroupBox', 'TRadioGroup', 'TCheckGroup', 'TPanel',
                                          'TFrame', 'ActionList', '', '', '', '',
                                          '', '', '', '', '', '', '', '', '', '', '',

                                          //Additional
                                          'TBitBtn', 'TSeedButton', 'TStaticText', 'TImage',
                                          'TShape', 'TBevel', 'TPaintBox',
                                          '', '', '', '', '', '', '', '', '', '',
                                          '', '', '', '', '', '', '', '', '', '',
                                          '', '', '',

                                          //Common Controls
                                          '', '', '', '', '', '', '', '', '', '',
                                          '', '', '', '', '', '', '', '', '', '',
                                          '', '', '', '', '', '', '', '', '', '',

                                          //Dialogs
                                          'TOpenDialog', '', '', '', '', '', '', '', '', '',
                                          '', '', '', '', '', '', '', '', '', '',
                                          '', '', '', '', '', '', '', '', '', '',

                                          //Data Controls  Tag-begin = 124
                                          'TDBNavigator', '', '', '', '', '', '', '', '', '',
                                          '', '', '', '', '', '', '', '', '', '',
                                          '', '', '', '', '', '', '', '', 'TDBGrid', '',

                                          //
                                          'TBitBtn', //Tag=30 //Register additional
                                          'TIBXConnection'
                                          );


var
  PSStudio: TfrmPSStudio;
  LocalConsoleIDE: TfrmConsoleIDE;

implementation

uses
  JvDesignImp;
{$R *.lfm}


function IsValidFormName(const S: string): Boolean;
var
  i: Integer;
begin
  Result := False;

  if S = '' then Exit;

  // erstes Zeichen
  if not (S[1] in ['A'..'Z', 'a'..'z', '_']) then Exit;

  // restliche Zeichen
  for i := 2 to Length(S) do
    if not (S[i] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) then Exit;

  Result := True;
end;

{ TfrmPSStudio }

procedure TfrmPSStudio.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  i: Integer;
  Comp: TPersistent;
begin
  if Key = VK_DELETE then
  begin
    for i := 0 to JvDesignPanel1.Surface.Count - 1 do
    begin
      Comp := JvDesignPanel1.Surface.Selection[i];

      if (Comp is TComponent) and
         (TComponent(Comp).Name = 'pnlDesign') then
      begin
        // Prevent deletion by default
        Key := 0;

        // Ask the user
        if MessageDlg('Do you really want to delete the main form?',
                      mtWarning, [mbYes, mbNo], 0) = mrYes then
        begin
          Key := VK_DELETE;
          OpenFileSilent(FStdFormTemplateFile);
        end
        else
        begin
          // User canceled → do nothing
        end;

        Exit; // RootControl found → exit loop
      end;
    end;
  end;
end;

procedure TfrmPSStudio.FormShow(Sender: TObject);
begin
  pgcLeft.ActivePage := tsOInspector;
  //pgcCenter.ActivePage := tsDesign;
  //OpenFileSilent(FStdFormTemplateFile);
  //LocalConsoleIDE.ed.Modified := false;
end;

procedure TfrmPSStudio.JvDesignSurface1SelectionChange(Sender: TObject);
var
  i: Integer;
  APersistent: TPersistent;
  RootCtrl: TComponent;
  AName: string;
  ArrayLength: Integer;
begin
  ArrayLength := Length(JvDesignPanel1.Surface.Selected);
  if ArrayLength = 0 then
    Exit;

  RootCtrl := JvDesignPanel1.FindComponent('pnlDesign');

  APersistent := TPersistent(JvDesignPanel1.Surface.Selected[0]);

  // Safety: nur weiter wenn es wirklich eine Komponente ist
  if not (APersistent is TComponent) then
    Exit;

  AName := TComponent(APersistent).Name;

  //Diese Controls dürfen NICHT selektiert werden
  if (AName = 'JvDesignPanel1') or
     (AName = 'pnlFormTitle') or
     (AName = 'btnTitleMaximize') or
     (AName = 'btnTitleMinimize') or
     (AName = 'btnTitleClose') then
  begin
    Selection.Clear;
    JvDesignPanel1.Surface.ClearSelection;

    //RootCtrl auch im Designer selektieren
    if Assigned(RootCtrl) then
    begin
      JvDesignPanel1.Surface.Selector.AddToSelection(TControl(RootCtrl));
      //Sync mit Inspector
      Selection.Add(RootCtrl);
      ThePropertyEditorHook.LookupRoot := RootCtrl;
      TheObjectInspector.Selection := Selection;
      TheObjectInspector.RefreshSelection;
      PropertyGrid.Selection := Selection;
      PropertyGrid.Refresh;
      JvDesignPanel1.Invalidate;
      Exit;
    end;
  end;

  //Erlaubte Selection → Object Inspector aktualisieren
  if JvDesignPanel1.Surface.Count > 0 then
  begin
    Selection.Clear;
    ThePropertyEditorHook.LookupRoot :=
      JvDesignPanel1.Surface.Selection[0];

    for i := 0 to JvDesignPanel1.Surface.Count - 1 do
      if not (IsNonVisualComponent(JvDesignPanel1.Surface.Selection[i].ClassName)) then
        Selection.Add(JvDesignPanel1.Surface.Selection[i])
      else begin
        if (TComponent(JvDesignPanel1.Surface.Selection[i]) is TIBXConnection) then
          Selection.Add((JvDesignPanel1.Surface.Selection[i] as TIBXConnection).PSIBXConnection)
      end;

    TheObjectInspector.Selection := Selection;
    TheObjectInspector.RefreshSelection;

    PropertyGrid.Selection := Selection;
  end
  else
  begin
    SetObjectInspectorRoot(RootCtrl);
  end;
end;

procedure TfrmPSStudio.csDesigning1Click(Sender: TObject);
begin
  JvDesignPanel1.Active := false;
  if WindowProcHook1.Checked then
    JvDesignPanel1.Surface.MessengerClass := TJvDesignWinControlHookMessenger
  else
    JvDesignPanel1.Surface.MessengerClass := TJvDesignDesignerMessenger;
  JvDesignPanel1.Active := true;
  JvDesignPanel1.Invalidate;
end;

procedure TfrmPSStudio.edtFormNameChange(Sender: TObject);
var
  TitlePanel: TPanel;
begin
  FFormName := edtFormName.Text;
  TitlePanel := TPanel(JvDesignPanel1.FindComponent('pnlFormTitle'));

  if TitlePanel <> nil then
    TitlePanel.Caption := edtFormName.Text;
end;

procedure TfrmPSStudio.edtFormNameExit(Sender: TObject);
begin
  if (not IsValidFormName(edtFormName.Text)) then
  begin
    edtFormName.Text := 'Form1';
    FFormName := edtFormName.Text;
  end;
end;

procedure TfrmPSStudio.edtFormNameKeyPress(Sender: TObject; var Key: Char);
begin
  // erlaubte Zeichen
  if not (Key in ['A'..'Z', 'a'..'z', '0'..'9', '_', #8]) then
    Key := #0;

  // erstes Zeichen darf keine Zahl sein
  if (edtFormName.SelStart = 0) and (Key in ['0'..'9']) then
    Key := #0;
end;

procedure TfrmPSStudio.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  LocalConsoleIDE.acDebugResetExecute(nil); //terminate any running script
  if LocalConsoleIDE.SaveCheck then //check if script changed and not yet saved
  begin
    {$IFDEF SCRIPTER_EMBEDDED}
      CloseAction := caHide;
    {$ELSE}
      CloseAction := caFree;
    {$ENDIF}
  end
  else
    CloseAction := caNone;
end;

procedure ExtractDataDirectory;
var
  DataStream: TResourceStream;
  MemoryStream: TMemoryStream;
  DataDir: string;
  AbUnZipper: TAbUnZipper;
begin
  AbUnZipper := TAbUnZipper.Create(nil);
  DataDir := ExtractFilePath(Application.ExeName) + Pathdelim + 'data';
  if not DirectoryExists(DataDir) then
  begin
    DataStream := TResourceStream.Create(HInstance, 'DATA', RT_RCDATA);
    MemoryStream := TMemoryStream.Create;
    try
      // Load the ZIP file from the resource into memory
      MemoryStream.LoadFromStream(DataStream);
      MemoryStream.Position := 0;

      // Extract the contents of the ZIP file to the target directory.
      AbUnZipper.Stream := MemoryStream;
      AbUnZipper.ExtractOptions := [eoCreateDirs, eoRestorePath]; // Create directories
      AbUnZipper.BaseDirectory := ExtractFilePath(Application.ExeName);
      AbUnZipper.ExtractFiles('*.*');
    finally
      MemoryStream.Free;
      DataStream.Free;
      AbUnZipper.Free;
    end;
  end;
end;

function FirstRun: boolean;
begin
  result := not DirectoryExists(ExtractFilePath(Application.ExeName) + Pathdelim + 'data');
end;

procedure TfrmPSStudio.FormCreate(Sender: TObject);
var RootCtrl: TComponent;
begin;
  if FirstRun then
    ExtractDataDirectory;

  FFileName := '';

  JvDesignPanel1.Surface.OnSelectionChange := @JvDesignSurface1SelectionChange;

  FStdFormTemplateFile := ExtractFilePath(Application.ExeName) +
       'data' + PathDelim + 'PSStudio' + PathDelim + 'FormTemplates' + PathDelim + 'StdTemplate.cfrm';

  // create the PropertyEditorHook (the interface to the properties)
  ThePropertyEditorHook:=TPropertyEditorHook.Create(nil);

  Selection:=TPersistentSelectionList.Create;

  // create the ObjectInspector
  TheObjectInspector := TObjectInspectorDlg.Create(pnlInsp);
  TheObjectInspector.Parent := pnlInsp;

  TheObjectInspector.PropertyEditorHook := ThePropertyEditorHook;
  TheObjectInspector.Align := alClient;

  // create the PropertyGrid
  PropertyGrid:=TOIPropertyGrid.CreateWithParams(Self,ThePropertyEditorHook, AllTypeKinds,25);

  // select the Form1 in the ObjectInspector
  TheObjectInspector.Show;         // For some reason this is not shown otherwise
  OpenDialog.InitialDir := ExtractFilePath(Application.ExeName);
  SaveDialog.InitialDir := OpenDialog.InitialDir;

  JvDesignPanel1.Surface.Active := true;

  JvDesignPanel1.Surface.OnControlDblClick := @OnControlDoubleClick;

  LocalConsoleIDE := TfrmConsoleIDE.Create(tsEditor);
  LocalConsoleIDE.Parent := tsEditor;
  LocalConsoleIDE.BorderStyle := bsNone;
  LocalConsoleIDE.pnlTools.Visible := false;
  LocalConsoleIDE.Align := alClient;
  LocalConsoleIDE.Visible := true;

  PropertyGrid.OnModified := @PropertyGridOnModified;
  OpenFileSilent(FStdFormTemplateFile);

  PropertyGrid.OnEditorFilter := @TIPropertyGrid1EditorFilter;

  RootCtrl := JvDesignPanel1.FindComponent('pnlDesign');
  if Assigned(RootCtrl) then
  begin
    JvDesignPanel1.Surface.Selector.AddToSelection(TControl(RootCtrl));
    Selection.Add(RootCtrl);
    ThePropertyEditorHook.LookupRoot := RootCtrl;
    TheObjectInspector.Selection := Selection;
    TheObjectInspector.RefreshSelection;
    PropertyGrid.Selection := Selection;
    PropertyGrid.Refresh;
    JvDesignPanel1.Invalidate;
    Exit;
  end else
    SetObjectInspectorRoot(JvDesignPanel1);
end;


procedure TfrmPSStudio.TIPropertyGrid1EditorFilter(Sender: TObject;
  aEditor: TPropertyEditor; var aShow: boolean);
var
  Prop: PPropInfo;
  Comp: TObject;
begin
    //Comp := PropertyGrid.Selection[0];
    //Prop := PropertyGrid.pr

    //if Assigned(Prop) and (Prop^.Name = 'Lines') then
    //begin
      // User klickte Button oder bearbeitete die Lines-Property
      //ShowMessage('EditorFilter');
      //ShowMemoLinesEditor(TMemo(Comp));
    //end;
end;

procedure TfrmPSStudio.TIPropertyGrid1EditingDone(Sender: TObject);
var
  Comp: TObject;
begin
{  if not Assigned(LastEditor) then Exit;

  if LastEditor.GetName = 'Lines' then
  begin
    Comp := LastEditor.GetComponent(0);

    if Comp is TMemo then
      ShowMessage('EditingDone');
      //ShowMemoLinesEditor(TMemo(Comp));
  end; }
end;

procedure EnsureEventExists(AControl: TControl; Lines: TStrings);
var EventName: string;
begin
  EventName := GetEventHandlerName(AControl);

  if not EventExists(Lines, EventName) then
    GenerateEvent(AControl, TStringList(Lines));
end;

procedure TfrmPSStudio.OnControlDoubleClick(Sender: TObject; AControl: TControl);
var
  EventName: string;
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Assign(LocalConsoleIDE.ed.Lines);

    // ignorieren
    if AControl.Name = 'JvDesignPanel1' then Exit;

    // CLOSE
    if AControl.Name = 'btnTitleClose' then
    begin
      EventName := 'FormClose';

      GenerateFormClose(Lines);
      AssignSpecialEvent(Lines,
        FFormName + '.OnClose := @FormClose;',
        FFormName);
    end

    // CREATE
    else if (AControl.Name = 'pnlDesign') or
            (AControl.Name = 'pnlFormTitle') then
    begin
      EventName := 'FormCreate';

      GenerateFormCreate(Lines);
      AssignSpecialEvent(Lines,
        FFormName + '.OnCreate := @FormCreate;',
        FFormName);
    end

    // STATE
    else if (AControl.Name = 'btnTitleMaximize') or
            (AControl.Name = 'btnTitleMinimize') then
    begin
      EventName := 'FormStateChange';

      GenerateFormStateChange(Lines);
      AssignSpecialEvent(Lines,
        FFormName + '.OnWindowStateChange := @FormStateChange;',
        FFormName);
    end

    // STANDARD
    else
    begin
      EventName := GetEventHandlerName(AControl);

      if not EventExists(Lines, EventName) then
      begin
        GenerateEvent(AControl, Lines);
        AssignEventToControl(AControl, Lines);
      end;
    end;

    // PSSIDE Lines aktualisieren
    LocalConsoleIDE.ed.Lines.Assign(Lines);

    // springen zum Event
    pgcCenter.ActivePage := tseditor;
    JumpToEventInEditor(LocalConsoleIDE.ed, EventName);

  finally
    Lines.Free;
  end;
end;

procedure TfrmPSStudio.PropertyGridOnModified(Sender: TObject);
begin
  try
    JvDesignPanel1.Repaint;
  except
    on E: Exception do
    begin
      // optional loggen
      ShowMessage(E.Message);
    end;
  end;
end;

procedure TfrmPSStudio.FormDestroy(Sender: TObject);
begin
  // PropertyEditorHook freigeben
  if Assigned(ThePropertyEditorHook) then
  begin
    ThePropertyEditorHook.Free;
    ThePropertyEditorHook := nil;
  end;

  // ObjectInspector freigeben
  if Assigned(TheObjectInspector) then
  begin
    TheObjectInspector.Free;
    TheObjectInspector := nil;
  end;

  // Selection-Liste freigeben
  if Assigned(Selection) then
  begin
    Selection.Free;
    Selection := nil;
  end;
end;


procedure TfrmPSStudio.JvDesignPanel1Change(Sender: TObject);
begin

end;

procedure TfrmPSStudio.JvDesignPanel1DblClick(Sender: TObject);
begin
  ShowMessage('DblClick');
end;

procedure TfrmPSStudio.JumpToControlEvent(AControl: TControl; Editor: TSynEdit);
var
  EventName: string;
begin
  if AControl = nil then Exit;

  EventName := GetEventHandlerName(AControl);
  if EventName = '' then Exit; // kein Default-Event

  // Event existiert noch nicht → Generator aufrufen
  //if not EventExists(Editor.Lines, EventName) then
    //GenerateCodeFromDesigner(JvDesignPanel1, TStringList(PSSIDE.ed.Lines), FFormName);

  // Springen in Editor
  JumpToEventInEditor(LocalConsoleIDE.ed, EventName);
end;

function TfrmPSStudio.GetOwner: TPersistent;
begin
  // this Form1 is the LookupRoot => GetOwner must be nil
  // see GetLookupRootForComponent
  Result:=nil;
end;

procedure TfrmPSStudio.SetObjectInspectorRoot(AComponent: TComponent);
begin
  Selection.Clear;

  ThePropertyEditorHook.LookupRoot := AComponent;
  Selection.Add(AComponent);

  TheObjectInspector.Selection := Selection;
  TheObjectInspector.RefreshSelection;
end;

procedure TfrmPSStudio.Active1Click(Sender: TObject);
begin
  JvDesignPanel1.Active := Active1.Checked;
  JvDesignPanel1.Invalidate;
end;

procedure TfrmPSStudio.acDebugRunExecute(Sender: TObject);
begin
  pgcCenter.ActivePage := tsEditor;
  GenerateCodeFromDesigner(JvDesignPanel1, TStringList(LocalConsoleIDE.ed.Lines), Trim(edtFormName.Text));
  LocalConsoleIDE.acDebugRunExecute(self);
end;

procedure TfrmPSStudio.acDebugPauseExecute(Sender: TObject);
begin
  LocalConsoleIDE.acDebugPauseExecute(self);
end;

procedure TfrmPSStudio.acDebugBreakPointExecute(Sender: TObject);
begin
  LocalConsoleIDE.acDebugBreakPointExecute(self);
end;

procedure TfrmPSStudio.acDebugDecompileExecute(Sender: TObject);
begin
  LocalConsoleIDE.acDebugDecompileExecute(self);
end;

procedure TfrmPSStudio.acDebugResetExecute(Sender: TObject);
begin
  LocalConsoleIDE.acDebugResetExecute(self);
end;

procedure TfrmPSStudio.acDebugStepIntoExecute(Sender: TObject);
begin
  LocalConsoleIDE.acDebugStepIntoExecute(self);
end;

procedure TfrmPSStudio.acDebugStepOverExecute(Sender: TObject);
begin
   LocalConsoleIDE.acDebugStepOverExecute(self);
end;

procedure TfrmPSStudio.acDebugSyntaxCheckExecute(Sender: TObject);
begin
  LocalConsoleIDE.acDebugSyntaxCheckExecute(self);
end;

procedure TfrmPSStudio.acFileNewExecute(Sender: TObject);
begin
  if not LocalConsoleIDE.SaveCheck then
    exit;
  JvDesignPanel1.Clear;
  LocalConsoleIDE.ed.Clear;
  edtFormName.Text := 'Form1';
  OpenFileSilent(FStdFormTemplateFile);
  GenerateCodeFromDesigner(JvDesignPanel1, TStringList(LocalConsoleIDE.ed.Lines), Trim(edtFormName.Text));
  FFileName := '';
end;

procedure TfrmPSStudio.OpenFileSilent(AFileName: string);
var
  BaseName, CfrmFile, RopsFile: string;
  SL: TStringList;
  i: Integer;
  Line, FormName: string;
  RootCtrl: TComponent;
begin
  LocalConsoleIDE.FErrorLine := -1;
  LocalConsoleIDE.ed.Repaint;

    BaseName := ChangeFileExt(AFilename, '');

    // Designer laden
    CfrmFile := BaseName + '.cfrm';
    if FileExists(CfrmFile) then
      JvDesignPanel1.LoadFromFile(CfrmFile);

    // Pascal-Code laden (.ROPS)
    RopsFile := BaseName + '.ROPS';
    if FileExists(RopsFile) then
    begin
      SL := TStringList.Create;
      try
        SL.LoadFromFile(RopsFile);
        LocalConsoleIDE.ed.Lines.Assign(SL);

        // Formularname aus <DESIGNER-VARS> auslesen
        FormName := '';
        i := 0;
        while (i < SL.Count) and (FormName = '') do
        begin
          Line := Trim(SL[i]);

          if Pos('//<DESIGNER-VARS-BEGIN>', Line) > 0 then
          begin
            // Suche nach "var" und erste TForm-Variable
            Inc(i);
            while (i < SL.Count) and (Trim(SL[i]) <> '//<DESIGNER-VARS-END>') and (FormName = '') do
            begin
              Line := Trim(SL[i]);
              if Pos(': TForm;', Line) > 0 then
              begin
                FormName := Trim(Copy(Line, 1, Pos(': TForm;', Line)-1));
                Break;
              end;
              Inc(i);
            end;
          end;

          Inc(i);
        end;

        if FormName <> '' then
        begin
          edtFormName.Text := FormName;
          FFormName := edtFormName.Text;
        end;

      finally
        SL.Free;
      end;
    end;

    Caption := 'PSStudio - ' + ExtractFileName(BaseName);
end;

procedure TfrmPSStudio.acFileOpenExecute(Sender: TObject);
begin
  if LocalConsoleIDE.SaveCheck then
    if OpenDialog.Execute then
    begin
      OpenFileSilent(OpenDialog.FileName);
      FFileName := OpenDialog.FileName;
    end;
end;

procedure TfrmPSStudio.acFileSaveAsExecute(Sender: TObject);
var BaseName, CfrmFile, RopsFile: string;
begin
  if SaveDialog.Execute then
  begin
    // Basisname (ohne Extension)
    BaseName := ChangeFileExt(SaveDialog.Filename, '');

    // Designer-Datei
    CfrmFile := BaseName + '.cfrm';
    JvDesignPanel1.SaveToFile(CfrmFile);

    // Pascal-Code-Datei (.ROPS)
    RopsFile := BaseName + '.ROPS';
    LocalConsoleIDE.ed.Lines.SaveToFile(RopsFile); // SynEdit Lines speichern
    LocalConsoleIDE.ed.Modified := false;

    Caption := 'PascalScript Studio' + ExtractFileName(BaseName);

    FFileName := SaveDialog.FileName;
  end;
end;

procedure TfrmPSStudio.acFileSaveExecute(Sender: TObject);
var
  BaseName, CfrmFile, RopsFile: string;
begin
  // Prüfen, ob ein Dateiname bereits existiert
  if FFileName = '' then
  begin
    // Kein Dateiname → Save As verwenden
    acFileSaveAsExecute(Sender);
    Exit;
  end;

  // Basisname ohne Extension
  BaseName := ChangeFileExt(FFileName, '');

  // Designer-Datei speichern
  CfrmFile := BaseName + '.cfrm';
  JvDesignPanel1.SaveToFile(CfrmFile);

  // Pascal-Code-Datei (.ROPS) speichern
  RopsFile := BaseName + '.ROPS';
  LocalConsoleIDE.ed.Lines.SaveToFile(RopsFile);
  LocalConsoleIDE.ed.Modified := false;

  // Caption aktualisieren
  Caption := 'PSStudio - ' + ExtractFileName(BaseName);
end;

procedure TfrmPSStudio.Rules1Click(Sender: TObject);
begin
    if Rules1.Checked then
  begin
    JvDesignPanel1.Color := clWhite;
    JvDesignPanel1.DrawRules := true;
    JvDesignPanel1.OnPaint := nil;
  end else
  begin
    JvDesignPanel1.Color := clBtnFace;
    JvDesignPanel1.DrawRules := false;
    JvDesignPanel1.OnPaint := @JvDesignPanelPaint;
  end;
  JvDesignPanel1.Invalidate;
end;

procedure TfrmPSStudio.JvDesignPanel1GetAddClass(Sender: TObject; var ioClass: String);
begin
  ioClass := DesignClass;
  if not StickyClass then
  begin
    DesignClass := '';
    SelectButton.Down  := true;
    SelectButton1.Down  := true;
    //SelectButton2.Down  := true;
  end;
  LocalConsoleIDE.ed.Modified := true;
end;

procedure TfrmPSStudio.JvDesignPanelPaint(Sender: TObject);
begin
  with JvDesignPanel1 do
     DesignPaintGrid(Canvas, ClientRect, Color);
end;

{procedure TfrmPSStudio.PaletteButtonClick(Sender: TObject);
begin
// StickyClass := (GetKeyState(VK_SHIFT) < 0);
    StickyClass := False;
   DesignClass := cClasses[TControl(Sender).Tag];
end;}

procedure TfrmPSStudio.PaletteButtonClick(Sender: TObject);
var NonVisual: boolean;
    CompName: string;
begin
  // StickyClass aktivieren, wenn Shift gedrückt gehalten wird
  StickyClass := (GetKeyState(VK_SHIFT) < 0);

  // DesignClass anhand des Tags des Buttons setzen
  DesignClass := cClasses[TControl(Sender).Tag];

  NonVisual := IsNonVisualComponent(DesignClass);

  CompName := JvDesignPanel1.Components[JvDesignPanel1.ComponentCount - 1].Name;
end;

procedure TfrmPSStudio.tsCodeTreeContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin

end;

procedure TfrmPSStudio.TIPropertyGrid1Modified(Sender: TObject);
var ctrl: TControl;
begin
  // Prüfen, ob eine Selection existiert
  if (JvDesignPanel1.ComponentCount > 1) and (JvDesignPanel1.Surface.Selected <> nil) then
  begin
    ctrl := TControl(JvDesignPanel1.Surface.Selection[0]);
    // Werte neu zuweisen, damit Änderungen übernommen werden
    ctrl.Left := ctrl.Left;
    ctrl.Top := ctrl.Top;
    ctrl.Width := ctrl.Width;
    ctrl.Height := ctrl.Height;
    ctrl.Refresh;
  end;
end;

procedure TfrmPSStudio.ToolButton33Click(Sender: TObject);
begin
  ShowMessage('JvDesignPanel1.Components[0].Name = ' + JvDesignPanel1.Components[0].Name);
  ShowMessage('JvDesignPanel1.ComponentCount = ' + IntToStr(JvDesignPanel1.ComponentCount));
  ShowMessage('Surface.ComponentCount = ' + IntToStr(JvDesignPanel1.Surface.ComponentCount));
  ShowMessage('Selection.Count = ' + IntToStr(Selection.Count));
end;

procedure TfrmPSStudio.tsEditorShow(Sender: TObject);
begin
  GenerateCodeFromDesigner(JvDesignPanel1, TStringList(LocalConsoleIDE.ed.Lines), Trim(edtFormName.Text));
end;

initialization

  //Standard
  RegisterClass(TMainMenu);
  RegisterClass(TPopupMenu);
  RegisterClass(TButton);
  RegisterClass(TLabel);
  RegisterClass(TEdit);
  RegisterClass(TMemo);
  RegisterClass(TToggleBox);
  RegisterClass(TCheckBox);
  RegisterClass(TRadioButton);
  RegisterClass(TListBox);
  RegisterClass(TComboBox);
  RegisterClass(TScrollBar);
  RegisterClass(TGroupBox);
  RegisterClass(TRadioGroup);
  RegisterClass(TCheckGroup);
  RegisterClass(TPanel);
  RegisterClass(TFrame);
  RegisterClass(TActionList);

  //Additional
  RegisterClass(TBitBtn);

  //Common Controls

  //Dialogs
  RegisterClass(TOpenDialog);

  //Data Controls
  RegisterClass(TDBNavigator);
  RegisterClass(TDBGrid);

  RegisterClass(TPSIBXConnection);
  RegisterClass(TIBXConnection);
end.

