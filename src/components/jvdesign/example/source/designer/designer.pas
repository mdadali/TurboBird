unit designer;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF Windows} Windows, {$ENDIF} LCLType, LCLIntf, LMessages,
  Math, Classes, SysUtils, FileUtil, RTTIGrids, Forms, Controls, Graphics, Dialogs,
  Menus, StdCtrls, ExtCtrls, JvDesignSurface, JvDesignUtils,
  RTTI, ObjectInspector, PropEdits, PropEditUtils, GraphPropEdits,



ComCtrls, ActnList, SynEdit, SynEditTypes, SynHighlighterPas, SynEditSearch,
SynEditMiscClasses, SynEditHighlighter, SynGutterBase, SynGutterMarks,
SynGutterLineNumber, SynGutterChanges, SynGutter, SynGutterCodeFolding,
SynEditMarkupSpecialLine, SynEditRegexSearch, SynEditMarks, PrintersDlgs,

  ide_editor;
type

  { TMainForm }

  TMainForm = class(TForm)
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
    PageControl2: TPageControl;
    PageControl3: TPageControl;
    Panel3: TPanel;
    Panel5: TPanel;
    pashighlighter: TSynPasSyn;
    pnlInsp: TPanel;
    PropertyGrid: TOIPropertyGrid;
    Active1: TMenuItem;
    ButtonButton: TToolButton;
    csDesigning1: TMenuItem;
    DelphiSelector1: TMenuItem;
    File1: TMenuItem;
    Grid1: TMenuItem;
    ImageButton: TToolButton;
    ImageList1: TImageList;
    LabelButton: TToolButton;
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
    PanelButton: TToolButton;
    Rules1: TMenuItem;
    Save1: TMenuItem;
    SaveDialog: TSaveDialog;
    SelectButton: TToolButton;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
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
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    VSSelector1: TMenuItem;
    WindowProcHook1: TMenuItem;
    procedure acDebugRunExecute(Sender: TObject);
    procedure acFileNewExecute(Sender: TObject);
    procedure acFileOpenExecute(Sender: TObject);
    procedure acFileSaveExecute(Sender: TObject);
    procedure Active1Click(Sender: TObject);
    procedure csDesigning1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure JvDesignPanel1Change(Sender: TObject);
    procedure JvDesignPanel1SelectionChange(Sender: TObject);
    procedure Rules1Click(Sender: TObject);
    procedure JvDesignPanel1GetAddClass(Sender: TObject; var ioClass: String);
    procedure JvDesignPanelPaint(Sender: TObject);
    procedure PaletteButtonClick(Sender: TObject);
    procedure TIPropertyGrid1Modified(Sender: TObject);
    procedure ToolButton33Click(Sender: TObject);
    procedure tsEditorShow(Sender: TObject);

  private
    { private declarations }

     procedure SetObjectInspectorRoot(AComponent: TComponent);
  public
    { public declarations }
    FFormName: string;

    DesignClass: string;
    StickyClass: Boolean;

    TheObjectInspector: TObjectInspectorDlg;
    ThePropertyEditorHook: TPropertyEditorHook;
    Selection: TPersistentSelectionList;

     procedure PropertyGridOnModified(Sender: TObject);
     procedure GenCodeFromDesigner(AJvDesignPanel: TJvDesignPanel; AStringList: TStringList; AFormName: string);

  protected
    function GetOwner: TPersistent; override;


  end;

const
  cClasses: array[0..18] of string = ( '', 'TMainMenu', 'TPopupMenu', 'TButton',
                                          'TLabel', 'TEdit', 'TMemo', 'TToggleBox',
                                          'TCheckBox', 'TRadioButton', 'TListBox',
                                          'TComboBox', 'TScrollBar', 'TGroupBox',
                                          'TRadioGroup', 'TCheckGroup', 'TPanel',
                                          'TFrame', 'ActionList');



var
  MainForm: TMainForm;

implementation

uses
  JvDesignImp;
{$R *.lfm}


{ TMainForm }

procedure TMainForm.JvDesignPanel1SelectionChange(Sender: TObject);
var i: integer;
begin
  if JvDesignPanel1.Surface.Count > 0 then
  begin
    Selection.Clear;
    ThePropertyEditorHook.LookupRoot := JvDesignPanel1.Surface.Selection[0];
    for i := 0 to JvDesignPanel1.Surface.Count - 1 do
      Selection.Add(JvDesignPanel1.Surface.Selection[i]);
    TheObjectInspector.Selection := Selection;
    TheObjectInspector.RefreshSelection;
    PropertyGrid.Selection := Selection;

  end else

  SetObjectInspectorRoot(JvDesignPanel1);
end;

procedure TMainForm.csDesigning1Click(Sender: TObject);
begin
  JvDesignPanel1.Active := false;
  if WindowProcHook1.Checked then
    JvDesignPanel1.Surface.MessengerClass := TJvDesignWinControlHookMessenger
  else
    JvDesignPanel1.Surface.MessengerClass := TJvDesignDesignerMessenger;
  JvDesignPanel1.Active := true;
  JvDesignPanel1.Invalidate;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // create the PropertyEditorHook (the interface to the properties)
  ThePropertyEditorHook:=TPropertyEditorHook.Create(nil);

  Selection:=TPersistentSelectionList.Create;

  // create the ObjectInspector
  //TheObjectInspector:=TObjectInspectorDlg.Create(Application);
  TheObjectInspector := TObjectInspectorDlg.Create(pnlInsp);
  TheObjectInspector.Parent := pnlInsp;

  TheObjectInspector.PropertyEditorHook := ThePropertyEditorHook;
  //TheObjectInspector.SetBounds(10,10,240,500);
  TheObjectInspector.Align := alClient;

  // create the PropertyGrid
  PropertyGrid:=TOIPropertyGrid.CreateWithParams(Self,ThePropertyEditorHook,
                                                 AllTypeKinds,25);


  {with PropertyGrid do begin
    Name:='PropertyGrid';
    Parent:=pnlInsp;
    Align:=alClient;
  end;}


  // select the Form1 in the ObjectInspector
  TheObjectInspector.Show;         // For some reason this is not shown otherwise
  OpenDialog.InitialDir := ExtractFilePath(Application.ExeName);
  SaveDialog.InitialDir := OpenDialog.InitialDir;
  JvDesignPanel1.Surface.Active := true;

  IDE := TIDE.Create(tsEditor);
  IDE.Parent := tsEditor;
  IDE.BorderStyle := bsNone;
  IDE.pnlTools.Visible := false;
  IDE.Align := alClient;
  IDE.Visible := true;

  SetObjectInspectorRoot(JvDesignPanel1);
  //JvDesignPanel1.Constraints.OnChange := nil;
  PropertyGrid.OnModified := @PropertyGridOnModified;
end;

procedure TMainForm.PropertyGridOnModified(Sender: TObject);
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

procedure TMainForm.FormDestroy(Sender: TObject);
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

procedure TMainForm.JvDesignPanel1Change(Sender: TObject);
begin

end;

function TMainForm.GetOwner: TPersistent;
begin
  // this Form1 is the LookupRoot => GetOwner must be nil
  // see GetLookupRootForComponent
  Result:=nil;
end;

procedure TMainForm.SetObjectInspectorRoot(AComponent: TComponent);
begin
  Selection.Clear;

  ThePropertyEditorHook.LookupRoot := AComponent;
  Selection.Add(AComponent);

  TheObjectInspector.Selection := Selection;
  TheObjectInspector.RefreshSelection;
end;

procedure TMainForm.Active1Click(Sender: TObject);
begin
  JvDesignPanel1.Active := Active1.Checked;
  JvDesignPanel1.Invalidate;
end;

procedure TMainForm.acDebugRunExecute(Sender: TObject);
begin
  GenCodeFromDesigner(JvDesignPanel1, TStringList(IDE.ed.Lines), Trim(edtFormName.Text));
  IDE.acDebugRunExecute(self);
end;

procedure TMainForm.acFileNewExecute(Sender: TObject);
begin
  JvDesignPanel1.Clear;
  IDE.ed.Clear;
  self.edtFormName.Text := 'NewForm';
  GenCodeFromDesigner(JvDesignPanel1, TStringList(IDE.ed.Lines), Trim(edtFormName.Text));
end;

procedure TMainForm.acFileOpenExecute(Sender: TObject);
var
  BaseName, CfrmFile, RopsFile: string;
  SL: TStringList;
  i: Integer;
  Line, FormName: string;
begin
  if OpenDialog.Execute then
  begin
    BaseName := ChangeFileExt(OpenDialog.Filename, '');

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
        IDE.ed.Lines.Assign(SL);

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
              Inc(i); // Schleife selbst hochzählen
            end;
          end;

          Inc(i);
        end;

        if FormName <> '' then
          edtFormName.Text := FormName;

      finally
        SL.Free;
      end;
    end;

    Caption := 'VF IDE - ' + ExtractFileName(BaseName);
  end;
end;

procedure TMainForm.acFileSaveExecute(Sender: TObject);
var
  BaseName, CfrmFile, RopsFile: string;
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
    IDE.ed.Lines.SaveToFile(RopsFile); // SynEdit Lines speichern

    Caption := 'VF IDE - ' + ExtractFileName(BaseName);
  end;
end;

procedure TMainForm.Rules1Click(Sender: TObject);
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

procedure TMainForm.JvDesignPanel1GetAddClass(Sender: TObject;
  var ioClass: String);
begin
  ioClass := DesignClass;
  if not StickyClass then
  begin
    DesignClass := '';
    SelectButton.Down  := true;
  end;
end;

procedure TMainForm.JvDesignPanelPaint(Sender: TObject);
begin
  with JvDesignPanel1 do
     DesignPaintGrid(Canvas, ClientRect, Color);
end;

{procedure TMainForm.PaletteButtonClick(Sender: TObject);
begin
// StickyClass := (GetKeyState(VK_SHIFT) < 0);
    StickyClass := False;
   DesignClass := cClasses[TControl(Sender).Tag];
end;}

procedure TMainForm.PaletteButtonClick(Sender: TObject);
begin
  // StickyClass aktivieren, wenn Shift gedrückt gehalten wird
  StickyClass := (GetKeyState(VK_SHIFT) < 0);

  // DesignClass anhand des Tags des Buttons setzen
  DesignClass := cClasses[TControl(Sender).Tag];
end;

{procedure TMainForm.TIPropertyGrid1Modified(Sender: TObject);
begin
  Form1.Surface.Selection[0].Left := TControl(Form1.Surface.Selection[0]).Left;
  Form1.Surface.Selection[0].Refresh;
end;}

procedure TMainForm.TIPropertyGrid1Modified(Sender: TObject);
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

procedure TMainForm.ToolButton33Click(Sender: TObject);
begin
  ShowMessage('JvDesignPanel1.Components[0].Name = ' + JvDesignPanel1.Components[0].Name);
  ShowMessage('JvDesignPanel1.ComponentCount = ' + IntToStr(JvDesignPanel1.ComponentCount));
  ShowMessage('Surface.ComponentCount = ' + IntToStr(JvDesignPanel1.Surface.ComponentCount));
end;

procedure TMainForm.tsEditorShow(Sender: TObject);
begin
  GenCodeFromDesigner(JvDesignPanel1, TStringList(IDE.ed.Lines), Trim(edtFormName.Text));
end;

procedure TMainForm.GenCodeFromDesigner(AJvDesignPanel: TJvDesignPanel; AStringList: TStringList; AFormName: string);
var
  i: Integer;
  Ctrl: TControl;
  UserCode, MainCode, UserGlobalVars: TStringList;

  // -----------------------------
  // Hilfsprozedur: Zeile hinzufügen
  // -----------------------------
  procedure Add(const S: string);
  begin
    AStringList.Add(S);
  end;

  // -----------------------------
  // Hilfsfunktion: String escapen
  // -----------------------------
  function Escape(const s: string): string;
  begin
    Result := StringReplace(s, '''', '''''', [rfReplaceAll]);
  end;

  // -----------------------------
  // Hilfsfunktion: Block extrahieren
  // -----------------------------
  function ExtractBlock(const SL: TStringList; const StartTag, EndTag: string): TStringList;
  var
    i: Integer;
    InBlock: Boolean;
  begin
    Result := TStringList.Create;
    InBlock := False;
    for i := 0 to SL.Count - 1 do
    begin
      if Pos(StartTag, SL[i]) > 0 then
      begin
        InBlock := True;
        Continue;
      end;
      if Pos(EndTag, SL[i]) > 0 then
        Break;
      if InBlock then
        Result.Add(SL[i]);
    end;
  end;

  // -----------------------------
  // Hilfsfunktion: Prüfen ob Event existiert
  // -----------------------------
  function HasEvent(const SL: TStringList; const EventName: string): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := 0 to SL.Count - 1 do
      if Pos('procedure ' + EventName, SL[i]) > 0 then
        Exit(True);
  end;

begin
  // -----------------------------
  // 1. Bestehenden Code sichern
  // -----------------------------
  UserCode := ExtractBlock(AStringList, '//<USERCODE-BEGIN>', '//<USERCODE-END>');
  MainCode := ExtractBlock(AStringList, '//<MAIN-BEGIN>', '//<MAIN-END>');
  UserGlobalVars := ExtractBlock(AStringList, '//<USER-GLOBAL-VARS-BEGIN>', '//<USER-GLOBAL-VARS-END>');

  try
    // -----------------------------
    // 2. Liste neu aufbauen
    // -----------------------------
    AStringList.Clear;

    // =====================================================
    // DESIGNER VARS
    // =====================================================
    Add('//<DESIGNER-VARS-BEGIN>');
    Add('var');
    Add('  ' + AFormName + ': TForm;');
    for i := 0 to AJvDesignPanel.ComponentCount - 1 do
    begin
      if not (AJvDesignPanel.Components[i] is TControl) then Continue;
      Ctrl := TControl(AJvDesignPanel.Components[i]);
      Add('  ' + Ctrl.Name + ': ' + Ctrl.ClassName + ';');
    end;
    Add('//<DESIGNER-VARS-END>');
    Add('');

    // =====================================================
    // USER-GLOBAL-VARS
    // =====================================================
    Add('//<USER-GLOBAL-VARS-BEGIN>');
    AStringList.AddStrings(UserGlobalVars);
    Add('//<USER-GLOBAL-VARS-END>');
    Add('');

    // =====================================================
    // USER-CODE (vor Designer-Code, damit Events gefunden werden)
    // =====================================================
    Add('//<USERCODE-BEGIN>');
    AStringList.AddStrings(UserCode);

    // fehlende Events automatisch ergänzen
    for i := 0 to AJvDesignPanel.ComponentCount - 1 do
    begin
      if not (AJvDesignPanel.Components[i] is TControl) then Continue;
      Ctrl := TControl(AJvDesignPanel.Components[i]);

      if Ctrl is TButton then
      begin
        if not HasEvent(UserCode, Ctrl.Name + '_OnClick') then
        begin
          Add('procedure ' + Ctrl.Name + '_OnClick(Sender: TObject);');
          Add('begin');
          Add('  ');
          Add('end;');
          Add('');
        end;
      end;

      if Ctrl is TEdit then
      begin
        if not HasEvent(UserCode, Ctrl.Name + '_OnChange') then
        begin
          Add('procedure ' + Ctrl.Name + '_OnChange(Sender: TObject);');
          Add('begin');
          Add('  ');
          Add('end;');
          Add('');
        end;
      end;
    end;
    Add('//<USERCODE-END>');
    Add('');

    // =====================================================
    // DESIGNER-CODE (CreateForm)
    // =====================================================
    Add('//<DESIGNER-BEGIN>');
    Add('procedure CreateNewForm;');
    Add('begin');
    Add('  ' + AFormName + ' := TForm.Create(nil);');
    Add('  ' + AFormName + '.Caption := ''' + Escape(AFormName) + ''';');
    Add('  ' + AFormName + '.Position := poDesigned;');
    Add('  ' + AFormName + '.Visible := True;');
    Add('');

    for i := 0 to AJvDesignPanel.ComponentCount - 1 do
    begin
      if not (AJvDesignPanel.Components[i] is TControl) then Continue;
      Ctrl := TControl(AJvDesignPanel.Components[i]);

      Add('  ' + Ctrl.Name + ' := ' + Ctrl.ClassName + '.Create(' + AFormName + ');');
      Add('  ' + Ctrl.Name + '.Parent := ' + AFormName + ';');
      Add('  ' + Ctrl.Name + '.Left := ' + IntToStr(Ctrl.Left) + ';');
      Add('  ' + Ctrl.Name + '.Top := ' + IntToStr(Ctrl.Top) + ';');
      Add('  ' + Ctrl.Name + '.Width := ' + IntToStr(Ctrl.Width) + ';');
      Add('  ' + Ctrl.Name + '.Height := ' + IntToStr(Ctrl.Height) + ';');

      if Ctrl is TButton then
      begin
        Add('  ' + Ctrl.Name + '.Caption := ''' + Escape(TButton(Ctrl).Caption) + ''';');
        Add('  ' + Ctrl.Name + '.OnClick := @' + Ctrl.Name + '_OnClick;');
      end;

      if Ctrl is TLabel then
        Add('  ' + Ctrl.Name + '.Caption := ''' + Escape(TLabel(Ctrl).Caption) + ''';');

      if Ctrl is TEdit then
      begin
        Add('  ' + Ctrl.Name + '.Text := ''' + Escape(TEdit(Ctrl).Text) + ''';');
        Add('  ' + Ctrl.Name + '.OnChange := @' + Ctrl.Name + '_OnChange;');
      end;

      Add('');
    end;

    Add('end;');
    Add('//<DESIGNER-END>');
    Add('');

    // =====================================================
    // MAIN-BLOCK am Ende
    // =====================================================
    Add('//<MAIN-BEGIN>');
    if MainCode.Count > 0 then
      AStringList.AddStrings(MainCode)
    else
    begin
      Add('begin');
      Add('  CreateNewForm;');
      Add('end.');
    end;
    Add('//<MAIN-END>');

  finally
    UserCode.Free;
    MainCode.Free;
    UserGlobalVars.Free;
  end;
end;

initialization
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
end.

