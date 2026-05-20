unit uCodeGenerator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ExtCtrls, Types, Graphics,
  SynEdit,
  JvDesignSurface,
  JvDesignUtils,

  uPSI_IBXConnection;

function  IsNonVisualComponent(AClassName: string): boolean;
function IsVisualControl(Ctrl: TControl): Boolean;
procedure AssignSpecialEvent(AStringList: TStringList; const Line, FormName: string);
procedure GenerateSpecialEvent(AStringList: TStringList; const EventName, Signature: string);
procedure GenerateFormCreate(AStringList: TStringList);
procedure GenerateFormClose(AStringList: TStringList);
procedure GenerateFormStateChange(AStringList: TStringList);

function GetControlClass(AControl: TControl): TClass;

procedure GenerateEvent(Ctrl: TControl; AStringList: TStringList);
procedure GenerateCodeFromDesigner(AJvDesignPanel: TJvDesignPanel; AStringList: TStringList; AFormName: string);
function GetDefaultEventName(Ctrl: TControl): string;
function GetEventHandlerName(Ctrl: TControl): string;
procedure JumpToEventInEditor(Editor: TSynEdit; const EventName: string);
function EventExists(Lines: TStrings; const EventName: string): Boolean;
procedure AssignEventToControl(Ctrl: TControl; ALines: TStringList);


function ExtractBlock(const SL: TStringList; const StartTag, EndTag: string): TStringList;
procedure InsertIntoUserCode(ALines: TStrings; ACode: TStrings);
function HasEvent(const SL: TStringList; const EventName: string): Boolean;

implementation

function IsNonVisualComponent(AClassName: string): boolean;
begin
  result := false;
  if (Trim(AClassName) = 'TPSIBXConnection') or
     (Trim(AClassName) = 'TIBXConnection')
  then
    result := true;
end;

function IsVisualControl(Ctrl: TControl): Boolean;
begin
  Result := not IsNonVisualComponent(Ctrl.ClassName);
end;

procedure GenerateSpecialEvent(AStringList: TStringList;
  const EventName, Signature: string);
var
  EventLines: TStringList;
  UserCode: TStringList;
begin
  //bestehenden UserCode holen
  UserCode := ExtractBlock(AStringList, '//<USERCODE-BEGIN>', '//<USERCODE-END>');
  try
    //schon vorhanden → raus
    if HasEvent(UserCode, EventName) then
      Exit;

    //Event erzeugen
    EventLines := TStringList.Create;
    try
      EventLines.Add('procedure ' + EventName + Signature + ';');
      EventLines.Add('begin');
      EventLines.Add('  ');
      EventLines.Add('end;');
      EventLines.Add('');

      //in USERCODE einfügen
      InsertIntoUserCode(AStringList, EventLines);

    finally
      EventLines.Free;
    end;

  finally
    UserCode.Free;
  end;
end;

procedure GenerateFormCreate(AStringList: TStringList);
begin
  GenerateSpecialEvent(AStringList,
    'FormCreate',
    '(Sender: TObject)');
end;

procedure GenerateFormClose(AStringList: TStringList);
begin
  GenerateSpecialEvent(AStringList,
    'FormClose',
    '(Sender: TObject; var CloseAction: TCloseAction)');
end;

procedure GenerateFormStateChange(AStringList: TStringList);
begin
  GenerateSpecialEvent(AStringList,
    'FormStateChange',
    '(Sender: TObject)');
end;

procedure AssignSpecialEvent(AStringList: TStringList; const Line, FormName: string);
var
  i: Integer;
begin
  // 1. Prüfen, ob Binding schon vorhanden ist
  for i := 0 to AStringList.Count - 1 do
    if Pos(Line, AStringList[i]) > 0 then Exit;

  // 2. Marker für Event-Ende suchen
  for i := 0 to AStringList.Count - 1 do
    if Trim(AStringList[i]) = '//<EVENT_BINDINGS-END>' then
    begin
      // 3. Binding einfügen
      AStringList.Insert(i, '  ' + Line);

      // 4. Speziell für OnCreate: direkt nach Binding triggern
      if Pos('.OnCreate :=', Line) > 0 then
      begin
        // z.B. Form1.OnCreate(Form1);
        AStringList.Insert(i+1, '  ' + FormName + '.OnCreate(' + FormName + ');');
      end;

      Exit;
    end;
end;

// -----------------------------
// Hilfsfunktionen
// -----------------------------
function GetControlClass(AControl: TControl): TClass;
begin
  if AControl is TButton then Exit(TButton);
  if AControl is TEdit then Exit(TEdit);
  if AControl is TMemo then Exit(TMemo);
  if AControl is TPanel then Exit(TPanel);
  if AControl is TLabel then Exit(TLabel);
  if AControl is TCheckBox then Exit(TCheckBox);
  if AControl is TRadioButton then Exit(TRadioButton);
  // andere Controls ergänzen
  Result := AControl.ClassType;
end;

function GetControlClassName(AControl: TControl): string;
begin
  Result := GetControlClass(AControl).ClassName;
end;
function Escape(const s: string): string;
begin
  Result := StringReplace(s, '''', '''''', [rfReplaceAll]);
end;

function GetDefaultEventName(Ctrl: TControl): string;
begin
  if Ctrl is TButton then Exit('OnClick');
  if Ctrl is TEdit then Exit('OnChange');
  if Ctrl is TMemo then Exit('OnChange');
  if Ctrl is TCheckBox then Exit('OnClick');
  if Ctrl is TRadioButton then Exit('OnClick');

  Result := '';
end;

function GetEventHandlerName(Ctrl: TControl): string;
begin
  Result := Ctrl.Name + '_' + GetDefaultEventName(Ctrl);
end;

procedure JumpToEventInEditor(Editor: TSynEdit; const EventName: string);
var
  i, j: Integer;
  Line: string;
begin
  for i := 0 to Editor.Lines.Count - 1 do
  begin
    Line := Editor.Lines[i];

    if Pos('procedure ' + EventName, Line) > 0 then
    begin
      // Suche das "begin" ab der nächsten Zeile
      for j := i + 1 to Editor.Lines.Count - 1 do
      begin
        if Trim(Editor.Lines[j]) = 'begin' then
        begin
          Editor.CaretY := j + 2;
          Editor.CaretX := 3; // eingerückt
          Editor.SetFocus;
          Exit;
        end;
      end;
    end;
  end;
end;

function EventExists(Lines: TStrings; const EventName: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Lines.Count - 1 do
    if Pos('procedure ' + EventName, Lines[i]) > 0 then
      Exit(True);
end;

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

function HasEvent(const SL: TStringList; const EventName: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to SL.Count - 1 do
    if Pos('procedure ' + EventName, SL[i]) > 0 then
      Exit(True);
end;

// -----------------------------
// Komponenten Eigenschaften hinzufügen
// -----------------------------
procedure WriteCommonProps(Ctrl: TControl; SL: TStringList);
var
  fs: string;
  i: Integer;
begin
  // Position & Größe
  SL.Add('  ' + Ctrl.Name + '.Left := ' + IntToStr(Ctrl.Left) + ';');
  SL.Add('  ' + Ctrl.Name + '.Top := ' + IntToStr(Ctrl.Top - 30) + ';');
  SL.Add('  ' + Ctrl.Name + '.Width := ' + IntToStr(Ctrl.Width) + ';');
  SL.Add('  ' + Ctrl.Name + '.Height := ' + IntToStr(Ctrl.Height) + ';');

  // Sichtbarkeit & Aktivierung
  SL.Add('  ' + Ctrl.Name + '.Visible := ' + BoolToStr(Ctrl.Visible, True) + ';');
  SL.Add('  ' + Ctrl.Name + '.Enabled := ' + BoolToStr(Ctrl.Enabled, True) + ';');

  // Tag
  SL.Add('  ' + Ctrl.Name + '.Tag := ' + IntToStr(Ctrl.Tag) + ';');

  // Color
  //if not (Ctrl is TButton) then
    //SL.Add('  ' + Ctrl.Name + '.Color := ' + IntToStr(TWinControl(Ctrl).Color) + ';');

  //SL.Add('  ' + Ctrl.Name + '.Color := ' + IntToStr(TWinControl(Ctrl).Color) + ';');

  // Font (Name, Size, Style, Color)
  {if Ctrl.Font <> nil then
  begin
    SL.Add('  ' + Ctrl.Name + '.Font.Name := ''' + Ctrl.Font.Name + ''';');
    SL.Add('  ' + Ctrl.Name + '.Font.Size := ' + IntToStr(Ctrl.Font.Size) + ';');

    // Font.Style
    fs := '';
    if fsBold in Ctrl.Font.Style then fs := fs + 'fsBold,';
    if fsItalic in Ctrl.Font.Style then fs := fs + 'fsItalic,';
    if fsUnderline in Ctrl.Font.Style then fs := fs + 'fsUnderline,';
    if fsStrikeOut in Ctrl.Font.Style then fs := fs + 'fsStrikeOut,';
    if fs <> '' then fs := Copy(fs, 1, Length(fs) - 1); // letztes Komma entfernen
    SL.Add('  ' + Ctrl.Name + '.Font.Style := [' + fs + '];');

    // Font.Color
    SL.Add('  ' + Ctrl.Name + '.Font.Color := ' + IntToStr(Ctrl.Font.Color) + ';');
  end;}

end;

procedure WriteSpecificProps(Ctrl: TControl; SL: TStringList);
begin
  // Caption für Controls, die es haben
  if Ctrl is TLabel then
    SL.Add('  ' + Ctrl.Name + '.Caption := ''' + Escape(TLabel(Ctrl).Caption) + ''';')
  else if Ctrl is TButton then
    SL.Add('  ' + Ctrl.Name + '.Caption := ''' + Escape(TButton(Ctrl).Caption) + ''';')
  else if Ctrl is TPanel then
    SL.Add('  ' + Ctrl.Name + '.Caption := ''' + Escape(TPanel(Ctrl).Caption) + ''';')
  else if Ctrl is TGroupBox then
    SL.Add('  ' + Ctrl.Name + '.Caption := ''' + Escape(TGroupBox(Ctrl).Caption) + ''';')
  else if Ctrl is TRadioButton then
    SL.Add('  ' + Ctrl.Name + '.Caption := ''' + Escape(TRadioButton(Ctrl).Caption) + ''';')
  else if Ctrl is TCheckBox then
    SL.Add('  ' + Ctrl.Name + '.Caption := ''' + Escape(TCheckBox(Ctrl).Caption) + ''';')
  else if Ctrl is TRadioGroup then
    SL.Add('  ' + Ctrl.Name + '.Caption := ''' + Escape(TRadioGroup(Ctrl).Caption) + ''';')
  else if Ctrl is TCheckGroup then
    SL.Add('  ' + Ctrl.Name + '.Caption := ''' + Escape(TCheckGroup(Ctrl).Caption) + ''';');

  // Color
  if (Ctrl is TWinControl) and not (Ctrl is TButton)
     and not (Ctrl is TScrollBar) and not (Ctrl is TToggleBox) then
    SL.Add('  ' + Ctrl.Name + '.Color := ' + IntToStr(TWinControl(Ctrl).Color) + ';');


  // Text / Lines / Items
  if Ctrl is TEdit then
    SL.Add('  ' + Ctrl.Name + '.Text := ''' + Escape(TEdit(Ctrl).Text) + ''';')
  else if Ctrl is TMemo then
    SL.Add('  ' + Ctrl.Name + '.Lines.Text := ''' + Escape(TMemo(Ctrl).Lines.Text) + ''';')
  else if Ctrl is TListBox then
    SL.Add('  ' + Ctrl.Name + '.Items.Text := ''' + Escape(TListBox(Ctrl).Items.Text) + ''';')
  else if Ctrl is TComboBox then
    SL.Add('  ' + Ctrl.Name + '.Items.Text := ''' + Escape(TComboBox(Ctrl).Items.Text) + ''';')
  else if Ctrl is TToggleBox then
    SL.Add('  ' + Ctrl.Name + '.Checked := ' + BoolToStr(TToggleBox(Ctrl).Checked, True) + ';');

  // Font (Name, Size, Color, Style)
  {if Ctrl is TWinControl then
    with TWinControl(Ctrl).Font do
      SL.Add('  ' + Ctrl.Name + '.Font.Name := ''' + Name + ''';');
      SL.Add('  ' + Ctrl.Name + '.Font.Size := ' + IntToStr(Size) + ';');
      SL.Add('  ' + Ctrl.Name + '.Font.Color := ' + IntToStr(Color) + ';');
      SL.Add('  ' + Ctrl.Name + '.Font.Style := [' +
        IfThen(fsBold in Style, 'fsBold,', '') +
        IfThen(fsItalic in Style, 'fsItalic,', '') +
        IfThen(fsUnderline in Style, 'fsUnderline,', '') +
        IfThen(fsStrikeOut in Style, 'fsStrikeOut,', '') +
        '];'); }

end;

procedure WriteNonVisualProps(Ctrl: TControl; SL: TStringList);
begin
  if Ctrl is TIBXConnection then
  begin
    SL.Add('  ' + Ctrl.Name + '.DatabaseName := ''' + Escape(TIBXConnection(Ctrl).PSIBXConnection.DatabaseName) + ''';');
    SL.Add('  ' + Ctrl.Name + '.UserName := ''' + Escape(TIBXConnection(Ctrl).PSIBXConnection.UserName) + ''';');
    SL.Add('  ' + Ctrl.Name + '.Password := ''' + Escape(TIBXConnection(Ctrl).PSIBXConnection.Password) + ''';');
    SL.Add('  ' + Ctrl.Name + '.Connected := ' + BoolToStr(TIBXConnection(Ctrl).PSIBXConnection.Connected, True) + ';');
  end;
end;

function IsTemplateControl(Ctrl: TControl): Boolean;
begin
  Result :=
    (Ctrl.Name = 'pnlDesign') or
    (Ctrl.Name = 'pnlFormTitle') or
    (Ctrl.Name = 'btnTitleClose') or
    (Ctrl.Name = 'btnTitleMinimize') or
    (Ctrl.Name = 'btnTitleMaximize');
end;

procedure WriteAllProps(Ctrl: TControl; SL: TStringList);
begin
  WriteCommonProps(Ctrl, SL);
  WriteSpecificProps(Ctrl, SL);
end;

procedure InsertIntoUserCode(ALines: TStrings; ACode: TStrings);
var
  i,j, BeginIndex, EndIndex: Integer;
begin
  BeginIndex := -1;
  EndIndex := -1;

  // Marker suchen
  for i := 0 to ALines.Count - 1 do
  begin
    if Trim(ALines[i]) = '//<USERCODE-BEGIN>' then
      BeginIndex := i;

    if Trim(ALines[i]) = '//<USERCODE-END>' then
    begin
      EndIndex := i;
      Break;
    end;
  end;

  // Falls Block fehlt → neu anlegen
  if (BeginIndex = -1) or (EndIndex = -1) or (BeginIndex > EndIndex) then
  begin
    // Block ans Ende setzen
    if (ALines.Count > 0) and (Trim(ALines[ALines.Count - 1]) <> '') then
      ALines.Add('');

    ALines.Add('//<USERCODE-BEGIN>');
    ALines.Add('//<USERCODE-END>');

    BeginIndex := ALines.Count - 2;
    EndIndex := ALines.Count - 1;
  end;

  // Optional: Leerzeile vor Einfügen
  if (EndIndex > BeginIndex + 1) then
  begin
    if Trim(ALines[EndIndex - 1]) <> '' then
    begin
      ALines.Insert(EndIndex, '');
      Inc(EndIndex);
    end;
  end;

  // Code einfügen (vor END)

    for j := 0 to ACode.Count - 1 do
    begin
      ALines.Insert(EndIndex + j, ACode[j]);
    end;

  // Optional: Leerzeile nach eingefügtem Code
  if (EndIndex + ACode.Count < ALines.Count) then
  begin
    if Trim(ALines[EndIndex + ACode.Count]) <> '' then
      ALines.Insert(EndIndex + ACode.Count, '');
  end;
end;

procedure AssignEventToControl(Ctrl: TControl; ALines: TStringList);
var
  i, BeginIndex, EndIndex: Integer;
  EventName, EventProp, LineToInsert: string;
begin
  EventProp := GetDefaultEventName(Ctrl);
  EventName := GetEventHandlerName(Ctrl);
  LineToInsert := Ctrl.Name + '.' + EventProp + ' := @' + EventName + ';';

  // Prüfen ob schon vorhanden
  for i := 0 to ALines.Count - 1 do
    if Trim(ALines[i]) = LineToInsert then Exit;

  // EVENTS-BLOCK suchen
  BeginIndex := -1;
  EndIndex := -1;
  for i := 0 to ALines.Count - 1 do
  begin
    if Trim(ALines[i]) = '//<EVENT_BINDINGS-BEGIN>' then BeginIndex := i;
    if Trim(ALines[i]) = '//<EVENT_BINDINGS-END>' then EndIndex := i;
  end;

  // Event-Zuweisung direkt vor END einfügen
  ALines.Insert(EndIndex, '  ' + LineToInsert);
end;

procedure GenerateEvent(Ctrl: TControl; AStringList: TStringList);
var
  EventLines: TStringList;
  UserCode: TStringList;
  EventName: string;
begin
  // 1. Eventnamen bestimmen
  EventName := GetEventHandlerName(Ctrl);

  // 2. bestehenden UserCode extrahieren
  UserCode := ExtractBlock(AStringList, '//<USERCODE-BEGIN>', '//<USERCODE-END>');
  try
    // 3. prüfen ob Event schon existiert
    if HasEvent(UserCode, EventName) then
      Exit;

    // 4. Event-Code erstellen
    EventLines := TStringList.Create;
    try
      EventLines.Add('procedure ' + EventName + '(Sender: TObject);');
      EventLines.Add('begin');
      EventLines.Add('  ');
      EventLines.Add('end;');
      EventLines.Add('');

      // 5. in UserCode-Bereich einfügen
      InsertIntoUserCode(AStringList, EventLines);

    finally
      EventLines.Free;
    end;

  finally
    UserCode.Free;
  end;
end;

procedure GenerateCodeFromDesigner(AJvDesignPanel: TJvDesignPanel; AStringList: TStringList; AFormName: string);
var
  HeaderComments, UserGlobals, UserCode, MainCode, EventBindings: TStringList;
  CtrlList: TList;
  RootPanel, TitlePanel: TPanel;
  i: Integer;

  procedure Add(const S: string);
  begin
    AStringList.Add(S);
  end;

  // -------------------------
  //Controls rekursiv sammeln
  // -------------------------
  procedure CollectControls(ParentCtrl: TControl; List: TList);
  var
    i: Integer;
    Child: TControl;
  begin
    if ParentCtrl is TWinControl then
      for i := 0 to TWinControl(ParentCtrl).ControlCount - 1 do
      begin
        Child := TWinControl(ParentCtrl).Controls[i];
        if IsTemplateControl(Child) then Continue;
        //if IsTemplateControl(Child) or (IsNonVisualComponent(Child.ClassName)) then Continue;

        List.Add(Child);
        CollectControls(Child, List);
      end;
  end;

  // -------------------------
  //Komponenten erzeugen
  // -------------------------
  procedure GenerateCreate(List: TList);
  var
    i: Integer;
    Ctrl: TControl;
    ParentName: string;
  begin
    for i := 0 to List.Count - 1 do
    begin
      Ctrl := TControl(List[i]);

      if IsNonVisualComponent(Ctrl.ClassName) then
      begin
        Add('  ' + Ctrl.Name + ' := ' + Ctrl.ClassName + '.Create(nil);');
        Add('');
        Continue;
      end;

      if Ctrl.Parent = RootPanel then
        ParentName := AFormName
      else
        ParentName := Ctrl.Parent.Name;

      Add('  ' + Ctrl.Name + ' := ' + Ctrl.ClassName + '.Create(' + ParentName + ');');
      Add('  ' + Ctrl.Name + '.Parent := ' + ParentName + ';');
      Add('');
    end;
  end;

begin
  RootPanel := TPanel(AJvDesignPanel.FindComponent('pnlDesign'));
  if RootPanel = nil then Exit;

  TitlePanel := TPanel(RootPanel.FindComponent('pnlFormTitle'));

  CtrlList := TList.Create;
  try
    CollectControls(RootPanel, CtrlList);

    HeaderComments := ExtractBlock(AStringList, '//<HEADER_COMMENTS-BEGIN>', '//<HEADER_COMMENTS-END>');
    UserGlobals := ExtractBlock(AStringList, '//<USER-GLOBALS-BEGIN>', '//<USER-GLOBALS-END>');
    UserCode := ExtractBlock(AStringList, '//<USERCODE-BEGIN>', '//<USERCODE-END>');
    MainCode := ExtractBlock(AStringList, '//<MAIN-BEGIN>', '//<MAIN-END>');
    EventBindings  := ExtractBlock(AStringList, '//<EVENT_BINDINGS-BEGIN>', '//<EVENT_BINDINGS-END>');

    try
      AStringList.Clear;

      Add('//<HEADER_COMMENTS-BEGIN>');
      AStringList.AddStrings(HeaderComments);
      Add('//<HEADER_COMMENTS-END>');
      Add('');

      // -------------------------
      // USER GLOBAL
      // -------------------------
      Add('//<USER-GLOBALS-BEGIN>');
      AStringList.AddStrings(UserGlobals);
      Add('//<USER-GLOBALS-END>');
      Add('');

      // -------------------------
      // VARS
      // -------------------------
      Add('//<DESIGNER-VARS-BEGIN>');
      Add('var');
      Add('  ' + AFormName + ': TForm;');
      for i := 0 to CtrlList.Count - 1 do
        Add('  ' + TControl(CtrlList[i]).Name + ': ' + TControl(CtrlList[i]).ClassName + ';');
      Add('//<DESIGNER-VARS-END>');
      Add('');

      // -------------------------
      // USER CODE
      // -------------------------
      Add('//<USERCODE-BEGIN>');
      AStringList.AddStrings(UserCode);
      Add('//<USERCODE-END>');
      Add('');

      // -------------------------
      // DESIGNER
      // -------------------------
      Add('//<DESIGNER-BEGIN>');
      Add('procedure CreateNewForm;');
      Add('begin');
      Add('  ' + AFormName + ' := TForm.Create(nil);');
      Add('  ' + AFormName + '.SetBounds(' +
        IntToStr(RootPanel.Left) + ', ' +
        IntToStr(RootPanel.Top) + ', ' +
        IntToStr(RootPanel.Width) + ', ' +
        IntToStr(RootPanel.Height) + ');');
      Add('  ' + AFormName + '.Color := ' + IntToStr(RootPanel.Color) + ';');
      Add('  ' + AFormName + '.Position := poDesigned;');
      {if TitlePanel <> nil then
        Add('  ' + AFormName + '.Caption := ''' + Escape(TitlePanel.Caption) + ''';')
      else
        Add('  ' + AFormName + '.Caption := ''' + Escape(AFormName) + ''';');
      Add('');}

      Add('  ' + AFormName + '.Caption := ''' + Escape(AFormName) + ''';');
      Add('');

      GenerateCreate(CtrlList);

      // -------------------------
      // PROPS – immer frisch setzen
      // -------------------------
      for i := 0 to CtrlList.Count - 1 do
      begin
        if IsNonVisualComponent(TControl(CtrlList[i]).ClassName) then
          WriteNonVisualProps(TControl(CtrlList[i]), AStringList)
        else
          WriteAllProps(TControl(CtrlList[i]), AStringList);
      end;

      // -------------------------
      //NEUER EVENTS-BLOCK (geschützt)
      // -------------------------
      Add('  //<EVENT_BINDINGS-BEGIN>');
      AStringList.AddStrings(EventBindings);
      Add('  //<EVENT_BINDINGS-END>');
      ///////////////////////////////////////////////

      Add('  ' + AFormName + '.Visible := True;');
      Add('end;');
      Add('//<DESIGNER-END>');
      Add('');

      // -------------------------
      // MAIN
      // -------------------------
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
      HeaderComments.Free;
      UserGlobals.Free;
      UserCode.Free;
      MainCode.Free;
      EventBindings.Free;
    end;

  finally
    CtrlList.Free;
  end;
end;


end.
