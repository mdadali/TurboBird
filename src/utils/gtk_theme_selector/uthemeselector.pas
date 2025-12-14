unit uthemeselector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, Buttons,
  IniFiles, StrUtils,

  SynEdit,
  Spin,
  Grids,
  DBGrids,
  DBCtrls,
  IBDynamicGrid,
  HTMLView;


type

  TSimpleTheme = record
    Name: string;
    BackgroundColor: TColor;
    TextColor: TColor;
    ButtonColor: TColor;
    FontName: string;
    FontSize: word;
    FontStyle: TFontStyles;
  end;

  { TfrmThemeSelector }

  TfrmThemeSelector = class(TForm)
    btnApply: TButton;
    Button1: TButton;
    cbThemes: TComboBox;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    Label1: TLabel;
    lblSelectTheme: TLabel;
    gbPreview: TGroupBox;
    lblPreviewText: TLabel;
    edtPreview: TEdit;
    btnPreview: TButton;
    ListBox1: TListBox;
    Memo1: TMemo;
    RadioButton1: TRadioButton;
    RadioGroup1: TRadioGroup;
    ScrollBar1: TScrollBar;
    procedure btnApplyClick(Sender: TObject);
    procedure cbThemesChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FThemeIniPath: string;
    FThemeIni: TIniFile;
    procedure LoadLastTheme;
    procedure SaveLastTheme(const ThemeName: string);
    procedure ApplyThemeNextStart(const ThemeName: string);
    function  ReadLastThemeFromIni: string;
    procedure SaveLastThemeToIni(const ThemeName: string);
    function  LoadThemesFromIni(const FileName: string): Integer;
  public
    procedure ApplyThemePreview(const Theme: TSimpleTheme; AParentControl: TWinControl);
  end;


var
  frmThemeSelector: TfrmThemeSelector;

  Themes: array of TSimpleTheme;
  Count, idx: Integer;


implementation

uses turbocommon;  // for inifile

{$R *.lfm}

{ === Letztes Theme laden === }
procedure TfrmThemeSelector.LoadLastTheme;
var theme: string;
begin
  try
    theme := turbocommon.fIniFile.ReadString('UI', 'Theme', '');
    if theme <> '' then
      cbThemes.ItemIndex := cbThemes.Items.IndexOf(theme)
    else
      cbThemes.ItemIndex := 0;
  finally
  end;
end;

{ === Letztes Theme speichern === }
procedure TfrmThemeSelector.SaveLastTheme(const ThemeName: string);
begin
  try
    turbocommon.fIniFile.WriteString('UI', 'Theme', ThemeName);
  finally
  end;
end;

{ === Hinweis: Theme nur beim Start verfügbar === }
procedure TfrmThemeSelector.ApplyThemeNextStart(const ThemeName: string);
begin
  //ShowMessage('The selected theme "' + ThemeName + '" will be applied next time you start TurboBird.');
end;

{ === FormCreate === }
procedure TfrmThemeSelector.FormCreate(Sender: TObject);
begin
  Caption := 'Theme Selector';
  FThemeIniPath :=
    IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
    'data' + DirectorySeparator +
    'config' + DirectorySeparator +
    ThemesIniFile;

  if not FileExists(FThemeIniPath) then
    exit;

  FThemeIni :=  TIniFile.Create(FThemeIniPath, []);
  LoadThemesFromIni(FThemeIniPath);

  cbThemes.Items.Clear;

  for idx := 0 to Length(Themes) - 1 do
    cbThemes.Items.Add(Themes[idx].Name);

    // Letztes Theme laden
  idx := cbThemes.Items.IndexOf(ReadLastThemeFromIni());

  if idx >= 0 then
    cbThemes.ItemIndex := idx
  else
    cbThemes.ItemIndex := 0;

    // Live Preview beim Wechsel
  ApplyThemePreview(Themes[cbThemes.ItemIndex], Self);
end;

procedure TfrmThemeSelector.FormDestroy(Sender: TObject);
begin
  FThemeIni.Free;
end;

procedure TfrmThemeSelector.FormShow(Sender: TObject);
begin
  //LoadLastTheme;
end;

{ === Buttons === }
procedure TfrmThemeSelector.btnApplyClick(Sender: TObject);
var
  theme: string;
begin
  if cbThemes.ItemIndex < 0 then Exit;
  theme := cbThemes.Text;
  SaveLastTheme(theme);
  ApplyThemeNextStart(theme);
  ApplyThemePreview(Themes[cbThemes.ItemIndex], TWinControl(self.Owner));
  ApplyThemePreview(Themes[cbThemes.ItemIndex], TWinControl(Sender));
  Close;
end;

procedure TfrmThemeSelector.cbThemesChange(Sender: TObject);
begin
  ApplyThemePreview(Themes[cbThemes.ItemIndex], self);
  SaveLastThemeToIni(cbThemes.Text);
end;

procedure TfrmThemeSelector.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
end;

procedure TfrmThemeSelector.ApplyThemePreview(const Theme: TSimpleTheme; AParentControl: TWinControl);
var
  i: Integer;
  c: TControl;
begin
  if AParentControl = nil then Exit;

  for i := 0 to AParentControl.ControlCount - 1 do
  begin
    c := AParentControl.Controls[i];

    if c is THTMLViewer then
      exit;

    if c is TSynEdit then
    begin
      TSynEdit(c).Gutter.Color := Theme.BackgroundColor;
        TSynEdit(c).Color := Theme.BackgroundColor;
    end;

    if c is TForm then
    begin
      TForm(c).Font.Size  := Theme.FontSize;
      TForm(c).Font.Style := Theme.FontStyle;
      TForm(c).Font.Color := Theme.TextColor;
      TForm(c).Color      := Theme.BackgroundColor;
    end;

    // === Globale Font-Anpassung ===
    if c is TControl then
    begin
      try
        if not (c is TLabel) then
          c.Color := Theme.BackgroundColor;
        //c.Font.Name := Theme.FontName;
        //c.Font.Size := Theme.FontSize;
        //c.Font.Style := Theme.FontStyle;
        //c.Font.Color := Theme.TextColor;
      except
        // einige Controls (z.B. ScrollBar) haben kein Font-Objekt
      end;
    end;

    // === Farb- und Stil-Anpassung nach Typ ===

    if c is TWinControl then
    begin
      TWinControl(c).Font.Name := Theme.FontName;
      TWinControl(c).Font.Size := Theme.FontSize;
      TWinControl(c).Font.Style := Theme.FontStyle;

      TWinControl(c).Color := Theme.BackgroundColor;
      TWinControl(c).Font.Color := Theme.TextColor;
    end;

    if c is TLabel then
    begin
      TLabel(c).Font.Name := Theme.FontName;
      TLabel(c).Font.Size := Theme.FontSize;
      TWinControl(c).Font.Style := Theme.FontStyle;
      TLabel(c).Font.Color := Theme.TextColor;
    end;


      if c is TButton then
    begin
      TButton(c).Font.Color := AdjustColorByBrightness(TButton(c).Color);
      TButton(c).Repaint;
    end;

    if c is TTreeView then
    begin
      MainTreeView.ExpandSignColor := AdjustColorByBrightness(turbocommon.MainTreeView.Color);
      {TTreeView(c).Font.Size  := Theme.FontSize;
      TTreeView(c).Font.Style := Theme.FontStyle;
      TTreeView(c).Font.Color := Theme.TextColor;
      TTreeView(c).Color      := Theme.BackgroundColor; }
    end;

    // === Rekursion für Container ===
    if c is TWinControl then
      ApplyThemePreview(Theme, TWinControl(c));
  end;
end;

function TfrmThemeSelector.LoadThemesFromIni(const FileName: string): Integer;
var
  Sections: TStringList;
  i, Count: Integer;
  FontStyleStr: string;
begin
  Result := 0;
  if not FileExists(FileName) then Exit;

  Sections := TStringList.Create;
  try
    FThemeIni.ReadSections(Sections);

    Count := Sections.Count;
    SetLength(Themes, Count);

    for i := 0 to Sections.Count - 1 do
    begin
      Themes[i].Name := Sections[i];

      Themes[i].BackgroundColor := StringToColor(
        FThemeIni.ReadString(Sections[i], 'BackgroundColor', '$00FFFFFF'));
      Themes[i].TextColor := StringToColor(
        FThemeIni.ReadString(Sections[i], 'TextColor', '$00000000'));
      Themes[i].ButtonColor := StringToColor(
        FThemeIni.ReadString(Sections[i], 'ButtonColor', '$00C0C0C0'));

      Themes[i].FontName := FThemeIni.ReadString(Sections[i], 'FontName', 'Segoe UI');
      Themes[i].FontSize := FThemeIni.ReadInteger(Sections[i], 'FontSize', 10);

      // FontStyle: erlaubt mehrere Werte, z.B. "Bold,Italic" oder "bold italic"
      FontStyleStr := LowerCase(Trim(FThemeIni.ReadString(Sections[i], 'FontStyle', 'normal')));

      Themes[i].FontStyle := []; // leeres Set = normale Schrift
      if Pos('bold', FontStyleStr) > 0 then Include(Themes[i].FontStyle, fsBold);
      if Pos('italic', FontStyleStr) > 0 then Include(Themes[i].FontStyle, fsItalic);
      if Pos('underline', FontStyleStr) > 0 then Include(Themes[i].FontStyle, fsUnderline);
      if Pos('strike', FontStyleStr) > 0 then Include(Themes[i].FontStyle, fsStrikeOut);
    end;

    Result := Count;
  finally
    Sections.Free;
  end;
end;

function TfrmThemeSelector.ReadLastThemeFromIni: string;
begin
  Result := '';
  Result := turbocommon.fIniFile.ReadString('UI', 'Theme', '');
end;

procedure TfrmThemeSelector.SaveLastThemeToIni(const ThemeName: string);
begin
  turbocommon.fIniFile.WriteString('UI', 'Theme', ThemeName);
end;

end.
