unit fSetFBClient;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ExtCtrls, ComCtrls,  FileUtil,
  {$IFDEF WINDOWS} Windows, {$ENDIF}
  {$IFDEF UNIX} Unix, {$ENDIF}
  {$IFDEF DARWIN} MacOSAll, {$ENDIF}
  StrUtils,

  uthemeselector;


type

  { TfrmSetFBClient }

  TfrmSetFBClient = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnBrowser: TButton;
    btnTest: TButton;
    btnSearch: TButton;
    edtClientLib: TEdit;
    Image1: TImage;
    lblInfo: TLabel;
    lstSuggestions: TListBox;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnBrowserClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lstSuggestionsClick(Sender: TObject);
  private
     function  IsValidLibrary(const Path: string): Boolean;
     procedure DoFirebirdClientSearch;
  public
     FBClientLibPath: string;
  end;

var
  frmSetFBClient: TfrmSetFBClient;
  InfoLabelCaption1, InfoLabelCaption2: string;

implementation

uses turbocommon;

procedure TfrmSetFBClient.FormCreate(Sender: TObject);
begin
  edtClientLib.Text := '';
  btnOK.Enabled := False;
end;

procedure TfrmSetFBClient.FormShow(Sender: TObject);
begin
  //if Assigned(frmThemeSelector) then
    //frmThemeSelector.btnApplyClick(self);
end;

procedure TfrmSetFBClient.lstSuggestionsClick(Sender: TObject);
begin
  if lstSuggestions.ItemIndex > -1 then
  begin
    edtClientLib.Text := lstSuggestions.Items[lstSuggestions.ItemIndex];
    btnTest.Enabled := true;
  end;
end;

procedure TfrmSetFBClient.btnBrowserClick(Sender: TObject);
begin
  OpenDialog1.Filter := 'Firebird Client Library|*.so*;*.dll';
  if OpenDialog1.Execute then
  begin
    edtClientLib.Text := OpenDialog1.FileName;
    FBClientLibPath   := OpenDialog1.FileName;
    btnTest.Enabled := true;
  end else
  begin
    btnTest.Enabled := false;
  end;
end;

procedure TfrmSetFBClient.btnTestClick(Sender: TObject);
begin
  if  turbocommon.LoadClientLibIBX(edtClientLib.Text) then
  begin
    ShowMessage('Client library loaded successfully.');
    btnOK.Enabled := True;
    FBClientLibPath := edtClientLib.Text;
  end else
  begin
    ShowMessage('Loading the library failed');
    btnOK.Enabled := False;
    FBClientLibPath := '';
  end;
end;

procedure TfrmSetFBClient.btnOKClick(Sender: TObject);
begin
  FBClientLibPath := edtClientLib.Text;
  turbocommon.InitialFBClientLibPath := FBClientLibPath;
  ModalResult := mrOk;
end;

procedure TfrmSetFBClient.btnCancelClick(Sender: TObject);
begin
  MessageDlg(
    'You have not selected a default Firebird client library.' + LineEnding +
    'Please open the application menu → File → Server Registry' + LineEnding +
    'and choose a valid client library for the corresponding server.',
    mtWarning, [mbOK], 0
  );

  ModalResult := mrCancel;
end;

procedure TfrmSetFBClient.btnSearchClick(Sender: TObject);
var
  WaitForm: TForm;
  InfoLabel: TLabel;
  i: integer;
begin
  // Dynamisches modales Infofenster erzeugen
  WaitForm := TForm.Create(Application);
  try
    WaitForm.BorderStyle := bsNone;
    WaitForm.Position := poScreenCenter;
    WaitForm.FormStyle := fsStayOnTop;
    WaitForm.Width := 300;
    WaitForm.Height := 100;
    WaitForm.Color := clWhite;
    WaitForm.Caption := '';
    //WaitForm.Enabled := False; // verhindert Interaktion

    // Label für Textanzeige
    InfoLabel := TLabel.Create(WaitForm);
    InfoLabel.Parent := WaitForm;
    InfoLabel.Align := alClient;
    InfoLabel.Alignment := taCenter;
    InfoLabel.Layout := tlCenter;
    InfoLabel.Caption := 'Search in progress – please wait...';
    InfoLabel.Font.Size := 12;

    // Fenster anzeigen und sichtbar machen
    self.Visible := false;

    WaitForm.FormStyle := fsSystemStayOnTop;
    WaitForm.Enabled := true;
    WaitForm.Show;
    Application.ProcessMessages;

    // Jetzt Firebird-Clients suchen
    DoFirebirdClientSearch;
    Application.ProcessMessages;
    WaitForm.ModalResult := mrOK;
    // Fenster schließen
    WaitForm.Close;
    Application.ProcessMessages;
  finally
    WaitForm.Free;
    self.Visible := true;
    Application.ProcessMessages;
  end;
end;

procedure TfrmSetFBClient.DoFirebirdClientSearch;
var
  SearchPaths: TStringList;
  I: Integer;
  ExeDir: String;

  procedure AddIfFBClient(const FileName: string);
  var
    Base: String;
  begin
    Base := ExtractFileName(FileName);
    if (Base.ToLower.StartsWith('libfbclient') or
        Base.ToLower.StartsWith('libfbembed') or
        SameText(Base, 'fbclient.dll') or
        SameText(Base, 'fbembed.dll')) and
       FileExists(FileName) then
    begin
      if lstSuggestions.Items.IndexOf(FileName) = -1 then
        lstSuggestions.Items.Add(FileName);
    end;
  end;

  procedure RecursiveSearch(const Dir: string);
  var
    FileList: TStringList;
    j: Integer;
  begin
    if not DirectoryExists(Dir) then Exit;

    try
      FileList := FindAllFiles(Dir, '*fbclient*', True); // rekursiv
      try
        for j := 0 to FileList.Count - 1 do
        begin
          AddIfFBClient(FileList[j]);
          Application.ProcessMessages;
        end;
      finally
        FileList.Free;
      end;
    except
      on E: Exception do ; // Ignoriere Fehler z. B. Permission denied
    end;
  end;

begin
  lstSuggestions.Items.Clear;
  SearchPaths := TStringList.Create;
  try
    ExeDir := ExtractFilePath(Application.ExeName);
    SearchPaths.Add(ExeDir);

    {$IFDEF WINDOWS}
    SearchPaths.Add('C:\Windows\System32');
    SearchPaths.Add('C:\Windows\SysWOW64');
    SearchPaths.Add(ExeDir + 'fbclient');
    {$ENDIF}

    {$IFDEF UNIX}
    SearchPaths.Add('/usr/lib/');
    SearchPaths.Add('/usr/lib64/');
    SearchPaths.Add('/usr/local/lib/');
    SearchPaths.Add('/opt/');
    {$ENDIF}

    {$IFDEF DARWIN}
    SearchPaths.Add('/opt/local/lib/');
    SearchPaths.Add('/Library/Frameworks/');
    {$ENDIF}

    for I := 0 to SearchPaths.Count - 1 do
      RecursiveSearch(SearchPaths[I]);

    if lstSuggestions.Items.Count = 0 then
      ShowMessage('No Firebird clients detected.');

  finally
    SearchPaths.Free;
  end;
end;

procedure TfrmSetFBClient.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin

end;

{function TfrmSetFBClient.IsValidLibrary(const Path: string): Boolean;
var
  h: TLibHandle;
begin
  h := LoadLibrary(PChar(Path));
  if h <> NilHandle then
  begin
    FreeLibrary(h);
    Result := True;
  end
  else
    Result := False;
end;
}

function TfrmSetFBClient.IsValidLibrary(const Path: string): Boolean;
var
  h: TLibHandle;
begin
  h := LoadLibrary(PChar(Path));
  if h <> NilHandle then
  begin
    FreeLibrary(h);
    Result := True;
  end
  else
    Result := False;
end;


{$R *.lfm}

end.

