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
  dynlibs, ibase60dyn;


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
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnBrowserClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lstSuggestionsClick(Sender: TObject);
  private
     function IsValidLibrary(const Path: string): Boolean;
     procedure DoFirebirdClientSearch;
  public
     FBClientLibPath: string;
  end;

var
  frmSetFBClient: TfrmSetFBClient;
  InfoLabelCaption1, InfoLabelCaption2: string;

implementation


procedure TfrmSetFBClient.FormCreate(Sender: TObject);
begin
  //lstSuggestions.Items.Add('/opt/firebird/lib/libfbclient.so');
  //lstSuggestions.Items.Add('/usr/lib/x86_64-linux-gnu/libfbclient.so.2');
  //lstSuggestions.Items.Add('/usr/lib64/libfbclient.so.2');

  edtClientLib.Text := '';
  btnOK.Enabled := False;
end;

procedure TfrmSetFBClient.lstSuggestionsClick(Sender: TObject);
begin
  if lstSuggestions.ItemIndex > -1 then
    edtClientLib.Text := lstSuggestions.Items[lstSuggestions.ItemIndex];
end;

procedure TfrmSetFBClient.btnBrowserClick(Sender: TObject);
begin
  OpenDialog1.Filter := 'Firebird Client Library|*.so*;*.dll';
  if OpenDialog1.Execute then
    edtClientLib.Text := OpenDialog1.FileName;
end;

procedure TfrmSetFBClient.btnOKClick(Sender: TObject);
begin
  FBClientLibPath := edtClientLib.Text;
  ModalResult := mrOk;
end;

procedure TfrmSetFBClient.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmSetFBClient.btnSearchClick(Sender: TObject);
var
  WaitForm: TForm;
  InfoLabel: TLabel;
begin
  // Dynamisches modales Infofenster erzeugen
  WaitForm := TForm.Create(nil);
  try
    WaitForm.BorderStyle := bsNone;
    WaitForm.Position := poScreenCenter;
    WaitForm.FormStyle := fsStayOnTop;
    WaitForm.Width := 300;
    WaitForm.Height := 100;
    WaitForm.Color := clWhite;
    WaitForm.Caption := '';
    WaitForm.Enabled := False; // verhindert Interaktion

    // Label für Textanzeige
    InfoLabel := TLabel.Create(WaitForm);
    InfoLabel.Parent := WaitForm;
    InfoLabel.Align := alClient;
    InfoLabel.Alignment := taCenter;
    InfoLabel.Layout := tlCenter;
    InfoLabel.Caption := 'Search in progress – please wait...';
    InfoLabel.Font.Size := 12;

    // Fenster anzeigen und sichtbar machen
    frmSetFBClient.Enabled := false;
    WaitForm.Show;
    Application.ProcessMessages;

    // Jetzt Firebird-Clients suchen
    DoFirebirdClientSearch;

    // Fenster schließen
    WaitForm.Close;
    frmSetFBClient.Enabled := true;
    Application.ProcessMessages;
  finally
    WaitForm.Free;
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

procedure TfrmSetFBClient.btnTestClick(Sender: TObject);
begin

  if IsValidLibrary(edtClientLib.Text) then
  begin
    ShowMessage('Client library loaded successfully.');
    btnOK.Enabled := True;
  end
  else
  begin
    ShowMessage('Loading the library failed');
    btnOK.Enabled := False;
  end;
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

