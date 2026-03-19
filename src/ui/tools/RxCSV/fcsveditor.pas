unit fCSVEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, csvdataset, Forms, Controls, Graphics, Dialogs, Clipbrd,
  DBCtrls, StdCtrls, ExtCtrls, ComCtrls, SynEdit, SynHighlighterSQL, RxDBGrid,

  turbocommon,
  uthemeselector,
  uGenSQLFromCSVDataset,

  fpdataexporter,
  frmBaseConfigExport,
  ;

type

  { TfrmCSVEditor }


  TfrmCSVEditor = class;

type
  TCSVLoadThread = class(TThread)
  private
    FFileName: string;
    FOwner: TfrmCSVEditor;
    procedure CloseDataset;
    procedure LoadCSVInDataset;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TfrmCSVEditor; const AFileName: string);
  end;

  TfrmCSVEditor = class(TForm)
    btnOpenFile: TButton;
    btnSaveFileAs: TButton;
    btnCreateSQL: TButton;
    btnCopySQL: TButton;
    chkBoxIgnoreOuterWhiteSpace: TCheckBox;
    chkBoxFirstLineAsFieldName: TCheckBox;
    chkBoxQuoteOuterWhiteSpace: TCheckBox;
    CSVDataset1: TCSVDataset;
    DataSource1: TDataSource;
    DBNavigator1: TDBNavigator;
    edtQuoteChar: TEdit;
    edtLineEnding: TEdit;
    edtDelimiter: TEdit;
    edtDefaultFieldLength: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    OpenDialog1: TOpenDialog;
    Panel2: TPanel;
    SaveDialog1: TSaveDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    RxDBGrid1: TRxDBGrid;
    ScrollBox1: TScrollBox;
    SynEdit1: TSynEdit;
    SynSQLSyn1: TSynSQLSyn;
    tsSQL: TTabSheet;
    tsCSVSettings: TTabSheet;
    tsGrid: TTabSheet;
    tsDetail: TTabSheet;
    procedure btnCopySQLClick(Sender: TObject);
    procedure btnCreateSQLClick(Sender: TObject);
    procedure btnOpenFileClick(Sender: TObject);
    procedure btnSaveFileAsClick(Sender: TObject);
    procedure CSVDataset1AfterDelete(DataSet: TDataSet);
    procedure DBNavigator1Click(Sender: TObject; Button: TDBNavButtonType);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FFileName: string;
    procedure LoadCSVFile(const FileName: string);
    procedure SaveCSVFile(const FileName: string);

    procedure SetCSVSettingsFromForm;
    procedure ReadIni;
    procedure WriteIni;
  public
  end;

//var
  //frmCSVEditor: TfrmCSVEditor;

implementation

{$R *.lfm}

constructor TCSVLoadThread.Create(AOwner: TfrmCSVEditor; const AFileName: string);
begin
  inherited Create(True); // suspended
  FreeOnTerminate := True;
  FOwner := AOwner;
  FFileName := AFileName;
  Start;
end;

procedure TCSVLoadThread.CloseDataset;
begin
  if FOwner.CSVDataset1.Active then
    FOwner.CSVDataset1.Close;
  FOwner.CSVDataset1.Fields.Clear;
end;

procedure TCSVLoadThread.LoadCSVInDataset;
begin

  FOwner.CSVDataset1.LoadFromCSVFile(FFileName);
  FOwner.RxDBGrid1.OptimizeColumnsWidthAll;
end;

procedure TCSVLoadThread.Execute;
begin
  // Dataset im Hauptthread vorbereiten
  Synchronize(@CloseDataset);

  // CSV im Hauptthread laden
  Synchronize(@LoadCSVInDataset);
end;



{ TfrmCSVEditor }
procedure  TfrmCSVEditor.ReadIni;
begin
  edtDefaultFieldLength.Text          := IntTostr(CSVDefaultFieldLength);
  edtDelimiter.Text                   := CSVDelimiter;
  //edtLineEnding.Text                := CSVLineEnding;
  edtQuoteChar.Text                   := CSVQuoteChar;
  chkBoxFirstLineAsFieldName.Checked  := CSVFirstLineAsFieldNames;
  chkBoxIgnoreOuterWhiteSpace.Checked := CSVIgnoreOuterWhitespace;
  chkBoxQuoteOuterWhiteSpace.Checked  := CSVQuoteOuterWhitespace;
end;

procedure TfrmCSVEditor.SetCSVSettingsFromForm;
begin
  CSVDataset1.CSVOptions.DefaultFieldLength := StrToIntDef(edtDefaultFieldLength.Text, 50);

  if edtDelimiter.Text <> '' then
    CSVDataset1.CSVOptions.Delimiter  := edtDelimiter.Text[1]
  else
   CSVDataset1.CSVOptions.Delimiter := ' ';

  if edtLineEnding.Text <> '' then
    CSVDataset1.CSVOptions.LineEnding := edtLineEnding.Text[1]
  else
    CSVDataset1.CSVOptions.LineEnding := ' ';

  if edtQuoteChar.Text <> '' then
    CSVDataset1.CSVOptions.QuoteChar  := edtQuoteChar.Text[1]
  else
    CSVDataset1.CSVOptions.QuoteChar:= '"';

  CSVDataset1.CSVOptions.FirstLineAsFieldNames := chkBoxFirstLineAsFieldName.Checked;
  CSVDataset1.CSVOptions.IgnoreOuterWhitespace := chkBoxIgnoreOuterWhiteSpace.Checked;
  CSVDataset1.CSVOptions.QuoteOuterWhitespace  := chkBoxQuoteOuterWhiteSpace.Checked;
end;

procedure TfrmCSVEditor.WriteIni;
begin
  CSVDefaultFieldLength := StrToIntDef(edtDefaultFieldLength.Text, 50);
  CSVDelimiter := edtDelimiter.Text[1];
  //CSVLineEnding := edtLineEnding.Text[1];
  CSVQuoteChar := edtQuoteChar.Text[1];
  CSVFirstLineAsFieldNames := chkBoxFirstLineAsFieldName.Checked;
  CSVIgnoreOuterWhitespace := chkBoxIgnoreOuterWhiteSpace.Checked;
  CSVQuoteOuterWhitespace := chkBoxQuoteOuterWhiteSpace.Checked;
end;

procedure TfrmCSVEditor.LoadCSVFile(const FileName: string);
begin
  if CSVDataset1.Active then
  begin
    CSVDataset1.Close;
    CSVDataset1.Fields.Clear;
  end;

  RxDBGrid1.BeginUpdate;
  CSVDataset1.LoadFromCSVFile(FileName);
  RxDBGrid1.OptimizeColumnsWidthAll;
  RxDBGrid1.EndUpdate(True);
end;

procedure TfrmCSVEditor.SaveCSVFile(const FileName: string);
begin
  if not CSVDataset1.Active then Exit;

  try
    CSVDataset1.SaveToCSVFile(FileName);
  except
    on E: Exception do
      ShowMessage('Fehler beim Speichern der CSV: ' + E.Message);
  end;

end;

procedure TfrmCSVEditor.btnOpenFileClick(Sender: TObject);
var
  WaitForm: TForm;
  WaitLabel: TLabel;
  CSVThread: TCSVLoadThread;
  StartTime, EndTime: TDateTime;
  DiffSeconds: Double;
  Hours, Minutes, Seconds: Integer;
begin
  if not OpenDialog1.Execute then Exit;

  if CSVDataset1.Active then
  begin
    CSVDataset1.Close;
    CSVDataset1.Fields.Clear;
  end;

  SetCSVSettingsFromForm;

  DBNavigator1.DataSource := nil;
  RxDBGrid1.DataSource := nil;

  // ===== START TIME =====
  StartTime := Now;

  // ---- Wartefenster ----
  WaitForm := TForm.Create(nil);
  try
    WaitForm.Width := 320;
    WaitForm.Height := 140;
    WaitForm.Position := poScreenCenter;
    WaitForm.BorderStyle := bsDialog;
    WaitForm.Caption := 'Please wait';
    WaitForm.FormStyle := fsStayOnTop;

    WaitLabel := TLabel.Create(WaitForm);
    WaitLabel.Parent := WaitForm;
    WaitLabel.Left := 20;
    WaitLabel.Top := 50;
    WaitLabel.Caption := 'Loading CSV file...' + sLineBreak + 'Please wait.';

    WaitForm.Show;
    Application.ProcessMessages;
    Sleep(10);

    // ---- Thread starten ----
    CSVThread := TCSVLoadThread.Create(Self, OpenDialog1.FileName);

    // ---- Warten bis Thread fertig ----
    while not CSVThread.Finished do
    begin
      Application.ProcessMessages;
      Sleep(10);
    end;

    DBNavigator1.DataSource := DataSource1;
    RxDBGrid1.DataSource    := DataSource1;
    RxDBGrid1.OptimizeColumnsWidthAll;

    btnCreateSQL.Enabled := true;
  finally
    WaitForm.Free;
  end;

  // ===== END TIME =====
  EndTime := Now;

  // ===== ELAPSED TIME SELBST BERECHNEN =====
  DiffSeconds := (EndTime - StartTime) * 86400.0;

  Hours := Trunc(DiffSeconds / 3600);
  Minutes := Trunc((DiffSeconds - Hours * 3600) / 60);
  Seconds := Trunc(DiffSeconds) mod 60;

  ShowMessage(
    'Start Time: ' + TimeToStr(StartTime) + sLineBreak +
    'End Time:   ' + TimeToStr(EndTime) + sLineBreak +
    'Elapsed:    ' +
    Format('%.2d:%.2d:%.2d', [Hours, Minutes, Seconds])
  );

  FFileName := OpenDialog1.FileName;
  btnSaveFileAs.Enabled := True;

end;

procedure TfrmCSVEditor.btnCreateSQLClick(Sender: TObject);
var
  GenSQLFromCSVDataset: TGenSQLFromCSVDataset;
  TempTableName: string;
  TempFieldLength: Word;
begin
  //Dataset prüfen
  if not CSVDataset1.Active then
  begin
    MessageDlg('Warning',
      'Dataset is not active.',
      mtWarning, [mbOK], 0);
    Exit;
  end;

  if CSVDataset1.FieldCount = 0 then
  begin
    MessageDlg('Warning',
      'Dataset has no fields.',
      mtWarning, [mbOK], 0);
    Exit;
  end;

  //Dateiname prüfen
  if Trim(FFileName) = '' then
  begin
    MessageDlg('Warning',
      'No file loaded.',
      mtWarning, [mbOK], 0);
    Exit;
  end;

  //Default Field Length absichern
  TempFieldLength := CSVDataset1.CSVOptions.DefaultFieldLength;
  if TempFieldLength = 0 then
    TempFieldLength := 50;

  //Tabellenname erzeugen
  TempTableName := UpperCase(ChangeFileExt(ExtractFileName(FFileName), ''));

  GenSQLFromCSVDataset := nil;

  try
    GenSQLFromCSVDataset := TGenSQLFromCSVDataset.Create(
      CSVDataset1,
      TempTableName,
      TempFieldLength
    );

    //SQL Anzeige bewusst einfach lassen
    SynEdit1.Lines.Text := GenSQLFromCSVDataset.SQL;
    PageControl1.ActivePage := tsSQL;

  except
    on E: Exception do
    begin
      MessageDlg('Error',
        'Error generating SQL:'#13#10 + E.Message,
        mtError, [mbOK], 0);
    end;
  end;

  FreeAndNil(GenSQLFromCSVDataset);
end;

procedure TfrmCSVEditor.btnSaveFileAsClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
    SaveCSVFile(SaveDialog1.FileName);
end;

procedure TfrmCSVEditor.CSVDataset1AfterDelete(DataSet: TDataSet);
begin
  SaveCSVFile(FFileName);
end;

procedure TfrmCSVEditor.DBNavigator1Click(Sender: TObject; Button: TDBNavButtonType);
begin
  if Button = nbRefresh then
  begin
    LoadCSVFile(FFileName);
    abort;
  end;
  if Button = nbPost then
  begin
    SaveCSVFile(FFileName);
    //abort;
  end;
end;

procedure TfrmCSVEditor.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  WriteIni;
  CloseAction := caFree;
end;

procedure TfrmCSVEditor.FormCreate(Sender: TObject);
begin
  ReadIni;
end;

procedure TfrmCSVEditor.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(self);

  SynEdit1.Color      := QWEditorBackgroundColor;
  SynEdit1.Font.Name  := QWEditorFontName;
  SynEdit1.Font.Size  := QWEditorFontSize;
  SynEdit1.Font.Color := QWEditorFontColor;
end;

procedure TfrmCSVEditor.btnCopySQLClick(Sender: TObject);
begin
  if Trim(SynEdit1.Lines.Text) = '' then
  begin
    MessageDlg('Warning',
      'Nothing to copy.',
      mtWarning, [mbOK], 0);
    Exit;
  end;

  try
    if SynEdit1.SelText <> '' then
      Clipboard.AsText := SynEdit1.SelText
    else
      Clipboard.AsText := SynEdit1.Lines.Text;

    MessageDlg('Info',
      'SQL copied to clipboard.',
      mtInformation, [mbOK], 0);

  except
    on E: Exception do
      MessageDlg('Error',
        'Failed to copy to clipboard:'#13#10 + E.Message,
        mtError, [mbOK], 0);
  end;
end;

end.
