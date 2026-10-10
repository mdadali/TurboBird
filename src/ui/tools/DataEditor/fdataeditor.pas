unit fdataeditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, DB, csvdataset, SdfData, dbf, Forms, Controls, Graphics,
  Dialogs, Clipbrd, DBCtrls, StdCtrls, ExtCtrls, ComCtrls, Menus, SynEdit,
  SynHighlighterSQL, RxDBGrid, RxDBGridExportPdf, RxDBGridPrintGrid,
  RxDBGridExportSpreadSheet, fpsDataset,


  IB,
  IBDatabase,
  IBQuery,
  IBTable,

  turbocommon,
  SysTables,
  uthemeselector,
  uGenSQLFromCSVDataset,
  c_json_dataset,
  uCreateTableFromDataSet,

  fdataexportersintrf,

  fpsTypes,

  uReport,
  uSystemInfo
  ;

type
  TDetectChoice = (dcUseDetected, dcKeepCurrent, dcCancel);

  TfrmDataEditor = class;

type
  TCSVLoadThread = class(TThread)
  private
    FFileName: string;
    FOwner: TfrmDataEditor;
    FLoadError: string;
    procedure CloseDataset;
    procedure LoadCSVInDataset;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TfrmDataEditor; const AFileName: string);
    property LoadError: string read FLoadError;
  end;

  { TfrmDataEditor }
  TfrmDataEditor = class(TForm)
    btnCopySQL: TButton;
    btnOpenFile: TButton;
    btnExportAs: TButton;
    btnCreateSQL: TButton;
    btnCreateTable: TButton;
    CheckBoxAutoDedectLineEnding: TCheckBox;
    CheckBoxAutoDedectQuoteChar: TCheckBox;
    CheckBoxAutoDedectDelimiter: TCheckBox;
    chkBoxIgnoreOuterWhiteSpace: TCheckBox;
    chkBoxFirstLineAsFieldName: TCheckBox;
    chkBoxQuoteOuterWhiteSpace: TCheckBox;
    cmbboxLineEnding: TComboBox;
    cmbboxDelimiter: TComboBox;
    cmbboxQuoteChar: TComboBox;
    CSVDataset1: TCSVDataset;
    DataSource1: TDataSource;
    Dbf1: TDbf;
    DBNavigator1: TDBNavigator;
    edtDefaultFieldLength: TEdit;
    FixedFormatDataSet1: TFixedFormatDataSet;
    JSONDataSet1: TJSONDataSet;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lmExportDataAsHtml: TMenuItem;
    lmExportDataAsMarkDownTable: TMenuItem;
    lmExportDataAsPDF: TMenuItem;
    lmExportDataAsSpreadSheet: TMenuItem;
    lmExportDataSet: TMenuItem;
    lmExportToClipboard: TMenuItem;
    lmPrintData: TMenuItem;
    lmStdExportFormats: TMenuItem;
    OpenDialog1: TOpenDialog;
    Panel2: TPanel;
    pmGrid: TPopupMenu;
    RxDBGridExportPDF1: TRxDBGridExportPDF;
    RxDBGridExportSpreadSheet1: TRxDBGridExportSpreadSheet;
    RxDBGridPrint1: TRxDBGridPrint;
    SaveDialog1: TSaveDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    RxDBGrid1: TRxDBGrid;
    ScrollBox1: TScrollBox;
    SdfDataSet1: TSdfDataSet;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    sWorksheetDataset1: TsWorksheetDataset;
    SynEditSQL: TSynEdit;
    SynSQLSyn1: TSynSQLSyn;
    tsSQL: TTabSheet;
    tsCSVSettings: TTabSheet;
    tsGrid: TTabSheet;
    tsDetail: TTabSheet;
    procedure btnCopySQLClick(Sender: TObject);
    procedure btnCreateSQLClick(Sender: TObject);
    procedure btnCreateTableClick(Sender: TObject);
    procedure btnOpenFileClick(Sender: TObject);
    procedure btnExportAsClick(Sender: TObject);
    procedure cmbboxDelimiterChange(Sender: TObject);
    procedure cmbboxLineEndingChange(Sender: TObject);
    procedure cmbboxQuoteCharChange(Sender: TObject);
    procedure CSVDataset1AfterDelete(DataSet: TDataSet);
    procedure DBNavigator1Click(Sender: TObject; Button: TDBNavButtonType);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lmExportDataAsHtmlClick(Sender: TObject);
    procedure lmExportDataAsMarkDownTableClick(Sender: TObject);
    procedure lmExportDataAsPDFClick(Sender: TObject);
    procedure lmExportDataAsSpreadSheetClick(Sender: TObject);
    procedure lmExportToClipboardClick(Sender: TObject);
    procedure lmPrintDataClick(Sender: TObject);
    procedure lmStdExportFormatsClick(Sender: TObject);
  private
    FFileName: string;

    FLoadTimer: TTimer;
    FLoadStartTime: TDateTime;
    FLoadElapsedLabel: TLabel;
    FLoadStatusLabel: TLabel;
    FLoadDotCount: Integer;
    procedure LoadTimerTick(Sender: TObject);

    procedure ResetAllDatasets;
    procedure LoadFile(const FileName: string);
    procedure SaveFile(const FileName: string);
    procedure SetCSVSettingsFromForm;
    function  SelectDataSetByFileName(const AFileName: string): Boolean;
    procedure SyncLineEndingCombo;
    procedure SyncDelimiterCombo;
    procedure SyncQuoteCharCombo;
    procedure ReadIni;
    procedure WriteIni;
    function AskUseDetected(
      const AFileName: string;
      const ACurLE, ADetLE: string;
      const ACurDel, ADetDel: Char;
      const ACurQC, ADetQC: Char): TDetectChoice;
  public
  end;

//var
  //frmDataEditor: TfrmDataEditor;

implementation

{$R *.lfm}

procedure TfrmDataEditor.LoadTimerTick(Sender: TObject);
var
  Elapsed: TDateTime;
begin
  if Assigned(FLoadElapsedLabel) then
  begin
    Elapsed := Now - FLoadStartTime;
    FLoadElapsedLabel.Caption := 'Elapsed: ' + FormatDateTime('hh:nn:ss', Elapsed);
  end;

  if Assigned(FLoadStatusLabel) then
  begin
    Inc(FLoadDotCount);
    if FLoadDotCount > 3 then
      FLoadDotCount := 0;

    case FLoadDotCount of
      0: FLoadStatusLabel.Caption := 'Loading';
      1: FLoadStatusLabel.Caption := 'Loading.';
      2: FLoadStatusLabel.Caption := 'Loading..';
      3: FLoadStatusLabel.Caption := 'Loading...';
    end;
  end;
end;

constructor TCSVLoadThread.Create(AOwner: TfrmDataEditor; const AFileName: string);
begin
  inherited Create(True);
  FreeOnTerminate := False;   // ← geändert: wir lesen LoadError nach Finish
  FOwner := AOwner;
  FFileName := AFileName;
  FLoadError := '';
  Start;
end;

procedure TCSVLoadThread.CloseDataset;
begin
  if not Assigned(FOwner.DataSource1.DataSet) then Exit;

  if FOwner.DataSource1.DataSet.Active then
    FOwner.DataSource1.DataSet.Close;
  FOwner.DataSource1.DataSet.Fields.Clear;
end;

procedure TCSVLoadThread.LoadCSVInDataset;
begin
  try
    if FOwner.DataSource1.DataSet = FOwner.CSVDataset1 then
      FOwner.CSVDataset1.LoadFromCSVFile(FFileName)

    else if FOwner.DataSource1.DataSet = FOwner.JSONDataset1 then
      FOwner.JSONDataset1.LoadFromFile(FFileName)

    else if FOwner.DataSource1.DataSet = FOwner.sWorksheetDataset1 then
    begin
      FOwner.sWorksheetDataset1.FileName := FFileName;
    end

    else if FOwner.DataSource1.DataSet = FOwner.Dbf1 then
    begin
      FOwner.Dbf1.FilePathFull := FFileName;
    end

    else if FOwner.DataSource1.DataSet = FOwner.SdfDataSet1 then
    begin
      FOwner.SdfDataSet1.FileName := FFileName;
    end

    else if FOwner.DataSource1.DataSet = FOwner.FixedFormatDataSet1 then
    begin
      FOwner.FixedFormatDataSet1.FileName := FFileName;
    end;

    FOwner.DataSource1.DataSet.Open;
    FOwner.RxDBGrid1.OptimizeColumnsWidthAll;

  except
    on E: Exception do
    begin
      // Fehler merken — wird in btnOpenFileClick angezeigt
      FLoadError := E.Message;

      // Kompletten Zustand plattmachen — nächster Ladeversuch startet sauber
      FOwner.ResetAllDatasets;
    end;
  end;
end;

procedure TCSVLoadThread.Execute;
begin
  // Dataset im Hauptthread vorbereiten
  Synchronize(@CloseDataset);

  // CSV im Hauptthread laden
  Synchronize(@LoadCSVInDataset);
end;

procedure TfrmDataEditor.SyncLineEndingCombo;
begin
  if CSVLineEnding = #13#10 then
    cmbboxLineEnding.ItemIndex := 0
  else if CSVLineEnding = #10 then
    cmbboxLineEnding.ItemIndex := 1
  else if CSVLineEnding = #13 then
    cmbboxLineEnding.ItemIndex := 2
  else if CSVLineEnding = LineEnding then
    cmbboxLineEnding.ItemIndex := 3
  else
    cmbboxLineEnding.ItemIndex := 1;   // Fallback LF
end;

procedure TfrmDataEditor.SyncDelimiterCombo;
begin
  if CSVDelimiter = ',' then
    cmbboxDelimiter.ItemIndex := 0
  else if CSVDelimiter = ';' then
    cmbboxDelimiter.ItemIndex := 1
  else if CSVDelimiter = #9 then
    cmbboxDelimiter.ItemIndex := 2
  else if CSVDelimiter = '|' then
    cmbboxDelimiter.ItemIndex := 3
  else if CSVDelimiter = ' ' then
    cmbboxDelimiter.ItemIndex := 4
  else
    cmbboxDelimiter.ItemIndex := 0;   // Fallback Komma
end;

procedure TfrmDataEditor.SyncQuoteCharCombo;
begin
  if not Assigned(cmbboxQuoteChar) then Exit;
  if cmbboxQuoteChar.Items.Count = 0 then Exit;

  if CSVQuoteChar = '"' then
    cmbboxQuoteChar.ItemIndex := 0
  else if CSVQuoteChar = '''' then
    cmbboxQuoteChar.ItemIndex := 1
  else if CSVQuoteChar = #0 then
    cmbboxQuoteChar.ItemIndex := 2
  else
  begin
    // Exotisches Zeichen — wir zeigen es direkt in der Combo
    cmbboxQuoteChar.ItemIndex := 3;   // Custom...
    cmbboxQuoteChar.Text := CSVQuoteChar;
  end;
end;

procedure TfrmDataEditor.SetCSVSettingsFromForm;
begin
  CSVDataset1.CSVOptions.DefaultFieldLength := StrToIntDef(edtDefaultFieldLength.Text, 50);
  CSVDataset1.CSVOptions.Delimiter := CSVDelimiter;
  CSVDataset1.CSVOptions.LineEnding := CSVLineEnding;

  if CSVQuoteChar <> #0 then
    CSVDataset1.CSVOptions.QuoteChar := CSVQuoteChar
  else
    CSVDataset1.CSVOptions.QuoteChar := #0;

  CSVDataset1.CSVOptions.FirstLineAsFieldNames := chkBoxFirstLineAsFieldName.Checked;
  CSVDataset1.CSVOptions.IgnoreOuterWhitespace := chkBoxIgnoreOuterWhiteSpace.Checked;
  CSVDataset1.CSVOptions.QuoteOuterWhitespace  := chkBoxQuoteOuterWhiteSpace.Checked;
end;

procedure TfrmDataEditor.ReadIni;
var TempStr: string;
begin
  TempStr := fIniFile.ReadString('CSVEditor','CSVQuoteChar','"');
  if TempStr <> '' then
    CSVQuoteChar := TempStr[1]
  else
    CSVQuoteChar := #0;   // leer = keine Quotes
  SyncQuoteCharCombo;
  edtDefaultFieldLength.Text          := IntToStr(CSVDefaultFieldLength);
  chkBoxFirstLineAsFieldName.Checked  := CSVFirstLineAsFieldNames;
  chkBoxIgnoreOuterWhiteSpace.Checked := CSVIgnoreOuterWhitespace;
  chkBoxQuoteOuterWhiteSpace.Checked  := CSVQuoteOuterWhitespace;

  // LineEnding-Combo anhand der INI-geladenen CSVLineEnding positionieren
  SyncLineEndingCombo;
  SyncDelimiterCombo;
  SyncQuoteCharCombo;
end;

procedure TfrmDataEditor.WriteIni;
begin
  CSVDefaultFieldLength := StrToIntDef(edtDefaultFieldLength.Text, 50);

  // CSVDelimiter wird bereits durch cmbboxDelimiterChange gesetzt
  // — hier nichts zu tun

// CSVQuoteChar wird durch cmbboxQuoteCharChange gesetzt — nichts tun

  CSVFirstLineAsFieldNames := chkBoxFirstLineAsFieldName.Checked;
  CSVIgnoreOuterWhitespace := chkBoxIgnoreOuterWhiteSpace.Checked;
  CSVQuoteOuterWhitespace  := chkBoxQuoteOuterWhiteSpace.Checked;
end;

procedure TfrmDataEditor.ResetAllDatasets;
begin
  RxDBGrid1.DataSource := nil;
  DBNavigator1.DataSource := nil;
  DataSource1.DataSet := nil;

  // ---- CSVDataset1 ----
  if Assigned(CSVDataset1) then
  begin
    try
      if CSVDataset1.Active then
      begin
        CSVDataset1.Close;
      end;
    except
      // Zustand kaputt — nichts tun, Fields.Clear kommt eh
    end;
    try
      CSVDataset1.Fields.Clear;
    except end;
    try
      CSVDataset1.FieldDefs.Clear;
    except end;
  end;

  // ---- JSONDataset1 ----
  if Assigned(JSONDataset1) then
  begin
    try
      if JSONDataset1.Active then
        JSONDataset1.Close;
    except end;
    try
      JSONDataset1.Fields.Clear;
    except end;
    try
      JSONDataset1.FieldDefs.Clear;
    except end;
  end;

  // ---- sWorksheetDataset1 ----
  if Assigned(sWorksheetDataset1) then
  begin
    try
      if sWorksheetDataset1.Active then
        sWorksheetDataset1.Close;
    except end;
    try
      sWorksheetDataset1.Fields.Clear;
    except end;
    try
      sWorksheetDataset1.FieldDefs.Clear;
    except end;
  end;

  // ---- Dbf1 ----
  if Assigned(Dbf1) then
  begin
    try
      if Dbf1.Active then
        Dbf1.Close;
    except end;
    try
      Dbf1.Fields.Clear;
    except end;
    try
      Dbf1.FieldDefs.Clear;
    except end;
  end;

  // ---- SdfDataSet1 ----
  if Assigned(SdfDataSet1) then
  begin
    try
      if SdfDataSet1.Active then
        SdfDataSet1.Close;
    except end;
    try
      SdfDataSet1.Fields.Clear;
    except end;
    try
      SdfDataSet1.FieldDefs.Clear;
    except end;
  end;

  // ---- FixedFormatDataSet1 ----
  if Assigned(FixedFormatDataSet1) then
  begin
    try
      if FixedFormatDataSet1.Active then
        FixedFormatDataSet1.Close;
    except end;
    try
      FixedFormatDataSet1.Fields.Clear;
    except end;
    try
      FixedFormatDataSet1.FieldDefs.Clear;
    except end;
  end;

  try
    RxDBGrid1.Columns.Clear;
  except end;

  try
    SynEditSQL.Lines.Clear;
  except end;
end;

procedure TfrmDataEditor.LoadFile(const FileName: string);
begin
  if DataSource1.DataSet.Active then
  begin
    DataSource1.DataSet.Close;
    DataSource1.DataSet.Fields.Clear;
  end;

  RxDBGrid1.BeginUpdate;

  if DataSource1.DataSet = CSVDataset1 then
    CSVDataset1.LoadFromCSVFile(FileName)

  else if DataSource1.DataSet = JSONDataset1 then
    JSONDataset1.LoadFromFile(FileName)

  else if DataSource1.DataSet = sWorksheetDataset1 then
  begin
    sWorksheetDataset1.FileName := FileName;
    sWorksheetDataset1.SheetName := ChangeFileExt(ExtractFileName(FileName), '');;
    sWorksheetDataset1.Open;
  end

  else if DataSource1.DataSet = Dbf1 then
  begin
    Dbf1.FilePathFull := FileName;
    Dbf1.Open;
  end

  else if DataSource1.DataSet = SdfDataSet1 then
  begin
    SdfDataSet1.FileName := FileName;
    SdfDataSet1.Open;
  end

  else if DataSource1.DataSet = FixedFormatDataSet1 then
  begin
    FixedFormatDataSet1.FileName := FileName;
    FixedFormatDataSet1.Open;
  end;

  RxDBGrid1.OptimizeColumnsWidthAll;
  RxDBGrid1.EndUpdate(True);
end;

procedure SaveJSONToFile(DataSet: TJSONDataSet; const FileName: string);
var
  JSONText: string;
  SL: TStringList;
begin
  // 1. JSON aus dem Dataset erzeugen
  JSONText := DataSet.SaveToJSON;

  // 2. In Datei schreiben
  SL := TStringList.Create;
  try
    SL.Text := JSONText;
    SL.SaveToFile(FileName);
  finally
    SL.Free;
  end;
end;

procedure TfrmDataEditor.SaveFile(const FileName: string);
var
  P: TPoint;
begin
  if not DataSource1.DataSet.Active then Exit;

  // Für dateibasierte Datasets: direkt speichern
  if DataSource1.DataSet = CSVDataSet1 then
    CSVDataSet1.SaveToCSVFile(FileName)
  else if DataSource1.DataSet = JSONDataSet1 then
    SaveJSONToFile(JSONDataSet1, FileName)
  else if DataSource1.DataSet = sWorksheetDataset1 then
    sWorksheetDataset1.Flush
  else if DataSource1.DataSet = Dbf1 then
  begin
    Dbf1.FilePathFull := FileName;
    Dbf1.Close;
    Dbf1.Open;
  end
  else if DataSource1.DataSet = SdfDataSet1 then
    SdfDataSet1.SaveFileAs(FileName)
  else if DataSource1.DataSet = FixedFormatDataSet1 then
    FixedFormatDataSet1.SaveFileAs(FileName)
  else
  begin
    // Für DB-Datasets: Export-Popup zeigen
    P := btnExportAs.ClientToScreen(Point(0, btnExportAs.Height));
    pmGrid.PopUp(P.X, P.Y);
  end;
end;

function TfrmDataEditor.SelectDataSetByFileName(const AFileName: string): Boolean;
var
  Ext: string;
  DetLE: string;
  DetDel, DetQC: Char;
  Choice: TDetectChoice;
begin
  Result := True;
  Ext := LowerCase(ExtractFileExt(AFileName));

  ResetAllDatasets;

  if (Ext = '.csv') then
  begin
    DataSource1.DataSet := CSVDataSet1;

    // Auto-Detection
    DetLE  := DetectLineEnding(AFileName);
    DetDel := DetectDelimiter(AFileName);
    DetQC  := DetectQuoteChar(AFileName, DetDel);

    Choice := AskUseDetected(AFileName,
                             CSVLineEnding, DetLE,
                             CSVDelimiter,  DetDel,
                             CSVQuoteChar,  DetQC);

    if Choice = dcCancel then
    begin
      Result := False;
      Exit;   // Datei-Öffnen wird vom Aufrufer abgebrochen
    end;

    if Choice = dcUseDetected then
    begin
      CSVLineEnding := DetLE;
      CSVDelimiter  := DetDel;
      CSVQuoteChar  := DetQC;
    end;
    // dcKeepCurrent: aktuelle Werte behalten — nichts tun

    SyncLineEndingCombo;
    SyncDelimiterCombo;
    SyncQuoteCharCombo;
  end

  else if (Ext = '.json') then
    DataSource1.DataSet := JSONDataSet1

  else if (Ext = '.xlsx') or (Ext = '.xls') or (Ext = '.xml')
       or (Ext = '.html') or (Ext = '.htm') or (Ext = '.ods')
       or (Ext = '.wiki') or (Ext = '.wikitable') then
  begin
    DataSource1.DataSet := sWorksheetDataset1;
    // ... FileFormat-Zuweisung wie gehabt ...
  end

  else if (Ext = '.dbf') then
    DataSource1.DataSet := Dbf1

  else if (Ext = '.sdf') then
    DataSource1.DataSet := SdfDataSet1

  else if (Ext = '.txt') then
    DataSource1.DataSet := FixedFormatDataSet1

  else
    raise Exception.Create('Unknown file extension: ' + Ext);
end;

procedure TfrmDataEditor.btnOpenFileClick(Sender: TObject);
var
  WaitForm: TForm;
  PnlTop, PnlInfo, PnlStatus, PnlBottom: TPanel;
  LblTitle, LblFile, LblSize, LblRows: TLabel;
  CSVThread: TCSVLoadThread;
  StartTime, EndTime: TDateTime;
  DiffSeconds, FileSizeMB: Double;
  FileSize: Int64;
  RowCount: Integer;
  Stats: TTransferStatistic;
  ReportForm: TfrmReport;
  LoadErr: string;
begin
  if not OpenDialog1.Execute then Exit;

  PageControl1.ActivePage := tsGrid;

  FFileName := OpenDialog1.FileName;

  // SelectDataSetByFileName kann durch User-Cancel false zurückgeben
  if not SelectDataSetByFileName(FFileName) then
  begin
    FFileName := '';      // Zustand zurücksetzen
    Exit;                 // Datei wird nicht geladen
  end;

  SynEditSQL.Lines.Clear;

  SetCSVSettingsFromForm;

  DBNavigator1.DataSource := nil;
  RxDBGrid1.DataSource := nil;

  // Dateigröße ermitteln
  FileSize := 0;
  if FileExists(FFileName) then
  begin
    with TFileStream.Create(FFileName, fmOpenRead or fmShareDenyNone) do
    try
      FileSize := Size;
    finally
      Free;
    end;
  end;
  FileSizeMB := FileSize / (1024 * 1024);

  StartTime := Now;
  FLoadStartTime := StartTime;

  // ============================================================
  // Warteformular
  // ============================================================
  WaitForm := TForm.Create(nil);
  try
    WaitForm.Caption := 'Loading...';
    WaitForm.Width := 520;
    WaitForm.Height := 260;
    WaitForm.Position := poScreenCenter;
    WaitForm.BorderStyle := bsDialog;
    WaitForm.FormStyle := fsStayOnTop;

    // === Titel ===
    PnlTop := TPanel.Create(WaitForm);
    PnlTop.Parent := WaitForm;
    PnlTop.Align := alTop;
    PnlTop.Height := 45;
    PnlTop.BevelOuter := bvNone;

    LblTitle := TLabel.Create(WaitForm);
    LblTitle.Parent := PnlTop;
    LblTitle.Left := 16;
    LblTitle.Top := 12;
    LblTitle.Caption := 'Loading file, please wait...';
    LblTitle.Font.Size := 11;
    LblTitle.Font.Style := [fsBold];

    // === Info-Panel ===
    PnlInfo := TPanel.Create(WaitForm);
    PnlInfo.Parent := WaitForm;
    PnlInfo.Align := alTop;
    PnlInfo.Height := 85;
    PnlInfo.BevelOuter := bvNone;

    LblFile := TLabel.Create(WaitForm);
    LblFile.Parent := PnlInfo;
    LblFile.Left := 16;
    LblFile.Top := 8;
    LblFile.Caption := 'File:     ' + ExtractFileName(FFileName);
    LblFile.Width := 480;

    LblSize := TLabel.Create(WaitForm);
    LblSize.Parent := PnlInfo;
    LblSize.Left := 16;
    LblSize.Top := 30;
    LblSize.Caption := Format('Size:     %s bytes (%.2f MB)',
      [FormatFloat('#,##0', FileSize), FileSizeMB]);
    LblSize.Width := 480;

    LblRows := TLabel.Create(WaitForm);
    LblRows.Parent := PnlInfo;
    LblRows.Left := 16;
    LblRows.Top := 52;
    LblRows.Caption := 'Rows:     reading...';
    LblRows.Width := 480;

    // === Status + Marquee ===
    PnlStatus := TPanel.Create(WaitForm);
    PnlStatus.Parent := WaitForm;
    PnlStatus.Align := alTop;
    PnlStatus.Height := 45;
    PnlStatus.BevelOuter := bvNone;

    FLoadStatusLabel := TLabel.Create(WaitForm);
    FLoadStatusLabel.Parent := PnlStatus;
    FLoadStatusLabel.Left := 16;
    FLoadStatusLabel.Top := 12;
    FLoadStatusLabel.Caption := 'Reading file, please wait...';
    FLoadStatusLabel.Font.Size := 10;
    FLoadStatusLabel.Font.Style := [fsBold];
    FLoadStatusLabel.Width := 480;

    // === Elapsed ===
    PnlBottom := TPanel.Create(WaitForm);
    PnlBottom.Parent := WaitForm;
    PnlBottom.Align := alClient;
    PnlBottom.BevelOuter := bvNone;

    FLoadElapsedLabel := TLabel.Create(WaitForm);
    FLoadElapsedLabel.Parent := PnlBottom;
    FLoadElapsedLabel.Left := 16;
    FLoadElapsedLabel.Top := 12;
    FLoadElapsedLabel.Caption := 'This may take a while for large files.';
    FLoadElapsedLabel.Width := 480;

    // === SOFORT SICHTBAR ===
    WaitForm.Show;
    WaitForm.BringToFront;
    Application.ProcessMessages;
    Sleep(100);
    Application.ProcessMessages;

    // ============================================================
    // Thread starten
    // ============================================================
    CSVThread := TCSVLoadThread.Create(Self, OpenDialog1.FileName);

    while not CSVThread.Finished do
    begin
      Application.ProcessMessages;
      Sleep(20);
    end;

    // ============================================================
    // Fehler prüfen — BEVOR wir die Grids reaktivieren
    // ============================================================
    LoadErr := CSVThread.LoadError;
    CSVThread.Free;
    CSVThread := nil;

    if LoadErr <> '' then
    begin
      LblRows.Caption := 'Rows:     — load failed —';
      FLoadStatusLabel.Caption := 'Error.';
      FLoadElapsedLabel.Caption := 'Elapsed: ' +
        FormatDateTime('hh:nn:ss', Now - StartTime);
      Application.ProcessMessages;

      MessageDlg(
        'Could not load file:' + sLineBreak + sLineBreak +
        'File: ' + ExtractFileName(FFileName) + sLineBreak +
        'Ext:  ' + LowerCase(ExtractFileExt(FFileName)) + sLineBreak +
        sLineBreak +
        'Error:' + sLineBreak + LoadErr,
        mtError, [mbOK], 0);

      Caption := 'Turbobird - Data Editor';
      btnCreateSQL.Enabled := False;
      btnExportAs.Enabled := False;

      Exit;
    end;

    // === Datasets reaktivieren ===
    DataSource1.DataSet := DataSource1.DataSet;   // Kick: Rebind erzwingen

    DBNavigator1.DataSource := DataSource1;
    RxDBGrid1.DataSource := DataSource1;
    RxDBGrid1.OptimizeColumnsWidthAll;

    // Grid explizit neu aufbauen
    RxDBGrid1.Columns.Clear;                    // ← jetzt HIER, nach Reattach
    if DataSource1.DataSet.Active and
       (not DataSource1.DataSet.IsEmpty) then
    begin
      DataSource1.DataSet.First;
      RxDBGrid1.OptimizeColumnsWidthAll;
    end;

    RxDBGrid1.Refresh;                          // Neu zeichnen
    RxDBGrid1.Invalidate;                       // Redraw erzwingen

    EndTime := Now;
    RowCount := DataSource1.DataSet.RecordCount;

    // Status aktualisieren
    LblRows.Caption := 'Rows:     ' + FormatFloat('#,##0', RowCount);
    FLoadStatusLabel.Caption := 'Finished.';
    FLoadElapsedLabel.Caption := 'Elapsed: ' +
      FormatDateTime('hh:nn:ss', EndTime - StartTime);
    Application.ProcessMessages;

    Sleep(400);

  finally
    WaitForm.Free;
  end;

  // ============================================================
  // End-Statistik als Report
  // ============================================================
  DiffSeconds := (EndTime - StartTime) * 86400.0;

  Caption := 'Turbobird - Data Editor (' + ExtractFileName(FFileName) + ')';
  btnCreateSQL.Enabled := True;
  btnExportAs.Enabled := True;

  // --- Statistik-Record füllen ---
  FillChar(Stats, SizeOf(Stats), 0);
  Stats.Kind             := tkLoadFile;
  Stats.SourceKind       := 'File';
  Stats.SourceFileName   := FFileName;
  Stats.SourceFileSize   := FileSize;
  Stats.SourceFileFormat := UpperCase(ExtractFileExt(FFileName));
  // Punkt am Anfang entfernen: '.CSV' → 'CSV'
  if (Stats.SourceFileFormat <> '') and (Stats.SourceFileFormat[1] = '.') then
    Delete(Stats.SourceFileFormat, 1, 1);
  Stats.OptionsExtra     := 'Read-Only';
  Stats.RowsProcessed    := RowCount;
  Stats.FieldsCount      := DataSource1.DataSet.FieldCount;
  Stats.ElapsedSeconds   := DiffSeconds;
  Stats.SystemInfo       := GetSystemInfo(FFileName);

  // --- CSV-Optionen (nur bei CSV-Dateien) ---
  // Aus dem Dataset lesen — das kennt den tatsächlichen Zustand
  // nach SetCSVSettingsFromForm und ist immer synchron.
  // --- CSV-Optionen (nur bei CSV-Dateien) ---
  // Aus dem Dataset lesen — das kennt den tatsächlichen Zustand
  // nach SetCSVSettingsFromForm und ist immer synchron.
  if (UpperCase(Stats.SourceFileFormat) = 'CSV') and
     (DataSource1.DataSet = CSVDataset1) then
  begin
    // Delimiter symbolisch darstellen
    if CSVDataset1.CSVOptions.Delimiter = #9 then
      Stats.CSVOptions := '    Delimiter: ' + QuotedStr('\t') + sLineBreak
    else if CSVDataset1.CSVOptions.Delimiter = ' ' then
      Stats.CSVOptions := '    Delimiter: ' + QuotedStr('Space') + sLineBreak
    else
      Stats.CSVOptions := '    Delimiter: ' +
        QuotedStr(CSVDataset1.CSVOptions.Delimiter) + sLineBreak;

    // QuoteChar: #0 → (none), sonst als Zeichen
    if CSVDataset1.CSVOptions.QuoteChar = #0 then
      Stats.CSVOptions := Stats.CSVOptions +
        '    Quote Char: (none)' + sLineBreak
    else
      Stats.CSVOptions := Stats.CSVOptions +
        '    Quote Char: ' + QuotedStr(CSVDataset1.CSVOptions.QuoteChar) + sLineBreak;

    Stats.CSVOptions := Stats.CSVOptions +
      '    Line Ending: ' +
        CSVLineEndingStrToName(CSVDataset1.CSVOptions.LineEnding) + sLineBreak +
      '    First Line As Field Names: ' +
        BoolToStr(CSVDataset1.CSVOptions.FirstLineAsFieldNames, 'Yes', 'No') + sLineBreak +
      '    Ignore Outer Whitespace: ' +
        BoolToStr(CSVDataset1.CSVOptions.IgnoreOuterWhitespace, 'Yes', 'No') + sLineBreak +
      '    Quote Outer Whitespace: ' +
        BoolToStr(CSVDataset1.CSVOptions.QuoteOuterWhitespace, 'Yes', 'No');
  end;

  // --- Report anzeigen ---
  ReportForm := TfrmReport.Create(nil);
  try
    ReportForm.SetReportText(FormatTransferReport(Stats));
    ReportForm.ShowModal;
  finally
    ReportForm.Free;
  end;
end;

procedure TfrmDataEditor.btnCreateSQLClick(Sender: TObject);
var
  GenSQLFromCSVDataset: TGenSQLFromCSVDataset;
  TempTableName: string;
  TempFieldLength: Word;
begin
  TempFieldLength := 50;

  if not DataSource1.DataSet.Active then
  begin
    MessageDlg('Warning', 'Dataset is not active.', mtWarning, [mbOK], 0);
    Exit;
  end;

  if DataSource1.DataSet.FieldCount = 0 then
  begin
    MessageDlg('Warning', 'Dataset has no fields.', mtWarning, [mbOK], 0);
    Exit;
  end;

  // Tabellennamen: Aus Dateinamen ODER aus Formular-Titel
  if FFileName <> '' then
    TempTableName := UpperCase(ChangeFileExt(ExtractFileName(FFileName), ''))
  else
  begin
    TempTableName := StringReplace(Caption, 'Turbobird - Data Editor (', '', []);
    TempTableName := StringReplace(TempTableName, ')', '', []);
  end;

  GenSQLFromCSVDataset := nil;
  try
    GenSQLFromCSVDataset := TGenSQLFromCSVDataset.Create(
      DataSource1.DataSet, TempTableName, TempFieldLength);

    SynEditSQL.Lines.Text := GenSQLFromCSVDataset.SQL;
    PageControl1.ActivePage := tsSQL;
    btnCopySQL.Enabled := true;
  except
    on E: Exception do
      MessageDlg('Error', 'Error generating SQL:'#13#10 + E.Message, mtError, [mbOK], 0);
  end;

  FreeAndNil(GenSQLFromCSVDataset);
end;

procedure TfrmDataEditor.btnCreateTableClick(Sender: TObject);
var
  frmCreateTable: TfrmCreateTableFromDataSet;
  TableName: string;
begin
  frmCreateTable := TfrmCreateTableFromDataSet.Create(self);

  if FFileName <> '' then
    frmCreateTable.Init(DataSource1.DataSet, FFileName)
  else
  begin
    // Aus dem Dataset-Titel einen Tabellennamen generieren
    TableName := StringReplace(Caption, 'Turbobird - Data Editor (', '', []);
    TableName := StringReplace(TableName, ')', '', []);
    frmCreateTable.Init(DataSource1.DataSet, TableName);
  end;

  frmCreateTable.ShowModal;
end;

procedure TfrmDataEditor.CSVDataset1AfterDelete(DataSet: TDataSet);
begin
  SaveFile(FFileName);
end;

procedure TfrmDataEditor.DBNavigator1Click(Sender: TObject; Button: TDBNavButtonType);
begin
  if Button = nbRefresh then
    if FFileName <> '' then
      LoadFile(FFileName)

  else if Button = nbPost then
    if FFileName <> '' then
      SaveFile(FFileName)
end;

procedure TfrmDataEditor.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  WriteIni;
  CloseAction := caFree;
end;

procedure TfrmDataEditor.FormCreate(Sender: TObject);
begin
  ReadIni;
  OpenDialog1.Filter :=
    'All supported files|*.csv;*.json;*.xml;*.html;*.htm;*.xls;*.xlsx;*.odt;*.dbf;*.sdf;*.txt;*.wiki;*.wikitable|' +

    'CSV files (*.csv)|*.csv|' +
    'JSON files (*.json)|*.json|' +
    'Excel-XML files (*.xml)|*.xml|' +
    'HTML files (*.html;*.htm)|*.html;*.htm|' +

    'Excel files (*.xls;*.xlsx)|*.xls;*.xlsx|' +
    'OpenDocument Calc (*.ods)|*.ods|' +

    'DBF files (*.dbf)|*.dbf|' +
    'SDF files (*.sdf)|*.sdf|' +
    'Text (Fixed Format) (*.txt)|*.txt|' +

    'WikiTable (Pipes) (*.wiki)|*.wiki|' +
    'WikiTable (MediaWiki) (*.wikitable)|*.wikitable|' +

    'All files (*.*)|*.*';
end;

procedure TfrmDataEditor.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(self);

  SynEditSQL.Color      := QWEditorBackgroundColor;
  SynEditSQL.Font.Name  := QWEditorFontName;
  SynEditSQL.Font.Size  := QWEditorFontSize;
  SynEditSQL.Font.Color := QWEditorFontColor;
end;

procedure TfrmDataEditor.btnExportAsClick(Sender: TObject);
var
  P: TPoint;
begin
  // Popup-Menü unter dem Button anzeigen
  P := btnExportAs.ClientToScreen(Point(0, btnExportAs.Height));
  pmGrid.PopUp(P.X, P.Y);
end;

procedure TfrmDataEditor.cmbboxDelimiterChange(Sender: TObject);
begin
  case cmbboxDelimiter.ItemIndex of
    0: CSVDelimiter := ',';     // Komma
    1: CSVDelimiter := ';';     // Semikolon
    2: CSVDelimiter := #9;      // Tab
    3: CSVDelimiter := '|';     // Pipe
    4: CSVDelimiter := ' ';     // Space
  else
    CSVDelimiter := ',';        // Fallback
  end;

  SetCSVSettingsFromForm;
end;

procedure TfrmDataEditor.cmbboxLineEndingChange(Sender: TObject);
begin
  case cmbboxLineEnding.ItemIndex of
    0: CSVLineEnding := #13#10;         // CRLF
    1: CSVLineEnding := #10;            // LF
    2: CSVLineEnding := #13;            // CR
    3: CSVLineEnding := LineEnding;     // plattformabhängig (System.LineEnding)
  else
    CSVLineEnding := #10;               // Fallback
  end;

  // Dataset sofort mit neuer Einstellung füttern
  SetCSVSettingsFromForm;
end;

procedure TfrmDataEditor.cmbboxQuoteCharChange(Sender: TObject);
var
  NewChar: string;
begin
  if csDestroying in ComponentState then Exit;
  if not Assigned(CSVDataset1) then Exit;

  case cmbboxQuoteChar.ItemIndex of
    0: CSVQuoteChar := '"';
    1: CSVQuoteChar := '''';
    2: CSVQuoteChar := #0;
    3:
      begin
        NewChar := '';
        if InputQuery('Custom Quote Char',
                       'Enter a single character to use as quote:',
                       NewChar) then
        begin
          if Length(NewChar) = 1 then
            CSVQuoteChar := NewChar[1]
          else if NewChar = '' then
            CSVQuoteChar := #0
          else
          begin
            ShowMessage('Please enter exactly one character (or leave empty for none).');
            SyncQuoteCharCombo;
            Exit;
          end;
        end
        else
        begin
          // User hat abgebrochen — Combo zurück
          SyncQuoteCharCombo;
          Exit;
        end;
      end;
  else
    CSVQuoteChar := '"';
  end;

  SetCSVSettingsFromForm;
end;

procedure TfrmDataEditor.lmStdExportFormatsClick(Sender: TObject);
begin
  if DataSource1.DataSet.IsEmpty then
  begin
    ShowMessage('DataSet has no records!');
    exit;
  end;

  ExportStdFormat(DataSource1.DataSet);
end;

procedure TfrmDataEditor.lmExportDataAsHtmlClick(Sender: TObject);
begin
  if DataSource1.DataSet.IsEmpty then
  begin
    ShowMessage('DataSet has no records!');
    exit;
  end;
  ExportDataHtml(DataSource1.DataSet);
end;

procedure TfrmDataEditor.lmExportDataAsMarkDownTableClick(Sender: TObject);
begin
  if DataSource1.DataSet.IsEmpty then
  begin
    ShowMessage('DataSet has no records!');
    exit;
  end;
  ExportDataMarkDownTable(DataSource1.DataSet);
end;

procedure TfrmDataEditor.lmExportDataAsPDFClick(Sender: TObject);
begin
  try
    RxDBGridExportPDF1.Execute;
  except
    raise;
  end;
end;

procedure TfrmDataEditor.lmExportDataAsSpreadSheetClick(Sender: TObject);
begin
  RxDBGridExportSpreadSheet1.Execute;
end;

procedure TfrmDataEditor.lmPrintDataClick(Sender: TObject);
begin
   RxDBGridPrint1.Execute;
end;


{ Copy query result to Clipboard }

procedure TfrmDataEditor.lmExportToClipboardClick(Sender: TObject);
var
  MaxExportRows, RowCount, CopiedRows: Integer;
  MsgText: string;
begin
  if DataSource1.DataSet.IsEmpty then
  begin
    MessageDlg('DataSet has no records!', mtError, [mbOK], 0);
    DataSource1.DataSet.EnableControls;
    Exit;
  end;

  // --- Max rows load from INI---
  MaxExportRows := fIniFile.ReadInteger('ClipboardExport', 'MaxExportRows', -1);
  if MaxExportRows <= 0 then
  begin
    if MaxExportRows = -1 then
    begin
      MaxExportRows := 200;
      fIniFile.WriteInteger('ClipboardExport', 'MaxExportRows', MaxExportRows);
      MessageDlg('No valid MaxExportRows entry found in turbobird.ini. Default 200 has been set.', mtWarning, [mbOK], 0);
    end
    else
    begin
      MessageDlg('MaxExportRows is set to 0 in turbobird.ini. ' +
                 'Exporting very large tables may cause program or system crash!', mtWarning, [mbOK], 0);
    end;
  end;

  RowCount := DataSource1.DataSet.RecordCount;

  if (MaxExportRows > 0) and (RowCount > MaxExportRows) then
  begin
    MessageDlg(
      'The result contains ' + IntToStr(RowCount) + ' rows, which exceeds the export limit of ' +
      IntToStr(MaxExportRows) + ' rows.'#13#10 +
      'Only the first ' + IntToStr(MaxExportRows) + ' rows will be copied to the clipboard.'#13#10#13#10 +
      'You can change this limit in turbobird.ini under [ClipboardExport].',
      mtWarning, [mbOK], 0
    );
  end;

  if MaxExportRows = 0 then
    CopiedRows := RowCount
  else if RowCount > MaxExportRows then
    CopiedRows := MaxExportRows
  else
    CopiedRows := RowCount;

  // --- Export ---
  try
    DataSource1.DataSet.DisableControls;
    ExportDataToClipboard(DataSource1.DataSet, MaxExportRows);

    MsgText := 'Successfully copied ' + IntToStr(CopiedRows) + ' records to the clipboard.';
    if MaxExportRows = 0 then
      MsgText := MsgText + ' (Warning: no export limit set in turbobird.ini! Large tables may cause program/system crash.)';

    MessageDlg(MsgText, mtInformation, [mbOK], 0);
  finally
    DataSource1.DataSet.First;
    DataSource1.DataSet.EnableControls;
  end;
end;

procedure TfrmDataEditor.btnCopySQLClick(Sender: TObject);
begin
  if Trim(SynEditSQL.Lines.Text) = '' then
  begin
    MessageDlg('Warning',
      'Nothing to copy.',
      mtWarning, [mbOK], 0);
    Exit;
  end;

  try
    if SynEditSQL.SelText <> '' then
      Clipboard.AsText := SynEditSQL.SelText
    else
      Clipboard.AsText := SynEditSQL.Lines.Text;

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

function TfrmDataEditor.AskUseDetected(
  const AFileName: string;
  const ACurLE, ADetLE: string;
  const ACurDel, ADetDel: Char;
  const ACurQC, ADetQC: Char): TDetectChoice;
var
  Dlg: TForm;
  Memo: TMemo;
  LblTitle: TLabel;
  BtnUseDetected, BtnKeepCurrent, BtnCancelFile: TButton;
  Content: TStringList;
  HasDiff: Boolean;
  CurLEName, DetLEName: string;
  CurDelStr, DetDelStr: string;
  CurQCStr, DetQCStr: string;
begin
  Result := dcUseDetected;
  if csDestroying in ComponentState then Exit;

  // Nur fragen wenn mindestens ein Wert abweicht
  HasDiff :=
    (ACurLE <> ADetLE) or
    (ACurDel <> ADetDel) or
    (ACurQC <> ADetQC);

  if not HasDiff then Exit;

  // Symbolische Namen fürs UI
  CurLEName := CSVLineEndingStrToName(ACurLE);
  DetLEName := CSVLineEndingStrToName(ADetLE);

  if ACurDel = #9 then CurDelStr := '\t'
  else if ACurDel = ' ' then CurDelStr := 'Space'
  else CurDelStr := QuotedStr(ACurDel);

  if ADetDel = #9 then DetDelStr := '\t'
  else if ADetDel = ' ' then DetDelStr := 'Space'
  else DetDelStr := QuotedStr(ADetDel);

  if ACurQC = #0 then CurQCStr := '(none)'
  else CurQCStr := QuotedStr(ACurQC);

  if ADetQC = #0 then DetQCStr := '(none)'
  else DetQCStr := QuotedStr(ADetQC);

  Dlg := TForm.Create(nil);
  try
    Dlg.Caption := 'CSV Auto-Detection';
    Dlg.Width := 560;
    Dlg.Height := 400;
    Dlg.Position := poScreenCenter;
    Dlg.BorderStyle := bsDialog;
    Dlg.FormStyle := fsStayOnTop;

    LblTitle := TLabel.Create(Dlg);
    LblTitle.Parent := Dlg;
    LblTitle.Left := 16;
    LblTitle.Top := 12;
    LblTitle.Width := 528;
    LblTitle.Caption := 'Auto-detection found different values than your current settings:';
    LblTitle.Font.Style := [fsBold];

    Content := TStringList.Create;
    try
      Content.Add('File: ' + ExtractFileName(AFileName));
      Content.Add('');
      Content.Add('                     Current       Detected');
      Content.Add('  ──────────────     ─────────     ─────────');

      if ACurLE <> ADetLE then
        Content.Add(Format('  LineEnding         %-13s %s', [CurLEName, DetLEName]))
      else
        Content.Add(Format('  LineEnding         %-13s (same)', [CurLEName]));

      if ACurDel <> ADetDel then
        Content.Add(Format('  Delimiter          %-13s %s', [CurDelStr, DetDelStr]))
      else
        Content.Add(Format('  Delimiter          %-13s (same)', [CurDelStr]));

      if ACurQC <> ADetQC then
        Content.Add(Format('  QuoteChar          %-13s %s', [CurQCStr, DetQCStr]))
      else
        Content.Add(Format('  QuoteChar          %-13s (same)', [CurQCStr]));

      Content.Add('');
      Content.Add('Choose an action:');
      Content.Add('');
      Content.Add('  [Use Detected]    — Load file with the auto-detected values');
      Content.Add('  [Keep Current]    — Load file with your current settings');
      Content.Add('  [Cancel]          — Abort — do not load the file');

      Memo := TMemo.Create(Dlg);
      Memo.Parent := Dlg;
      Memo.Left := 16;
      Memo.Top := 38;
      Memo.Width := 528;
      Memo.Height := 250;
      Memo.ReadOnly := True;
      Memo.ScrollBars := ssVertical;
      Memo.Font.Name := 'Courier New';
      Memo.Font.Size := 10;
      Memo.Lines.Assign(Content);
    finally
      Content.Free;
    end;

    // ============================================================
    // Drei Buttons: Use Detected | Keep Current | Cancel
    // ============================================================
    BtnUseDetected := TButton.Create(Dlg);
    BtnUseDetected.Parent := Dlg;
    BtnUseDetected.Caption := 'Use Detected';
    BtnUseDetected.Left := 152;
    BtnUseDetected.Top := 305;
    BtnUseDetected.Width := 120;
    BtnUseDetected.Default := True;
    BtnUseDetected.ModalResult := mrYes;

    BtnKeepCurrent := TButton.Create(Dlg);
    BtnKeepCurrent.Parent := Dlg;
    BtnKeepCurrent.Caption := 'Keep Current';
    BtnKeepCurrent.Left := 280;
    BtnKeepCurrent.Top := 305;
    BtnKeepCurrent.Width := 120;
    BtnKeepCurrent.ModalResult := mrNo;

    BtnCancelFile := TButton.Create(Dlg);
    BtnCancelFile.Parent := Dlg;
    BtnCancelFile.Caption := 'Cancel';
    BtnCancelFile.Left := 408;
    BtnCancelFile.Top := 305;
    BtnCancelFile.Width := 120;
    BtnCancelFile.ModalResult := mrCancel;

    case Dlg.ShowModal of
      mrYes: Result := dcUseDetected;
      mrNo:  Result := dcKeepCurrent;
    else
      Result := dcCancel;
    end;

  finally
    Dlg.Free;
  end;
end;

end.
