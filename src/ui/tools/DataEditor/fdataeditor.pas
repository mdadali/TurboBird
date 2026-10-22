unit fdataeditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, DB, csvdataset, SdfData, dbf, fpjsondataset, Forms,
  Controls, Graphics, Dialogs, Clipbrd, DBCtrls, StdCtrls, ExtCtrls, ComCtrls,
  Menus, SynEdit, SynHighlighterSQL, RxDBGrid, RxDBGridExportPdf,
  RxDBGridPrintGrid, RxDBGridExportSpreadSheet, fpsDataset,


  IB,
  IBDatabase,
  IBQuery,
  IBTable,

  turbocommon,
  SysTables,
  uthemeselector,
  uGenSQLFromCSVDataset,
  uCSVFastDataset,
  uJSONFastDataSet,
  uCreateTableFromDataSet,

  fdataexportersintrf,

  fpsTypes,

  uReport,
  uSystemInfo
;


type
  TCSVConfigStatus = (
    ccsOptimal,       // Zeile passt, Performance gut → kein Dialog
    ccsSuboptimal,    // Zeile passt, aber langsam → Dialog empfohlen
    ccsBroken         // Zeile passt nicht → Dialog zwingend
  );

  TCSVConfigCheck = record
    Status:            TCSVConfigStatus;
    MaxRowWidth:       Int64;
    CurrentBlockSize:  Int64;
    CurrentBufferSize: Int64;
    OptimalBlockSize:  Int64;
    OptimalBufferSize: Int64;
    Reason:            string;
  end;

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
    Button1: TButton;
    chkBoxAutoTuneCSVBuffer: TCheckBox;
    CheckBoxAutoDedectLineEnding: TCheckBox;
    CheckBoxAutoDedectQuoteChar: TCheckBox;
    CheckBoxAutoDedectDelimiter: TCheckBox;
    chkBoxIgnoreOuterWhiteSpace: TCheckBox;
    chkBoxFirstLineAsFieldName: TCheckBox;
    chkBoxQuoteOuterWhiteSpace: TCheckBox;
    cmbboxLineEnding: TComboBox;
    cmbboxDelimiter: TComboBox;
    cmbboxQuoteChar: TComboBox;
    CSVFastDataset1: TCSVFastDataset;
    DataSource1: TDataSource;
    Dbf1: TDbf;
    DBNavigator1: TDBNavigator;
    edtProgressUpdatePercent: TEdit;
    edtRowWidthScanRows: TEdit;
    edtFrom: TEdit;
    edtBlockSize: TEdit;
    edtBufferSize: TEdit;
    edtDefaultFieldLength: TEdit;
    edtInitialCellCapacity: TEdit;
    edtTo: TEdit;
    FixedFormatDataSet1: TFixedFormatDataSet;
    grboxCSVFileRead: TGroupBox;
    grboxRangeOptions: TGroupBox;
    JSONFastDataSet1: TJSONFastDataSet;
    Label1: TLabel;
    Label10: TLabel;
    Label5: TLabel;
    lblRowWidthScanRows: TLabel;
    lblActivePerformanceSettings: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    lbBlockSize: TLabel;
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
    rbAllRows: TRadioButton;
    rbRange: TRadioButton;
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
    tsCommonSettings: TTabSheet;
    tsSQL: TTabSheet;
    tsCSVSettings: TTabSheet;
    tsGrid: TTabSheet;
    tsDetail: TTabSheet;
    procedure btnCopySQLClick(Sender: TObject);
    procedure btnCreateSQLClick(Sender: TObject);
    procedure btnCreateTableClick(Sender: TObject);
    procedure btnOpenFileClick(Sender: TObject);
    procedure btnExportAsClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure chkBoxAutoTuneCSVBufferChange(Sender: TObject);
    procedure cmbboxDelimiterChange(Sender: TObject);
    procedure cmbboxLineEndingChange(Sender: TObject);
    procedure cmbboxQuoteCharChange(Sender: TObject);
    procedure CSVFastDataset1AfterDelete(DataSet: TDataSet);
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

    procedure rbRangeChange(Sender: TObject);
  private
    FFileName: string;

    FLoadTimer: TTimer;
    FLoadStartTime: TDateTime;
    FLoadElapsedLabel: TLabel;
    FLoadStatusLabel: TLabel;
    FLoadDotCount: Integer;

    // Progress-Fenster Komponenten
    FProgressForm: TForm;
    FProgPhaseLbl: TLabel;
    FProgBar: TProgressBar;
    FProgInfoLbl: TLabel;
    FProgSpeedLbl: TLabel;
    FProgElapsedLbl: TLabel;
    FProgETALbl: TLabel;
    FProgCancelBtn: TButton;

    // RAM-Komponenten
    FProgRAMBar: TProgressBar;
    FProgRAMInfoLbl: TLabel;
    FProgRAMStatusLbl: TLabel;

    // Progress-State
    FProgTotalRows: Int64;
    FProgStartTime: TDateTime;
    FProgTotalRAMMB: Int64;
    FRAMWarningShown: Boolean;
    FLoadProgressCounter: Integer;

    // Live-RAM-State (während Load)
    FInitialFreeRAMMB:   Int64;    // beim Start gemessen
    FCurrentFreeRAMMB:   Int64;    // bei jedem Progress-Event neu gelesen
    FPeakUsedRAMMB:      Int64;    // höchster gemessener Systemverbrauch
    FPeakEstimatedMB:    Int64;    // höchste Dataset-Schätzung
    FLoadMaxRowWidth:    Int64;    // vom Scan, für die Schätzung
    FCancelRequested: boolean;

    // Live-Swap-State (NEU)
    FInitialSwapTotalMB: Int64;    // Swap-Gesamt beim Start
    FInitialSwapUsedMB:  Int64;    // Swap-Verbrauch beim Load-Start
    FCurrentSwapUsedMB:  Int64;    // aktueller Swap-Verbrauch
    FLastSwapUsedMB:     Int64;    // für Delta-Erkennung
    FPeakSwapUsedMB:     Int64;    // höchster gemessener Swap-Verbrauch
    FThrashingDetected:  Boolean;  // Swap-Wachstum > 500 MB erkannt


    // Interval-Sampling für Load-Speed
    FProgressSamples: array of Double;    // rows/sec pro Intervall
    FLastProgressRows: Int64;
    FLastProgressSeconds: Double;

    FScanStartTick: QWord;
    FLastProgressBytes: Int64;
    FProgressBytesSamples: array of Double;

    FProgressAutoTuneNote: string;
    function CheckCSVConfiguration(
      AMaxRowWidth: Int64;
      ACurrentBlockSize: Int64;
      ACurrentBufferSize: Int64): TCSVConfigCheck;
    function AskApplyCSVConfig(const ACheck: TCSVConfigCheck): TDetectChoice;
    procedure ShowProgressAutoTuneNote(const ACheck: TCSVConfigCheck);
    procedure ResetAutoTuneProgressNote;

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
    function AskUseDetected(const AFileName: string; const ACurLE, ADetLE: string; const ACurDel, ADetDel: Char; const ACurQC, ADetQC: Char): TDetectChoice;
    procedure HandleScanProgress(ABytesRead, ATotalBytes: Int64);
    procedure HandleLoadProgress(
      ARowsRead: Int64;
      ABytesRead: Int64;
      AElapsedSeconds: Double);
    procedure ProgCancelClick(Sender: TObject);
    procedure BuildProgressForm;
  public
  end;

//var
  //frmDataEditor: TfrmDataEditor;

implementation

{$R *.lfm}

procedure TfrmDataEditor.BuildProgressForm;
var
  PnlInfo, PnlProgress, PnlRAM, PnlCancel: TPanel;
  Separator: TLabel;
begin
  FProgressForm := TForm.Create(nil);
  FProgressForm.Caption     := 'Loading...';
  FProgressForm.Width       := 560;
  FProgressForm.Height      := 480;    // was 440 — mehr Platz für PnlRAM
  FProgressForm.Position    := poScreenCenter;
  FProgressForm.BorderStyle := bsDialog;
  FProgressForm.FormStyle   := fsNormal;

  // ============================================================
  // === Info-Panel (oben) ===
  // ============================================================
  PnlInfo := TPanel.Create(FProgressForm);
  PnlInfo.Parent := FProgressForm;
  PnlInfo.Align := alTop;
  PnlInfo.Height := 90;
  PnlInfo.BevelOuter := bvNone;

  FProgPhaseLbl := TLabel.Create(FProgressForm);
  FProgPhaseLbl.Parent := PnlInfo;
  FProgPhaseLbl.Left := 16;
  FProgPhaseLbl.Top := 12;
  FProgPhaseLbl.Width := 520;
  FProgPhaseLbl.Caption := 'Preparing...';
  FProgPhaseLbl.Font.Size := 12;
  FProgPhaseLbl.Font.Style := [fsBold];

  FProgInfoLbl := TLabel.Create(FProgressForm);
  FProgInfoLbl.Parent := PnlInfo;
  FProgInfoLbl.Left := 16;
  FProgInfoLbl.Top := 45;
  FProgInfoLbl.Width := 520;
  FProgInfoLbl.Caption := '';

  // ============================================================
  // === Progress-Panel (Mitte, Rows/Scan) ===
  // ============================================================
  PnlProgress := TPanel.Create(FProgressForm);
  PnlProgress.Parent := FProgressForm;
  PnlProgress.Align := alTop;
  PnlProgress.Height := 105;
  PnlProgress.BevelOuter := bvNone;

  FProgBar := TProgressBar.Create(FProgressForm);
  FProgBar.Parent := PnlProgress;
  FProgBar.Left := 16;
  FProgBar.Top := 10;
  FProgBar.Width := 520;
  FProgBar.Height := 24;
  FProgBar.Min := 0;
  FProgBar.Max := 100;
  FProgBar.Position := 0;

  FProgElapsedLbl := TLabel.Create(FProgressForm);
  FProgElapsedLbl.Parent := PnlProgress;
  FProgElapsedLbl.Left := 16;
  FProgElapsedLbl.Top := 45;
  FProgElapsedLbl.Width := 250;

  FProgSpeedLbl := TLabel.Create(FProgressForm);
  FProgSpeedLbl.Parent := PnlProgress;
  FProgSpeedLbl.Left := 280;
  FProgSpeedLbl.Top := 45;
  FProgSpeedLbl.Width := 256;

  FProgETALbl := TLabel.Create(FProgressForm);
  FProgETALbl.Parent := PnlProgress;
  FProgETALbl.Left := 16;
  FProgETALbl.Top := 68;
  FProgETALbl.Width := 520;

  // ============================================================
  // === RAM-Panel ===
  // ============================================================
  PnlRAM := TPanel.Create(FProgressForm);
  PnlRAM.Parent := FProgressForm;
  PnlRAM.Align := alTop;
  PnlRAM.Height := 145;                // was 105 — mehr Platz
  PnlRAM.BevelOuter := bvNone;

  // Trennlinie
  Separator := TLabel.Create(FProgressForm);
  Separator.Parent := PnlRAM;
  Separator.Left := 16;
  Separator.Top := 0;
  Separator.Width := 520;
  Separator.Caption := '──────────────────────────────────────────';
  Separator.Font.Color := clGray;

  // RAM-Info: 3 Zeilen (System RAM / Dataset / Swap)
  FProgRAMInfoLbl := TLabel.Create(FProgressForm);
  FProgRAMInfoLbl.Parent := PnlRAM;
  FProgRAMInfoLbl.Left := 16;
  FProgRAMInfoLbl.Top := 22;           // was 20 — leicht nach unten
  FProgRAMInfoLbl.Width := 520;
  FProgRAMInfoLbl.AutoSize := True;    // wächst mit 3-zeiligem Text
  FProgRAMInfoLbl.Caption := 'RAM:      ...';

  // RAM-Bar: unterhalb der 3-zeiligen Info (AutoSize-Höhe ~48 px)
  FProgRAMBar := TProgressBar.Create(FProgressForm);
  FProgRAMBar.Parent := PnlRAM;
  FProgRAMBar.Left := 16;
  FProgRAMBar.Top := 80;               // was 45 — unter die 3-Zeilen-Info
  FProgRAMBar.Width := 520;
  FProgRAMBar.Height := 20;
  FProgRAMBar.Min := 0;
  FProgRAMBar.Max := 100;
  FProgRAMBar.Position := 0;

  // Status-Zeile: unterhalb der Bar
  FProgRAMStatusLbl := TLabel.Create(FProgressForm);
  FProgRAMStatusLbl.Parent := PnlRAM;
  FProgRAMStatusLbl.Left := 16;
  FProgRAMStatusLbl.Top := 108;        // was 70 — unter die Bar
  FProgRAMStatusLbl.Width := 520;
  FProgRAMStatusLbl.Caption := '';

  // ============================================================
  // === Cancel-Panel (unten) ===
  // ============================================================
  PnlCancel := TPanel.Create(FProgressForm);
  PnlCancel.Parent := FProgressForm;
  PnlCancel.Align := alClient;
  PnlCancel.BevelOuter := bvNone;

  FProgCancelBtn := TButton.Create(FProgressForm);
  FProgCancelBtn.Parent := PnlCancel;
  FProgCancelBtn.Caption := 'Cancel';
  FProgCancelBtn.Width := 100;
  FProgCancelBtn.Height := 30;
  FProgCancelBtn.Left := (FProgressForm.ClientWidth - 100) div 2;
  FProgCancelBtn.Top := 15;
  FProgCancelBtn.OnClick := @ProgCancelClick;
end;

procedure TfrmDataEditor.HandleScanProgress(ABytesRead, ATotalBytes: Int64);
var
  Percent: Integer;
  ElapsedSeconds, BytesPerSec: Double;
begin
  if ATotalBytes <= 0 then Exit;

  Percent := (ABytesRead * 100) div ATotalBytes;
  if Percent > 100 then Percent := 100;

  FProgBar.Position := Percent;

  // Zeit seit Scan-Start (monotonic, Millisekunden-genau)
  ElapsedSeconds := (GetTickCount64 - FScanStartTick) / 1000.0;
  if ElapsedSeconds > 0.2 then
    BytesPerSec := ABytesRead / ElapsedSeconds
  else
    BytesPerSec := 0;

  FProgInfoLbl.Caption := Format('Scan:   %s / %s   (%d%%)',
    [FormatBytesSmart(ABytesRead),
     FormatBytesSmart(ATotalBytes),
     Percent]);

  if BytesPerSec > 0 then
    FProgSpeedLbl.Caption := Format('Speed:  %s/sec  |  Elapsed: %s',
      [FormatBytesSmart(Round(BytesPerSec)),
       FormatDateTime('hh:nn:ss', ElapsedSeconds / SecsPerDay)])
  else
    FProgSpeedLbl.Caption := '';

  FProgETALbl.Caption := '';   // beim Scan kein ETA — Zeilen sind noch nicht gezählt

  Application.ProcessMessages;
end;

procedure TfrmDataEditor.HandleLoadProgress(
  ARowsRead: Int64;
  ABytesRead: Int64;
  AElapsedSeconds: Double);
var
  Percent: Integer;
  RowsPerSec, BytesPerSec, ETASeconds: Double;
  IntervalRows, IntervalBytes: Double;
  UsedRAMMB: Int64;
  UsedPercent: Integer;
  EstimatedMB: Int64;
  SwapUsedMB: Int64;
  SwapDeltaMB: Int64;
  SwapTotalMB: Int64;
  StatusText: string;
  StatusColor: TColor;
begin
  // ============================================================
  // Interval-Sample für Report (rows/sec UND bytes/sec)
  // ============================================================
  if (FLastProgressSeconds > 0) and
     (AElapsedSeconds > FLastProgressSeconds) then
  begin
    IntervalRows := (ARowsRead - FLastProgressRows) /
                    (AElapsedSeconds - FLastProgressSeconds);
    IntervalBytes := (ABytesRead - FLastProgressBytes) /
                     (AElapsedSeconds - FLastProgressSeconds);
    if IntervalRows > 0 then
    begin
      SetLength(FProgressSamples, Length(FProgressSamples) + 1);
      FProgressSamples[High(FProgressSamples)] := IntervalRows;

      SetLength(FProgressBytesSamples, Length(FProgressBytesSamples) + 1);
      FProgressBytesSamples[High(FProgressBytesSamples)] := IntervalBytes;
    end;
  end;
  FLastProgressRows    := ARowsRead;
  FLastProgressBytes   := ABytesRead;
  FLastProgressSeconds := AElapsedSeconds;

  // ============================================================
  // Rows-Progress
  // ============================================================
  if FProgTotalRows > 0 then
  begin
    Percent := (ARowsRead * 100) div FProgTotalRows;
    if Percent > 100 then Percent := 100;
  end
  else
    Percent := 0;

  FProgBar.Position := Percent;
  FProgInfoLbl.Caption := Format('Rows:   %s / %s   (%d%%)',
    [FormatFloat('#,##0', ARowsRead),
     FormatFloat('#,##0', FProgTotalRows),
     Percent]);

  // ============================================================
  // Speed-Zeile: rows/sec | bytes/sec
  // ============================================================
  if AElapsedSeconds > 0.5 then
  begin
    RowsPerSec  := ARowsRead / AElapsedSeconds;
    BytesPerSec := ABytesRead / AElapsedSeconds;

    FProgSpeedLbl.Caption := Format('Speed:  %s rows/sec  |  %s/sec',
      [FormatFloat('#,##0', RowsPerSec),
       FormatBytesSmart(Round(BytesPerSec))]);

    if (FProgTotalRows > ARowsRead) and (RowsPerSec > 0) then
    begin
      ETASeconds := (FProgTotalRows - ARowsRead) / RowsPerSec;
      FProgETALbl.Caption := Format('ETA:    %s',
        [FormatDateTime('hh:nn:ss', ETASeconds / SecsPerDay)]);
    end
    else
      FProgETALbl.Caption := '';
  end
  else
  begin
    FProgSpeedLbl.Caption := '';
    FProgETALbl.Caption   := '';
  end;

  FProgElapsedLbl.Caption := Format('Elapsed: %s  |  Loaded: %s',
    [FormatDateTime('hh:nn:ss', AElapsedSeconds / SecsPerDay),
     FormatBytesSmart(ABytesRead)]);

  // ============================================================
  // Live-RAM
  // ============================================================
  FCurrentFreeRAMMB := GetRAMFreeMB;
  if FCurrentFreeRAMMB < 0 then
    FCurrentFreeRAMMB := FInitialFreeRAMMB;

  UsedRAMMB := FProgTotalRAMMB - FCurrentFreeRAMMB;
  if UsedRAMMB < 0 then UsedRAMMB := 0;

  if UsedRAMMB > FPeakUsedRAMMB then
    FPeakUsedRAMMB := UsedRAMMB;

  if (FLoadMaxRowWidth > 0) and (ARowsRead > 0) then
    EstimatedMB := (ARowsRead * (FLoadMaxRowWidth + 64)) div (1024 * 1024)
  else
    EstimatedMB := 0;

  if EstimatedMB > FPeakEstimatedMB then
    FPeakEstimatedMB := EstimatedMB;

  // ============================================================
  // Swap
  // ============================================================
  SwapTotalMB := GetSwapTotalMB;
  SwapUsedMB  := GetSwapUsedMB;
  if SwapUsedMB < 0 then SwapUsedMB := 0;

  if SwapUsedMB > FPeakSwapUsedMB then
    FPeakSwapUsedMB := SwapUsedMB;

  SwapDeltaMB := SwapUsedMB - FLastSwapUsedMB;
  if (SwapDeltaMB > 500) and (FLastSwapUsedMB >= 0) then
    FThrashingDetected := True;
  FLastSwapUsedMB     := SwapUsedMB;
  FCurrentSwapUsedMB  := SwapUsedMB;

  // ============================================================
  // Prozent für die Bar
  // ============================================================
  if FProgTotalRAMMB > 0 then
    UsedPercent := (UsedRAMMB * 100) div FProgTotalRAMMB
  else
    UsedPercent := 0;
  if UsedPercent > 100 then UsedPercent := 100;

  FProgRAMBar.Position := UsedPercent;

  // ------------------------------------------------------------
  // Anzeige — drei Zeilen
  // ------------------------------------------------------------
  if SwapTotalMB > 0 then
    FProgRAMInfoLbl.Caption := Format(
      'System RAM: %s used / %s total   (%d%%)' + sLineBreak +
      'Dataset:    %s used (estimated)' + sLineBreak +
      'Swap:       %s used / %s total',
      [FormatBytesSmart(UsedRAMMB * 1024 * 1024),
       FormatBytesSmart(FProgTotalRAMMB * 1024 * 1024),
       UsedPercent,
       FormatBytesSmart(EstimatedMB * 1024 * 1024),
       FormatBytesSmart(SwapUsedMB * 1024 * 1024),
       FormatBytesSmart(SwapTotalMB * 1024 * 1024)])
  else
    FProgRAMInfoLbl.Caption := Format(
      'System RAM: %s used / %s total   (%d%%)' + sLineBreak +
      'Dataset:    %s used (estimated)',
      [FormatBytesSmart(UsedRAMMB * 1024 * 1024),
       FormatBytesSmart(FProgTotalRAMMB * 1024 * 1024),
       UsedPercent,
       FormatBytesSmart(EstimatedMB * 1024 * 1024)]);

  // ------------------------------------------------------------
  // Status + Farbe
  // ------------------------------------------------------------
  if FThrashingDetected and (UsedPercent < 85) then
  begin
    StatusText  := 'Status:   WARNING - swapping (load is slower)';
    StatusColor := clMaroon;
  end
  else if UsedPercent >= 85 then
  begin
    StatusText  := 'Status:   CRITICAL - low memory!';
    StatusColor := clRed;
  end
  else if UsedPercent >= 70 then
  begin
    StatusText  := 'Status:   WARNING - memory getting tight';
    StatusColor := clMaroon;
  end
  else
  begin
    StatusText  := 'Status:   OK';
    StatusColor := clGreen;
  end;

  FProgRAMStatusLbl.Caption    := StatusText;
  FProgRAMStatusLbl.Font.Color := StatusColor;
  if StatusColor <> clGreen then
    FProgRAMStatusLbl.Font.Style := [fsBold]
  else
    FProgRAMStatusLbl.Font.Style := [];

  // ------------------------------------------------------------
  // Warnung bei kritischem RAM (einmal pro Load)
  // ------------------------------------------------------------
  if (UsedPercent >= 85) and (not FRAMWarningShown) then
  begin
    FRAMWarningShown := True;

    if MessageDlg(
      'Warning: Low memory!' + sLineBreak + sLineBreak +
      'System RAM:  ' + FormatBytesSmart(UsedRAMMB * 1024 * 1024) +
                       ' used / ' + FormatBytesSmart(FProgTotalRAMMB * 1024 * 1024) + ' total' + sLineBreak +
      'Dataset:     ' + FormatBytesSmart(EstimatedMB * 1024 * 1024) +
                       ' used (estimated)' + sLineBreak +
      'Swap:        ' + FormatBytesSmart(SwapUsedMB * 1024 * 1024) +
                       ' used / ' + FormatBytesSmart(SwapTotalMB * 1024 * 1024) + ' total' + sLineBreak +
      sLineBreak +
      'The system may become unresponsive or kill the application.' + sLineBreak +
      sLineBreak +
      'Cancel the load?',
      mtWarning, [mbYes, mbNo], 0) = mrYes then
    begin
      FCancelRequested := True;
      FProgCancelBtn.Enabled := False;
      FProgCancelBtn.Caption := 'Cancelling...';
    end;
  end;

  Application.ProcessMessages;
end;

procedure TfrmDataEditor.ProgCancelClick(Sender: TObject);
begin
  FCancelRequested := True;   // ← reicht, weil der Reader den Pointer hat
  FProgCancelBtn.Enabled := False;
  FProgCancelBtn.Caption := 'Cancelling...';
end;

procedure TfrmDataEditor.rbRangeChange(Sender: TObject);
begin
  edtFrom.Enabled := rbRange.Checked;
  edtTo.Enabled   := rbRange.Checked;
end;

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

    // Grid + Navigator komplett abschalten, BEVOR das Dataset geöffnet wird
    if Assigned(FOwner.DataSource1) then
    begin
      if Assigned(FOwner.RxDBGrid1) then
        FOwner.RxDBGrid1.DataSource := nil;
      if Assigned(FOwner.DBNavigator1) then
        FOwner.DBNavigator1.DataSource := nil;
    end;

    if FOwner.DataSource1.DataSet = FOwner.CSVFastDataset1 then
      FOwner.CSVFastDataset1.LoadFromCSVFile(FFileName)

    else if FOwner.DataSource1.DataSet = FOwner.JSONFastDataSet1 then
      FOwner.JSONFastDataSet1.LoadFromFile(FFileName)

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

    // JETZT erst wieder verbinden
    FOwner.RxDBGrid1.DataSource := FOwner.DataSource1;
    FOwner.DBNavigator1.DataSource := FOwner.DataSource1;

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
  // ============================================================
  // CSV-Optionen
  // ============================================================
  CSVFastDataset1.CSVOptions.DefaultFieldLength := StrToIntDef(edtDefaultFieldLength.Text, 50);
  CSVFastDataset1.CSVOptions.Delimiter := CSVDelimiter;
  CSVFastDataset1.CSVOptions.LineEnding := CSVLineEnding;

  if CSVQuoteChar <> #0 then
    CSVFastDataset1.CSVOptions.QuoteChar := CSVQuoteChar
  else
    CSVFastDataset1.CSVOptions.QuoteChar := #0;

  CSVFastDataset1.CSVOptions.FirstLineAsFieldNames := chkBoxFirstLineAsFieldName.Checked;
  CSVFastDataset1.CSVOptions.IgnoreOuterWhitespace := chkBoxIgnoreOuterWhiteSpace.Checked;
  CSVFastDataset1.CSVOptions.QuoteOuterWhitespace  := chkBoxQuoteOuterWhiteSpace.Checked;

  // ============================================================
  // Performance-Parameter: GUI → Dataset (alle in BYTES)
  // ============================================================
  TCSVFastDataset(CSVFastDataset1).BlockSize :=
      StrToIntDef(edtBlockSize.Text, 1048576);
  TCSVFastDataset(CSVFastDataset1).BufferSize :=
      StrToInt64Def(edtBufferSize.Text, 2097152);
  TCSVFastDataset(CSVFastDataset1).InitialCellCapacity :=
      StrToIntDef(edtInitialCellCapacity.Text, 64);

  // ============================================================
  // Dataset → globale Variablen
  // (die Setter könnten Werte angepasst haben)
  // ============================================================
  CSVBlockSizeBytes      := TCSVFastDataset(CSVFastDataset1).BlockSize;
  CSVBufferSizeBytes     := TCSVFastDataset(CSVFastDataset1).BufferSize;
  CSVInitialCellCapacity := TCSVFastDataset(CSVFastDataset1).InitialCellCapacity;

  // ============================================================
  // Globale Variablen → GUI (Rückspiegelung)
  // ============================================================
  edtBlockSize.Text           := IntToStr(CSVBlockSizeBytes);
  edtBufferSize.Text          := IntToStr(CSVBufferSizeBytes);
  edtInitialCellCapacity.Text := IntToStr(CSVInitialCellCapacity);

  // ============================================================
  // Info-Label mit den tatsächlich aktiven Werten
  // ============================================================
  lblActivePerformanceSettings.Caption := Format(
    'Active: Block=%s, Buffer=%s, Cells=%d, ScanRows=%d',
    [FormatBytesSmart(CSVBlockSizeBytes),
     FormatBytesSmart(CSVBufferSizeBytes),
     CSVInitialCellCapacity,
     StrToInt64Def(edtRowWidthScanRows.Text, 0)]);
end;

procedure TfrmDataEditor.ReadIni;
begin
  edtDefaultFieldLength.Text          := IntToStr(CSVDefaultFieldLength);
  chkBoxFirstLineAsFieldName.Checked  := CSVFirstLineAsFieldNames;
  chkBoxIgnoreOuterWhiteSpace.Checked := CSVIgnoreOuterWhitespace;
  chkBoxQuoteOuterWhiteSpace.Checked  := CSVQuoteOuterWhitespace;

  // Auto-Detect-Checkboxen
  CheckBoxAutoDedectLineEnding.Checked := AutoDetectLineEnding;
  CheckBoxAutoDedectDelimiter.Checked  := AutoDetectDelimiter;
  CheckBoxAutoDedectQuoteChar.Checked  := AutoDetectQuoteChar;

  //CSVFastDataSet Performance-Parameter aus grboxCSVFileRead
  edtBlockSize.Text  := IntToStr(CSVBlockSizeBytes);
  edtBufferSize.Text := IntToStr(CSVBufferSizeBytes);
  edtInitialCellCapacity.Text := IntToStr(CSVInitialCellCapacity);

  edtProgressUpdatePercent.Text := IntToStr(CSVProgressUpdatePercent);

  chkBoxAutoTuneCSVBuffer.Checked := CSVAutoTuneCSVBuffer;

  // Combos synchronisieren — sie zeigen die globalen Werte an
  SyncLineEndingCombo;
  SyncDelimiterCombo;
  SyncQuoteCharCombo;
end;

procedure TfrmDataEditor.WriteIni;
begin
  CSVDefaultFieldLength := StrToIntDef(edtDefaultFieldLength.Text, 50);

  // CSVDelimiter / CSVQuoteChar / CSVLineEnding werden bereits
  // durch die Combo-Change-Handler aktuell gehalten — hier nichts tun.

  CSVFirstLineAsFieldNames := chkBoxFirstLineAsFieldName.Checked;
  CSVIgnoreOuterWhitespace := chkBoxIgnoreOuterWhiteSpace.Checked;
  CSVQuoteOuterWhitespace  := chkBoxQuoteOuterWhiteSpace.Checked;

  // Auto-Detect-Checkboxen
  AutoDetectLineEnding := CheckBoxAutoDedectLineEnding.Checked;
  AutoDetectDelimiter  := CheckBoxAutoDedectDelimiter.Checked;
  AutoDetectQuoteChar  := CheckBoxAutoDedectQuoteChar.Checked;


  //CSVFastDataSet Performance-Parameter
  CSVBlockSizeBytes  := StrToIntDef(edtBlockSize.Text, 1048576);
  CSVBufferSizeBytes := StrToInt64Def(edtBufferSize.Text, 2097152);
  CSVInitialCellCapacity := StrToIntDef(edtInitialCellCapacity.Text, 64);
  CSVProgressUpdatePercent := StrToIntDef(edtProgressUpdatePercent.Text, 1);
  CSVAutoTuneCSVBuffer :=  chkBoxAutoTuneCSVBuffer.Checked;
end;

procedure TfrmDataEditor.ResetAllDatasets;
begin
  // ============================================================
  // 1. Grid + Navigator zuerst abklemmen — keine Events mehr
  // ============================================================
  if Assigned(RxDBGrid1) then
    RxDBGrid1.DataSource := nil;
  if Assigned(DBNavigator1) then
    DBNavigator1.DataSource := nil;
  if Assigned(DataSource1) then
    DataSource1.DataSet := nil;

  // ============================================================
  // 2. Alle Datasets schließen und leeren
  //    Jedes Dataset einzeln, mit eigener Schutzklammer —
  //    ein kaputtes Dataset darf die anderen nicht blockieren.
  // ============================================================

  // ---- CSVFastDataset1 ----
  if Assigned(CSVFastDataset1) then
  begin
    if CSVFastDataset1.Active then
      CSVFastDataset1.DisableControls;
    try
      try
        if CSVFastDataset1.Active then
          CSVFastDataset1.Close;
      except
        // Zustand kaputt — Close überspringen, Clear kommt eh
      end;
      try
        CSVFastDataset1.Fields.Clear;
      except end;
      try
        CSVFastDataset1.FieldDefs.Clear;
      except end;
    finally
      if CSVFastDataset1.Active then
        CSVFastDataset1.EnableControls;
    end;
  end;

  // ---- JSONFastDataSe1 ----
  if Assigned(JSONFastDataSet1) then
  begin
    if JSONFastDataSet1.Active then
      JSONFastDataSet1.DisableControls;
    try
      try
        if JSONFastDataSet1.Active then
          JSONFastDataSet1.Close;
      except end;
      try
        JSONFastDataSet1.Fields.Clear;
      except end;
      try
        JSONFastDataSet1.FieldDefs.Clear;
      except end;
    finally
      if JSONFastDataSet1.Active then
        JSONFastDataSet1.EnableControls;
    end;
  end;

  // ---- sWorksheetDataset1 ----
  if Assigned(sWorksheetDataset1) then
  begin
    if sWorksheetDataset1.Active then
      sWorksheetDataset1.DisableControls;
    try
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
    finally
      if sWorksheetDataset1.Active then
        sWorksheetDataset1.EnableControls;
    end;
  end;

  // ---- Dbf1 ----
  if Assigned(Dbf1) then
  begin
    if Dbf1.Active then
      Dbf1.DisableControls;
    try
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
    finally
      if Dbf1.Active then
        Dbf1.EnableControls;
    end;
  end;

  // ---- SdfDataSet1 ----
  if Assigned(SdfDataSet1) then
  begin
    if SdfDataSet1.Active then
      SdfDataSet1.DisableControls;
    try
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
    finally
      if SdfDataSet1.Active then
        SdfDataSet1.EnableControls;
    end;
  end;

  // ---- FixedFormatDataSet1 ----
  if Assigned(FixedFormatDataSet1) then
  begin
    if FixedFormatDataSet1.Active then
      FixedFormatDataSet1.DisableControls;
    try
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
    finally
      if FixedFormatDataSet1.Active then
        FixedFormatDataSet1.EnableControls;
    end;
  end;

  // ============================================================
  // 3. Grid-Spalten und SQL-Editor leeren
  // ============================================================
  if Assigned(RxDBGrid1) then
  begin
    try
      RxDBGrid1.Columns.Clear;
    except end;
  end;

  if Assigned(SynEditSQL) then
  begin
    try
      SynEditSQL.Lines.Clear;
    except end;
  end;
end;

{procedure TfrmDataEditor.LoadFile(const FileName: string);
begin
  if DataSource1.DataSet.Active then
  begin
    DataSource1.DataSet.Close;
    DataSource1.DataSet.Fields.Clear;
  end;

  RxDBGrid1.BeginUpdate;

  if DataSource1.DataSet = CSVFastDataset1 then
    CSVFastDataset1.LoadFromCSVFile(FileName)

  else if DataSource1.DataSet = JSONFastDataSe1 then
    JSONFastDataSe1.LoadFromFile(FileName)

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

  //RxDBGrid1.OptimizeColumnsWidthAll;
  RxDBGrid1.EndUpdate(True);
end;}

procedure TfrmDataEditor.LoadFile(const FileName: string);
var
  DS: TDataSet;
begin
  DS := DataSource1.DataSet;
  if not Assigned(DS) then Exit;

  // ============================================================
  // 1. Alten Zustand sauber abräumen — Grid + Navigator abklemmen,
  //    Dataset-Steuerelemente deaktivieren
  // ============================================================
  RxDBGrid1.BeginUpdate;
  RxDBGrid1.DataSource := nil;
  DBNavigator1.DataSource := nil;

  DS.DisableControls;
  try
    if DS.Active then
    begin
      DS.Close;
      DS.Fields.Clear;
    end;

    // ============================================================
    // 2. Datei laden — je nach Dataset-Typ
    //    Alles innerhalb der DisableControls-Klammer, damit
    //    kein einziges DataChange-Event ans Grid geht.
    // ============================================================
    if DS = CSVFastDataset1 then
      CSVFastDataset1.LoadFromCSVFile(FileName)

    else if DS = JSONFastDataSet1 then
      JSONFastDataSet1.LoadFromFile(FileName)

    else if DS = sWorksheetDataset1 then
    begin
      sWorksheetDataset1.FileName := FileName;
      sWorksheetDataset1.SheetName := ChangeFileExt(ExtractFileName(FileName), '');
      sWorksheetDataset1.Open;
    end

    else if DS = Dbf1 then
    begin
      Dbf1.FilePathFull := FileName;
      Dbf1.Open;
    end

    else if DS = SdfDataSet1 then
    begin
      SdfDataSet1.FileName := FileName;
      SdfDataSet1.Open;
    end

    else if DS = FixedFormatDataSet1 then
    begin
      FixedFormatDataSet1.FileName := FileName;
      FixedFormatDataSet1.Open;
    end;

    // ============================================================
    // 3. Cursor auf den Anfang — noch innerhalb der Sperre
    // ============================================================
    if DS.Active and (not DS.IsEmpty) then
      DS.First;

  finally
    DS.EnableControls;
  end;

  // ============================================================
  // 4. Grid + Navigator wieder verbinden
  //    Erst JETZT, nach dem EnableControls, damit das Grid
  //    genau EINEN Refresh bekommt statt n.
  // ============================================================
  RxDBGrid1.DataSource := DataSource1;
  DBNavigator1.DataSource := DataSource1;

  // ============================================================
  // 5. Spaltenbreiten — sample-basiert, nicht über alle Rows
  // ============================================================

  RxDBGrid1.EndUpdate(True);
end;

procedure SaveJSONToFile(DataSet: TJSONFastDataSet; const FileName: string);
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
  if DataSource1.DataSet = CSVFastDataset1 then
    CSVFastDataset1.SaveToCSVFile(FileName)
  else if DataSource1.DataSet = JSONFastDataSet1 then
    SaveJSONToFile(JSONFastDataSet1, FileName)
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
    DataSource1.DataSet := CSVFastDataset1;

    // ============================================================
    // Auto-Detection — nur für aktivierte Checkboxen
    // ============================================================
    if CheckBoxAutoDedectLineEnding.Checked then
      DetLE := DetectLineEnding(AFileName)
    else
      DetLE := CSVLineEnding;   // nicht erkannt → gleich Current

    if CheckBoxAutoDedectDelimiter.Checked then
      DetDel := DetectDelimiter(AFileName)
    else
      DetDel := CSVDelimiter;

    if CheckBoxAutoDedectQuoteChar.Checked then
      // QuoteChar-Detection braucht den Delimiter
      DetQC := DetectQuoteChar(AFileName, DetDel)
    else
      DetQC := CSVQuoteChar;

    // ============================================================
    // Dialog — nur wenn mindestens ein aktives Feld abweicht
    // ============================================================
    Choice := AskUseDetected(AFileName,
                             CSVLineEnding, DetLE,
                             CSVDelimiter,  DetDel,
                             CSVQuoteChar,  DetQC);

    if Choice = dcCancel then
    begin
      Result := False;
      Exit;
    end;

    if Choice = dcUseDetected then
    begin
      // Nur die Felder übernehmen, deren Checkbox an ist
      if CheckBoxAutoDedectLineEnding.Checked then
        CSVLineEnding := DetLE;
      if CheckBoxAutoDedectDelimiter.Checked then
        CSVDelimiter := DetDel;
      if CheckBoxAutoDedectQuoteChar.Checked then
        CSVQuoteChar := DetQC;
    end;
    // dcKeepCurrent: nichts tun

    SyncLineEndingCombo;
    SyncDelimiterCombo;
    SyncQuoteCharCombo;
  end

  else if (Ext = '.json') then
    DataSource1.DataSet := JSONFastDataSet1

  else if (Ext = '.xlsx') or (Ext = '.xls') or (Ext = '.xml')
       or (Ext = '.html') or (Ext = '.htm') or (Ext = '.ods')
       or (Ext = '.wiki') or (Ext = '.wikitable') then
  begin
    DataSource1.DataSet := sWorksheetDataset1;

    if Ext = '.xlsx' then
      sWorksheetDataset1.FileFormat := sfOOXML
    else if Ext = '.xls' then
      sWorksheetDataset1.FileFormat := sfExcel8
    else if Ext = '.xml' then
      sWorksheetDataset1.FileFormat := sfExcelXML
    else if (Ext = '.html') or (Ext = '.htm') then
      sWorksheetDataset1.FileFormat := sfHTML
    else if Ext = '.ods' then
      sWorksheetDataset1.FileFormat := sfOpenDocument
    else if Ext = '.wiki' then
      sWorksheetDataset1.FileFormat := sfWikiTable_Pipes
    else if Ext = '.wikitable' then
      sWorksheetDataset1.FileFormat := sfWikiTable_WikiMedia;
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
  CSVThread: TCSVLoadThread;
  FileSize: Int64;
  FreeRAMBytes: Int64;
  RowCount: Integer;
  Stats: TTransferStatistic;
  ReportForm: TfrmReport;
  LoadErr: string;
  MaxLineWidth: Int64;
  ScanStartTime: TDateTime;
  ScanElapsed: Double;
  MsgText: string;
  RowWidthScanMode: string;
  FromRow: Int64;
  ToRow: Int64;
  ScanRows: Int64;
  TotalRowsInRange: Int64;
  Cancelled: Boolean;
  LoadSpeedAvg: Double;
  LoadBytesPerSecAvg: Double;
  Sum: Double;
  I: Integer;

  Check: TCSVConfigCheck;
  Choice: TDetectChoice;
  BlockSizeOriginalBytes: Int64;
  BufferSizeOriginalBytes: Int64;
  BlockAutoAdjusted: Boolean;
  BufferAutoAdjusted: Boolean;
begin
  // ============================================================
  // Initialisierung
  // ============================================================
  MaxLineWidth            := 0;
  ScanElapsed             := 0;
  TotalRowsInRange        := 0;
  RowWidthScanMode        := 'None';
  FromRow                 := 1;
  ToRow                   := 0;
  ScanRows                := 0;
  Cancelled               := False;
  LoadSpeedAvg            := 0;
  LoadBytesPerSecAvg      := 0;

  BlockSizeOriginalBytes  := CSVBlockSizeBytes;
  BufferSizeOriginalBytes := CSVBufferSizeBytes;
  BlockAutoAdjusted       := False;
  BufferAutoAdjusted      := False;

  if not OpenDialog1.Execute then Exit;

  PageControl1.ActivePage := tsGrid;
  FFileName := OpenDialog1.FileName;

  if not SelectDataSetByFileName(FFileName) then
  begin
    FFileName := '';
    Exit;
  end;

  SynEditSQL.Lines.Clear;
  SetCSVSettingsFromForm;

  // ============================================================
  // Range aus GUI lesen
  // ============================================================
  if rbRange.Checked then
  begin
    FromRow := StrToInt64Def(edtFrom.Text, 1);
    ToRow   := StrToInt64Def(edtTo.Text, 1000000);
    if ToRow < FromRow then ToRow := FromRow;
    RowWidthScanMode := Format('Range %d..%d', [FromRow, ToRow]);
  end
  else
  begin
    FromRow := 1;
    ToRow   := 0;
    RowWidthScanMode := 'All Rows';
  end;

  ScanRows := StrToInt64Def(edtRowWidthScanRows.Text, 0);

  // ============================================================
  // Dateigröße ermitteln
  // ============================================================
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

  // ============================================================
  // Vorab-Warnung bei knappem RAM
  // (GetRAMFreeMB liefert MemAvailable — inkl. reclaimable Cache)
  // ============================================================
  if FileSize > 0 then
  begin
    FreeRAMBytes := GetRAMFreeMB * 1024 * 1024;
    if FileSize > (FreeRAMBytes * 8 div 10) then
    begin
      MsgText := Format(
        'Warning: Large file!' + sLineBreak + sLineBreak +
        'File size:      %s' + sLineBreak +
        'Free RAM:       %s' + sLineBreak +
        sLineBreak +
        'The file may not fit in memory. Loading could cause' + sLineBreak +
        'system slowdown or memory errors.' + sLineBreak + sLineBreak +
        'Continue anyway?',
        [FormatBytesSmart(FileSize), FormatBytesSmart(FreeRAMBytes)]);

      if MessageDlg(MsgText, mtWarning, [mbYes, mbNo], 0) = mrNo then
        Exit;
    end;
  end;

  // ============================================================
  // Progress-Fenster aufbauen
  // ============================================================
  BuildProgressForm;

  FProgInfoLbl.Caption := Format('File: %s   (%s)',
    [ExtractFileName(FFileName), FormatBytesSmart(FileSize)]);

  // ---- RAM/Swap-Basiswerte (für Progress-Fenster während Scan) ----
  FProgTotalRAMMB      := GetRAMTotalMB;
  FInitialFreeRAMMB    := GetRAMFreeMB;
  FCurrentFreeRAMMB    := FInitialFreeRAMMB;
  FPeakUsedRAMMB       := 0;
  FPeakEstimatedMB     := 0;
  FLoadMaxRowWidth     := 0;
  FRAMWarningShown     := False;
  FLoadProgressCounter := 0;
  FCancelRequested     := False;

  FInitialSwapTotalMB  := GetSwapTotalMB;
  FInitialSwapUsedMB   := GetSwapUsedMB;
  FCurrentSwapUsedMB   := GetSwapUsedMB;
  FLastSwapUsedMB      := FCurrentSwapUsedMB;
  FPeakSwapUsedMB      := 0;
  FThrashingDetected   := False;

  FScanStartTick        := 0;
  FLastProgressBytes    := 0;
  SetLength(FProgressSamples, 0);
  SetLength(FProgressBytesSamples, 0);
  ResetAutoTuneProgressNote;

  FProgressForm.Show;
  FProgressForm.BringToFront;
  Sleep(200);

  FLoadStartTime := Now;

  try
    // ============================================================
    // Phase 1: Scan
    // ============================================================
    if LowerCase(ExtractFileExt(FFileName)) = '.csv' then
    begin
      FProgPhaseLbl.Caption := 'Analyzing row width...';
      FProgBar.Position := 0;
      FProgInfoLbl.Caption := Format('File: %s   (%s)',
        [ExtractFileName(FFileName), FormatBytesSmart(FileSize)]);
      FProgSpeedLbl.Caption   := '';
      FProgElapsedLbl.Caption := '';
      FProgETALbl.Caption     := '';
      Application.ProcessMessages;

      try
        ScanStartTime  := Now;
        FScanStartTick := GetTickCount64;
        MaxLineWidth := EstimateCSVMaxLineWidth(
          FFileName,
          FromRow,
          ToRow,
          ScanRows,
          CSVQuoteChar,
          TotalRowsInRange,
          @HandleScanProgress,
          @FCancelRequested);
        ScanElapsed := (Now - ScanStartTime) * SecsPerDay;
      except
        on E: EAbort do
        begin
          Cancelled := True;
          FProgPhaseLbl.Caption := 'Cancelled by user (scan)';
          Application.ProcessMessages;
          Sleep(1000);
          Exit;
        end;
      end;

      // --------------------------------------------------------
      // Configuration Check
      // --------------------------------------------------------
      if MaxLineWidth > 0 then
      begin
        Check := CheckCSVConfiguration(
          MaxLineWidth,
          CSVBlockSizeBytes,
          CSVBufferSizeBytes);

        case Check.Status of

          ccsOptimal:
            begin
              // Nichts tun — Load startet sofort
            end;

          ccsSuboptimal:
            begin
              Choice := AskApplyCSVConfig(Check);
              case Choice of
                dcUseDetected:
                  begin
                    if chkBoxAutoTuneCSVBuffer.Checked then
                      ShowProgressAutoTuneNote(Check);

                    BlockSizeOriginalBytes  := CSVBlockSizeBytes;
                    BufferSizeOriginalBytes := CSVBufferSizeBytes;

                    BlockAutoAdjusted  := Check.OptimalBlockSize  <> CSVBlockSizeBytes;
                    BufferAutoAdjusted := Check.OptimalBufferSize <> CSVBufferSizeBytes;

                    CSVBlockSizeBytes  := Check.OptimalBlockSize;
                    CSVBufferSizeBytes := Check.OptimalBufferSize;
                    edtBlockSize.Text  := IntToStr(CSVBlockSizeBytes);
                    edtBufferSize.Text := IntToStr(CSVBufferSizeBytes);
                    SetCSVSettingsFromForm;
                  end;

                dcKeepCurrent:
                  begin
                    // Nichts ändern — weiter mit aktuellen Werten
                  end;

                dcCancel:
                  begin
                    FProgPhaseLbl.Caption := 'Cancelled by user (configuration)';
                    Application.ProcessMessages;
                    Sleep(500);
                    Exit;
                  end;
              end;
            end;

          ccsBroken:
            begin
              Choice := AskApplyCSVConfig(Check);
              if Choice = dcCancel then
              begin
                FProgPhaseLbl.Caption := 'Cancelled by user (configuration)';
                Application.ProcessMessages;
                Sleep(500);
                Exit;
              end;

              BlockSizeOriginalBytes  := CSVBlockSizeBytes;
              BufferSizeOriginalBytes := CSVBufferSizeBytes;

              BlockAutoAdjusted  := Check.OptimalBlockSize  <> CSVBlockSizeBytes;
              BufferAutoAdjusted := Check.OptimalBufferSize <> CSVBufferSizeBytes;

              CSVBlockSizeBytes  := Check.OptimalBlockSize;
              CSVBufferSizeBytes := Check.OptimalBufferSize;
              edtBlockSize.Text  := IntToStr(CSVBlockSizeBytes);
              edtBufferSize.Text := IntToStr(CSVBufferSizeBytes);
              SetCSVSettingsFromForm;
            end;
        end;
      end;
    end;

    // ============================================================
    // Phase 2: Load
    // ============================================================
    FProgPhaseLbl.Caption := 'Loading rows...';
    FProgBar.Position := 0;

    if ScanRows > 0 then
      FProgTotalRows := ToRow - FromRow + 1
    else if TotalRowsInRange > 0 then
      FProgTotalRows := TotalRowsInRange
    else
      FProgTotalRows := 0;

    // ---- RAM/Swap-Basiswerte für die LOAD-Phase zurücksetzen ----
    FInitialFreeRAMMB    := GetRAMFreeMB;
    FCurrentFreeRAMMB    := FInitialFreeRAMMB;
    FPeakUsedRAMMB       := 0;
    FPeakEstimatedMB     := 0;
    FLoadMaxRowWidth     := MaxLineWidth;
    FRAMWarningShown     := False;

    FInitialSwapTotalMB  := GetSwapTotalMB;
    FCurrentSwapUsedMB   := GetSwapUsedMB;
    FLastSwapUsedMB      := FCurrentSwapUsedMB;
    FPeakSwapUsedMB      := 0;
    FThrashingDetected   := False;

    // ---- Progress-Samples zurücksetzen ----
    SetLength(FProgressSamples, 0);
    SetLength(FProgressBytesSamples, 0);
    FLastProgressRows    := 0;
    FLastProgressBytes   := 0;
    FLastProgressSeconds := 0;

    // ---- Dataset konfigurieren ----
    TCSVFastDataset(CSVFastDataset1).OnProgress := @HandleLoadProgress;
    TCSVFastDataset(CSVFastDataset1).TotalRowsForProgress := FProgTotalRows;
    TCSVFastDataset(CSVFastDataset1).ProgressUpdatePercent :=
      StrToIntDef(edtProgressUpdatePercent.Text, 1);

    TCSVFastDataset(CSVFastDataset1).SkipFirstRows := FromRow - 1;
    if ToRow > 0 then
      TCSVFastDataset(CSVFastDataset1).StopAfterRow := ToRow - FromRow + 1
    else
      TCSVFastDataset(CSVFastDataset1).StopAfterRow := 0;

    Application.ProcessMessages;

    DBNavigator1.DataSource := nil;
    RxDBGrid1.DataSource    := nil;

    FCancelRequested := False;
    TCSVFastDataset(CSVFastDataset1).CancelFlag := @FCancelRequested;

    try
      CSVThread := TCSVLoadThread.Create(Self, OpenDialog1.FileName);

      while not CSVThread.Finished do
      begin
        Application.ProcessMessages;
        Sleep(20);
      end;

      LoadErr := CSVThread.LoadError;
      CSVThread.Free;
      CSVThread := nil;
    except
      on E: EAbort do
      begin
        Cancelled := True;
        LoadErr := 'Cancelled by user';
      end;
    end;

    TCSVFastDataset(CSVFastDataset1).OnProgress := nil;

    if TCSVFastDataset(CSVFastDataset1).Aborted then
    begin
      Cancelled := True;
      if LoadErr = '' then
        LoadErr := 'Cancelled by user';
    end;

    // ============================================================
    // Cancel-Behandlung
    // ============================================================
    if Cancelled or (LoadErr = 'Cancelled by user') then
    begin
      FProgPhaseLbl.Caption := 'Cancelled by user';
      Application.ProcessMessages;

      try
        RxDBGrid1.DataSource    := nil;
        DBNavigator1.DataSource := nil;
        DataSource1.DataSet     := nil;

        if CSVFastDataset1.Active then
          CSVFastDataset1.Close;
        CSVFastDataset1.Fields.Clear;
        CSVFastDataset1.FieldDefs.Clear;
      except
      end;

      try
        RxDBGrid1.Columns.Clear;
      except
      end;

      Caption := 'Turbobird - Data Editor (' + ExtractFileName(FFileName) + ')';
      btnCreateSQL.Enabled := False;
      btnExportAs.Enabled  := False;

      Sleep(500);
      Exit;
    end;

    // ============================================================
    // Fehlerprüfung
    // ============================================================
    FProgBar.Position := 100;
    Application.ProcessMessages;

    if LoadErr <> '' then
    begin
      FProgPhaseLbl.Caption := 'Error';
      FProgInfoLbl.Caption  := 'Load failed: ' + LoadErr;
      Application.ProcessMessages;
      Sleep(1500);

      MessageDlg(
        'Could not load file:' + sLineBreak + sLineBreak +
        'File: ' + ExtractFileName(FFileName) + sLineBreak +
        'Ext:  ' + LowerCase(ExtractFileExt(FFileName)) + sLineBreak +
        sLineBreak +
        'Error:' + sLineBreak + LoadErr,
        mtError, [mbOK], 0);

      Caption := 'Turbobird - Data Editor';
      btnCreateSQL.Enabled := False;
      btnExportAs.Enabled  := False;
      Exit;
    end;

    // ============================================================
    // Grid reaktivieren
    // ============================================================
    RxDBGrid1.DataSource    := nil;
    DBNavigator1.DataSource := nil;
    RxDBGrid1.Columns.Clear;

    RxDBGrid1.DataSource    := DataSource1;
    DBNavigator1.DataSource := DataSource1;

    if DataSource1.DataSet.Active and
       (not DataSource1.DataSet.IsEmpty) then
      DataSource1.DataSet.First;

    RxDBGrid1.Refresh;

    RowCount := DataSource1.DataSet.RecordCount;

    FProgPhaseLbl.Caption := Format('Finished: %s rows',
      [FormatFloat('#,##0', RowCount)]);
    Application.ProcessMessages;
    Sleep(500);

  finally
    if Assigned(FProgressForm) then
      FreeAndNil(FProgressForm);
  end;

  // ============================================================
  // End-Statistik als Report
  // Total Time = Scan + Load (ohne Dialog-Zeit)
  // ============================================================
  Caption := 'Turbobird - Data Editor (' + ExtractFileName(FFileName) + ')';
  btnCreateSQL.Enabled := True;
  btnExportAs.Enabled  := True;

  // ============================================================
  // Load-Speed: Interval-Mean ohne letztes Sample
  // ============================================================
  LoadSpeedAvg := 0;
  if Length(FProgressSamples) > 1 then
  begin
    Sum := 0;
    for I := 0 to Length(FProgressSamples) - 2 do
      Sum := Sum + FProgressSamples[I];
    LoadSpeedAvg := Sum / (Length(FProgressSamples) - 1);
  end
  else if Length(FProgressSamples) = 1 then
    LoadSpeedAvg := FProgressSamples[0];

  LoadBytesPerSecAvg := 0;
  if Length(FProgressBytesSamples) > 1 then
  begin
    Sum := 0;
    for I := 0 to Length(FProgressBytesSamples) - 2 do
      Sum := Sum + FProgressBytesSamples[I];
    LoadBytesPerSecAvg := Sum / (Length(FProgressBytesSamples) - 1);
  end
  else if Length(FProgressBytesSamples) = 1 then
    LoadBytesPerSecAvg := FProgressBytesSamples[0];

  // ============================================================
  // Statistik-Record füllen
  // ============================================================
  FillChar(Stats, SizeOf(Stats), 0);
  Stats.Kind             := tkLoadFile;
  Stats.SourceKind       := 'File';
  Stats.SourceFileName   := FFileName;
  Stats.SourceFileSize   := FileSize;
  Stats.SourceFileFormat := UpperCase(ExtractFileExt(FFileName));
  if (Stats.SourceFileFormat <> '') and (Stats.SourceFileFormat[1] = '.') then
    Delete(Stats.SourceFileFormat, 1, 1);
  Stats.OptionsExtra   := 'Read-Only';
  Stats.RowsProcessed  := RowCount;
  Stats.FieldsCount    := DataSource1.DataSet.FieldCount;
  Stats.ElapsedSeconds := ScanElapsed + FLastProgressSeconds;

  Stats.LoadElapsedSeconds := FLastProgressSeconds;
  Stats.RowsPerSecond      := LoadSpeedAvg;
  Stats.BytesPerSecond     := LoadBytesPerSecAvg;

  // Memory
  Stats.PeakRAMUsageMB  := FPeakUsedRAMMB;
  Stats.PeakEstimatedMB := FPeakEstimatedMB;

  // Swap
  Stats.PeakSwapUsedMB   := FPeakSwapUsedMB;
  Stats.SwapTotalMB      := FInitialSwapTotalMB;
  Stats.SwapStartUsedMB  := FInitialSwapUsedMB;
  Stats.ThrashingDetected := FThrashingDetected;

  Stats.SystemInfo := GetSystemInfo(FFileName);

  // ============================================================
  // CSV-Optionen für den Report
  // ============================================================
  if (UpperCase(Stats.SourceFileFormat) = 'CSV') and
     (DataSource1.DataSet = CSVFastDataset1) then
  begin
    if CSVFastDataset1.CSVOptions.Delimiter = #9 then
      Stats.CSVOptions := '    Delimiter: ' + QuotedStr('\t') + sLineBreak
    else if CSVFastDataset1.CSVOptions.Delimiter = ' ' then
      Stats.CSVOptions := '    Delimiter: ' + QuotedStr('Space') + sLineBreak
    else
      Stats.CSVOptions := '    Delimiter: ' +
        QuotedStr(CSVFastDataset1.CSVOptions.Delimiter) + sLineBreak;

    if CSVFastDataset1.CSVOptions.QuoteChar = #0 then
      Stats.CSVOptions := Stats.CSVOptions +
        '    Quote Char: (none)' + sLineBreak
    else
      Stats.CSVOptions := Stats.CSVOptions +
        '    Quote Char: ' + QuotedStr(CSVFastDataset1.CSVOptions.QuoteChar) + sLineBreak;

    Stats.CSVOptions := Stats.CSVOptions +
      '    Line Ending: ' +
        CSVLineEndingStrToName(CSVFastDataset1.CSVOptions.LineEnding) + sLineBreak +
      '    First Line As Field Names: ' +
        BoolToStr(CSVFastDataset1.CSVOptions.FirstLineAsFieldNames, 'Yes', 'No') + sLineBreak +
      '    Ignore Outer Whitespace: ' +
        BoolToStr(CSVFastDataset1.CSVOptions.IgnoreOuterWhitespace, 'Yes', 'No') + sLineBreak +
      '    Quote Outer Whitespace: ' +
        BoolToStr(CSVFastDataset1.CSVOptions.QuoteOuterWhitespace, 'Yes', 'No');
  end;

  // ============================================================
  // CSV Buffer Performance
  // ============================================================
  Stats.CSVBlockSizeBytes        := CSVBlockSizeBytes;
  Stats.CSVBufferSizeBytes       := CSVBufferSizeBytes;
  Stats.CSVInitialCellCapacity   := CSVInitialCellCapacity;
  Stats.CSVRowWidthScanMode      := RowWidthScanMode;
  Stats.CSVRowWidthScanRows      := TotalRowsInRange;
  Stats.CSVRowWidthScanIsCustom  := (ScanRows > 0);
  Stats.CSVRowWidthScanTime      := ScanElapsed;
  Stats.CSVMaxRowWidth           := MaxLineWidth;
  Stats.CSVBufferAutoAdjusted    := BufferAutoAdjusted;
  Stats.CSVBufferOriginalBytes   := BufferSizeOriginalBytes;
  Stats.CSVBlockAutoAdjusted     := BlockAutoAdjusted;
  Stats.CSVBlockOriginalBytes    := BlockSizeOriginalBytes;

  // ============================================================
  // Report anzeigen
  // ============================================================
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

procedure TfrmDataEditor.CSVFastDataset1AfterDelete(DataSet: TDataSet);
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
  chkBoxAutoTuneCSVBuffer.Checked :=  CSVAutoTuneCSVBuffer;

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

procedure TfrmDataEditor.Button1Click(Sender: TObject);
begin
  ShowMessage(IntToStr(CSVFastDataset1.RecordCount));
end;

procedure TfrmDataEditor.chkBoxAutoTuneCSVBufferChange(Sender: TObject);
begin
  CSVAutoTuneCSVBuffer := chkBoxAutoTuneCSVBuffer.Checked;
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
  if not Assigned(CSVFastDataset1) then Exit;

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
  ShowLE, ShowDel, ShowQC: Boolean;
  CurLEName, DetLEName: string;
  CurDelStr, DetDelStr: string;
  CurQCStr, DetQCStr: string;

  function DelimToStr(C: Char): string;
  begin
    if C = #9 then Result := '\t'
    else if C = ' ' then Result := 'Space'
    else if C = #0 then Result := '(none)'
    else Result := QuotedStr(C);
  end;

  function QCToStr(C: Char): string;
  begin
    if C = #0 then Result := '(none)'
    else Result := QuotedStr(C);
  end;

begin
  Result := dcUseDetected;
  if csDestroying in ComponentState then Exit;

  // ============================================================
  // Nur Felder mit aktivierter Checkbox werden berücksichtigt
  // ============================================================
  ShowLE  := CheckBoxAutoDedectLineEnding.Checked;
  ShowDel := CheckBoxAutoDedectDelimiter.Checked;
  ShowQC  := CheckBoxAutoDedectQuoteChar.Checked;

  // Wenn kein Auto-Detect aktiv → sofort raus, kein Dialog
  if not (ShowLE or ShowDel or ShowQC) then
    Exit;

  // ============================================================
  // HasDiff: nur aktive Checkboxen zählen
  // ============================================================
  HasDiff :=
    (ShowLE  and (ACurLE  <> ADetLE))  or
    (ShowDel and (ACurDel <> ADetDel)) or
    (ShowQC  and (ACurQC  <> ADetQC));

  if not HasDiff then Exit;

  // ============================================================
  // Symbolische Namen fürs UI
  // ============================================================
  CurLEName := CSVLineEndingStrToName(ACurLE);
  DetLEName := CSVLineEndingStrToName(ADetLE);

  Dlg := TForm.Create(nil);
  try
    Dlg.Caption := 'CSV Auto-Detection';
    Dlg.Width := 600;
    Dlg.Height := 420;
    Dlg.Position := poScreenCenter;
    Dlg.BorderStyle := bsDialog;
    Dlg.FormStyle := fsStayOnTop;

    LblTitle := TLabel.Create(Dlg);
    LblTitle.Parent := Dlg;
    LblTitle.Left := 16;
    LblTitle.Top := 12;
    LblTitle.Width := 568;
    LblTitle.Caption := 'Auto-detection found different values than your current settings:';
    LblTitle.Font.Style := [fsBold];

    Content := TStringList.Create;
    try
      Content.Add('File: ' + ExtractFileName(AFileName));
      Content.Add('');
      Content.Add('                     Current       Detected');
      Content.Add('  ──────────────     ─────────     ─────────');

      // ----------------------------------------------------------
      // LineEnding-Zeile
      // ----------------------------------------------------------
      if ShowLE then
      begin
        if ACurLE <> ADetLE then
          Content.Add(Format('  LineEnding         %-13s %s', [CurLEName, DetLEName]))
        else
          Content.Add(Format('  LineEnding         %-13s (same)', [CurLEName]));
      end
      else
        Content.Add(Format('  LineEnding         %-13s (auto-off)', [CurLEName]));

      // ----------------------------------------------------------
      // Delimiter-Zeile
      // ----------------------------------------------------------
      if ShowDel then
      begin
        if ACurDel <> ADetDel then
          Content.Add(Format('  Delimiter          %-13s %s', [DelimToStr(ACurDel), DelimToStr(ADetDel)]))
        else
          Content.Add(Format('  Delimiter          %-13s (same)', [DelimToStr(ACurDel)]));
      end
      else
        Content.Add(Format('  Delimiter          %-13s (auto-off)', [DelimToStr(ACurDel)]));

      // ----------------------------------------------------------
      // QuoteChar-Zeile
      // ----------------------------------------------------------
      if ShowQC then
      begin
        if ACurQC <> ADetQC then
          Content.Add(Format('  QuoteChar          %-13s %s', [QCToStr(ACurQC), QCToStr(ADetQC)]))
        else
          Content.Add(Format('  QuoteChar          %-13s (same)', [QCToStr(ACurQC)]));
      end
      else
        Content.Add(Format('  QuoteChar          %-13s (auto-off)', [QCToStr(ACurQC)]));

      Content.Add('');
      Content.Add('Note: (auto-off) = auto-detection disabled for this field');
      Content.Add('      via the checkbox in the CSV Settings tab.');
      Content.Add('');
      Content.Add('Choose an action:');
      Content.Add('');
      Content.Add('  [Use Detected]    — Apply detected values for fields with auto-detect ON');
      Content.Add('  [Keep Current]    — Load file with your current settings');
      Content.Add('  [Cancel]          — Abort — do not load the file');

      Memo := TMemo.Create(Dlg);
      Memo.Parent := Dlg;
      Memo.Left := 16;
      Memo.Top := 38;
      Memo.Width := 568;
      Memo.Height := 270;
      Memo.ReadOnly := True;
      Memo.ScrollBars := ssVertical;
      Memo.Font.Name := 'Courier New';
      Memo.Font.Size := 10;
      Memo.Lines.Assign(Content);
    finally
      Content.Free;
    end;

    // ============================================================
    // Drei Buttons
    // ============================================================
    BtnUseDetected := TButton.Create(Dlg);
    BtnUseDetected.Parent := Dlg;
    BtnUseDetected.Caption := 'Use Detected';
    BtnUseDetected.Left := 192;
    BtnUseDetected.Top := 330;
    BtnUseDetected.Width := 120;
    BtnUseDetected.Default := True;
    BtnUseDetected.ModalResult := mrYes;

    BtnKeepCurrent := TButton.Create(Dlg);
    BtnKeepCurrent.Parent := Dlg;
    BtnKeepCurrent.Caption := 'Keep Current';
    BtnKeepCurrent.Left := 320;
    BtnKeepCurrent.Top := 330;
    BtnKeepCurrent.Width := 120;
    BtnKeepCurrent.ModalResult := mrNo;

    BtnCancelFile := TButton.Create(Dlg);
    BtnCancelFile.Parent := Dlg;
    BtnCancelFile.Caption := 'Cancel';
    BtnCancelFile.Left := 448;
    BtnCancelFile.Top := 330;
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

function TfrmDataEditor.CheckCSVConfiguration(
  AMaxRowWidth: Int64;
  ACurrentBlockSize: Int64;
  ACurrentBufferSize: Int64): TCSVConfigCheck;
const
  MIN_PERFORMANCE_BLOCK = 1048576;   // 1 MB
  MAX_BLOCK_HARD_LIMIT  = 16777216;  // 16 MB
  BUFFER_SAFETY_MARGIN  = 16384;     // 16 KB
var
  NeedBlock: Int64;
  NeedBuffer: Int64;
begin
  Result.Status            := ccsOptimal;
  Result.MaxRowWidth       := AMaxRowWidth;   // NEU
  Result.CurrentBlockSize  := ACurrentBlockSize;
  Result.CurrentBufferSize := ACurrentBufferSize;
  Result.Reason            := '';

  if AMaxRowWidth <= 0 then
  begin
    Result.OptimalBlockSize  := ACurrentBlockSize;
    Result.OptimalBufferSize := ACurrentBufferSize;
    Exit;
  end;

  NeedBlock := AMaxRowWidth * 2;
  if NeedBlock < MIN_PERFORMANCE_BLOCK then
    NeedBlock := MIN_PERFORMANCE_BLOCK;
  if NeedBlock > MAX_BLOCK_HARD_LIMIT then
    NeedBlock := MAX_BLOCK_HARD_LIMIT;

  NeedBuffer := NeedBlock * 2;
  if NeedBuffer < (AMaxRowWidth * 2 + BUFFER_SAFETY_MARGIN) then
    NeedBuffer := AMaxRowWidth * 2 + BUFFER_SAFETY_MARGIN;

  Result.OptimalBlockSize  := NeedBlock;
  Result.OptimalBufferSize := NeedBuffer;

  if ACurrentBufferSize < (AMaxRowWidth * 2 + BUFFER_SAFETY_MARGIN) then
  begin
    Result.Status := ccsBroken;
    Result.Reason := Format(
      'The current BufferSize (%s) is too small for rows of ' +
      'up to %s. The load would fail.',
      [FormatBytesSmart(ACurrentBufferSize), FormatBytesSmart(AMaxRowWidth)]);
    Exit;
  end;

  if ACurrentBlockSize < (AMaxRowWidth * 2) then
  begin
    Result.Status := ccsSuboptimal;
    Result.Reason := Format(
      'The current BlockSize (%s) is smaller than 2 × max row width ' +
      '(%s). Rows will be reassembled across multiple blocks, ' +
      'which can be several times slower.',
      [FormatBytesSmart(ACurrentBlockSize), FormatBytesSmart(AMaxRowWidth)]);
    Exit;
  end;

  if ACurrentBufferSize < (ACurrentBlockSize * 2) then
  begin
    Result.Status := ccsSuboptimal;
    Result.Reason := Format(
      'The current BufferSize (%s) is smaller than 2 × BlockSize (%s). ' +
      'This may cause unnecessary refills.',
      [FormatBytesSmart(ACurrentBufferSize),
       FormatBytesSmart(ACurrentBlockSize)]);
    Exit;
  end;
end;

function TfrmDataEditor.AskApplyCSVConfig(const ACheck: TCSVConfigCheck): TDetectChoice;
var
  Dlg: TForm;
  Memo: TMemo;
  LblTitle: TLabel;
  BtnApply, BtnKeep, BtnCancel: TButton;
  Content: TStringList;
  Ratio: Double;
begin
  // ------------------------------------------------------------
  // ccsBroken: Dialog zwingend, kein "Keep Current"
  //           (Keep Current würde garantiert zum Fehler führen)
  // ------------------------------------------------------------
  if ACheck.Status = ccsBroken then
  begin
    Result := dcCancel;

    Dlg := TForm.Create(nil);
    try
      Dlg.Caption     := 'CSV Configuration — ERROR';
      Dlg.Width       := 640;
      Dlg.Height      := 340;
      Dlg.Position    := poScreenCenter;
      Dlg.BorderStyle := bsDialog;

      LblTitle := TLabel.Create(Dlg);
      LblTitle.Parent  := Dlg;
      LblTitle.Left    := 16;
      LblTitle.Top     := 12;
      LblTitle.Width   := 608;
      LblTitle.Caption := 'The current settings will cause the load to fail:';
      LblTitle.Font.Style := [fsBold];
      LblTitle.Font.Color := clRed;

      Content := TStringList.Create;
      try
        Content.Add(Format('  Max Row Width (from scan):  %s',
          [FormatBytesSmart(ACheck.MaxRowWidth)]));
        Content.Add('');
        Content.Add('                     Current        Required');
        Content.Add('  ─────────────     ─────────      ─────────');
        Content.Add(Format('  BlockSize         %-14s %s',
          [FormatBytesSmart(ACheck.CurrentBlockSize),
           FormatBytesSmart(ACheck.OptimalBlockSize)]));
        Content.Add(Format('  BufferSize        %-14s %s',
          [FormatBytesSmart(ACheck.CurrentBufferSize),
           FormatBytesSmart(ACheck.OptimalBufferSize)]));
        Content.Add('');
        Content.Add(ACheck.Reason);
        Content.Add('');
        Content.Add('Choose an action:');
        Content.Add('');
        Content.Add('  [Fix and Continue]  — Apply required values and start load');
        Content.Add('  [Cancel]            — Do not load the file');

        Memo := TMemo.Create(Dlg);
        Memo.Parent     := Dlg;
        Memo.Left       := 16;
        Memo.Top        := 38;
        Memo.Width      := 608;
        Memo.Height     := 220;
        Memo.ReadOnly   := True;
        Memo.ScrollBars := ssVertical;
        Memo.Font.Name  := 'Courier New';
        Memo.Font.Size  := 10;
        Memo.Lines.Assign(Content);
      finally
        Content.Free;
      end;

      BtnApply := TButton.Create(Dlg);
      BtnApply.Parent      := Dlg;
      BtnApply.Caption     := 'Fix and Continue';
      BtnApply.Left        := 340;
      BtnApply.Top         := 270;
      BtnApply.Width       := 140;
      BtnApply.Default     := True;
      BtnApply.ModalResult := mrYes;

      BtnCancel := TButton.Create(Dlg);
      BtnCancel.Parent      := Dlg;
      BtnCancel.Caption     := 'Cancel';
      BtnCancel.Left        := 490;
      BtnCancel.Top         := 270;
      BtnCancel.Width       := 134;
      BtnCancel.ModalResult := mrCancel;

      case Dlg.ShowModal of
        mrYes: Result := dcUseDetected;   // Fix and Continue
      else
        Result := dcCancel;               // Cancel oder Fenster geschlossen
      end;
    finally
      Dlg.Free;
    end;
    Exit;
  end;

  // ------------------------------------------------------------
  // ccsSuboptimal + AutoTuneCSVBuffer = True: KEIN Dialog.
  // Der Hinweis erscheint im Progress-Fenster und im Report.
  // ------------------------------------------------------------
  if chkBoxAutoTuneCSVBuffer.Checked  then
  begin
    Result := dcUseDetected;
    Exit;
  end;

  // ------------------------------------------------------------
  // ccsSuboptimal + AutoTuneCSVBuffer = False: Dialog mit 3 Optionen
  // ------------------------------------------------------------
  if (ACheck.CurrentBlockSize > 0) and
     (ACheck.CurrentBlockSize < ACheck.OptimalBlockSize) then
    Ratio := ACheck.OptimalBlockSize / ACheck.CurrentBlockSize
  else
    Ratio := 0;

  Result := dcCancel;

  Dlg := TForm.Create(nil);
  try
    Dlg.Caption     := 'CSV Configuration Check';
    Dlg.Width       := 640;
    Dlg.Height      := 380;
    Dlg.Position    := poScreenCenter;
    Dlg.BorderStyle := bsDialog;

    LblTitle := TLabel.Create(Dlg);
    LblTitle.Parent  := Dlg;
    LblTitle.Left    := 16;
    LblTitle.Top     := 12;
    LblTitle.Width   := 608;
    LblTitle.Caption := 'Configuration could be improved for this file:';
    LblTitle.Font.Style := [fsBold];

    Content := TStringList.Create;
    try
      Content.Add(Format('  Max Row Width (from scan):  %s',
        [FormatBytesSmart(ACheck.MaxRowWidth)]));
      Content.Add('');
      Content.Add('                     Current        Recommended');
      Content.Add('  ─────────────     ─────────      ───────────');
      Content.Add(Format('  BlockSize         %-14s %s',
        [FormatBytesSmart(ACheck.CurrentBlockSize),
         FormatBytesSmart(ACheck.OptimalBlockSize)]));
      Content.Add(Format('  BufferSize        %-14s %s',
        [FormatBytesSmart(ACheck.CurrentBufferSize),
         FormatBytesSmart(ACheck.OptimalBufferSize)]));
      Content.Add('');
      Content.Add(ACheck.Reason);
      if Ratio > 1.5 then
        Content.Add(Format('  (BlockSize ratio: %.1f× → potentially %.1f× faster load)',
          [Ratio, Ratio]));
      Content.Add('');
      Content.Add('Choose an action:');
      Content.Add('');
      Content.Add('  [Use Recommended]  — Apply recommended values and start load');
      Content.Add('  [Keep Current]     — Start load with current settings (slower)');
      Content.Add('  [Cancel]           — Do not load the file');

      Memo := TMemo.Create(Dlg);
      Memo.Parent     := Dlg;
      Memo.Left       := 16;
      Memo.Top        := 38;
      Memo.Width      := 608;
      Memo.Height     := 260;
      Memo.ReadOnly   := True;
      Memo.ScrollBars := ssVertical;
      Memo.Font.Name  := 'Courier New';
      Memo.Font.Size  := 10;
      Memo.Lines.Assign(Content);
    finally
      Content.Free;
    end;

    BtnApply := TButton.Create(Dlg);
    BtnApply.Parent      := Dlg;
    BtnApply.Caption     := 'Use Recommended';
    BtnApply.Left        := 200;
    BtnApply.Top         := 310;
    BtnApply.Width       := 140;
    BtnApply.Default     := True;
    BtnApply.ModalResult := mrYes;

    BtnKeep := TButton.Create(Dlg);
    BtnKeep.Parent      := Dlg;
    BtnKeep.Caption     := 'Keep Current';
    BtnKeep.Left        := 345;
    BtnKeep.Top         := 310;
    BtnKeep.Width       := 140;
    BtnKeep.ModalResult := mrNo;

    BtnCancel := TButton.Create(Dlg);
    BtnCancel.Parent      := Dlg;
    BtnCancel.Caption     := 'Cancel';
    BtnCancel.Left        := 490;
    BtnCancel.Top         := 310;
    BtnCancel.Width       := 134;
    BtnCancel.ModalResult := mrCancel;

    case Dlg.ShowModal of
      mrYes: Result := dcUseDetected;    // Use Recommended
      mrNo:  Result := dcKeepCurrent;    // Keep Current
    else
      Result := dcCancel;                // Cancel oder Fenster geschlossen
    end;
  finally
    Dlg.Free;
  end;
end;

procedure TfrmDataEditor.ShowProgressAutoTuneNote(const ACheck: TCSVConfigCheck);
begin
  FProgressAutoTuneNote := Format(
    'Auto-tuned: BlockSize %s → %s, BufferSize %s → %s',
    [FormatBytesSmart(ACheck.CurrentBlockSize),
     FormatBytesSmart(ACheck.OptimalBlockSize),
     FormatBytesSmart(ACheck.CurrentBufferSize),
     FormatBytesSmart(ACheck.OptimalBufferSize)]);

  // Zeile im Progress-Fenster anzeigen — unterhalb der Info-Zeile
  if Assigned(FProgETALbl) then
    FProgETALbl.Caption := FProgressAutoTuneNote;
end;

procedure TfrmDataEditor.ResetAutoTuneProgressNote;
begin
  FProgressAutoTuneNote := '';
end;

end.
