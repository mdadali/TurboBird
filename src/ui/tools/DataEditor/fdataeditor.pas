unit fdataeditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, DB, csvdataset, SdfData, dbf, fpjsondataset, Forms,
  Controls, Graphics, Dialogs, Clipbrd, DBCtrls, StdCtrls, ExtCtrls, ComCtrls,
  Menus, Buttons, SynEdit, SynHighlighterSQL, RxDBGrid, RxDBGridExportPdf,
  RxDBGridPrintGrid, RxDBGridExportSpreadSheet, fpsDataset,
  IB, IBDatabase, IBQuery, IBTable,
  turbocommon, SysTables, uthemeselector, uGenSQLFromCSVDataset,
  uCSVFastDataset, uJSONFastDataSet, uCreateTableFromDataSet,
  fdataexportersintrf, fpsTypes, uReport, uSystemInfo;

type
  TCSVConfigStatus = (ccsOptimal, ccsSuboptimal, ccsBroken);

  TCSVConfigCheck = record
    Status:            TCSVConfigStatus;
    MaxRowWidth:       Int64;
    CurrentBlockSize:  Int64;
    CurrentBufferSize: Int64;
    OptimalBlockSize:  Int64;
    OptimalBufferSize: Int64;
    Reason:            string;
  end;

  TDetectChoice = (dcUseDetected, dcKeepCurrent, dcCancel);

  TfrmDataEditor = class;

  { TfrmDataEditor }
  TfrmDataEditor = class(TForm)
    BitBtn1: TBitBtn;
    btnCopySQL: TButton;
    btnOpenFile: TButton;
    btnExportAs: TButton;
    btnCreateSQL: TButton;
    btnCreateTable: TButton;
    Button1: TButton;
    btnSaveFile: TButton;
    CheckBoxAutoDedectDelimiter: TCheckBox;
    CheckBoxAutoDedectLineEnding: TCheckBox;
    CheckBoxAutoDedectQuoteChar: TCheckBox;
    CheckBoxAutoDedecDecimalSeparator: TCheckBox;   // ← neu
    chkBoxAutoTuneCSVBuffer: TCheckBox;
    chkBoxFirstLineAsFieldName: TCheckBox;
    chkBoxIgnoreOuterWhiteSpace: TCheckBox;
    chkBoxQuoteOuterWhiteSpace: TCheckBox;
    cmbboxDelimiter: TComboBox;
    cmbboxLineEnding: TComboBox;
    cmbboxQuoteChar: TComboBox;
    ComboBox1: TComboBox;                            // ← neu: Decimal Separator
    CSVFastDataset1: TCSVFastDataset;
    DataSource1: TDataSource;
    Dbf1: TDbf;
    DBNavigator1: TDBNavigator;
    edtSaveFrom: TEdit;
    edtSaveProgressUpdatePercent: TEdit;
    edtSaveBufferSize: TEdit;
    edtDefaultFieldLength: TEdit;
    edtProgressUpdatePercent: TEdit;
    edtRowWidthScanRows: TEdit;
    edtFrom: TEdit;
    edtBlockSize: TEdit;
    edtBufferSize: TEdit;
    edtInitialCellCapacity: TEdit;
    edtTo: TEdit;
    edtSaveTo: TEdit;
    FixedFormatDataSet1: TFixedFormatDataSet;
    grboxCSVFileRead: TGroupBox;
    grboxLoadRangeOptions: TGroupBox;
    grboxCSVFileWrite: TGroupBox;
    grboxCSVOptions: TGroupBox;
    grboxSaveRangeOptions: TGroupBox;
    JSONFastDataSet1: TJSONFastDataSet;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    lblSaveActiveSettings: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lblRowWidthScanRows: TLabel;
    lblActivePerformanceSettings: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    lbBlockSize: TLabel;
    lmDecimalseparator: TLabel;                      // ← neu
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
    rbLoadAllRows: TRadioButton;
    rbSaveAllRows: TRadioButton;
    rbRange: TRadioButton;
    rbSaveRange: TRadioButton;
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
    SpeedButton1: TSpeedButton;
    sWorksheetDataset1: TsWorksheetDataset;
    SynEditSQL: TSynEdit;
    SynSQLSyn1: TSynSQLSyn;
    ToggleBox1: TToggleBox;
    tsCommonSettings: TTabSheet;
    tsSQL: TTabSheet;
    tsGrid: TTabSheet;
    tsDetail: TTabSheet;

    procedure BitBtn1Click(Sender: TObject);
    procedure btnCopySQLClick(Sender: TObject);
    procedure btnCreateSQLClick(Sender: TObject);
    procedure btnCreateTableClick(Sender: TObject);
    procedure btnOpenFileClick(Sender: TObject);
    procedure btnExportAsClick(Sender: TObject);
    procedure btnSaveFileClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure chkBoxAutoTuneCSVBufferChange(Sender: TObject);
    procedure cmbboxDelimiterChange(Sender: TObject);
    procedure cmbboxLineEndingChange(Sender: TObject);
    procedure cmbboxQuoteCharChange(Sender: TObject);
    procedure cmbboxDecimalSeparatorChange(Sender: TObject);   // ← neu
    procedure CSVFastDataset1AfterDelete(DataSet: TDataSet);
    procedure CSVFastDataset1AfterPost(DataSet: TDataSet);
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
    FIsLoading: Boolean;

    // ---- Progress (Reader) ----
    FProgressForm: TForm;
    FProgPhaseLbl: TLabel;
    FProgBar: TProgressBar;
    FProgInfoLbl: TLabel;
    FProgSpeedLbl: TLabel;
    FProgElapsedLbl: TLabel;
    FProgETALbl: TLabel;
    FProgCancelBtn: TButton;
    FProgRAMBar: TProgressBar;
    FProgRAMInfoLbl: TLabel;
    FProgRAMStatusLbl: TLabel;

    FProgTotalRows: Int64;
    FProgTotalRAMMB: Int64;
    FRAMWarningShown: Boolean;
    FInitialFreeRAMMB: Int64;
    FCurrentFreeRAMMB: Int64;
    FPeakUsedRAMMB: Int64;
    FPeakEstimatedMB: Int64;
    FLoadMaxRowWidth: Int64;
    FCancelRequested: Boolean;

    FInitialSwapTotalMB: Int64;
    FInitialSwapUsedMB: Int64;
    FCurrentSwapUsedMB: Int64;
    FLastSwapUsedMB: Int64;
    FPeakSwapUsedMB: Int64;
    FThrashingDetected: Boolean;

    FProgressSamples: array of Double;
    FProgressBytesSamples: array of Double;
    FLastProgressRows: Int64;
    FLastProgressSeconds: Double;
    FLastProgressBytes: Int64;
    FScanStartTick: QWord;
    FProgressAutoTuneNote: string;

    // ---- Progress (Writer) ----
    FSaveProgressForm: TForm;
    FSavePhaseLbl: TLabel;
    FSaveInfoLbl: TLabel;
    FSaveBar: TProgressBar;
    FSaveSpeedLbl: TLabel;
    FSaveElapsedLbl: TLabel;
    FSaveETALbl: TLabel;
    FSaveCancelBtn: TButton;

    FSaveTotalRows: Int64;
    FSaveStartTime: TDateTime;
    FSaveLastRows: Int64;
    FSaveLastSeconds: Double;
    FSaveSpeedSamples: array of Double;
    FSaveCancelRequested: Boolean;

    FDataSetChanged: Boolean;

    // ---- Helper ----
    function  CheckCSVConfiguration(
                AMaxRowWidth: Int64;
                ACurrentBlockSize: Int64;
                ACurrentBufferSize: Int64): TCSVConfigCheck;
    function  AskApplyCSVConfig(const ACheck: TCSVConfigCheck): TDetectChoice;
    procedure ShowProgressAutoTuneNote(const ACheck: TCSVConfigCheck);
    procedure ResetAutoTuneProgressNote;

    procedure ResetDataset(DS: TDataSet);
    procedure ResetAllDatasets;
    procedure LoadFile(const FileName: string);
    procedure SaveFile(const FileName: string; AskForFileName: Boolean = False);
    procedure SetCSVSettingsFromForm;
    procedure SetSaveSettingsFromForm;
    function  SelectDataSetByFileName(const AFileName: string): Boolean;
    procedure SyncLineEndingCombo;
    procedure SyncDelimiterCombo;
    procedure SyncQuoteCharCombo;
    procedure SyncDecimalSeparatorCombo;             // ← neu
    procedure ReadIni;
    procedure WriteIni;
    procedure ReadIniSaveSettings;
    procedure WriteIniSaveSettings;

    function  AskUseDetected(
                const AFileName: string;
                const ACurLE, ADetLE: string;
                const ACurDel, ADetDel: Char;
                const ACurQC, ADetQC: Char;
                const ACurDec, ADetDec: Char): TDetectChoice;   // ← erweitert

    procedure LoadSelectedDataset(const AFileName: string; out AError: string);

    procedure HandleScanProgress(ABytesRead, ATotalBytes: Int64);
    procedure HandleLoadProgress(ARowsRead: Int64; ABytesRead: Int64;
      AElapsedSeconds: Double);
    procedure ProgCancelClick(Sender: TObject);
    procedure BuildProgressForm;

    procedure rbSaveRangeChange(Sender: TObject);
    procedure BuildSaveProgressForm;
    procedure FreeSaveProgressForm;
    procedure HandleSaveProgress(ARowsWritten: Int64; ABytesWritten: Int64;
      AElapsedSeconds: Double);
    procedure SaveCancelClick(Sender: TObject);

    procedure UpdateSaveButton;
    procedure MarkDataSetChanged;
    procedure ClearDataSetChanged;
    procedure GenericDatasetModified(DataSet: TDataSet);
  public
  end;

implementation

{$R *.lfm}

{ ==================================================================
  Progress-Fenster (Reader)
  ================================================================== }

procedure TfrmDataEditor.BuildProgressForm;
var
  PnlInfo, PnlProgress, PnlRAM, PnlCancel: TPanel;
  Separator: TLabel;
begin
  FProgressForm := TForm.Create(Self);
  FProgressForm.Parent := Self;

  FProgressForm.Caption     := 'Loading...';
  FProgressForm.Width       := 560;
  FProgressForm.Height      := 470;
  FProgressForm.Position    := poOwnerFormCenter;
  FProgressForm.BorderStyle := bsNone;
  FProgressForm.FormStyle   := fsNormal;

  FProgressForm.Left := (self.Width - FProgressForm.Width) div 2 ;
  FProgressForm.Top  := ((self.Height - FProgressForm.Height) div 2) + 30;

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

  PnlRAM := TPanel.Create(FProgressForm);
  PnlRAM.Parent := FProgressForm;
  PnlRAM.Align := alTop;
  PnlRAM.Height := 145;
  PnlRAM.BevelOuter := bvNone;

  Separator := TLabel.Create(FProgressForm);
  Separator.Parent := PnlRAM;
  Separator.Left := 16;
  Separator.Top := 0;
  Separator.Width := 520;
  Separator.Caption := '──────────────────────────────────────────';
  Separator.Font.Color := clGray;

  FProgRAMInfoLbl := TLabel.Create(FProgressForm);
  FProgRAMInfoLbl.Parent := PnlRAM;
  FProgRAMInfoLbl.Left := 16;
  FProgRAMInfoLbl.Top := 22;
  FProgRAMInfoLbl.Width := 520;
  FProgRAMInfoLbl.AutoSize := True;
  FProgRAMInfoLbl.Caption := 'RAM:      ...';

  FProgRAMBar := TProgressBar.Create(FProgressForm);
  FProgRAMBar.Parent := PnlRAM;
  FProgRAMBar.Left := 16;
  FProgRAMBar.Top := 80;
  FProgRAMBar.Width := 520;
  FProgRAMBar.Height := 20;
  FProgRAMBar.Min := 0;
  FProgRAMBar.Max := 100;

  FProgRAMStatusLbl := TLabel.Create(FProgressForm);
  FProgRAMStatusLbl.Parent := PnlRAM;
  FProgRAMStatusLbl.Left := 16;
  FProgRAMStatusLbl.Top := 108;
  FProgRAMStatusLbl.Width := 520;

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

  ElapsedSeconds := (GetTickCount64 - FScanStartTick) / 1000.0;
  if ElapsedSeconds > 0.2 then
    BytesPerSec := ABytesRead / ElapsedSeconds
  else
    BytesPerSec := 0;

  FProgInfoLbl.Caption := Format('Scan:   %s / %s   (%d%%)',
    [FormatBytesSmart(ABytesRead), FormatBytesSmart(ATotalBytes), Percent]);

  if BytesPerSec > 0 then
    FProgSpeedLbl.Caption := Format('Speed:  %s/sec  |  Elapsed: %s',
      [FormatBytesSmart(Round(BytesPerSec)),
       FormatDateTime('hh:nn:ss', ElapsedSeconds / SecsPerDay)])
  else
    FProgSpeedLbl.Caption := '';

  FProgETALbl.Caption := '';
  Application.ProcessMessages;
end;

procedure TfrmDataEditor.HandleLoadProgress(ARowsRead: Int64;
  ABytesRead: Int64; AElapsedSeconds: Double);
var
  Percent: Integer;
  RowsPerSec, BytesPerSec, ETASeconds: Double;
  IntervalRows, IntervalBytes: Double;
  UsedRAMMB, EstimatedMB: Int64;
  UsedPercent: Integer;
  SwapUsedMB, SwapDeltaMB, SwapTotalMB: Int64;
  StatusText: string;
  StatusColor: TColor;
begin
  if (FLastProgressSeconds > 0) and (AElapsedSeconds > FLastProgressSeconds) then
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
     FormatFloat('#,##0', FProgTotalRows), Percent]);

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

  FCurrentFreeRAMMB := GetRAMFreeMB;
  if FCurrentFreeRAMMB < 0 then FCurrentFreeRAMMB := FInitialFreeRAMMB;

  UsedRAMMB := FProgTotalRAMMB - FCurrentFreeRAMMB;
  if UsedRAMMB < 0 then UsedRAMMB := 0;
  if UsedRAMMB > FPeakUsedRAMMB then FPeakUsedRAMMB := UsedRAMMB;

  if (FLoadMaxRowWidth > 0) and (ARowsRead > 0) then
    EstimatedMB := (ARowsRead * (FLoadMaxRowWidth + 64)) div (1024 * 1024)
  else
    EstimatedMB := 0;
  if EstimatedMB > FPeakEstimatedMB then FPeakEstimatedMB := EstimatedMB;

  SwapTotalMB := GetSwapTotalMB;
  SwapUsedMB  := GetSwapUsedMB;
  if SwapUsedMB < 0 then SwapUsedMB := 0;
  if SwapUsedMB > FPeakSwapUsedMB then FPeakSwapUsedMB := SwapUsedMB;

  SwapDeltaMB := SwapUsedMB - FLastSwapUsedMB;
  if (SwapDeltaMB > 500) and (FLastSwapUsedMB >= 0) then
    FThrashingDetected := True;
  FLastSwapUsedMB    := SwapUsedMB;
  FCurrentSwapUsedMB := SwapUsedMB;

  if FProgTotalRAMMB > 0 then
    UsedPercent := (UsedRAMMB * 100) div FProgTotalRAMMB
  else
    UsedPercent := 0;
  if UsedPercent > 100 then UsedPercent := 100;
  FProgRAMBar.Position := UsedPercent;

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
  FCancelRequested := True;
  FProgCancelBtn.Enabled := False;
  FProgCancelBtn.Caption := 'Cancelling...';
end;

procedure TfrmDataEditor.rbRangeChange(Sender: TObject);
begin
  edtFrom.Enabled := rbRange.Checked;
  edtTo.Enabled   := rbRange.Checked;
end;

{ ==================================================================
  Sync-Combos
  ================================================================== }

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
    cmbboxLineEnding.ItemIndex := 1;
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
    cmbboxDelimiter.ItemIndex := 0;
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
    cmbboxQuoteChar.ItemIndex := 3;
    cmbboxQuoteChar.Text := CSVQuoteChar;
  end;
end;

procedure TfrmDataEditor.SyncDecimalSeparatorCombo;
begin
  if not Assigned(ComboBox1) then Exit;
  if CSVDecimalSeparator = ',' then
    ComboBox1.ItemIndex := 1
  else
    ComboBox1.ItemIndex := 0;
end;

{ ==================================================================
  Settings — Form → Dataset
  ================================================================== }

procedure TfrmDataEditor.SetCSVSettingsFromForm;
begin
  CSVFastDataset1.CSVOptions.DefaultFieldLength :=
    StrToIntDef(edtDefaultFieldLength.Text, 50);
  CSVFastDataset1.CSVOptions.Delimiter  := CSVDelimiter;
  CSVFastDataset1.CSVOptions.LineEnding := CSVLineEnding;

  if CSVQuoteChar <> #0 then
    CSVFastDataset1.CSVOptions.QuoteChar := CSVQuoteChar
  else
    CSVFastDataset1.CSVOptions.QuoteChar := #0;

  CSVFastDataset1.CSVOptions.FirstLineAsFieldNames :=
    chkBoxFirstLineAsFieldName.Checked;
  CSVFastDataset1.CSVOptions.IgnoreOuterWhitespace :=
    chkBoxIgnoreOuterWhiteSpace.Checked;
  CSVFastDataset1.CSVOptions.QuoteOuterWhitespace  :=
    chkBoxQuoteOuterWhiteSpace.Checked;

  // ---- DecimalSeparator: GUI → globale Var → Dataset ----
  if Trim(ComboBox1.Text) = ',' then
    CSVDecimalSeparator := ','
  else
    CSVDecimalSeparator := '.';
  TCSVFastDataset(CSVFastDataset1).DecimalSeparator := CSVDecimalSeparator;

  // Performance
  TCSVFastDataset(CSVFastDataset1).BlockSize :=
    StrToIntDef(edtBlockSize.Text, 1048576);
  TCSVFastDataset(CSVFastDataset1).BufferSize :=
    StrToInt64Def(edtBufferSize.Text, 2097152);
  TCSVFastDataset(CSVFastDataset1).InitialCellCapacity :=
    StrToIntDef(edtInitialCellCapacity.Text, 64);

  CSVBlockSizeBytes      := TCSVFastDataset(CSVFastDataset1).BlockSize;
  CSVBufferSizeBytes     := TCSVFastDataset(CSVFastDataset1).BufferSize;
  CSVInitialCellCapacity := TCSVFastDataset(CSVFastDataset1).InitialCellCapacity;

  edtBlockSize.Text           := IntToStr(CSVBlockSizeBytes);
  edtBufferSize.Text          := IntToStr(CSVBufferSizeBytes);
  edtInitialCellCapacity.Text := IntToStr(CSVInitialCellCapacity);

  lblActivePerformanceSettings.Caption := Format(
    'Active: Block=%s, Buffer=%s, Cells=%d, ScanRows=%d',
    [FormatBytesSmart(CSVBlockSizeBytes),
     FormatBytesSmart(CSVBufferSizeBytes),
     CSVInitialCellCapacity,
     StrToInt64Def(edtRowWidthScanRows.Text, 0)]);

  SetSaveSettingsFromForm;
end;

procedure TfrmDataEditor.SetSaveSettingsFromForm;
var
  SaveBufferSize: Int64;
  SaveProgressPercent: Integer;
begin
  SaveBufferSize := StrToInt64Def(edtSaveBufferSize.Text, 4194304);
  if SaveBufferSize < 65536 then
    SaveBufferSize := 65536;

  SaveProgressPercent := StrToIntDef(edtSaveProgressUpdatePercent.Text, 1);
  if SaveProgressPercent < 1 then
    SaveProgressPercent := 1;

  TCSVFastDataset(CSVFastDataset1).SaveBufferSize            := SaveBufferSize;
  TCSVFastDataset(CSVFastDataset1).SaveProgressUpdatePercent := SaveProgressPercent;
  TCSVFastDataset(CSVFastDataset1).SaveWriteBOM              :=
    fIniFile.ReadBool('CSVEditor', 'SaveWriteBOM', False);

  if rbSaveRange.Checked then
  begin
    TCSVFastDataset(CSVFastDataset1).SaveSkipFirstRows :=
      StrToInt64Def(edtSaveFrom.Text, 0);
    TCSVFastDataset(CSVFastDataset1).SaveStopAfterRow :=
      StrToInt64Def(edtSaveTo.Text, 0);
  end
  else
  begin
    TCSVFastDataset(CSVFastDataset1).SaveSkipFirstRows := 0;
    TCSVFastDataset(CSVFastDataset1).SaveStopAfterRow  := 0;
  end;

  edtSaveBufferSize.Text            := IntToStr(SaveBufferSize);
  edtSaveProgressUpdatePercent.Text := IntToStr(SaveProgressPercent);

  lblSaveActiveSettings.Caption := Format(
    'Active: SaveBuffer=%s, SaveProgress=%d%%, Range=%s',
    [FormatBytesSmart(SaveBufferSize),
     SaveProgressPercent,
     BoolToStr(rbSaveRange.Checked, 'Range', 'All')]);
end;

{ ==================================================================
  INI
  ================================================================== }

procedure TfrmDataEditor.ReadIni;
begin
  edtDefaultFieldLength.Text          := IntToStr(CSVDefaultFieldLength);
  chkBoxFirstLineAsFieldName.Checked  := CSVFirstLineAsFieldNames;
  chkBoxIgnoreOuterWhiteSpace.Checked := CSVIgnoreOuterWhitespace;
  chkBoxQuoteOuterWhiteSpace.Checked  := CSVQuoteOuterWhitespace;

  CheckBoxAutoDedectLineEnding.Checked       := AutoDetectLineEnding;
  CheckBoxAutoDedectDelimiter.Checked        := AutoDetectDelimiter;
  CheckBoxAutoDedectQuoteChar.Checked        := AutoDetectQuoteChar;
  CheckBoxAutoDedecDecimalSeparator.Checked  := AutoDetectDecimalSeparator;

  edtBlockSize.Text             := IntToStr(CSVBlockSizeBytes);
  edtBufferSize.Text            := IntToStr(CSVBufferSizeBytes);
  edtInitialCellCapacity.Text   := IntToStr(CSVInitialCellCapacity);
  edtProgressUpdatePercent.Text := IntToStr(CSVProgressUpdatePercent);
  chkBoxAutoTuneCSVBuffer.Checked := CSVAutoTuneCSVBuffer;

  edtRowWidthScanRows.Text := IntToStr(
    fIniFile.ReadInteger('CSV', 'RowWidthScanRows', 0));

  ReadIniSaveSettings;

  SyncLineEndingCombo;
  SyncDelimiterCombo;
  SyncQuoteCharCombo;
  SyncDecimalSeparatorCombo;
end;

procedure TfrmDataEditor.WriteIni;
begin
  CSVDefaultFieldLength := StrToIntDef(edtDefaultFieldLength.Text, 50);
  CSVFirstLineAsFieldNames := chkBoxFirstLineAsFieldName.Checked;
  CSVIgnoreOuterWhitespace := chkBoxIgnoreOuterWhiteSpace.Checked;
  CSVQuoteOuterWhitespace  := chkBoxQuoteOuterWhiteSpace.Checked;

  AutoDetectLineEnding        := CheckBoxAutoDedectLineEnding.Checked;
  AutoDetectDelimiter         := CheckBoxAutoDedectDelimiter.Checked;
  AutoDetectQuoteChar         := CheckBoxAutoDedectQuoteChar.Checked;
  AutoDetectDecimalSeparator  := CheckBoxAutoDedecDecimalSeparator.Checked;

  // Persistiere aktuelles DecimalSeparator-Zeichen
  if Trim(ComboBox1.Text) = ',' then
    CSVDecimalSeparator := ','
  else
    CSVDecimalSeparator := '.';

  CSVBlockSizeBytes        := StrToIntDef(edtBlockSize.Text, 1048576);
  CSVBufferSizeBytes       := StrToInt64Def(edtBufferSize.Text, 2097152);
  CSVInitialCellCapacity   := StrToIntDef(edtInitialCellCapacity.Text, 64);
  CSVProgressUpdatePercent := StrToIntDef(edtProgressUpdatePercent.Text, 1);
  CSVAutoTuneCSVBuffer     := chkBoxAutoTuneCSVBuffer.Checked;

  fIniFile.WriteInteger('CSV', 'RowWidthScanRows',
    StrToIntDef(edtRowWidthScanRows.Text, 0));

  WriteIniSaveSettings;
end;

procedure TfrmDataEditor.ReadIniSaveSettings;
begin
  edtSaveBufferSize.Text            := IntToStr(CSVSaveBufferSizeBytes);
  edtSaveProgressUpdatePercent.Text := IntToStr(CSVSaveProgressUpdatePercent);

  rbSaveAllRows.Checked := not CSVSaveUseRange;
  rbSaveRange.Checked   := CSVSaveUseRange;
  edtSaveFrom.Enabled   := CSVSaveUseRange;
  edtSaveTo.Enabled     := CSVSaveUseRange;
  edtSaveTo.Text        := IntToStr(CSVSaveStopAfterRow);
end;

procedure TfrmDataEditor.WriteIniSaveSettings;
begin
  CSVSaveBufferSizeBytes       := StrToInt64Def(edtSaveBufferSize.Text, 4194304);
  CSVSaveProgressUpdatePercent := StrToIntDef(edtSaveProgressUpdatePercent.Text, 1);
  CSVSaveUseRange := rbSaveRange.Checked;
  if CSVSaveUseRange then
    CSVSaveStopAfterRow := StrToInt64Def(edtSaveTo.Text, 0)
  else
    CSVSaveStopAfterRow := 0;
end;

{ ==================================================================
  Reset / Load / Save
  ================================================================== }

procedure TfrmDataEditor.ResetDataset(DS: TDataSet);
begin
  if DS = nil then Exit;
  if DS.Active then DS.DisableControls;
  try
    try if DS.Active then DS.Close;         except end;
    try DS.Fields.Clear;                    except end;
    try DS.FieldDefs.Clear;                 except end;
  finally
    if DS.Active then DS.EnableControls;
  end;
end;

procedure TfrmDataEditor.ResetAllDatasets;
begin
  if Assigned(RxDBGrid1)    then RxDBGrid1.DataSource := nil;
  if Assigned(DBNavigator1) then DBNavigator1.DataSource := nil;
  if Assigned(DataSource1)  then DataSource1.DataSet := nil;

  ResetDataset(CSVFastDataset1);
  ResetDataset(JSONFastDataSet1);
  ResetDataset(sWorksheetDataset1);
  ResetDataset(Dbf1);
  ResetDataset(SdfDataSet1);
  ResetDataset(FixedFormatDataSet1);

  if Assigned(RxDBGrid1) then
    try RxDBGrid1.Columns.Clear; except end;
  if Assigned(SynEditSQL) then
    try SynEditSQL.Lines.Clear;  except end;
end;

procedure TfrmDataEditor.LoadFile(const FileName: string);
var
  DS: TDataSet;
begin
  DS := DataSource1.DataSet;
  if not Assigned(DS) then Exit;

  RxDBGrid1.BeginUpdate;
  try
    RxDBGrid1.DataSource    := nil;
    DBNavigator1.DataSource := nil;

    DS.DisableControls;
    try
      if DS.Active then
      begin
        DS.Close;
        DS.Fields.Clear;
      end;

      if DS = CSVFastDataset1 then
        CSVFastDataset1.LoadFromCSVFile(FileName)
      else if DS = JSONFastDataSet1 then
        JSONFastDataSet1.LoadFromFile(FileName)
      else if DS = sWorksheetDataset1 then
      begin
        sWorksheetDataset1.FileName  := FileName;
        sWorksheetDataset1.SheetName :=
          ChangeFileExt(ExtractFileName(FileName), '');
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

      if DS.Active and (not DS.IsEmpty) then
        DS.First;
    finally
      DS.EnableControls;
    end;

    RxDBGrid1.DataSource    := DataSource1;
    DBNavigator1.DataSource := DataSource1;

    ClearDataSetChanged;
  finally
    RxDBGrid1.EndUpdate(True);
  end;
end;

procedure SaveJSONToFile(DataSet: TJSONFastDataSet; const FileName: string);
var
  JSONText: string;
  SL: TStringList;
begin
  JSONText := DataSet.SaveToJSON;
  SL := TStringList.Create;
  try
    SL.Text := JSONText;
    SL.SaveToFile(FileName);
  finally
    SL.Free;
  end;
end;

procedure TfrmDataEditor.SaveFile(const FileName: string;
  AskForFileName: Boolean = False);
var
  P: TPoint;
  TargetFileName: string;
  SaveErr: string;
  SaveStartTime, SaveEndTime: TDateTime;
  SaveElapsed: Double;
  LoadSpeedAvg, Sum: Double;
  I: Integer;
begin
  if not Assigned(DataSource1.DataSet) then Exit;
  if not DataSource1.DataSet.Active then Exit;

  if AskForFileName or FDataSetChanged then
  begin
    if FileName <> '' then
      SaveDialog1.FileName := FileName;
    if not SaveDialog1.Execute then
      Exit;
    TargetFileName := SaveDialog1.FileName;
  end
  else
    TargetFileName := FileName;

  if TargetFileName = '' then
  begin
    MessageDlg('No target file name.', mtError, [mbOK], 0);
    Exit;
  end;

  if DataSource1.DataSet = CSVFastDataset1 then
  begin
    SaveErr := '';
    FSaveCancelRequested := False;
    FSaveStartTime       := Now;
    FSaveLastRows        := 0;
    FSaveLastSeconds     := 0;
    SetLength(FSaveSpeedSamples, 0);

    SetCSVSettingsFromForm;
    SetSaveSettingsFromForm;

    if TCSVFastDataset(CSVFastDataset1).SaveStopAfterRow > 0 then
      FSaveTotalRows := TCSVFastDataset(CSVFastDataset1).SaveStopAfterRow
    else
    begin
      FSaveTotalRows := CSVFastDataset1.RecordCount;
      if FSaveTotalRows > 0 then
        Dec(FSaveTotalRows, TCSVFastDataset(CSVFastDataset1).SaveSkipFirstRows);
    end;
    if FSaveTotalRows < 0 then FSaveTotalRows := 0;

    BuildSaveProgressForm;
    FSavePhaseLbl.Caption := 'Saving CSV...';
    FSaveInfoLbl.Caption := Format('File: %s   (%s rows)',
      [ExtractFileName(TargetFileName),
       FormatFloat('#,##0', FSaveTotalRows)]);
    FSaveProgressForm.Show;
    FSaveProgressForm.BringToFront;
    Application.ProcessMessages;

    try
      TCSVFastDataset(CSVFastDataset1).OnSaveProgress := @HandleSaveProgress;
      TCSVFastDataset(CSVFastDataset1).SaveCancelFlag := @FSaveCancelRequested;

      try
        CSVFastDataset1.SaveToCSVFile(TargetFileName);
      except
        on E: EAbort do
          SaveErr := 'Cancelled by user';
        on E: Exception do
          SaveErr := E.ClassName + ': ' + E.Message;
      end;

      TCSVFastDataset(CSVFastDataset1).OnSaveProgress := nil;
      TCSVFastDataset(CSVFastDataset1).SaveCancelFlag := nil;

      SaveEndTime := Now;
      SaveElapsed := (SaveEndTime - FSaveStartTime) * SecsPerDay;

      FSaveBar.Position := 100;
      if FSaveCancelRequested then
        FSavePhaseLbl.Caption := 'Cancelled by user'
      else
        FSavePhaseLbl.Caption := Format('Finished: %s rows',
          [FormatFloat('#,##0', CSVFastDataset1.RecordCount)]);
      Application.ProcessMessages;
      Sleep(500);
    finally
      FreeSaveProgressForm;
    end;

    if SaveErr <> '' then
    begin
      MessageDlg(
        'Could not save file:' + sLineBreak + sLineBreak +
        'File: ' + TargetFileName + sLineBreak + sLineBreak +
        'Error:' + sLineBreak + SaveErr,
        mtError, [mbOK], 0);
      Exit;
    end;

    if not FSaveCancelRequested then
    begin
      LoadSpeedAvg := 0;
      if Length(FSaveSpeedSamples) > 1 then
      begin
        Sum := 0;
        for I := 0 to Length(FSaveSpeedSamples) - 2 do
          Sum := Sum + FSaveSpeedSamples[I];
        LoadSpeedAvg := Sum / (Length(FSaveSpeedSamples) - 1);
      end
      else if Length(FSaveSpeedSamples) = 1 then
        LoadSpeedAvg := FSaveSpeedSamples[0];

      FFileName := TargetFileName;
      Caption := 'Turbobird - Data Editor (' +
        ExtractFileName(FFileName) + ')';

      ClearDataSetChanged;

      if AskForFileName then
        ShowMessage(Format(
          'Saved: %s' + sLineBreak +
          'Rows:  %s' + sLineBreak +
          'Time:  %.3f s' + sLineBreak +
          'Speed: %s rows/sec',
          [TargetFileName,
           FormatFloat('#,##0', CSVFastDataset1.RecordCount),
           SaveElapsed,
           FormatFloat('#,##0', LoadSpeedAvg)]));
    end;
  end
  else if DataSource1.DataSet = JSONFastDataSet1 then
  begin
    SaveJSONToFile(JSONFastDataSet1, TargetFileName);
    FFileName := TargetFileName;
    Caption := 'Turbobird - Data Editor (' +
      ExtractFileName(FFileName) + ')';
    ClearDataSetChanged;
  end
  else if DataSource1.DataSet = sWorksheetDataset1 then
  begin
    sWorksheetDataset1.Flush;
    ClearDataSetChanged;
  end
  else if DataSource1.DataSet = Dbf1 then
  begin
    Dbf1.FilePathFull := TargetFileName;
    Dbf1.Close;
    Dbf1.Open;
    FFileName := TargetFileName;
    ClearDataSetChanged;
  end
  else if DataSource1.DataSet = SdfDataSet1 then
  begin
    SdfDataSet1.SaveFileAs(TargetFileName);
    FFileName := TargetFileName;
    ClearDataSetChanged;
  end
  else if DataSource1.DataSet = FixedFormatDataSet1 then
  begin
    FixedFormatDataSet1.SaveFileAs(TargetFileName);
    FFileName := TargetFileName;
    ClearDataSetChanged;
  end
  else
  begin
    P := btnExportAs.ClientToScreen(Point(0, btnExportAs.Height));
    pmGrid.PopUp(P.X, P.Y);
  end;
end;

{ ==================================================================
  SelectDataSetByFileName
  ================================================================== }

function TfrmDataEditor.SelectDataSetByFileName(const AFileName: string): Boolean;
var
  Ext, DetLE: string;
  DetDel, DetQC, DetDec: Char;
  Choice: TDetectChoice;
begin
  Result := True;
  Ext := LowerCase(ExtractFileExt(AFileName));

  ResetAllDatasets;

  if (Ext = '.csv') then
  begin
    DataSource1.DataSet := CSVFastDataset1;

    // ---- Delimiter / Quote / LineEnding detection ----
    if CheckBoxAutoDedectLineEnding.Checked then
      DetLE := DetectLineEnding(AFileName)
    else
      DetLE := CSVLineEnding;

    if CheckBoxAutoDedectDelimiter.Checked then
      DetDel := DetectDelimiter(AFileName)
    else
      DetDel := CSVDelimiter;

    if CheckBoxAutoDedectQuoteChar.Checked then
      DetQC := DetectQuoteChar(AFileName, DetDel)
    else
      DetQC := CSVQuoteChar;

    // ---- DecimalSeparator detection (braucht DetDel + DetQC) ----
    if CheckBoxAutoDedecDecimalSeparator.Checked then
      DetDec := DetectDecimalSeparator(AFileName, DetDel, DetQC)
    else
      DetDec := CSVDecimalSeparator;

    Choice := AskUseDetected(AFileName,
                             CSVLineEnding, DetLE,
                             CSVDelimiter,  DetDel,
                             CSVQuoteChar,  DetQC,
                             CSVDecimalSeparator, DetDec);

    if Choice = dcCancel then
    begin
      Result := False;
      Exit;
    end;

    if Choice = dcUseDetected then
    begin
      if CheckBoxAutoDedectLineEnding.Checked then CSVLineEnding := DetLE;
      if CheckBoxAutoDedectDelimiter.Checked  then CSVDelimiter  := DetDel;
      if CheckBoxAutoDedectQuoteChar.Checked  then CSVQuoteChar  := DetQC;
      if CheckBoxAutoDedecDecimalSeparator.Checked then
        CSVDecimalSeparator := DetDec;
    end;

    SyncLineEndingCombo;
    SyncDelimiterCombo;
    SyncQuoteCharCombo;
    SyncDecimalSeparatorCombo;
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

{ ==================================================================
  Load (synchron)
  ================================================================== }

procedure TfrmDataEditor.LoadSelectedDataset(const AFileName: string;
  out AError: string);
begin
  AError := '';
  try
    if DataSource1.DataSet = CSVFastDataset1 then
      CSVFastDataset1.LoadFromCSVFile(AFileName)

    else if DataSource1.DataSet = JSONFastDataSet1 then
      JSONFastDataSet1.LoadFromFile(AFileName)

    else if DataSource1.DataSet = sWorksheetDataset1 then
    begin
      sWorksheetDataset1.FileName := AFileName;
      sWorksheetDataset1.SheetName :=
        ChangeFileExt(ExtractFileName(AFileName), '');
      sWorksheetDataset1.Open;
    end

    else if DataSource1.DataSet = Dbf1 then
    begin
      Dbf1.FilePathFull := AFileName;
      Dbf1.Open;
    end

    else if DataSource1.DataSet = SdfDataSet1 then
    begin
      SdfDataSet1.FileName := AFileName;
      SdfDataSet1.Open;
    end

    else if DataSource1.DataSet = FixedFormatDataSet1 then
    begin
      FixedFormatDataSet1.FileName := AFileName;
      FixedFormatDataSet1.Open;
    end;
  except
    on E: EAbort do
      raise;
    on E: Exception do
    begin
      AError := E.ClassName + ': ' + E.Message;
      ResetAllDatasets;
    end;
  end;
end;

{ ==================================================================
  btnOpenFileClick
  ================================================================== }

procedure TfrmDataEditor.btnOpenFileClick(Sender: TObject);
var
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
  FromRow, ToRow, ScanRows, TotalRowsInRange: Int64;
  Cancelled: Boolean;
  LoadSpeedAvg, LoadBytesPerSecAvg, Sum: Double;
  I: Integer;

  Check: TCSVConfigCheck;
  Choice: TDetectChoice;
  BlockSizeOriginalBytes, BufferSizeOriginalBytes: Int64;
  BlockAutoAdjusted, BufferAutoAdjusted: Boolean;

  Gen: TGenSQLFromCSVDataset;
  BM: TBookmark;
  MarginInfo: string;
begin
  if FIsLoading then Exit;
  FIsLoading := True;
  btnOpenFile.Enabled := False;
  try

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
    MarginInfo              := '';

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
    SetSaveSettingsFromForm;
    SetCSVSettingsFromForm;

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

    if FileSize > 0 then
    begin
      FreeRAMBytes := GetRAMFreeMB * 1024 * 1024;
      if FileSize > (FreeRAMBytes * 8 div 10) then
      begin
        MsgText := Format(
          'Warning: Large file!' + sLineBreak + sLineBreak +
          'File size:      %s' + sLineBreak +
          'Free RAM:       %s' + sLineBreak + sLineBreak +
          'The file may not fit in memory. Loading could cause' + sLineBreak +
          'system slowdown or memory errors.' + sLineBreak + sLineBreak +
          'Continue anyway?',
          [FormatBytesSmart(FileSize), FormatBytesSmart(FreeRAMBytes)]);

        if MessageDlg(MsgText, mtWarning, [mbYes, mbNo], 0) = mrNo then
          Exit;
      end;
    end;

    BuildProgressForm;
    FProgressForm.Show;
    frmThemeSelector.btnApplyClick(FProgressForm);

    FProgInfoLbl.Caption := Format('File: %s   (%s)',
      [ExtractFileName(FFileName), FormatBytesSmart(FileSize)]);

    FProgTotalRAMMB      := GetRAMTotalMB;
    FInitialFreeRAMMB    := GetRAMFreeMB;
    FCurrentFreeRAMMB    := FInitialFreeRAMMB;
    FPeakUsedRAMMB       := 0;
    FPeakEstimatedMB     := 0;
    FLoadMaxRowWidth     := 0;
    FRAMWarningShown     := False;
    FCancelRequested     := False;

    FInitialSwapTotalMB  := GetSwapTotalMB;
    FInitialSwapUsedMB   := GetSwapUsedMB;
    FCurrentSwapUsedMB   := GetSwapUsedMB;
    FLastSwapUsedMB      := FCurrentSwapUsedMB;
    FPeakSwapUsedMB      := 0;
    FThrashingDetected   := False;

    FScanStartTick       := 0;
    FLastProgressBytes   := 0;
    SetLength(FProgressSamples, 0);
    SetLength(FProgressBytesSamples, 0);
    ResetAutoTuneProgressNote;

    try
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
            FFileName, FromRow, ToRow, ScanRows,
            CSVQuoteChar, TotalRowsInRange,
            @HandleScanProgress, @FCancelRequested);
          ScanElapsed := (Now - ScanStartTime) * SecsPerDay;
        except
          on E: EAbort do
          begin
            Cancelled := True;
            FProgPhaseLbl.Caption := 'Cancelled by user (scan)';
            Application.ProcessMessages;
            Sleep(30);
            Exit;
          end;
        end;

        if MaxLineWidth > 0 then
        begin
          Check := CheckCSVConfiguration(
            MaxLineWidth, CSVBlockSizeBytes, CSVBufferSizeBytes);

          case Check.Status of
            ccsOptimal: ;
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
                  dcKeepCurrent: ;
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

      FProgPhaseLbl.Caption := 'Loading rows...';
      FProgBar.Position := 0;

      if ScanRows > 0 then
        FProgTotalRows := ToRow - FromRow + 1
      else if TotalRowsInRange > 0 then
        FProgTotalRows := TotalRowsInRange
      else
        FProgTotalRows := 0;

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

      SetLength(FProgressSamples, 0);
      SetLength(FProgressBytesSamples, 0);
      FLastProgressRows    := 0;
      FLastProgressBytes   := 0;
      FLastProgressSeconds := 0;

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

      LoadErr := '';
      try
        FProgressForm.BringToFront;
        Application.ProcessMessages;
        Sleep(30);

        LoadSelectedDataset(FFileName, LoadErr);
      except
        on E: EAbort do
        begin
          Cancelled := True;
          LoadErr := 'Cancelled by user';
          ResetAllDatasets;
        end;
      end;

      TCSVFastDataset(CSVFastDataset1).OnProgress := nil;
      TCSVFastDataset(CSVFastDataset1).CancelFlag := nil;

      if TCSVFastDataset(CSVFastDataset1).Aborted then
      begin
        Cancelled := True;
        if LoadErr = '' then LoadErr := 'Cancelled by user';
      end;

      if Cancelled or (LoadErr = 'Cancelled by user') then
      begin
        FProgPhaseLbl.Caption := 'Cancelled by user';
        Application.ProcessMessages;

        try
          RxDBGrid1.DataSource    := nil;
          DBNavigator1.DataSource := nil;
          DataSource1.DataSet     := nil;

          if CSVFastDataset1.Active then CSVFastDataset1.Close;
          CSVFastDataset1.Fields.Clear;
          CSVFastDataset1.FieldDefs.Clear;
        except
        end;

        try RxDBGrid1.Columns.Clear; except end;

        Caption := 'Turbobird - Data Editor (' +
          ExtractFileName(FFileName) + ')';
        btnCreateSQL.Enabled := False;
        btnExportAs.Enabled  := False;

        Sleep(500);
        Exit;
      end;

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

      FFileName := OpenDialog1.FileName;
      Caption := 'Turbobird - Data Editor (' +
        ExtractFileName(FFileName) + ')';

    finally
      if Assigned(FProgressForm) then
        FreeAndNil(FProgressForm);
    end;

  finally
    FIsLoading := False;
    btnOpenFile.Enabled := True;
  end;

  Caption := 'Turbobird - Data Editor (' + ExtractFileName(FFileName) + ')';
  btnCreateSQL.Enabled := True;
  btnExportAs.Enabled  := True;

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

  FillChar(Stats, SizeOf(Stats), 0);
  Stats.Kind             := tkLoadFile;
  Stats.SourceKind       := 'File';
  Stats.SourceFileName   := FFileName;
  Stats.SourceFileSize   := FileSize;
  Stats.SourceFileFormat := UpperCase(ExtractFileExt(FFileName));
  if (Stats.SourceFileFormat <> '') and
     (Stats.SourceFileFormat[1] = '.') then
    Delete(Stats.SourceFileFormat, 1, 1);
  Stats.OptionsExtra   := 'Read-Only';
  Stats.RowsProcessed  := RowCount;
  Stats.FieldsCount    := DataSource1.DataSet.FieldCount;

  Stats.FieldNames := '';
  Stats.FieldTypes := '';
  MarginInfo       := '';

  if DataSource1.DataSet = CSVFastDataset1 then
  begin
    BM := DataSource1.DataSet.GetBookmark;
    try
      try
        Gen := TGenSQLFromCSVDataset.Create(DataSource1.DataSet, '', 50, 6);
        try
          for I := 0 to High(Gen.Fields) do
          begin
            if Stats.FieldNames <> '' then
            begin
              Stats.FieldNames := Stats.FieldNames + ', ';
              Stats.FieldTypes := Stats.FieldTypes + ', ';
            end;
            Stats.FieldNames := Stats.FieldNames + Gen.Fields[I].FieldName;
            Stats.FieldTypes := Stats.FieldTypes + Gen.Fields[I].FieldType;
          end;

          if Gen.AnyMarginApplied then
            MarginInfo := Format(
              'Sampled: %d of %s rows — types upsized',
              [Gen.TotalRowsAnalyzed,
               FormatFloat('#,##0', Gen.TotalRows)]);
        finally
          Gen.Free;
        end;
      except
        Stats.FieldNames := '';
        Stats.FieldTypes := '';
        for I := 0 to DataSource1.DataSet.FieldCount - 1 do
        begin
          if Stats.FieldNames <> '' then
          begin
            Stats.FieldNames := Stats.FieldNames + ', ';
            Stats.FieldTypes := Stats.FieldTypes + ', ';
          end;
          Stats.FieldNames := Stats.FieldNames +
            DataSource1.DataSet.Fields[I].FieldName;
          Stats.FieldTypes := Stats.FieldTypes +
            FieldTypeToString(DataSource1.DataSet.Fields[I].DataType);
        end;
      end;
    finally
      if Assigned(BM) then
      begin
        DataSource1.DataSet.GotoBookmark(BM);
        DataSource1.DataSet.FreeBookmark(BM);
      end;
    end;
  end
  else
  begin
    for I := 0 to DataSource1.DataSet.FieldCount - 1 do
    begin
      if Stats.FieldNames <> '' then
      begin
        Stats.FieldNames := Stats.FieldNames + ', ';
        Stats.FieldTypes := Stats.FieldTypes + ', ';
      end;
      Stats.FieldNames := Stats.FieldNames +
        DataSource1.DataSet.Fields[I].FieldName;
      Stats.FieldTypes := Stats.FieldTypes +
        FieldTypeToString(DataSource1.DataSet.Fields[I].DataType);
    end;
  end;

  if MarginInfo <> '' then
    Stats.OptionsExtra := Stats.OptionsExtra + sLineBreak + '  ' + MarginInfo;

  Stats.OptionsExtra := Stats.OptionsExtra + sLineBreak +
    '  Field Types: Preview (Firebird 6.0). ' +
    'Actual types are generated at Create Table time based on target FB version.';

  Stats.ElapsedSeconds     := ScanElapsed + FLastProgressSeconds;
  Stats.LoadElapsedSeconds := FLastProgressSeconds;
  Stats.RowsPerSecond      := LoadSpeedAvg;
  Stats.BytesPerSecond     := LoadBytesPerSecAvg;

  Stats.PeakRAMUsageMB  := FPeakUsedRAMMB;
  Stats.PeakEstimatedMB := FPeakEstimatedMB;

  Stats.PeakSwapUsedMB    := FPeakSwapUsedMB;
  Stats.SwapTotalMB       := FInitialSwapTotalMB;
  Stats.SwapStartUsedMB   := FInitialSwapUsedMB;
  Stats.ThrashingDetected := FThrashingDetected;

  Stats.SystemInfo := GetSystemInfo(FFileName);

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
        '    Quote Char: ' +
        QuotedStr(CSVFastDataset1.CSVOptions.QuoteChar) + sLineBreak;

    Stats.CSVOptions := Stats.CSVOptions +
      '    Line Ending: ' +
        CSVLineEndingStrToName(CSVFastDataset1.CSVOptions.LineEnding) + sLineBreak +
      '    Decimal Separator: ' +
        QuotedStr(CSVDecimalSeparator) + sLineBreak +
      '    First Line As Field Names: ' +
        BoolToStr(CSVFastDataset1.CSVOptions.FirstLineAsFieldNames, 'Yes', 'No') + sLineBreak +
      '    Ignore Outer Whitespace: ' +
        BoolToStr(CSVFastDataset1.CSVOptions.IgnoreOuterWhitespace, 'Yes', 'No') + sLineBreak +
      '    Quote Outer Whitespace: ' +
        BoolToStr(CSVFastDataset1.CSVOptions.QuoteOuterWhitespace, 'Yes', 'No');
  end;

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

  ReportForm := TfrmReport.Create(nil);
  try
    ReportForm.SetReportText(FormatTransferReport(Stats));
    ReportForm.ShowModal;
  finally
    ReportForm.Free;
  end;
end;

{ ==================================================================
  SQL / Table
  ================================================================== }

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

  if FFileName <> '' then
    TempTableName := UpperCase(ChangeFileExt(ExtractFileName(FFileName), ''))
  else
  begin
    TempTableName := StringReplace(Caption, 'Turbobird - Data Editor (', '', []);
    TempTableName := StringReplace(TempTableName, ')', '', []);
  end;

  GenSQLFromCSVDataset := nil;
  try
    // Preview: FB 6.0 (modernste realistische Version)
    // Beim echten Ausführen wird die Ziel-Version erkannt
    GenSQLFromCSVDataset := TGenSQLFromCSVDataset.Create(
      DataSource1.DataSet, TempTableName, TempFieldLength, 6);

    SynEditSQL.Lines.Text :=
      '-- Preview (Firebird 6.0). Actual types are detected when ' +
      'you click "Create Table".' + sLineBreak +
      GenSQLFromCSVDataset.SQL;
    PageControl1.ActivePage := tsSQL;
    btnCopySQL.Enabled := True;
  except
    on E: Exception do
      MessageDlg('Error', 'Error generating SQL:'#13#10 + E.Message,
        mtError, [mbOK], 0);
  end;

  FreeAndNil(GenSQLFromCSVDataset);
end;

procedure TfrmDataEditor.btnCreateTableClick(Sender: TObject);
var
  frmCreateTable: TfrmCreateTableFromDataSet;
  TableName: string;
begin
  // Guard: keine registrierten Datenbanken
  if Length(RegisteredDatabases) = 0 then
  begin
    MessageDlg(
      'No database registered.',
      'Please register a server and a database first.' + sLineBreak +
      sLineBreak +
      'Right-click on a server node in the main tree view ' +
      'to open the Database Registry.',
      mtWarning, [mbOK], 0);
    Exit;
  end;

  frmCreateTable := TfrmCreateTableFromDataSet.Create(Self);
  try
    if FFileName <> '' then
      frmCreateTable.Init(DataSource1.DataSet, FFileName)
    else
    begin
      TableName := StringReplace(Caption, 'Turbobird - Data Editor (', '', []);
      TableName := StringReplace(TableName, ')', '', []);
      frmCreateTable.Init(DataSource1.DataSet, TableName);
    end;

    frmCreateTable.ShowModal;
  finally
    frmCreateTable.Free;
  end;
end;

{ ==================================================================
  Navigator / Form-Callbacks
  ================================================================== }

procedure TfrmDataEditor.DBNavigator1Click(Sender: TObject;
  Button: TDBNavButtonType);
begin
  if Button = nbRefresh then
  begin
    if FFileName <> '' then
      LoadFile(FFileName);
  end;
end;

procedure TfrmDataEditor.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if FIsLoading then
  begin
    FCancelRequested := True;
    CloseAction := caNone;
    Exit;
  end;

  if Assigned(CSVFastDataset1) and CSVFastDataset1.Active then
  begin
    try
      if CSVFastDataset1.Modified then
        SaveFile(FFileName, False);
    except
      on E: Exception do
      begin
        if MessageDlg(
          'Could not save changes:' + sLineBreak + E.Message + sLineBreak +
          'Close anyway?',
          mtWarning, [mbYes, mbNo], 0) = mrNo then
        begin
          CloseAction := caNone;
          Exit;
        end;
      end;
    end;
  end;

  WriteIni;
  FreeSaveProgressForm;
  CloseAction := caFree;
end;

procedure TfrmDataEditor.FormCreate(Sender: TObject);
begin
  FDataSetChanged := False;
  FIsLoading      := False;

  chkBoxAutoTuneCSVBuffer.Checked := CSVAutoTuneCSVBuffer;

  // ---- DecimalSeparator-Combo verdrahten (OnChange) ----
  if Assigned(ComboBox1) then
    ComboBox1.OnChange := @cmbboxDecimalSeparatorChange;

  // ---- Change-Tracking für ALLE Datasets ----
  CSVFastDataset1.AfterPost      := @GenericDatasetModified;
  CSVFastDataset1.AfterDelete    := @GenericDatasetModified;

  JSONFastDataSet1.AfterPost     := @GenericDatasetModified;
  JSONFastDataSet1.AfterDelete   := @GenericDatasetModified;

  sWorksheetDataset1.AfterPost   := @GenericDatasetModified;
  sWorksheetDataset1.AfterDelete := @GenericDatasetModified;

  Dbf1.AfterPost                 := @GenericDatasetModified;
  Dbf1.AfterDelete               := @GenericDatasetModified;

  SdfDataSet1.AfterPost          := @GenericDatasetModified;
  SdfDataSet1.AfterDelete        := @GenericDatasetModified;

  FixedFormatDataSet1.AfterPost   := @GenericDatasetModified;
  FixedFormatDataSet1.AfterDelete := @GenericDatasetModified;

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
  frmThemeSelector.btnApplyClick(Self);

  SynEditSQL.Color      := QWEditorBackgroundColor;
  SynEditSQL.Font.Name  := QWEditorFontName;
  SynEditSQL.Font.Size  := QWEditorFontSize;
  SynEditSQL.Font.Color := QWEditorFontColor;
end;

{ ==================================================================
  Buttons
  ================================================================== }

procedure TfrmDataEditor.btnExportAsClick(Sender: TObject);
var P: TPoint;
begin
  P := btnExportAs.ClientToScreen(Point(0, btnExportAs.Height));
  pmGrid.PopUp(P.X, P.Y);
end;

procedure TfrmDataEditor.btnSaveFileClick(Sender: TObject);
begin
  SaveFile(FFileName, True);
end;

procedure TfrmDataEditor.Button1Click(Sender: TObject);
begin
  ShowMessage(IntToStr(CSVFastDataset1.RecordCount));
end;

procedure TfrmDataEditor.chkBoxAutoTuneCSVBufferChange(Sender: TObject);
begin
  CSVAutoTuneCSVBuffer := chkBoxAutoTuneCSVBuffer.Checked;
end;

{ ==================================================================
  Combo-Change-Handler
  ================================================================== }

procedure TfrmDataEditor.cmbboxDelimiterChange(Sender: TObject);
begin
  case cmbboxDelimiter.ItemIndex of
    0: CSVDelimiter := ',';
    1: CSVDelimiter := ';';
    2: CSVDelimiter := #9;
    3: CSVDelimiter := '|';
    4: CSVDelimiter := ' ';
  else
    CSVDelimiter := ',';
  end;
  SetCSVSettingsFromForm;
end;

procedure TfrmDataEditor.cmbboxLineEndingChange(Sender: TObject);
begin
  case cmbboxLineEnding.ItemIndex of
    0: CSVLineEnding := #13#10;
    1: CSVLineEnding := #10;
    2: CSVLineEnding := #13;
    3: CSVLineEnding := LineEnding;
  else
    CSVLineEnding := #10;
  end;
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
            ShowMessage('Please enter exactly one character ' +
                        '(or leave empty for none).');
            SyncQuoteCharCombo;
            Exit;
          end;
        end
        else
        begin
          SyncQuoteCharCombo;
          Exit;
        end;
      end;
  else
    CSVQuoteChar := '"';
  end;

  SetCSVSettingsFromForm;
end;

procedure TfrmDataEditor.cmbboxDecimalSeparatorChange(Sender: TObject);
begin
  if csDestroying in ComponentState then Exit;
  if Trim(ComboBox1.Text) = ',' then
    CSVDecimalSeparator := ','
  else
    CSVDecimalSeparator := '.';
  SetCSVSettingsFromForm;
end;

{ ==================================================================
  Change-Tracking
  ================================================================== }

procedure TfrmDataEditor.CSVFastDataset1AfterDelete(DataSet: TDataSet);
begin
  MarkDataSetChanged;
end;

procedure TfrmDataEditor.CSVFastDataset1AfterPost(DataSet: TDataSet);
begin
  MarkDataSetChanged;
end;

procedure TfrmDataEditor.GenericDatasetModified(DataSet: TDataSet);
begin
  MarkDataSetChanged;
end;

procedure TfrmDataEditor.UpdateSaveButton;
begin
  if Assigned(btnSaveFile) then
    btnSaveFile.Enabled := FDataSetChanged;
end;

procedure TfrmDataEditor.MarkDataSetChanged;
begin
  FDataSetChanged := True;
  UpdateSaveButton;
end;

procedure TfrmDataEditor.ClearDataSetChanged;
begin
  FDataSetChanged := False;
  UpdateSaveButton;
end;

{ ==================================================================
  Export
  ================================================================== }

procedure TfrmDataEditor.lmStdExportFormatsClick(Sender: TObject);
begin
  if DataSource1.DataSet.IsEmpty then
  begin
    ShowMessage('DataSet has no records!');
    Exit;
  end;
  ExportStdFormat(DataSource1.DataSet);
end;

procedure TfrmDataEditor.lmExportDataAsHtmlClick(Sender: TObject);
begin
  if DataSource1.DataSet.IsEmpty then
  begin
    ShowMessage('DataSet has no records!');
    Exit;
  end;
  ExportDataHtml(DataSource1.DataSet);
end;

procedure TfrmDataEditor.lmExportDataAsMarkDownTableClick(Sender: TObject);
begin
  if DataSource1.DataSet.IsEmpty then
  begin
    ShowMessage('DataSet has no records!');
    Exit;
  end;
  ExportDataMarkDownTable(DataSource1.DataSet);
end;

procedure TfrmDataEditor.lmExportDataAsPDFClick(Sender: TObject);
begin
  RxDBGridExportPDF1.Execute;
end;

procedure TfrmDataEditor.lmExportDataAsSpreadSheetClick(Sender: TObject);
begin
  RxDBGridExportSpreadSheet1.Execute;
end;

procedure TfrmDataEditor.lmPrintDataClick(Sender: TObject);
begin
  RxDBGridPrint1.Execute;
end;

procedure TfrmDataEditor.lmExportToClipboardClick(Sender: TObject);
var
  MaxExportRows, RowCount, CopiedRows: Integer;
  MsgText: string;
begin
  if DataSource1.DataSet.IsEmpty then
  begin
    MessageDlg('DataSet has no records!', mtError, [mbOK], 0);
    Exit;
  end;

  MaxExportRows := fIniFile.ReadInteger('ClipboardExport', 'MaxExportRows', -1);
  if MaxExportRows <= 0 then
  begin
    if MaxExportRows = -1 then
    begin
      MaxExportRows := 200;
      fIniFile.WriteInteger('ClipboardExport', 'MaxExportRows', MaxExportRows);
      MessageDlg('No valid MaxExportRows entry found in turbobird.ini. ' +
                 'Default 200 has been set.', mtWarning, [mbOK], 0);
    end
    else
      MessageDlg('MaxExportRows is set to 0 in turbobird.ini. ' +
                 'Exporting very large tables may cause program or system crash!',
                 mtWarning, [mbOK], 0);
  end;

  RowCount := DataSource1.DataSet.RecordCount;

  if (MaxExportRows > 0) and (RowCount > MaxExportRows) then
    MessageDlg(
      'The result contains ' + IntToStr(RowCount) +
      ' rows, which exceeds the export limit of ' +
      IntToStr(MaxExportRows) + ' rows.'#13#10 +
      'Only the first ' + IntToStr(MaxExportRows) +
      ' rows will be copied to the clipboard.'#13#10#13#10 +
      'You can change this limit in turbobird.ini under [ClipboardExport].',
      mtWarning, [mbOK], 0);

  if MaxExportRows = 0 then
    CopiedRows := RowCount
  else if RowCount > MaxExportRows then
    CopiedRows := MaxExportRows
  else
    CopiedRows := RowCount;

  try
    DataSource1.DataSet.DisableControls;
    ExportDataToClipboard(DataSource1.DataSet, MaxExportRows);

    MsgText := 'Successfully copied ' + IntToStr(CopiedRows) +
               ' records to the clipboard.';
    if MaxExportRows = 0 then
      MsgText := MsgText + ' (Warning: no export limit set in turbobird.ini! ' +
                 'Large tables may cause program/system crash.)';

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
    MessageDlg('Warning', 'Nothing to copy.', mtWarning, [mbOK], 0);
    Exit;
  end;

  try
    if SynEditSQL.SelText <> '' then
      Clipboard.AsText := SynEditSQL.SelText
    else
      Clipboard.AsText := SynEditSQL.Lines.Text;

    MessageDlg('Info', 'SQL copied to clipboard.',
      mtInformation, [mbOK], 0);
  except
    on E: Exception do
      MessageDlg('Error',
        'Failed to copy to clipboard:'#13#10 + E.Message,
        mtError, [mbOK], 0);
  end;
end;

procedure TfrmDataEditor.BitBtn1Click(Sender: TObject);
begin
  ShowMessage(
    'Class: ' + CSVFastDataset1.FieldByName('F_DOUBLE').ClassName + sLineBreak +
    'AsString: ' + CSVFastDataset1.FieldByName('F_DOUBLE').AsString + sLineBreak +
    'AsFloat: ' + FloatToStr(CSVFastDataset1.FieldByName('F_DOUBLE').AsFloat)
  );
end;

{ ==================================================================
  AskUseDetected
  ================================================================== }

function TfrmDataEditor.AskUseDetected(
  const AFileName: string;
  const ACurLE, ADetLE: string;
  const ACurDel, ADetDel: Char;
  const ACurQC, ADetQC: Char;
  const ACurDec, ADetDec: Char): TDetectChoice;
var
  Dlg: TForm;
  Memo: TMemo;
  LblTitle: TLabel;
  BtnUseDetected, BtnKeepCurrent, BtnCancelFile: TButton;
  Content: TStringList;
  HasDiff, ShowLE, ShowDel, ShowQC, ShowDec: Boolean;
  CurLEName, DetLEName: string;

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

  ShowLE  := CheckBoxAutoDedectLineEnding.Checked;
  ShowDel := CheckBoxAutoDedectDelimiter.Checked;
  ShowQC  := CheckBoxAutoDedectQuoteChar.Checked;
  ShowDec := CheckBoxAutoDedecDecimalSeparator.Checked;

  if not (ShowLE or ShowDel or ShowQC or ShowDec) then Exit;

  HasDiff :=
    (ShowLE  and (ACurLE  <> ADetLE))  or
    (ShowDel and (ACurDel <> ADetDel)) or
    (ShowQC  and (ACurQC  <> ADetQC))  or
    (ShowDec and (ACurDec <> ADetDec));

  if not HasDiff then Exit;

  CurLEName := CSVLineEndingStrToName(ACurLE);
  DetLEName := CSVLineEndingStrToName(ADetLE);

  Dlg := TForm.Create(nil);
  try
    Dlg.Caption     := 'CSV Auto-Detection';
    Dlg.Width       := 600;
    Dlg.Height      := 460;
    Dlg.Position    := poScreenCenter;
    Dlg.BorderStyle := bsDialog;
    Dlg.FormStyle   := fsStayOnTop;

    LblTitle := TLabel.Create(Dlg);
    LblTitle.Parent := Dlg;
    LblTitle.Left := 16;
    LblTitle.Top := 12;
    LblTitle.Width := 568;
    LblTitle.Caption :=
      'Auto-detection found different values than your current settings:';
    LblTitle.Font.Style := [fsBold];

    Content := TStringList.Create;
    try
      Content.Add('File: ' + ExtractFileName(AFileName));
      Content.Add('');
      Content.Add('                     Current       Detected');
      Content.Add('  ──────────────     ─────────     ─────────');

      if ShowLE then
      begin
        if ACurLE <> ADetLE then
          Content.Add(Format('  LineEnding         %-13s %s',
            [CurLEName, DetLEName]))
        else
          Content.Add(Format('  LineEnding         %-13s (same)',
            [CurLEName]));
      end
      else
        Content.Add(Format('  LineEnding         %-13s (auto-off)',
          [CurLEName]));

      if ShowDel then
      begin
        if ACurDel <> ADetDel then
          Content.Add(Format('  Delimiter          %-13s %s',
            [DelimToStr(ACurDel), DelimToStr(ADetDel)]))
        else
          Content.Add(Format('  Delimiter          %-13s (same)',
            [DelimToStr(ACurDel)]));
      end
      else
        Content.Add(Format('  Delimiter          %-13s (auto-off)',
          [DelimToStr(ACurDel)]));

      if ShowQC then
      begin
        if ACurQC <> ADetQC then
          Content.Add(Format('  QuoteChar          %-13s %s',
            [QCToStr(ACurQC), QCToStr(ADetQC)]))
        else
          Content.Add(Format('  QuoteChar          %-13s (same)',
            [QCToStr(ACurQC)]));
      end
      else
        Content.Add(Format('  QuoteChar          %-13s (auto-off)',
          [QCToStr(ACurQC)]));

      if ShowDec then
      begin
        if ACurDec <> ADetDec then
          Content.Add(Format('  Decimal Sep        %-13s %s',
            [QuotedStr(ACurDec), QuotedStr(ADetDec)]))
        else
          Content.Add(Format('  Decimal Sep        %-13s (same)',
            [QuotedStr(ACurDec)]));
      end
      else
        Content.Add(Format('  Decimal Sep        %-13s (auto-off)',
          [QuotedStr(ACurDec)]));

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
      Memo.Height := 310;
      Memo.ReadOnly := True;
      Memo.ScrollBars := ssVertical;
      Memo.Font.Name := 'Courier New';
      Memo.Font.Size := 10;
      Memo.Lines.Assign(Content);
    finally
      Content.Free;
    end;

    BtnUseDetected := TButton.Create(Dlg);
    BtnUseDetected.Parent := Dlg;
    BtnUseDetected.Caption := 'Use Detected';
    BtnUseDetected.Left := 192;
    BtnUseDetected.Top := 370;
    BtnUseDetected.Width := 120;
    BtnUseDetected.Default := True;
    BtnUseDetected.ModalResult := mrYes;

    BtnKeepCurrent := TButton.Create(Dlg);
    BtnKeepCurrent.Parent := Dlg;
    BtnKeepCurrent.Caption := 'Keep Current';
    BtnKeepCurrent.Left := 320;
    BtnKeepCurrent.Top := 370;
    BtnKeepCurrent.Width := 120;
    BtnKeepCurrent.ModalResult := mrNo;

    BtnCancelFile := TButton.Create(Dlg);
    BtnCancelFile.Parent := Dlg;
    BtnCancelFile.Caption := 'Cancel';
    BtnCancelFile.Left := 448;
    BtnCancelFile.Top := 370;
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

{ ==================================================================
  CheckCSVConfiguration
  ================================================================== }

function TfrmDataEditor.CheckCSVConfiguration(
  AMaxRowWidth: Int64;
  ACurrentBlockSize: Int64;
  ACurrentBufferSize: Int64): TCSVConfigCheck;
const
  MIN_PERFORMANCE_BLOCK = 1048576;
  MAX_BLOCK_HARD_LIMIT  = 16777216;
  BUFFER_SAFETY_MARGIN  = 16384;
var
  NeedBlock, NeedBuffer: Int64;
begin
  Result.Status            := ccsOptimal;
  Result.MaxRowWidth       := AMaxRowWidth;
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

function TfrmDataEditor.AskApplyCSVConfig(
  const ACheck: TCSVConfigCheck): TDetectChoice;
var
  Dlg: TForm;
  Memo: TMemo;
  LblTitle: TLabel;
  BtnApply, BtnKeep, BtnCancel: TButton;
  Content: TStringList;
  Ratio: Double;
begin
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
      LblTitle.Parent := Dlg;
      LblTitle.Left := 16;
      LblTitle.Top := 12;
      LblTitle.Width := 608;
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
        Memo.Parent := Dlg;
        Memo.Left := 16;
        Memo.Top := 38;
        Memo.Width := 608;
        Memo.Height := 220;
        Memo.ReadOnly := True;
        Memo.ScrollBars := ssVertical;
        Memo.Font.Name := 'Courier New';
        Memo.Font.Size := 10;
        Memo.Lines.Assign(Content);
      finally
        Content.Free;
      end;

      BtnApply := TButton.Create(Dlg);
      BtnApply.Parent := Dlg;
      BtnApply.Caption := 'Fix and Continue';
      BtnApply.Left := 340;
      BtnApply.Top := 270;
      BtnApply.Width := 140;
      BtnApply.Default := True;
      BtnApply.ModalResult := mrYes;

      BtnCancel := TButton.Create(Dlg);
      BtnCancel.Parent := Dlg;
      BtnCancel.Caption := 'Cancel';
      BtnCancel.Left := 490;
      BtnCancel.Top := 270;
      BtnCancel.Width := 134;
      BtnCancel.ModalResult := mrCancel;

      case Dlg.ShowModal of
        mrYes: Result := dcUseDetected;
      else
        Result := dcCancel;
      end;
    finally
      Dlg.Free;
    end;
    Exit;
  end;

  if chkBoxAutoTuneCSVBuffer.Checked then
  begin
    Result := dcUseDetected;
    Exit;
  end;

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
    LblTitle.Parent := Dlg;
    LblTitle.Left := 16;
    LblTitle.Top := 12;
    LblTitle.Width := 608;
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
      Memo.Parent := Dlg;
      Memo.Left := 16;
      Memo.Top := 38;
      Memo.Width := 608;
      Memo.Height := 260;
      Memo.ReadOnly := True;
      Memo.ScrollBars := ssVertical;
      Memo.Font.Name := 'Courier New';
      Memo.Font.Size := 10;
      Memo.Lines.Assign(Content);
    finally
      Content.Free;
    end;

    BtnApply := TButton.Create(Dlg);
    BtnApply.Parent := Dlg;
    BtnApply.Caption := 'Use Recommended';
    BtnApply.Left := 200;
    BtnApply.Top := 310;
    BtnApply.Width := 140;
    BtnApply.Default := True;
    BtnApply.ModalResult := mrYes;

    BtnKeep := TButton.Create(Dlg);
    BtnKeep.Parent := Dlg;
    BtnKeep.Caption := 'Keep Current';
    BtnKeep.Left := 345;
    BtnKeep.Top := 310;
    BtnKeep.Width := 140;
    BtnKeep.ModalResult := mrNo;

    BtnCancel := TButton.Create(Dlg);
    BtnCancel.Parent := Dlg;
    BtnCancel.Caption := 'Cancel';
    BtnCancel.Left := 490;
    BtnCancel.Top := 310;
    BtnCancel.Width := 134;
    BtnCancel.ModalResult := mrCancel;

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

procedure TfrmDataEditor.ShowProgressAutoTuneNote(const ACheck: TCSVConfigCheck);
begin
  FProgressAutoTuneNote := Format(
    'Auto-tuned: BlockSize %s → %s, BufferSize %s → %s',
    [FormatBytesSmart(ACheck.CurrentBlockSize),
     FormatBytesSmart(ACheck.OptimalBlockSize),
     FormatBytesSmart(ACheck.CurrentBufferSize),
     FormatBytesSmart(ACheck.OptimalBufferSize)]);

  if Assigned(FProgETALbl) then
    FProgETALbl.Caption := FProgressAutoTuneNote;
end;

procedure TfrmDataEditor.ResetAutoTuneProgressNote;
begin
  FProgressAutoTuneNote := '';
end;

{ ==================================================================
  Save-Progress
  ================================================================== }

procedure TfrmDataEditor.rbSaveRangeChange(Sender: TObject);
begin
  edtSaveFrom.Enabled := rbSaveRange.Checked;
  edtSaveTo.Enabled   := rbSaveRange.Checked;
end;

procedure TfrmDataEditor.BuildSaveProgressForm;
var
  PnlInfo, PnlProgress, PnlCancel: TPanel;
begin
  FSaveProgressForm := TForm.Create(nil);
  FSaveProgressForm.Caption     := 'Saving CSV...';
  FSaveProgressForm.Width       := 560;
  FSaveProgressForm.Height      := 340;
  FSaveProgressForm.Position    := poScreenCenter;
  FSaveProgressForm.BorderStyle := bsDialog;
  FSaveProgressForm.FormStyle   := fsNormal;

  PnlInfo := TPanel.Create(FSaveProgressForm);
  PnlInfo.Parent := FSaveProgressForm;
  PnlInfo.Align := alTop;
  PnlInfo.Height := 90;
  PnlInfo.BevelOuter := bvNone;

  FSavePhaseLbl := TLabel.Create(FSaveProgressForm);
  FSavePhaseLbl.Parent := PnlInfo;
  FSavePhaseLbl.Left := 16;
  FSavePhaseLbl.Top := 12;
  FSavePhaseLbl.Width := 520;
  FSavePhaseLbl.Caption := 'Preparing...';
  FSavePhaseLbl.Font.Size := 12;
  FSavePhaseLbl.Font.Style := [fsBold];

  FSaveInfoLbl := TLabel.Create(FSaveProgressForm);
  FSaveInfoLbl.Parent := PnlInfo;
  FSaveInfoLbl.Left := 16;
  FSaveInfoLbl.Top := 45;
  FSaveInfoLbl.Width := 520;

  PnlProgress := TPanel.Create(FSaveProgressForm);
  PnlProgress.Parent := FSaveProgressForm;
  PnlProgress.Align := alTop;
  PnlProgress.Height := 105;
  PnlProgress.BevelOuter := bvNone;

  FSaveBar := TProgressBar.Create(FSaveProgressForm);
  FSaveBar.Parent := PnlProgress;
  FSaveBar.Left := 16;
  FSaveBar.Top := 10;
  FSaveBar.Width := 520;
  FSaveBar.Height := 24;
  FSaveBar.Min := 0;
  FSaveBar.Max := 100;

  FSaveElapsedLbl := TLabel.Create(FSaveProgressForm);
  FSaveElapsedLbl.Parent := PnlProgress;
  FSaveElapsedLbl.Left := 16;
  FSaveElapsedLbl.Top := 45;
  FSaveElapsedLbl.Width := 250;

  FSaveSpeedLbl := TLabel.Create(FSaveProgressForm);
  FSaveSpeedLbl.Parent := PnlProgress;
  FSaveSpeedLbl.Left := 280;
  FSaveSpeedLbl.Top := 45;
  FSaveSpeedLbl.Width := 256;

  FSaveETALbl := TLabel.Create(FSaveProgressForm);
  FSaveETALbl.Parent := PnlProgress;
  FSaveETALbl.Left := 16;
  FSaveETALbl.Top := 68;
  FSaveETALbl.Width := 520;

  PnlCancel := TPanel.Create(FSaveProgressForm);
  PnlCancel.Parent := FSaveProgressForm;
  PnlCancel.Align := alClient;
  PnlCancel.BevelOuter := bvNone;

  FSaveCancelBtn := TButton.Create(FSaveProgressForm);
  FSaveCancelBtn.Parent := PnlCancel;
  FSaveCancelBtn.Caption := 'Cancel';
  FSaveCancelBtn.Width := 100;
  FSaveCancelBtn.Height := 30;
  FSaveCancelBtn.Left := (FSaveProgressForm.ClientWidth - 100) div 2;
  FSaveCancelBtn.Top := 15;
  FSaveCancelBtn.OnClick := @SaveCancelClick;
end;

procedure TfrmDataEditor.FreeSaveProgressForm;
begin
  if Assigned(FSaveProgressForm) then
    FreeAndNil(FSaveProgressForm);
end;

procedure TfrmDataEditor.HandleSaveProgress(ARowsWritten: Int64;
  ABytesWritten: Int64; AElapsedSeconds: Double);
var
  Percent: Integer;
  RowsPerSec, BytesPerSec, ETASeconds: Double;
  IntervalRows: Double;
begin
  if (FSaveLastSeconds > 0) and (AElapsedSeconds > FSaveLastSeconds) then
  begin
    IntervalRows := (ARowsWritten - FSaveLastRows) /
                    (AElapsedSeconds - FSaveLastSeconds);
    if IntervalRows > 0 then
    begin
      SetLength(FSaveSpeedSamples, Length(FSaveSpeedSamples) + 1);
      FSaveSpeedSamples[High(FSaveSpeedSamples)] := IntervalRows;
    end;
  end;
  FSaveLastRows    := ARowsWritten;
  FSaveLastSeconds := AElapsedSeconds;

  if FSaveTotalRows > 0 then
  begin
    Percent := (ARowsWritten * 100) div FSaveTotalRows;
    if Percent > 100 then Percent := 100;
  end
  else
    Percent := 0;

  FSaveBar.Position := Percent;
  FSaveInfoLbl.Caption := Format('Rows:   %s / %s   (%d%%)',
    [FormatFloat('#,##0', ARowsWritten),
     FormatFloat('#,##0', FSaveTotalRows), Percent]);

  if AElapsedSeconds > 0.5 then
  begin
    RowsPerSec  := ARowsWritten / AElapsedSeconds;
    BytesPerSec := ABytesWritten / AElapsedSeconds;

    FSaveSpeedLbl.Caption := Format('Speed:  %s rows/sec  |  %s/sec',
      [FormatFloat('#,##0', RowsPerSec),
       FormatBytesSmart(Round(BytesPerSec))]);

    if (FSaveTotalRows > ARowsWritten) and (RowsPerSec > 0) then
    begin
      ETASeconds := (FSaveTotalRows - ARowsWritten) / RowsPerSec;
      FSaveETALbl.Caption := Format('ETA:    %s',
        [FormatDateTime('hh:nn:ss', ETASeconds / SecsPerDay)]);
    end
    else
      FSaveETALbl.Caption := '';
  end
  else
  begin
    FSaveSpeedLbl.Caption := '';
    FSaveETALbl.Caption   := '';
  end;

  FSaveElapsedLbl.Caption := Format('Elapsed: %s  |  Written: %s',
    [FormatDateTime('hh:nn:ss', AElapsedSeconds / SecsPerDay),
     FormatBytesSmart(ABytesWritten)]);

  Application.ProcessMessages;
end;

procedure TfrmDataEditor.SaveCancelClick(Sender: TObject);
begin
  FSaveCancelRequested := True;
  FSaveCancelBtn.Enabled := False;
  FSaveCancelBtn.Caption := 'Cancelling...';
end;

end.
