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
  uCSVFastDataset,
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
    Button1: TButton;
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
    JSONDataSet1: TJSONDataSet;
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

    FLastFreeRAMMB: Int64;

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
    procedure HandleLoadProgress(ARowsRead: Int64; AElapsedSeconds: Double; AFreeRAMMB: Int64);
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
begin
  FProgressForm := TForm.Create(nil);
  FProgressForm.Caption     := 'Loading...';
  FProgressForm.Width       := 560;
  FProgressForm.Height      := 440;
  FProgressForm.Position    := poScreenCenter;
  FProgressForm.BorderStyle := bsDialog;
  FProgressForm.FormStyle   := fsStayOnTop;

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
  PnlRAM.Height := 105;
  PnlRAM.BevelOuter := bvNone;

  // Trennlinie
  with TLabel.Create(FProgressForm) do
  begin
    Parent := PnlRAM;
    Left := 16;
    Top := 0;
    Width := 520;
    Caption := '──────────────────────────────────────────';
    Font.Color := clGray;
  end;

  FProgRAMInfoLbl := TLabel.Create(FProgressForm);
  FProgRAMInfoLbl.Parent := PnlRAM;
  FProgRAMInfoLbl.Left := 16;
  FProgRAMInfoLbl.Top := 20;
  FProgRAMInfoLbl.Width := 520;
  FProgRAMInfoLbl.Caption := 'RAM:      ...';

  FProgRAMBar := TProgressBar.Create(FProgressForm);
  FProgRAMBar.Parent := PnlRAM;
  FProgRAMBar.Left := 16;
  FProgRAMBar.Top := 45;
  FProgRAMBar.Width := 520;
  FProgRAMBar.Height := 20;
  FProgRAMBar.Min := 0;
  FProgRAMBar.Max := 100;
  FProgRAMBar.Position := 0;

  FProgRAMStatusLbl := TLabel.Create(FProgressForm);
  FProgRAMStatusLbl.Parent := PnlRAM;
  FProgRAMStatusLbl.Left := 16;
  FProgRAMStatusLbl.Top := 70;
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
begin
  if ATotalBytes <= 0 then Exit;
  Percent := (ABytesRead * 100) div ATotalBytes;
  if Percent > 100 then Percent := 100;

  FProgBar.Position := Percent;
  FProgElapsedLbl.Caption := Format('Scan:   %s / %s',
    [FormatBytesSmart(ABytesRead), FormatBytesSmart(ATotalBytes)]);
  Application.ProcessMessages;
end;

procedure TfrmDataEditor.HandleLoadProgress(ARowsRead: Int64;
  AElapsedSeconds: Double; AFreeRAMMB: Int64);
var
  Percent: Integer;
  RowsPerSec, ETASeconds: Double;
  UsedRAMMB: Int64;
  UsedPercent: Integer;
begin
  // ============================================================
  // Rows-Progress (wie gehabt)
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

  if AElapsedSeconds > 0.5 then
  begin
    RowsPerSec := ARowsRead / AElapsedSeconds;
    FProgSpeedLbl.Caption := Format('Speed:  %s rows/sec',
      [FormatFloat('#,##0', RowsPerSec)]);

    if (FProgTotalRows > ARowsRead) and (RowsPerSec > 0) then
    begin
      ETASeconds := (FProgTotalRows - ARowsRead) / RowsPerSec;
      FProgETALbl.Caption := Format('ETA:    %s',
        [FormatDateTime('hh:nn:ss', ETASeconds / SecsPerDay)]);
    end;
  end;

  FProgElapsedLbl.Caption := Format('Elapsed: %s',
    [FormatDateTime('hh:nn:ss', AElapsedSeconds / SecsPerDay)]);

  // ============================================================
  // RAM-Update (nur wenn AFreeRAMMB >= 0, sonst letzten Wert nutzen)
  // ============================================================
  if AFreeRAMMB >= 0 then
    FLastFreeRAMMB := AFreeRAMMB;

  if FLastFreeRAMMB > 0 then
  begin
    // RAM-Bar: Verbrauch in % von Total
    UsedRAMMB := FProgTotalRAMMB - FLastFreeRAMMB;
    if UsedRAMMB < 0 then UsedRAMMB := 0;
    if FProgTotalRAMMB > 0 then
      UsedPercent := (UsedRAMMB * 100) div FProgTotalRAMMB
    else
      UsedPercent := 0;
    if UsedPercent > 100 then UsedPercent := 100;

    FProgRAMBar.Position := UsedPercent;
    FProgRAMInfoLbl.Caption := Format('RAM:      %s used / %s total   (%d%%)',
      [FormatBytesSmart(UsedRAMMB * 1024 * 1024),
       FormatBytesSmart(FProgTotalRAMMB * 1024 * 1024),
       UsedPercent]);

    // Status + Farbe
    if UsedPercent >= 85 then
    begin
      FProgRAMStatusLbl.Caption := 'Status:   CRITICAL - low memory!';
      FProgRAMStatusLbl.Font.Color := clRed;
      FProgRAMStatusLbl.Font.Style := [fsBold];
    end
    else if UsedPercent >= 70 then
    begin
      FProgRAMStatusLbl.Caption := 'Status:   WARNING - memory getting tight';
      FProgRAMStatusLbl.Font.Color := clMaroon;
      FProgRAMStatusLbl.Font.Style := [fsBold];
    end
    else
    begin
      FProgRAMStatusLbl.Caption := 'Status:   OK';
      FProgRAMStatusLbl.Font.Color := clGreen;
      FProgRAMStatusLbl.Font.Style := [];
    end;

    // ============================================================
    // Warnung bei kritischem RAM (nur einmal)
    // ============================================================
    if (UsedPercent >= 85) and (not FRAMWarningShown) then
    begin
      FRAMWarningShown := True;

      if MessageDlg(
        'Warning: Low memory!' + sLineBreak + sLineBreak +
        'Used RAM:  ' + FormatBytesSmart(UsedRAMMB * 1024 * 1024) + sLineBreak +
        'Free RAM:  ' + FormatBytesSmart(FLastFreeRAMMB * 1024 * 1024) + sLineBreak +
        sLineBreak +
        'The system may become unresponsive or kill the application.' + sLineBreak +
        sLineBreak +
        'Cancel the load?',
        mtWarning, [mbYes, mbNo], 0) = mrYes then
      begin
        if Assigned(CSVFastDataset1) then
          TCSVFastDataset(CSVFastDataset1).CancelRequested := True;
        FProgCancelBtn.Enabled := False;
        FProgCancelBtn.Caption := 'Cancelling...';
      end;
    end;
  end;

  Application.ProcessMessages;
end;

procedure TfrmDataEditor.ProgCancelClick(Sender: TObject);
begin
  if Assigned(CSVFastDataset1) then
    TCSVFastDataset(CSVFastDataset1).CancelRequested := True;
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
  // CSVFastDataSet Performance-Parameter
  // ============================================================
  TCSVFastDataset(CSVFastDataset1).BlockSize :=
      StrToIntDef(edtBlockSize.Text, 1024);
  TCSVFastDataset(CSVFastDataset1).BufferSize :=
      StrToIntDef(edtBufferSize.Text, 2048);
  TCSVFastDataset(CSVFastDataset1).InitialCellCapacity :=
      StrToIntDef(edtInitialCellCapacity.Text, 64);

  // ============================================================
  // Tatsächliche (ggf. geklemmte) Werte zurück in GUI + Globals
  // ============================================================
  CSVBlockSizeKB         := TCSVFastDataset(CSVFastDataset1).BlockSize;
  CSVBufferSizeKB        := TCSVFastDataset(CSVFastDataset1).BufferSize;
  CSVInitialCellCapacity := TCSVFastDataset(CSVFastDataset1).InitialCellCapacity;

  edtBlockSize.Text           := IntToStr(CSVBlockSizeKB);
  edtBufferSize.Text          := IntToStr(CSVBufferSizeKB);
  edtInitialCellCapacity.Text := IntToStr(CSVInitialCellCapacity);

  // ============================================================
  // Info-Label mit den tatsächlich aktiven Werten
  // ============================================================
  lblActivePerformanceSettings.Caption := Format(
    'Active: Block=%d KB, Buffer=%d KB, Cells=%d, ScanRows=%d',
    [CSVBlockSizeKB,
     CSVBufferSizeKB,
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
  edtBlockSize.Text           := IntToStr(CSVBlockSizeKB);
  edtBufferSize.Text          := IntToStr(CSVBufferSizeKB);
  edtInitialCellCapacity.Text := IntToStr(CSVInitialCellCapacity);

  edtProgressUpdatePercent.Text := IntToStr(CSVProgressUpdatePercent);

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
  CSVBlockSizeKB         := StrToIntDef(edtBlockSize.Text, 1024);
  CSVBufferSizeKB        := StrToIntDef(edtBufferSize.Text, 2048);
  CSVInitialCellCapacity := StrToIntDef(edtInitialCellCapacity.Text, 64);
  CSVProgressUpdatePercent := StrToIntDef(edtProgressUpdatePercent.Text, 1);
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

  // ---- JSONDataset1 ----
  if Assigned(JSONDataset1) then
  begin
    if JSONDataset1.Active then
      JSONDataset1.DisableControls;
    try
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
    finally
      if JSONDataset1.Active then
        JSONDataset1.EnableControls;
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

    else if DS = JSONDataset1 then
      JSONDataset1.LoadFromFile(FileName)

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
  if DataSource1.DataSet = CSVFastDataset1 then
    CSVFastDataset1.SaveToCSVFile(FileName)
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
    DataSource1.DataSet := JSONDataSet1

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
  StartTime, EndTime: TDateTime;
  DiffSeconds, FileSizeMB: Double;
  FileSize: Int64;
  RowCount: Integer;
  Stats: TTransferStatistic;
  ReportForm: TfrmReport;
  LoadErr: string;
  MaxLineWidth: Int64;
  RequiredBufferKB: Int64;
  ScanStartTime: TDateTime;
  ScanElapsed: Double;
  MsgText: string;
  BufferSizeOriginal: Integer;
  BufferAutoAdjusted: Boolean;
  RowWidthScanMode: string;
  FromRow: Int64;
  ToRow: Int64;
  ScanRows: Int64;
  TotalRowsInRange: Int64;
  FreeRAMBytes: Int64;
begin
  MaxLineWidth       := 0;
  RequiredBufferKB   := 0;
  ScanElapsed        := 0;
  TotalRowsInRange   := 0;
  BufferAutoAdjusted := False;
  BufferSizeOriginal := CSVBufferSizeKB;
  RowWidthScanMode   := 'None';
  FromRow            := 1;
  ToRow              := 0;
  ScanRows           := 0;

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

  // Range aus GUI
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

  // Dateigröße
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
  // Progress-Fenster aufbauen
  // ============================================================
  BuildProgressForm;

  // Datei-Info
  FProgInfoLbl.Caption := Format('File: %s   (%s)',
    [ExtractFileName(FFileName), FormatBytesSmart(FileSize)]);

  // RAM-Init
  FProgTotalRAMMB     := GetRAMTotalMB;
  FLastFreeRAMMB      := GetRAMFreeMB;
  FRAMWarningShown    := False;
  FLoadProgressCounter := 0;

  // RAM-Init
  FProgTotalRAMMB := GetRAMTotalMB;
  FRAMWarningShown := False;
  FLoadProgressCounter := 0;

  // Labels mit Datei-Info
  FProgInfoLbl.Caption := Format('File: %s   (%s)',
    [ExtractFileName(FFileName), FormatBytesSmart(FileSize)]);

  FProgressForm.Show;
  FProgressForm.BringToFront;
  Application.ProcessMessages;
  Sleep(200);
  Application.ProcessMessages;

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
      FProgSpeedLbl.Caption := '';
      FProgElapsedLbl.Caption := '';
      FProgETALbl.Caption := '';
      Application.ProcessMessages;

      ScanStartTime := Now;
      MaxLineWidth := EstimateCSVMaxLineWidth(
        FFileName,
        FromRow,
        ToRow,
        ScanRows,
        CSVQuoteChar,
        TotalRowsInRange,
        @HandleScanProgress);
      ScanElapsed := (Now - ScanStartTime) * SecsPerDay;

      // BufferSize anpassen
      if MaxLineWidth > 0 then
      begin
        RequiredBufferKB := (MaxLineWidth * 2) div 1024 + 16;
        if RequiredBufferKB > CSVBufferSizeKB then
        begin
          BufferSizeOriginal := CSVBufferSizeKB;
          BufferAutoAdjusted := True;
          CSVBufferSizeKB := RequiredBufferKB;
          edtBufferSize.Text := IntToStr(RequiredBufferKB);
          SetCSVSettingsFromForm;
        end;
      end;
    end;

    // ============================================================
    // Vorab-Warnung bei knappem RAM
    // ============================================================
    if FileSize > 0 then
    begin
      FreeRAMBytes := GetRAMFreeMB * 1024 * 1024;
      if FileSize > (FreeRAMBytes * 8 div 10) then   // > 80%
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
        begin
          if Assigned(FProgressForm) then
            FreeAndNil(FProgressForm);
          Exit;
        end;
      end;
    end;

    // ============================================================
    // Phase 2: Load
    // ============================================================
    FProgPhaseLbl.Caption := 'Loading rows...';
    FProgBar.Position := 0;

    // Progress-Total bestimmen
    if ScanRows > 0 then
      // CustomScan: theoretischer Range
      FProgTotalRows := ToRow - FromRow + 1
    else if TotalRowsInRange > 0 then
      // FullScan: exakte Zahl
      FProgTotalRows := TotalRowsInRange
    else
      FProgTotalRows := 0;

    // Am Dataset setzen
    TCSVFastDataset(CSVFastDataset1).OnProgress := @HandleLoadProgress;
    TCSVFastDataset(CSVFastDataset1).TotalRowsForProgress := FProgTotalRows;
    TCSVFastDataset(CSVFastDataset1).ProgressUpdatePercent :=
      StrToIntDef(edtProgressUpdatePercent.Text, 1);
    TCSVFastDataset(CSVFastDataset1).CancelRequested := False;

    // Range ans Dataset
    TCSVFastDataset(CSVFastDataset1).SkipFirstRows := FromRow - 1;
    if ToRow > 0 then
      TCSVFastDataset(CSVFastDataset1).StopAfterRow := ToRow - FromRow + 1
    else
      TCSVFastDataset(CSVFastDataset1).StopAfterRow := 0;

    Application.ProcessMessages;

    DBNavigator1.DataSource := nil;
    RxDBGrid1.DataSource := nil;

    // Thread starten
    CSVThread := TCSVLoadThread.Create(Self, OpenDialog1.FileName);

    while not CSVThread.Finished do
    begin
      Application.ProcessMessages;
      Sleep(20);
    end;

    LoadErr := CSVThread.LoadError;
    CSVThread.Free;
    CSVThread := nil;

    TCSVFastDataset(CSVFastDataset1).OnProgress := nil;

    // ProgressBar auf 100%
    FProgBar.Position := 100;
    Application.ProcessMessages;

    if LoadErr <> '' then
    begin
      FProgPhaseLbl.Caption := 'Error';
      FProgInfoLbl.Caption := 'Load failed: ' + LoadErr;
      Sleep(1500);
      Exit;
    end;

    // Grid reaktivieren
    RxDBGrid1.DataSource := nil;
    DBNavigator1.DataSource := nil;
    RxDBGrid1.Columns.Clear;
    RxDBGrid1.DataSource := DataSource1;
    DBNavigator1.DataSource := DataSource1;

    if DataSource1.DataSet.Active and
       (not DataSource1.DataSet.IsEmpty) then
      DataSource1.DataSet.First;

    RxDBGrid1.Refresh;

    EndTime  := Now;
    RowCount := DataSource1.DataSet.RecordCount;

    // Kurz Erfolg anzeigen
    FProgPhaseLbl.Caption := Format('Finished: %s rows',
      [FormatFloat('#,##0', RowCount)]);
    Application.ProcessMessages;
    Sleep(500);

  finally
    FreeAndNil(FProgressForm);
  end;

  // ============================================================
  // End-Statistik als Report
  // ============================================================
  DiffSeconds := (EndTime - StartTime) * 86400.0;

  Caption := 'Turbobird - Data Editor (' + ExtractFileName(FFileName) + ')';
  btnCreateSQL.Enabled := True;
  btnExportAs.Enabled  := True;

  // --- Statistik-Record füllen ---
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
  Stats.ElapsedSeconds := DiffSeconds;
  Stats.SystemInfo     := GetSystemInfo(FFileName);

  // --- CSV-Optionen ---
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

  // --- CSV Buffer Performance ---
  Stats.CSVBlockSizeKB          := CSVBlockSizeKB;
  Stats.CSVBufferSizeKB         := CSVBufferSizeKB;
  Stats.CSVInitialCellCapacity  := CSVInitialCellCapacity;
  Stats.CSVRowWidthScanMode     := RowWidthScanMode;
  Stats.CSVRowWidthScanRows     := TotalRowsInRange;
  Stats.CSVRowWidthScanIsCustom := (ScanRows > 0);
  Stats.CSVRowWidthScanTime     := ScanElapsed;
  Stats.CSVMaxRowWidth          := MaxLineWidth;
  Stats.CSVBufferAutoAdjusted   := BufferAutoAdjusted;
  Stats.CSVBufferOriginalKB     := BufferSizeOriginal;

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

end.
