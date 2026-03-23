unit fCSVEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, csvdataset, SdfData, dbf, Forms, Controls, Graphics,
  Dialogs, Clipbrd, DBCtrls, StdCtrls, ExtCtrls, ComCtrls, Menus, SynEdit,
  SynHighlighterSQL, RxDBGrid, RxDBGridExportPdf, RxDBGridPrintGrid,
  RxDBGridExportSpreadSheet, fpsDataset,

  turbocommon,
  uthemeselector,
  uGenSQLFromCSVDataset, c_json_dataset,

  fdataexportersintrf,

  fpsTypes
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
    Dbf1: TDbf;
    DBNavigator1: TDBNavigator;
    edtQuoteChar: TEdit;
    edtLineEnding: TEdit;
    edtDelimiter: TEdit;
    edtDefaultFieldLength: TEdit;
    FixedFormatDataSet1: TFixedFormatDataSet;
    JSONDataSet1: TJSONDataSet;
    Label1: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    Label6: TLabel;
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
    procedure lmExportDataAsHtmlClick(Sender: TObject);
    procedure lmExportDataAsMarkDownTableClick(Sender: TObject);
    procedure lmExportDataAsPDFClick(Sender: TObject);
    procedure lmExportDataAsSpreadSheetClick(Sender: TObject);
    procedure lmExportToClipboardClick(Sender: TObject);
    procedure lmPrintDataClick(Sender: TObject);
    procedure lmStdExportFormatsClick(Sender: TObject);
  private
    FFileName: string;
    procedure LoadFile(const FileName: string);
    procedure SaveFile(const FileName: string);

    procedure SetCSVSettingsFromForm;
    procedure SelectDataSetByFileName(const AFileName: string);
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
  if FOwner.DataSource1.DataSet.Active then
    FOwner.DataSource1.DataSet.Close;
  FOwner.DataSource1.DataSet.Fields.Clear;
end;

procedure TCSVLoadThread.LoadCSVInDataset;
begin
  if FOwner.DataSource1.DataSet = FOwner.CSVDataset1 then
    FOwner.CSVDataset1.LoadFromCSVFile(FFileName)

  else if FOwner.DataSource1.DataSet = FOwner.JSONDataset1 then
    FOwner.JSONDataset1.LoadFromFile(FFileName)

  else if FOwner.DataSource1.DataSet = FOwner.sWorksheetDataset1 then
  begin
    FOwner.sWorksheetDataset1.FileName := FFileName;
    //FOwner.sWorksheetDataset1.Open;
  end

  else if FOwner.DataSource1.DataSet = FOwner.Dbf1 then
  begin
    FOwner.Dbf1.FilePathFull := FFileName;
    //FOwner.Dbf1.Open;
  end

  else if FOwner.DataSource1.DataSet = FOwner.SdfDataSet1 then
  begin
    FOwner.SdfDataSet1.FileName := FFileName;
    //FOwner.SdfDataSet1.Open;
  end

  else if FOwner.DataSource1.DataSet = FOwner.FixedFormatDataSet1 then
  begin
    FOwner.FixedFormatDataSet1.FileName := FFileName;
    //FOwner.FixedFormatDataSet1.Open;
  end;

  FOwner.DataSource1.DataSet.Open;
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

procedure TfrmCSVEditor.LoadFile(const FileName: string);
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

procedure TfrmCSVEditor.SaveFile(const FileName: string);
begin
  if not DataSource1.DataSet.Active then Exit;

  try
    if DataSource1.DataSet = CSVDataSet1 then
      CSVDataSet1.SaveToCSVFile(FileName)

    else if DataSource1.DataSet = JSONDataSet1 then
    begin
      SaveJSONToFile(JSONDataSet1, FileName)
    end

    else if DataSource1.DataSet = sWorksheetDataset1 then
      sWorksheetDataset1.Flush     //SaveToFile(FileName)

    else if DataSource1.DataSet = Dbf1 then
    begin
      Dbf1.FilePathFull := FileName;
      Dbf1.Close;
      Dbf1.Open; // ggf. nötig je nach Lib
    end

    else if DataSource1.DataSet = SdfDataSet1 then
      SdfDataSet1.SaveFileAs(FileName)

    else if DataSource1.DataSet = FixedFormatDataSet1 then
      FixedFormatDataSet1.SaveFileAs(FileName)

    else
      raise Exception.Create('Unbekannter Dataset-Typ beim Speichern');

  except
    on E: Exception do
      ShowMessage('Fehler beim Speichern: ' + E.Message);
  end;
end;

procedure TfrmCSVEditor.SelectDataSetByFileName(const AFileName: string);
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(AFileName));

  if DataSource1.DataSet <> nil then
  begin
    if DataSource1.DataSet.Active then
      DataSource1.DataSet.Close;
  end;

  if (Ext = '.csv') then
    DataSource1.DataSet := CSVDataSet1

  else if (Ext = '.json') then
      DataSource1.DataSet := JSONDataSet1

  else if (Ext = '.xlsx') or (Ext = '.xls') or (Ext = '.xml')
       or (Ext = '.html') or (Ext = '.ods')
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

    else if Ext = '.odt' then
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

  FFileName := OpenDialog1.FileName;
  SelectDataSetByFileName(FFileName);

  SynEdit1.Lines.Clear;
  if DataSource1.DataSet.Active then
  begin
    DataSource1.DataSet.Close;
    DataSource1.DataSet.Fields.Clear;
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

  //FFileName := OpenDialog1.FileName;
  Caption := 'Turbobird - Data Editor (' + ExtractFileName(FFileName) + ')';

  btnSaveFileAs.Enabled := True;
end;

procedure TfrmCSVEditor.btnCreateSQLClick(Sender: TObject);
var
  GenSQLFromCSVDataset: TGenSQLFromCSVDataset;
  TempTableName: string;
  TempFieldLength: Word;
begin
  TempFieldLength := 0;
  //Dataset prüfen
  if not DataSource1.DataSet.Active then
  begin
    MessageDlg('Warning',
      'Dataset is not active.',
      mtWarning, [mbOK], 0);
    Exit;
  end;

  if DataSource1.DataSet.FieldCount = 0 then
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
  //TempFieldLength := CSVDataset1.CSVOptions.DefaultFieldLength;
  if TempFieldLength = 0 then
    TempFieldLength := 50;

  //Tabellenname erzeugen
  TempTableName := UpperCase(ChangeFileExt(ExtractFileName(FFileName), ''));

  GenSQLFromCSVDataset := nil;

  try
    GenSQLFromCSVDataset := TGenSQLFromCSVDataset.Create(
      DataSource1.DataSet,
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

procedure TfrmCSVEditor.CSVDataset1AfterDelete(DataSet: TDataSet);
begin
  SaveFile(FFileName);
end;

procedure TfrmCSVEditor.DBNavigator1Click(Sender: TObject; Button: TDBNavButtonType);
begin
  if Button = nbRefresh then
  begin
    LoadFile(FFileName);
    abort;
  end;
  if Button = nbPost then
  begin
    SaveFile(FFileName);
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

procedure TfrmCSVEditor.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(self);

  SynEdit1.Color      := QWEditorBackgroundColor;
  SynEdit1.Font.Name  := QWEditorFontName;
  SynEdit1.Font.Size  := QWEditorFontSize;
  SynEdit1.Font.Color := QWEditorFontColor;
end;

procedure TfrmCSVEditor.btnSaveFileAsClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
    SaveFile(SaveDialog1.FileName);
end;

procedure TfrmCSVEditor.lmStdExportFormatsClick(Sender: TObject);
begin
  if DataSource1.DataSet.IsEmpty then
  begin
    ShowMessage('DataSet has no records!');
    exit;
  end;

  ExportStdFormat(DataSource1.DataSet);
end;

procedure TfrmCSVEditor.lmExportDataAsHtmlClick(Sender: TObject);
begin
  if DataSource1.DataSet.IsEmpty then
  begin
    ShowMessage('DataSet has no records!');
    exit;
  end;
  ExportDataHtml(DataSource1.DataSet);
end;

procedure TfrmCSVEditor.lmExportDataAsMarkDownTableClick(Sender: TObject);
begin
  if DataSource1.DataSet.IsEmpty then
  begin
    ShowMessage('DataSet has no records!');
    exit;
  end;
  ExportDataMarkDownTable(DataSource1.DataSet);
end;

procedure TfrmCSVEditor.lmExportDataAsPDFClick(Sender: TObject);
begin
  try
    RxDBGridExportPDF1.Execute;
  except
    raise;
  end;
end;

procedure TfrmCSVEditor.lmExportDataAsSpreadSheetClick(Sender: TObject);
begin
  RxDBGridExportSpreadSheet1.Execute;
end;

procedure TfrmCSVEditor.lmPrintDataClick(Sender: TObject);
begin
   RxDBGridPrint1.Execute;
end;


{ Copy query result to Clipboard }

procedure TfrmCSVEditor.lmExportToClipboardClick(Sender: TObject);
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
