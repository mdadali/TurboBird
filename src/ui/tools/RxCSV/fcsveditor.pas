unit fCSVEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, csvdataset, Forms, Controls, Graphics, Dialogs,
  DBCtrls, StdCtrls, ExtCtrls, ComCtrls, RxDBGrid;

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
    SaveDialog1: TSaveDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    RxDBGrid1: TRxDBGrid;
    ScrollBox1: TScrollBox;
    tsCSVSettings: TTabSheet;
    tsGrid: TTabSheet;
    tsDetail: TTabSheet;
    procedure btnOpenFileClick(Sender: TObject);
    procedure btnSaveFileAsClick(Sender: TObject);
    procedure CSVDataset1AfterDelete(DataSet: TDataSet);
    procedure CSVDataset1BeforePost(DataSet: TDataSet);
    procedure DBNavigator1Click(Sender: TObject; Button: TDBNavButtonType);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FFileName: string;
    procedure LoadCSVFile(const FileName: string);
    procedure SaveCSVFile(const FileName: string);

    procedure SetCSVFromFormSettings;
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

end;

procedure TfrmCSVEditor.WriteIni;
begin

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

procedure TfrmCSVEditor.SetCSVFromFormSettings;
begin
  CSVDataset1.CSVOptions.DefaultFieldLength := StrToIntDef(edtDefaultFieldLength.Text, 50);
  CSVDataset1.CSVOptions.Delimiter  := edtDelimiter.Text[1];
  CSVDataset1.CSVOptions.LineEnding := edtLineEnding.Text[1];
  CSVDataset1.CSVOptions.QuoteChar  := edtQuoteChar.Text[1];
  CSVDataset1.CSVOptions.FirstLineAsFieldNames := chkBoxFirstLineAsFieldName.Checked;
  CSVDataset1.CSVOptions.IgnoreOuterWhitespace := chkBoxIgnoreOuterWhiteSpace.Checked;
  CSVDataset1.CSVOptions.QuoteOuterWhitespace  := chkBoxQuoteOuterWhiteSpace.Checked;
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

  SetCSVFromFormSettings;

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

procedure TfrmCSVEditor.btnSaveFileAsClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
    SaveCSVFile(SaveDialog1.FileName);
end;

procedure TfrmCSVEditor.CSVDataset1AfterDelete(DataSet: TDataSet);
begin
  SaveCSVFile(FFileName);
end;

procedure TfrmCSVEditor.CSVDataset1BeforePost(DataSet: TDataSet);
begin
  // Hier nur Validierungen; kein ApplyUpdates!
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

end.
