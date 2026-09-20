unit SQLHistory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, memds, FileUtil, SynHighlighterSQL, SynEdit,
  LResources, Forms, Controls, Graphics, Dialogs, DBGrids, Buttons, StdCtrls,
  EditBtn, ExtCtrls, DBCtrls, ComCtrls, DBExtCtrls, RxDBGrid,
  turbocommon,
  uthemeselector;

type

  { TfmSQLHistory }

  TfmSQLHistory = class(TForm)
    bbDelete: TBitBtn;
    bbExportToTextFile: TBitBtn;
    bbInsert: TBitBtn;
    btnClearHistory: TButton;
    cbSQLType: TComboBox;
    cxAfterDate: TCheckBox;
    cxOverwrite: TCheckBox;
    Datasource1: TDatasource;
    DateEdit1: TDateEdit;
    DBDateEdit1: TDBDateEdit;
    DBEdit1: TDBEdit;
    DBMemo1: TDBMemo;
    DBNavigator1: TDBNavigator;
    DBNavigator2: TDBNavigator;
    DBText1: TDBText;
    GroupBox1: TGroupBox;
    grboxInsertOption: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    grboxSQLType: TGroupBox;
    Label2: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    DBGrid1: TRxDBGrid;
    SaveDialog1: TSaveDialog;
    Splitter1: TSplitter;
    SQLType: TLabel;
    Time: TLabel;
    tsDetails: TTabSheet;
    tsHistory: TTabSheet;
    procedure bbCloseClick(Sender: TObject);
    procedure bbDeleteClick(Sender: TObject);
    procedure bbExportToTextFileClick(Sender: TObject);
    procedure bbInsertClick(Sender: TObject);
    procedure btnClearHistoryClick(Sender: TObject);
    procedure cbSQLTypeChange(Sender: TObject);
    procedure cxAfterDateClick(Sender: TObject);
    procedure DBGrid1DblClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FNodeInfos: TPNodeInfos;
    FQueryForm: TForm;
    FFilterType: string;

    procedure ApplyHistoryFilter;
    procedure mdsHistoryFilterRecord(DataSet: TDataSet;
          var Accept: Boolean);

  public
    { public declarations }
    procedure Init(DatabaseTitle: string; QueryForm: TForm; ANodeInfos: TPNodeInfos);
  end; 

var
  fmSQLHistory: TfmSQLHistory;

implementation

{ TfmSQLHistory }

uses Main, QueryWindow;

procedure TfmSQLHistory.mdsHistoryFilterRecord(DataSet: TDataSet;
  var Accept: Boolean);
var
  CurrType: string;
begin
  if FFilterType = '' then
  begin
    Accept := True;
    Exit;
  end;

  CurrType := UpperCase(DataSet.FieldByName('SQLType').AsString);
  Accept := (FFilterType = CurrType) or
            ((FFilterType = 'DDL,DML') and ((CurrType = 'DDL') or (CurrType = 'DML')));
end;

procedure TfmSQLHistory.ApplyHistoryFilter;
begin
  case cbSQLType.ItemIndex of
    0: FFilterType := '';         // All
    1: FFilterType := 'DDL,DML';
    2: FFilterType := 'DDL';
    3: FFilterType := 'DML';
    4: FFilterType := 'SELECT';
    5: FFilterType := 'SCRIPT';
  else
    FFilterType := '';
  end;

  fmMain.mdsHistory.DisableControls;
  try
    fmMain.mdsHistory.Close;
    fmMain.mdsHistory.Filtered := False;
    if FFilterType <> '' then
      fmMain.mdsHistory.Filtered := True;
  finally
    fmMain.mdsHistory.Open;
    fmMain.mdsHistory.EnableControls;
  end;
end;

procedure TfmSQLHistory.cbSQLTypeChange(Sender: TObject);
begin
  ApplyHistoryFilter;
end;

procedure TfmSQLHistory.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  fmMain.mdsHistory.SaveToFile(fmMain.CurrentHistoryFile);
  Datasource1.DataSet:= nil;
  if Assigned(FNodeInfos) then
    FNodeInfos^.ViewForm := nil;
  //CloseAction:= caFree;
end;

procedure TfmSQLHistory.FormCreate(Sender: TObject);
begin
  DateEdit1.Date:= Now - 7;

  fmMain.mdsHistory.OnFilterRecord := @mdsHistoryFilterRecord;
  FFilterType := '';
end;

procedure TfmSQLHistory.FormShow(Sender: TObject);
begin
  DBGrid1.OptimizeColumnsWidthAll;
  frmThemeSelector.btnApplyClick(self);
end;

procedure TfmSQLHistory.bbInsertClick(Sender: TObject);
var
  SQLStatement: string;
  i: Integer;
  aStatement: string;
begin
//  SQLStatement:= (fmMain.mdsHistory.FieldByName('SQLStatement').AsString);
  for i:= 0 to DBGrid1.SelectedRows.Count - 1 do
  begin
    Datasource1.DataSet.GotoBookmark(DBGrid1.SelectedRows.Items[i]);
    aStatement := fmMain.mdsHistory.FieldByName('SQLStatement').AsString;
    if Pos(';', aStatement) = 0 then
      aStatement:= aStatement + ';';
    SQLStatement += aStatement;
  end;

  if cxOverwrite.Checked then
    (FQueryForm as TfmQueryWindow).meQuery.Lines.Clear;

  (FQueryForm as TfmQueryWindow).meQuery.Lines.Text:= (FQueryForm as TfmQueryWindow).meQuery.Lines.Text + SQLStatement;
  Close;
end;

procedure TfmSQLHistory.btnClearHistoryClick(Sender: TObject);
begin
  if MessageDlg('Clear complete SQL history for this database?',
       mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;

  try
    fmMain.mdsHistory.DisableControls;
    try
      fmMain.mdsHistory.First;
      while not fmMain.mdsHistory.EOF do
        fmMain.mdsHistory.Delete;

      fmMain.mdsHistory.SaveToFile(fmMain.CurrentHistoryFile);
    finally
      fmMain.mdsHistory.EnableControls;
    end;
  except
    on E: Exception do
      MessageDlg('Failed to clear history:' + sLineBreak + E.Message,
        mtError, [mbOK], 0);
  end;
end;

procedure TfmSQLHistory.cxAfterDateClick(Sender: TObject);
begin
  DateEdit1.Enabled:= cxAfterDate.Checked;
end;

procedure TfmSQLHistory.DBGrid1DblClick(Sender: TObject);
begin
  bbInsertClick(nil);
end;

procedure TfmSQLHistory.bbDeleteClick(Sender: TObject);
begin
  if MessageDlg('Are you sure you want to delete current record of history', mtConfirmation, [mbYes, mbNo], 0) = mrYes
    then
      fmMain.mdsHistory.Delete;
end;

procedure TfmSQLHistory.bbCloseClick(Sender: TObject);
begin
  Close;
  Parent.Free;
end;

procedure TfmSQLHistory.bbExportToTextFileClick(Sender: TObject);
var
  CurrType: string;
  List: TStringList;
  Line: string;
begin
  if SaveDialog1.Execute then
  with fmMain.mdsHistory do
  begin
    DBGrid1.Visible:= False;
    First;
    List:= TStringList.Create;
    try
      while not Eof do
      begin
        if (not cxAfterDate.Checked) or (FieldByName('Time').AsDateTime > DateEdit1.Date) then
        begin
          CurrType:= FieldByName('SQLType').AsString;
          if (cbSQLType.ItemIndex = 0) or
            ((CurrType = 'DDL') and (cbSQLType.ItemIndex in [1, 2])) or
            ((CurrType = 'DML') and (cbSQLType.ItemIndex in [1, 3])) or
            ((CurrType = 'SELECT') and (cbSQLType.ItemIndex = 4)) then
          begin
            List.Add('-- ' + FieldByName('Time').AsString);
            Line:= FieldByName('SQLStatement').AsString;
            if Pos(';', Line) = 0 then
              Line:= Line + ';';
            List.Add(Line);
          end;
        end;
        Next;
      end;
      List.SaveToFile(SaveDialog1.FileName);
    finally
      List.Free;
    end;
    DBGrid1.Visible:= True;
  end;
end;

procedure TfmSQLHistory.Init(DatabaseTitle: string; QueryForm: TForm; ANodeInfos: TPNodeInfos);
begin
  FNodeInfos := ANodeInfos;
  FQueryForm:= QueryForm;
  Caption:= 'SQL History for: ' + DatabaseTitle;
  Datasource1.DataSet:= fmMain.mdsHistory;
  fmMain.mdsHistory.Last;
end;

initialization
  {$I sqlhistory.lrs}

end.
