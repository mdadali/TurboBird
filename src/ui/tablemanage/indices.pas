unit Indices;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  Grids, Buttons, StdCtrls, CheckLst,

  turbocommon,
  fbcommon,

  fsimpleobjextractor,

  uthemeselector;

type

  { TfmIndices }

  TfmIndices = class(TForm)
    bbRefreshIndices: TBitBtn;
    bbDropIndices: TBitBtn;
    sgIndices: TStringGrid;
    GroupBox2: TGroupBox;
    cbIndexType: TComboBox;
    edIndexName: TEdit;
    Label3: TLabel;
    cxUnique: TCheckBox;
    cbSortType: TComboBox;
    bbCreateIndex: TBitBtn;
    Label1: TLabel;
    Label4: TLabel;
    Label2: TLabel;
    clbFields: TCheckListBox;

    procedure bbRefreshClick(Sender: TObject);
    procedure bbDropClick(Sender: TObject);
    procedure bbCreateClick(Sender: TObject);
    procedure cbIndexTypeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private
    FDBIndex: Integer;
    FTableName: string;
    FNodeInfos: TPNodeInfos;
    FExtractor: TSimpleObjExtractor;

  public
    procedure Init(ADBIndex: Integer; const ATableName: string;
                   ANodeInfos: TPNodeInfos; AExtractor: TSimpleObjExtractor);
    procedure FillIndices;
  end;


implementation

{$R *.lfm}

uses Main, QueryWindow;

{ TfmIndices }

procedure TfmIndices.Init(ADBIndex: Integer; const ATableName: string;
  ANodeInfos: TPNodeInfos; AExtractor: TSimpleObjExtractor);
begin
  FDBIndex := ADBIndex;
  FTableName := ATableName;
  FNodeInfos := ANodeInfos;
  FExtractor := AExtractor;

  Caption := 'Indices: ' + FTableName;
end;

procedure TfmIndices.FillIndices;
var
  Items: TStringList;
  i: Integer;
  RawFields: TFBFieldRawArray;
begin
  sgIndices.RowCount := 1;

  Items := TStringList.Create;
  try
    FExtractor.Extract(otIndexes, FTableName, [],
      AlwaysQuoteIdentifiers, TStrings(Items));

    for i := 0 to Items.Count - 1 do
    begin
      sgIndices.RowCount := i + 2;
      sgIndices.Cells[0, i + 1] := Items[i];  // "IDX_NAME: UNIQUE ON (Feld1, Feld2)"
    end;
  finally
    Items.Free;
  end;

  // Felder für "Create Index" laden — ohne BLOBs
  edIndexName.Text := 'IX_' + FTableName + '_' + IntToStr(sgIndices.RowCount);

  clbFields.Clear;
  RawFields := FExtractor.GetTableFieldsRaw(FTableName);
  for i := 0 to High(RawFields) do
  begin
    // BLOB-Felder können nicht in Index aufgenommen werden.
    // 261 = RDB$FIELD_TYPE für BLOB (alle Versionen FB 1.5–6)
    if RawFields[i].FieldType <> 261 then
      clbFields.Items.Add(RawFields[i].FieldName);
  end;

  if sgIndices.RowCount > 1 then
    sgIndices.Row := 1;
end;

procedure TfmIndices.bbRefreshClick(Sender: TObject);
begin
  FillIndices;
end;

procedure TfmIndices.bbDropClick(Sender: TObject);
var
  Line, IndexName: string;
  ColonPos: Integer;
begin
  with sgIndices do
  begin
    if RowCount <= 1 then Exit;

    Line := Cells[0, Row];
    ColonPos := Pos(':', Line);
    if ColonPos > 0 then
      IndexName := Trim(Copy(Line, 1, ColonPos - 1))
    else
      IndexName := Trim(Line);

    if MessageDlg('Are you sure you want to drop index: ' + IndexName, mtConfirmation,
      [mbYes, mbNo], 0) = mrYes then
    begin
      fmMain.ShowCompleteQueryWindow(FDBIndex, 'Drop Index on table: ' + FTableName,
        'DROP INDEX ' + IndexName, @bbRefreshClick);
    end;
  end;
end;

procedure TfmIndices.bbCreateClick(Sender: TObject);
var
  Fields: string;
  i: Integer;
  QWindow: TfmQueryWindow;
  FirstLine: string;
begin
  Fields := '';
  for i := 0 to clbFields.Count - 1 do
    if clbFields.Checked[i] then
      Fields := Fields + Trim(clbFields.Items[i]) + ',';
  Delete(Fields, Length(Fields), 1);

  if Trim(Fields) = '' then
    MessageDlg('Error', 'You should select at least one field', mtError, [mbOk], 0)
  else if Trim(edIndexName.Text) = '' then
    MessageDlg('Error', 'You should enter the new index name', mtError, [mbOk], 0)
  else
  begin
    QWindow := fmMain.ShowQueryWindow(FDBIndex, 'Create new index');
    QWindow.meQuery.Lines.Clear;

    FirstLine := 'CREATE ';
    if cxUnique.Checked then
      FirstLine := FirstLine + 'UNIQUE ';
    FirstLine := FirstLine + cbSortType.Text + ' INDEX ' + edIndexName.Text;
    QWindow.meQuery.Lines.Text := FirstLine + LineEnding + 'ON ' + FTableName + ' (' + Fields + ')';

    QWindow.OnCommit := @bbRefreshClick;
    QWindow.Show;
  end;
end;

procedure TfmIndices.cbIndexTypeChange(Sender: TObject);
begin
  edIndexName.Text := 'IX_' + FTableName + '_' + IntToStr(sgIndices.RowCount);
end;

procedure TfmIndices.FormShow(Sender: TObject);
begin
  FillIndices;
  frmThemeSelector.btnApplyClick(Self);
end;

end.
