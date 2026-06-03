unit NotNullConstraints;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  Grids, CheckLst,

  turbocommon,
  fbcommon,

  fsimpleobjextractor,
  fmetaquerys,

  uthemeselector;

type

  { TfmNotNullConstraints }

  TfmNotNullConstraints = class(TForm)
    bbClose: TBitBtn;
    bbRefresh: TBitBtn;
    bbApply: TBitBtn;
    chkLstBoxFields: TCheckListBox;
    pnBottom: TPanel;
    pnlTop: TPanel;
    sgNotNullConstraints: TStringGrid;

    procedure bbApplyClick(Sender: TObject);
    procedure bbRefreshClick(Sender: TObject);
    procedure bbCloseClick(Sender: TObject);
    procedure chkLstBoxFieldsClickCheck(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private
    FDBIndex: Integer;
    FTableName: string;
    FNodeInfos: TPNodeInfos;
    FExtractor: TSimpleObjExtractor;
    FOrigNotNullFields: TStringList;  // Ursprüngliche NOT NULL Felder

    procedure LoadFields;

  public
    procedure Init(ADBIndex: Integer; const ATableName: string; ANodeInfos: TPNodeInfos; AExtractor: TSimpleObjExtractor);
    procedure FillNotNullConstraints;

  end;


implementation

{$R *.lfm}

uses Main, QueryWindow;

{ TfmNotNullConstraints }

procedure TfmNotNullConstraints.Init(ADBIndex: Integer; const ATableName: string;
  ANodeInfos: TPNodeInfos; AExtractor: TSimpleObjExtractor);
begin
  FDBIndex := ADBIndex;
  FTableName := ATableName;
  FNodeInfos := ANodeInfos;
  FExtractor := AExtractor;
  FOrigNotNullFields := TStringList.Create;

  Caption := 'Not Null Constraints: ' + FTableName;

  sgNotNullConstraints.Cells[0, 0] := 'Constraint Definition';
  sgNotNullConstraints.ColWidths[0] := 300;
end;

procedure TfmNotNullConstraints.LoadFields;
var
  FieldsQuery: TIsolatedQuery;
  FieldName: string;
  IsNotNull: Boolean;
  i: Integer;
begin
  chkLstBoxFields.Clear;
  FOrigNotNullFields.Clear;

  FieldsQuery := GetFieldsIsolated(RegisteredDatabases[FDBIndex].IBDatabase, FTableName);
  try
    while not FieldsQuery.Query.EOF do
    begin
      FieldName := Trim(FieldsQuery.Query.FieldByName('field_name').AsString);
      IsNotNull := (FieldsQuery.Query.FieldByName('field_not_null_constraint').AsString = '1');

      chkLstBoxFields.Items.Add(FieldName);
      chkLstBoxFields.Checked[chkLstBoxFields.Count - 1] := IsNotNull;

      if IsNotNull then
        FOrigNotNullFields.Add(FieldName);

      FieldsQuery.Query.Next;
    end;
  finally
    FieldsQuery.Free;
  end;

  // Button-Status
  bbApply.Enabled := False;
end;

procedure TfmNotNullConstraints.FillNotNullConstraints;
var
  Items: TStringList;
  i: Integer;
begin
  sgNotNullConstraints.RowCount := 1;

  Items := TStringList.Create;
  try
    FExtractor.Extract(otNotNullConstraints, FTableName, [], AlwaysQuoteIdentifiers, TStrings(Items));

    for i := 0 to Items.Count - 1 do
    begin
      sgNotNullConstraints.RowCount := i + 2;
      sgNotNullConstraints.Cells[0, i + 1] := Items[i];
    end;

  finally
    Items.Free;
  end;

  // Felder laden
  LoadFields;
end;

procedure TfmNotNullConstraints.chkLstBoxFieldsClickCheck(Sender: TObject);
var
  i: Integer;
  HasChanges: Boolean;
  FieldName: string;
  CurrentlyNotNull: Boolean;
  WasNotNull: Boolean;
begin
  HasChanges := False;
  for i := 0 to chkLstBoxFields.Count - 1 do
  begin
    FieldName := chkLstBoxFields.Items[i];
    CurrentlyNotNull := chkLstBoxFields.Checked[i];
    WasNotNull := (FOrigNotNullFields.IndexOf(FieldName) >= 0);

    if CurrentlyNotNull <> WasNotNull then
    begin
      HasChanges := True;
      Break;
    end;
  end;

  bbApply.Enabled := HasChanges;
end;

procedure TfmNotNullConstraints.bbApplyClick(Sender: TObject);
var
  QWindow: TfmQueryWindow;
  SQL: TStringList;
  i: Integer;
  FieldName: string;
  CurrentlyNotNull: Boolean;
  WasNotNull: Boolean;
begin
  SQL := TStringList.Create;
  try
    for i := 0 to chkLstBoxFields.Count - 1 do
    begin
      FieldName := MakeCaseSensitiveAuto(chkLstBoxFields.Items[i]);
      CurrentlyNotNull := chkLstBoxFields.Checked[i];
      WasNotNull := (FOrigNotNullFields.IndexOf(chkLstBoxFields.Items[i]) >= 0);

      if CurrentlyNotNull and not WasNotNull then
      begin
        // SET NOT NULL
        SQL.Add('ALTER TABLE ' + MakeCaseSensitiveAuto(FTableName) +
          ' ALTER COLUMN ' + FieldName + ' SET NOT NULL ^;');
      end
      else if not CurrentlyNotNull and WasNotNull then
      begin
        // DROP NOT NULL
        SQL.Add('ALTER TABLE ' + MakeCaseSensitiveAuto(FTableName) +
          ' ALTER COLUMN ' + FieldName + ' DROP NOT NULL ^;');
      end;
    end;

    if SQL.Count = 0 then
    begin
      ShowMessage('No changes detected.');
      Exit;
    end;

    QWindow := fmMain.ShowQueryWindow(FDBIndex, 'Modify Not Null Constraints: ' + FTableName);
    QWindow.meQuery.Lines.Clear;
    QWindow.meQuery.Lines.Add('SET TERM ^;');
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.AddStrings(SQL);
    QWindow.meQuery.Lines.Add('');
    QWindow.meQuery.Lines.Add('SET TERM ;^');

    QWindow.OnCommit := @bbRefreshClick;
    QWindow.Show;

  finally
    SQL.Free;
  end;
end;

procedure TfmNotNullConstraints.bbRefreshClick(Sender: TObject);
begin
  FillNotNullConstraints;
end;

procedure TfmNotNullConstraints.bbCloseClick(Sender: TObject);
begin
  Close;
  FOrigNotNullFields.Free;
end;

procedure TfmNotNullConstraints.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(Self);
end;

end.
