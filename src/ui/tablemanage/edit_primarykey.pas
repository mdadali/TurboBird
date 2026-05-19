unit edit_primarykey;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  Grids, ComCtrls, ExtCtrls, CheckLst,

  turbocommon,
  fbcommon,

  fsimpleobjextractor,
  fmetaquerys,

  uthemeselector;

type

  { TfmPrimaryKey }

  TfmPrimaryKey = class(TForm)
    bbEdit: TBitBtn;
    bbRefresh: TBitBtn;
    bbClose: TButton;
    bbDrop: TBitBtn;
    chkLstBoxFields: TCheckListBox;
    pnlTop: TPanel;
    pnBottom: TPanel;
    sgPrimaryKey: TStringGrid;

    procedure bbEditClick(Sender: TObject);
    procedure bbDropClick(Sender: TObject);
    procedure bbRefreshClick(Sender: TObject);
    procedure bbCloseClick(Sender: TObject);
    procedure chkLstBoxFieldsClickCheck(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private
    FDBIndex: Integer;
    FTableName: string;
    FNodeInfos: TPNodeInfos;
    FExtractor: TSimpleObjExtractor;

    procedure LoadFields;                          // Felder in CheckListBox laden
    procedure CheckPKFields;                       // PK-Felder anhaken

  public
    procedure Init(ADBIndex: Integer; const ATableName: string; ANodeInfos: TPNodeInfos; AExtractor: TSimpleObjExtractor);
    procedure FillPrimaryKey;

  end;


implementation

{$R *.lfm}

uses Main, QueryWindow;

{ TfmPrimaryKey }

procedure TfmPrimaryKey.Init(ADBIndex: Integer; const ATableName: string; ANodeInfos: TPNodeInfos; AExtractor: TSimpleObjExtractor);
begin
  FDBIndex := ADBIndex;
  FTableName := ATableName;
  FNodeInfos := ANodeInfos;
  FExtractor := AExtractor;

  Caption := 'Primary Key: ' + FTableName;

  sgPrimaryKey.Cells[0, 0] := 'Primary Key Definition';
  sgPrimaryKey.ColWidths[0] := 400;
end;

procedure TfmPrimaryKey.LoadFields;
var
  FieldsQuery: TIsolatedQuery;
begin
  chkLstBoxFields.Clear;

  FieldsQuery := GetFieldsIsolated(RegisteredDatabases[FDBIndex].IBDatabase, FTableName);
  try
    while not FieldsQuery.Query.EOF do
    begin
      chkLstBoxFields.Items.Add(Trim(FieldsQuery.Query.FieldByName('field_name').AsString));
      FieldsQuery.Query.Next;
    end;
  finally
    FieldsQuery.Free;
  end;
end;

procedure TfmPrimaryKey.CheckPKFields;
var
  PKConstraintName, PKIndexName: string;
  FieldsList: TStringList;
  i, j: Integer;
begin
  // Alle Checkboxen deaktivieren
  for i := 0 to chkLstBoxFields.Count - 1 do
    chkLstBoxFields.Checked[i] := False;

  // Aktuelle PK-Felder ermitteln
  FieldsList := TStringList.Create;
  try
    PKIndexName := GetPrimaryKeyIndexNameIsolated(
      RegisteredDatabases[FDBIndex].IBDatabase, FTableName, PKConstraintName);

    if PKIndexName <> '' then
    begin
      GetConstraintFieldsIsolated(
        RegisteredDatabases[FDBIndex].IBDatabase, FTableName, PKIndexName, FieldsList);

      // PK-Felder anhaken
      for i := 0 to chkLstBoxFields.Count - 1 do
      begin
        for j := 0 to FieldsList.Count - 1 do
        begin
          if SameText(chkLstBoxFields.Items[i], FieldsList[j]) then
          begin
            chkLstBoxFields.Checked[i] := True;
            Break;
          end;
        end;
      end;
    end;

  finally
    FieldsList.Free;
  end;
end;

procedure TfmPrimaryKey.FillPrimaryKey;
var
  Items: TStringList;
  i: Integer;
begin
  sgPrimaryKey.RowCount := 1;

  Items := TStringList.Create;
  try
    FExtractor.Extract(otPrimaryKeys, FTableName, [], AlwaysQuoteIdentifiers, TStrings(Items));

    for i := 0 to Items.Count - 1 do
    begin
      sgPrimaryKey.RowCount := i + 2;
      sgPrimaryKey.Cells[0, i + 1] := Items[i];
    end;

  finally
    Items.Free;
  end;

  // Felder laden und PK-Felder checken
  LoadFields;
  CheckPKFields;

  // Button-Status initial setzen
  chkLstBoxFieldsClickCheck(nil);
end;

procedure TfmPrimaryKey.bbEditClick(Sender: TObject);
var
  QWindow: TfmQueryWindow;
  PKConstraintName, PKIndexName: string;
  CheckedFields: string;
  i: Integer;
  HasFields: Boolean;
begin
  // Gewählte Felder aus CheckListBox sammeln
  CheckedFields := '';
  HasFields := False;
  for i := 0 to chkLstBoxFields.Count - 1 do
  begin
    if chkLstBoxFields.Checked[i] then
    begin
      HasFields := True;
      if CheckedFields <> '' then
        CheckedFields := CheckedFields + ', ';
      CheckedFields := CheckedFields + MakeCaseSensitiveAuto(chkLstBoxFields.Items[i]);
    end;
  end;

  if not HasFields then
  begin
    ShowMessage('Please select at least one field for the Primary Key.');
    Exit;
  end;

  // Constraint-Namen ermitteln
  PKIndexName := GetPrimaryKeyIndexNameIsolated(
    RegisteredDatabases[FDBIndex].IBDatabase, FTableName, PKConstraintName);

  QWindow := fmMain.ShowQueryWindow(FDBIndex, 'Edit Primary Key: ' + FTableName);
  QWindow.meQuery.Lines.Clear;
  QWindow.meQuery.Lines.Add('SET TERM ^;');
  QWindow.meQuery.Lines.Add('');

  if PKConstraintName <> '' then
  begin
    QWindow.meQuery.Lines.Add('-- Drop existing Primary Key');
    QWindow.meQuery.Lines.Add('ALTER TABLE ' + MakeCaseSensitiveAuto(FTableName) + ' DROP CONSTRAINT ' + PKConstraintName + ' ^;');
    QWindow.meQuery.Lines.Add('');
  end;

  QWindow.meQuery.Lines.Add('-- Create new Primary Key');
  QWindow.meQuery.Lines.Add('ALTER TABLE ' + MakeCaseSensitiveAuto(FTableName) + ' ADD PRIMARY KEY (' + CheckedFields + ') ^;');
  QWindow.meQuery.Lines.Add('');
  QWindow.meQuery.Lines.Add('SET TERM ;^');

  QWindow.OnCommit := @bbRefreshClick;
  QWindow.Show;
end;

procedure TfmPrimaryKey.bbDropClick(Sender: TObject);
var
  QWindow: TfmQueryWindow;
  PKConstraintName: string;
  PKIndexName: string;
begin
  if sgPrimaryKey.RowCount > 1 then
  begin
    if MessageDlg('Are you sure you want to drop the Primary Key from ' + FTableName + '?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      PKIndexName := GetPrimaryKeyIndexNameIsolated(
        RegisteredDatabases[FDBIndex].IBDatabase, FTableName, PKConstraintName);

      QWindow := fmMain.ShowQueryWindow(FDBIndex, 'Drop Primary Key: ' + FTableName);
      QWindow.meQuery.Lines.Clear;
      QWindow.meQuery.Lines.Add('SET TERM ^;');
      QWindow.meQuery.Lines.Add('');
      QWindow.meQuery.Lines.Add('ALTER TABLE ' + MakeCaseSensitiveAuto(FTableName) + ' DROP CONSTRAINT ' + PKConstraintName + ' ^;');
      QWindow.meQuery.Lines.Add('');
      QWindow.meQuery.Lines.Add('SET TERM ;^');
      QWindow.OnCommit := @bbRefreshClick;
      QWindow.Show;
    end;
  end;
end;

procedure TfmPrimaryKey.bbRefreshClick(Sender: TObject);
begin
  FillPrimaryKey;
end;

procedure TfmPrimaryKey.bbCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfmPrimaryKey.chkLstBoxFieldsClickCheck(Sender: TObject);
var
  i: Integer;
  HasChecked: Boolean;
  PKConstraintName, PKIndexName: string;
  HasPK: Boolean;
begin
  // Prüfen ob mindestens ein Feld gecheckt ist
  HasChecked := False;
  for i := 0 to chkLstBoxFields.Count - 1 do
  begin
    if chkLstBoxFields.Checked[i] then
    begin
      HasChecked := True;
      Break;
    end;
  end;

  // Prüfen ob bereits ein PK existiert
  PKIndexName := GetPrimaryKeyIndexNameIsolated(
    RegisteredDatabases[FDBIndex].IBDatabase, FTableName, PKConstraintName);
  HasPK := (PKConstraintName <> '');

  // Edit: aktiv wenn Felder gecheckt UND (PK existiert ODER neu)
  bbEdit.Enabled := HasChecked;

  // Drop: aktiv wenn PK existiert
  bbDrop.Enabled := HasPK;
end;

procedure TfmPrimaryKey.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(Self);
  FillPrimaryKey;
end;

end.
