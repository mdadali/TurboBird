unit UniqueConstraints;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Grids,
  CheckLst, StdCtrls, Buttons,

  turbocommon,
  fbcommon,

  fsimpleobjextractor,
  fmetaquerys,

  uthemeselector;

type

  { TfmUniqueConstraints }

  TfmUniqueConstraints = class(TForm)
    bbClose: TButton;
    bbDrop: TBitBtn;
    bbEdit: TBitBtn;
    bbRefresh: TBitBtn;
    bbNew: TBitBtn;
    chkLstBoxFields: TCheckListBox;
    edConstraintName: TEdit;
    Label1: TLabel;
    pnBottom: TPanel;
    pnlRight: TPanel;
    pnlTop: TPanel;
    sgUniqueConstraints: TStringGrid;

    procedure bbEditClick(Sender: TObject);
    procedure bbDropClick(Sender: TObject);
    procedure bbRefreshClick(Sender: TObject);
    procedure bbCloseClick(Sender: TObject);
    procedure bbNewClick(Sender: TObject);
    procedure chkLstBoxFieldsClickCheck(Sender: TObject);
    procedure sgUniqueConstraintsClick(Sender: TObject);
    procedure sgUniqueConstraintsSelection(Sender: TObject; aCol, aRow: Integer);
    procedure FormShow(Sender: TObject);

  private
    FDBIndex: Integer;
    FTableName: string;
    FNodeInfos: TPNodeInfos;
    FExtractor: TSimpleObjExtractor;

    procedure LoadFields;
    procedure LoadConstraintFields(const ConstraintName: string);

  public
    procedure Init(ADBIndex: Integer; const ATableName: string; ANodeInfos: TPNodeInfos; AExtractor: TSimpleObjExtractor);
    procedure FillUniqueConstraints;

  end;

implementation

{$R *.lfm}

uses Main, QueryWindow;

{ TfmUniqueConstraints }

procedure TfmUniqueConstraints.Init(ADBIndex: Integer; const ATableName: string;
  ANodeInfos: TPNodeInfos; AExtractor: TSimpleObjExtractor);
begin
  FDBIndex := ADBIndex;
  FTableName := ATableName;
  FNodeInfos := ANodeInfos;
  FExtractor := AExtractor;

  Caption := 'Unique Constraints: ' + FTableName;

  sgUniqueConstraints.Cells[0, 0] := 'Constraint Definition';
  sgUniqueConstraints.ColWidths[0] := 400;
end;

procedure TfmUniqueConstraints.LoadFields;
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

procedure TfmUniqueConstraints.LoadConstraintFields(const ConstraintName: string);
var
  ConstraintFields: TStringList;
  i, j: Integer;
  SQL: string;
  Qry: TIsolatedQuery;
begin
  // Alle Checkboxen deaktivieren
  for i := 0 to chkLstBoxFields.Count - 1 do
    chkLstBoxFields.Checked[i] := False;

  if ConstraintName = '' then
  begin
    edConstraintName.Text := '';
    Exit;
  end;

  // Constraint-Name ins Edit-Feld
  edConstraintName.Text := ConstraintName;

  // Felder des Constraints ermitteln
  SQL :=
    'SELECT TRIM(isg.RDB$FIELD_NAME) AS FIELD_NAME ' +
    'FROM RDB$RELATION_CONSTRAINTS rc ' +
    'JOIN RDB$INDEX_SEGMENTS isg ON rc.RDB$INDEX_NAME = isg.RDB$INDEX_NAME ' +
    'WHERE rc.RDB$RELATION_NAME = ' + QuotedStr(MakeCaseSensitiveAuto(FTableName)) + ' ' +
    'AND rc.RDB$CONSTRAINT_NAME = ' + QuotedStr(ConstraintName) + ' ' +
    'ORDER BY isg.RDB$FIELD_POSITION';

  Qry := TIsolatedQuery.Create(RegisteredDatabases[FDBIndex].IBDatabase, SQL);
  try
    while not Qry.Query.EOF do
    begin
      for i := 0 to chkLstBoxFields.Count - 1 do
      begin
        if SameText(chkLstBoxFields.Items[i], Trim(Qry.Query.FieldByName('FIELD_NAME').AsString)) then
        begin
          chkLstBoxFields.Checked[i] := True;
          Break;
        end;
      end;
      Qry.Query.Next;
    end;
  finally
    Qry.Free;
  end;
end;

procedure TfmUniqueConstraints.FillUniqueConstraints;
var
  Items: TStringList;
  i: Integer;
begin
  sgUniqueConstraints.RowCount := 1;

  Items := TStringList.Create;
  try
    FExtractor.Extract(otUniqueConstraints, FTableName, [], AlwaysQuoteIdentifiers, TStrings(Items));

    for i := 0 to Items.Count - 1 do
    begin
      sgUniqueConstraints.RowCount := i + 2;
      sgUniqueConstraints.Cells[0, i + 1] := Items[i];
    end;

  finally
    Items.Free;
  end;

  // Felder laden (nur beim ersten Mal)
  if chkLstBoxFields.Count = 0 then
    LoadFields;

  // Wenn Einträge vorhanden, ersten selektieren
  if sgUniqueConstraints.RowCount > 1 then
  begin
    sgUniqueConstraints.Row := 1;
    sgUniqueConstraintsSelection(nil, 0, 1);
  end
  else
  begin
    edConstraintName.Text := '';
    for i := 0 to chkLstBoxFields.Count - 1 do
      chkLstBoxFields.Checked[i] := False;
  end;

  // Button-Status
  bbEdit.Enabled := (sgUniqueConstraints.RowCount > 1);
  bbDrop.Enabled := (sgUniqueConstraints.RowCount > 1);
  bbNew.Enabled := True;
end;

procedure TfmUniqueConstraints.sgUniqueConstraintsSelection(Sender: TObject; aCol, aRow: Integer);
var
  Line: string;
  ColonPos: Integer;
  ConstraintName: string;
begin
  if (aRow > 0) and (aRow < sgUniqueConstraints.RowCount) then
  begin
    Line := sgUniqueConstraints.Cells[0, aRow];
    // Extrahiere Constraint-Namen aus "UK_NAME: UNIQUE (Feld1, Feld2)"
    ColonPos := Pos(':', Line);
    if ColonPos > 0 then
      ConstraintName := Trim(Copy(Line, 1, ColonPos - 1))
    else
      ConstraintName := '';

    LoadConstraintFields(ConstraintName);
  end;
end;

procedure TfmUniqueConstraints.chkLstBoxFieldsClickCheck(Sender: TObject);
var
  i: Integer;
  HasChecked: Boolean;
begin
  HasChecked := False;
  for i := 0 to chkLstBoxFields.Count - 1 do
  begin
    if chkLstBoxFields.Checked[i] then
    begin
      HasChecked := True;
      Break;
    end;
  end;

  // New: aktiv wenn Felder gecheckt UND Name nicht leer
  bbNew.Enabled := HasChecked and (Trim(edConstraintName.Text) <> '');
end;

procedure TfmUniqueConstraints.sgUniqueConstraintsClick(Sender: TObject);
begin

end;

procedure TfmUniqueConstraints.bbEditClick(Sender: TObject);
var
  QWindow: TfmQueryWindow;
  ConstraintName: string;
  CheckedFields: string;
  i: Integer;
  HasFields: Boolean;
  Line: string;
  ColonPos: Integer;
begin
  if sgUniqueConstraints.RowCount <= 1 then
    Exit;

  // Aktuellen Constraint-Namen ermitteln
  Line := sgUniqueConstraints.Cells[0, sgUniqueConstraints.Row];
  ColonPos := Pos(':', Line);
  if ColonPos > 0 then
    ConstraintName := Trim(Copy(Line, 1, ColonPos - 1))
  else
    ConstraintName := '';

  if ConstraintName = '' then
    Exit;

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
    ShowMessage('Please select at least one field for the Unique Constraint.');
    Exit;
  end;

  QWindow := fmMain.ShowQueryWindow(FDBIndex, 'Edit Unique Constraint: ' + ConstraintName);
  QWindow.meQuery.Lines.Clear;
  QWindow.meQuery.Lines.Add('SET TERM ^;');
  QWindow.meQuery.Lines.Add('');
  QWindow.meQuery.Lines.Add('-- Drop existing Unique Constraint');
  QWindow.meQuery.Lines.Add('ALTER TABLE ' + MakeCaseSensitiveAuto(FTableName) +
    ' DROP CONSTRAINT ' + ConstraintName + ' ^;');
  QWindow.meQuery.Lines.Add('');
  QWindow.meQuery.Lines.Add('-- Create new Unique Constraint');
  QWindow.meQuery.Lines.Add('ALTER TABLE ' + MakeCaseSensitiveAuto(FTableName) +
    ' ADD CONSTRAINT ' + ConstraintName + ' UNIQUE (' + CheckedFields + ') ^;');
  QWindow.meQuery.Lines.Add('');
  QWindow.meQuery.Lines.Add('SET TERM ;^');

  QWindow.OnCommit := @bbRefreshClick;
  QWindow.Show;
end;

procedure TfmUniqueConstraints.bbNewClick(Sender: TObject);
var
  QWindow: TfmQueryWindow;
  CheckedFields: string;
  i: Integer;
  NewName: string;
begin
  // Gewählte Felder aus CheckListBox sammeln
  CheckedFields := '';
  for i := 0 to chkLstBoxFields.Count - 1 do
  begin
    if chkLstBoxFields.Checked[i] then
    begin
      if CheckedFields <> '' then
        CheckedFields := CheckedFields + ', ';
      CheckedFields := CheckedFields + MakeCaseSensitiveAuto(chkLstBoxFields.Items[i]);
    end;
  end;

  if CheckedFields = '' then
  begin
    ShowMessage('Please select at least one field.');
    Exit;
  end;

  NewName := Trim(edConstraintName.Text);
  if NewName = '' then
  begin
    ShowMessage('Please enter a constraint name.');
    Exit;
  end;

  QWindow := fmMain.ShowQueryWindow(FDBIndex, 'New Unique Constraint: ' + NewName);
  QWindow.meQuery.Lines.Clear;
  QWindow.meQuery.Lines.Add('SET TERM ^;');
  QWindow.meQuery.Lines.Add('');
  QWindow.meQuery.Lines.Add('ALTER TABLE ' + MakeCaseSensitiveAuto(FTableName) +
    ' ADD CONSTRAINT ' + NewName + ' UNIQUE (' + CheckedFields + ') ^;');
  QWindow.meQuery.Lines.Add('');
  QWindow.meQuery.Lines.Add('SET TERM ;^');

  QWindow.OnCommit := @bbRefreshClick;
  QWindow.Show;
end;

procedure TfmUniqueConstraints.bbDropClick(Sender: TObject);
var
  QWindow: TfmQueryWindow;
  ConstraintName: string;
  Line: string;
  ColonPos: Integer;
begin
  if sgUniqueConstraints.RowCount > 1 then
  begin
    Line := sgUniqueConstraints.Cells[0, sgUniqueConstraints.Row];
    ColonPos := Pos(':', Line);
    if ColonPos > 0 then
      ConstraintName := Trim(Copy(Line, 1, ColonPos - 1))
    else
      ConstraintName := Line;

    if MessageDlg('Are you sure you want to drop Unique Constraint ' + ConstraintName + '?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      QWindow := fmMain.ShowQueryWindow(FDBIndex, 'Drop Unique Constraint: ' + ConstraintName);
      QWindow.meQuery.Lines.Clear;
      QWindow.meQuery.Lines.Add('SET TERM ^;');
      QWindow.meQuery.Lines.Add('');
      QWindow.meQuery.Lines.Add('ALTER TABLE ' + MakeCaseSensitiveAuto(FTableName) +
        ' DROP CONSTRAINT ' + ConstraintName + ' ^;');
      QWindow.meQuery.Lines.Add('');
      QWindow.meQuery.Lines.Add('SET TERM ;^');
      QWindow.OnCommit := @bbRefreshClick;
      QWindow.Show;
    end;
  end;
end;

procedure TfmUniqueConstraints.bbRefreshClick(Sender: TObject);
begin
  FillUniqueConstraints;
end;

procedure TfmUniqueConstraints.bbCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfmUniqueConstraints.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(Self);
  FillUniqueConstraints;
end;

end.
