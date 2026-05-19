unit CheckConstraints;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Grids,
  StdCtrls, Buttons,

  turbocommon,
  fbcommon,

  fsimpleobjextractor,
  fmetaquerys,

  uthemeselector;

type

  { TfmCheckConstraints }

  TfmCheckConstraints = class(TForm)
    bbClose: TButton;
    bbDrop: TBitBtn;
    bbEdit: TBitBtn;
    bbNew: TBitBtn;
    bbRefresh: TBitBtn;
    edConstraintName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    meCheckExpression: TMemo;
    pnlMiddle: TPanel;
    pnBottom: TPanel;
    pnlTop: TPanel;
    sgCheckConstraints: TStringGrid;

    procedure bbEditClick(Sender: TObject);
    procedure bbDropClick(Sender: TObject);
    procedure bbRefreshClick(Sender: TObject);
    procedure bbCloseClick(Sender: TObject);
    procedure bbNewClick(Sender: TObject);
    procedure sgCheckConstraintsSelection(Sender: TObject; aCol, aRow: Integer);
    procedure FormShow(Sender: TObject);

  private
    FDBIndex: Integer;
    FTableName: string;
    FNodeInfos: TPNodeInfos;
    FExtractor: TSimpleObjExtractor;

    procedure LoadConstraintDetail(const ConstraintName: string);

  public
    procedure Init(ADBIndex: Integer; const ATableName: string; ANodeInfos: TPNodeInfos; AExtractor: TSimpleObjExtractor);
    procedure FillCheckConstraints;

  end;

implementation

{$R *.lfm}

uses Main, QueryWindow;

{ TfmCheckConstraints }

procedure TfmCheckConstraints.Init(ADBIndex: Integer; const ATableName: string;
  ANodeInfos: TPNodeInfos; AExtractor: TSimpleObjExtractor);
begin
  FDBIndex := ADBIndex;
  FTableName := ATableName;
  FNodeInfos := ANodeInfos;
  FExtractor := AExtractor;

  Caption := 'Check Constraints: ' + FTableName;

  sgCheckConstraints.Cells[0, 0] := 'Constraint Definition';
  sgCheckConstraints.ColWidths[0] := 400;
end;

procedure TfmCheckConstraints.LoadConstraintDetail(const ConstraintName: string);
var
  SQL: string;
  Qry: TIsolatedQuery;
  RawSource: string;
begin
  edConstraintName.Text := '';
  meCheckExpression.Clear;

  if ConstraintName = '' then
    Exit;

  edConstraintName.Text := ConstraintName;

  SQL :=
    'SELECT TRIM(trg.RDB$TRIGGER_SOURCE) AS CHECK_SOURCE ' +
    'FROM RDB$RELATION_CONSTRAINTS rc ' +
    'JOIN RDB$CHECK_CONSTRAINTS cc ON rc.RDB$CONSTRAINT_NAME = cc.RDB$CONSTRAINT_NAME ' +
    'JOIN RDB$TRIGGERS trg ON cc.RDB$TRIGGER_NAME = trg.RDB$TRIGGER_NAME ' +
    'WHERE rc.RDB$RELATION_NAME = ' + QuotedStr(FTableName) + ' ' +
    'AND rc.RDB$CONSTRAINT_NAME = ' + QuotedStr(ConstraintName) + ' ' +
    'AND rc.RDB$CONSTRAINT_TYPE = ''CHECK'' ' +
    'ORDER BY trg.RDB$TRIGGER_NAME';

  Qry := TIsolatedQuery.Create(RegisteredDatabases[FDBIndex].IBDatabase, SQL);
  try
    if not Qry.Query.EOF then
    begin
      RawSource := Trim(Qry.Query.FieldByName('CHECK_SOURCE').AsString);

      if Pos('CHECK', UpperCase(RawSource)) = 1 then
      begin
        RawSource := Trim(Copy(RawSource, Length('CHECK') + 1, MaxInt));
        // Äußere Klammern entfernen falls vorhanden
        if (Length(RawSource) >= 2) and (RawSource[1] = '(') and (RawSource[Length(RawSource)] = ')') then
          RawSource := Trim(Copy(RawSource, 2, Length(RawSource) - 2));
      end;

      meCheckExpression.Text := RawSource;
    end;
  finally
    Qry.Free;
  end;
end;

procedure TfmCheckConstraints.FillCheckConstraints;
var
  Items: TStringList;
  i: Integer;
begin
  sgCheckConstraints.RowCount := 1;

  Items := TStringList.Create;
  try
    FExtractor.Extract(otCheckConstraints, FTableName, [], AlwaysQuoteIdentifiers, TStrings(Items));

    for i := 0 to Items.Count - 1 do
    begin
      sgCheckConstraints.RowCount := i + 2;
      sgCheckConstraints.Cells[0, i + 1] := Items[i];
    end;

  finally
    Items.Free;
  end;

  // Wenn Einträge vorhanden, ersten selektieren
  if sgCheckConstraints.RowCount > 1 then
  begin
    sgCheckConstraints.Row := 1;
    sgCheckConstraintsSelection(nil, 0, 1);
  end
  else
  begin
    edConstraintName.Text := '';
    meCheckExpression.Clear;
  end;

  // Button-Status
  bbEdit.Enabled := (sgCheckConstraints.RowCount > 1);
  bbDrop.Enabled := (sgCheckConstraints.RowCount > 1);
  bbNew.Enabled := True;
end;

procedure TfmCheckConstraints.sgCheckConstraintsSelection(Sender: TObject; aCol, aRow: Integer);
var
  Line: string;
  ColonPos: Integer;
  ConstraintName: string;
begin
  if (aRow > 0) and (aRow < sgCheckConstraints.RowCount) then
  begin
    Line := sgCheckConstraints.Cells[0, aRow];
    // Extrahiere Constraint-Namen aus "CHK_NAME: CHECK"
    ColonPos := Pos(':', Line);
    if ColonPos > 0 then
      ConstraintName := Trim(Copy(Line, 1, ColonPos - 1))
    else
      ConstraintName := '';

    LoadConstraintDetail(ConstraintName);
  end;
end;

procedure TfmCheckConstraints.bbEditClick(Sender: TObject);
var
  QWindow: TfmQueryWindow;
  ConstraintName: string;
  NewExpression: string;
  Line: string;
  ColonPos: Integer;
begin
  if sgCheckConstraints.RowCount <= 1 then
    Exit;

  // Aktuellen Constraint-Namen ermitteln
  Line := sgCheckConstraints.Cells[0, sgCheckConstraints.Row];
  ColonPos := Pos(':', Line);
  if ColonPos > 0 then
    ConstraintName := Trim(Copy(Line, 1, ColonPos - 1))
  else
    ConstraintName := '';

  if ConstraintName = '' then
    Exit;

  NewExpression := Trim(meCheckExpression.Text);
  if NewExpression = '' then
  begin
    ShowMessage('Please enter a check expression.');
    Exit;
  end;

  QWindow := fmMain.ShowQueryWindow(FDBIndex, 'Edit Check Constraint: ' + ConstraintName);
  QWindow.meQuery.Lines.Clear;
  QWindow.meQuery.Lines.Add('SET TERM ^;');
  QWindow.meQuery.Lines.Add('');
  QWindow.meQuery.Lines.Add('-- Drop existing Check Constraint');
  QWindow.meQuery.Lines.Add('ALTER TABLE ' + MakeCaseSensitiveAuto(FTableName) +
    ' DROP CONSTRAINT ' + ConstraintName + ' ^;');
  QWindow.meQuery.Lines.Add('');
  QWindow.meQuery.Lines.Add('-- Create new Check Constraint');
  QWindow.meQuery.Lines.Add('ALTER TABLE ' + MakeCaseSensitiveAuto(FTableName) +
    ' ADD CONSTRAINT ' + ConstraintName + ' CHECK (' + NewExpression + ') ^;');
  QWindow.meQuery.Lines.Add('');
  QWindow.meQuery.Lines.Add('SET TERM ;^');

  QWindow.OnCommit := @bbRefreshClick;
  QWindow.Show;
end;

procedure TfmCheckConstraints.bbNewClick(Sender: TObject);
var
  QWindow: TfmQueryWindow;
  NewName: string;
  NewExpression: string;
begin
  NewName := Trim(edConstraintName.Text);
  if NewName = '' then
  begin
    ShowMessage('Please enter a constraint name.');
    Exit;
  end;

  NewExpression := Trim(meCheckExpression.Text);
  if NewExpression = '' then
  begin
    ShowMessage('Please enter a check expression.');
    Exit;
  end;

  QWindow := fmMain.ShowQueryWindow(FDBIndex, 'New Check Constraint: ' + NewName);
  QWindow.meQuery.Lines.Clear;
  QWindow.meQuery.Lines.Add('SET TERM ^;');
  QWindow.meQuery.Lines.Add('');
  QWindow.meQuery.Lines.Add('ALTER TABLE ' + MakeCaseSensitiveAuto(FTableName) +
    ' ADD CONSTRAINT ' + NewName + ' CHECK (' + NewExpression + ') ^;');
  QWindow.meQuery.Lines.Add('');
  QWindow.meQuery.Lines.Add('SET TERM ;^');

  QWindow.OnCommit := @bbRefreshClick;
  QWindow.Show;
end;

procedure TfmCheckConstraints.bbDropClick(Sender: TObject);
var
  QWindow: TfmQueryWindow;
  ConstraintName: string;
  Line: string;
  ColonPos: Integer;
begin
  if sgCheckConstraints.RowCount > 1 then
  begin
    Line := sgCheckConstraints.Cells[0, sgCheckConstraints.Row];
    ColonPos := Pos(':', Line);
    if ColonPos > 0 then
      ConstraintName := Trim(Copy(Line, 1, ColonPos - 1))
    else
      ConstraintName := Line;

    if MessageDlg('Are you sure you want to drop Check Constraint ' + ConstraintName + '?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      QWindow := fmMain.ShowQueryWindow(FDBIndex, 'Drop Check Constraint: ' + ConstraintName);
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

procedure TfmCheckConstraints.bbRefreshClick(Sender: TObject);
begin
  FillCheckConstraints;
end;

procedure TfmCheckConstraints.bbCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfmCheckConstraints.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(Self);
  FillCheckConstraints;
end;

end.
