unit CheckConstraints;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Grids,
  StdCtrls, Buttons, SynEdit, SynHighlighterSQL,

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
    bbPost: TBitBtn;
    bbRefresh: TBitBtn;
    edConstraintName: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    meCheckExpression: TSynEdit;
    pnlConstraintsName: TPanel;
    pnlMiddle: TPanel;
    pnBottom: TPanel;
    pnlTop: TPanel;
    sgCheckConstraints: TStringGrid;
    Splitter1: TSplitter;
    SynSQLSyn1: TSynSQLSyn;

    procedure bbEditClick(Sender: TObject);
    procedure bbDropClick(Sender: TObject);
    procedure bbNewClick(Sender: TObject);
    procedure bbPostClick(Sender: TObject);
    procedure bbRefreshClick(Sender: TObject);
    procedure bbCloseClick(Sender: TObject);
    procedure sgCheckConstraintsSelection(Sender: TObject; aCol, aRow: Integer);
    procedure FormShow(Sender: TObject);

  private
    FDBIndex: Integer;
    FTableName: string;
    FNodeInfos: TPNodeInfos;
    FExtractor: TSimpleObjExtractor;

    FEditMode: Boolean;
    FEditingConstraintName: string;

    procedure LoadConstraintDetail(const ConstraintName: string);
    procedure ClearEditFields;
    procedure UpdateButtonStates;

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

  FEditMode := False;
  FEditingConstraintName := '';

  Caption := 'Check Constraints: ' + FTableName;

  sgCheckConstraints.Cells[0, 0] := 'Constraint Definition';
  sgCheckConstraints.ColWidths[0] := 400;
end;

procedure TfmCheckConstraints.ClearEditFields;
begin
  edConstraintName.Text := '';
  meCheckExpression.Clear;
end;

procedure TfmCheckConstraints.UpdateButtonStates;
var
  HasSelection: Boolean;
  HasContent: Boolean;
begin
  HasSelection := (sgCheckConstraints.RowCount > 1) and (sgCheckConstraints.Row > 0);
  HasContent := (Trim(edConstraintName.Text) <> '') and (Trim(meCheckExpression.Text) <> '');

  if FEditMode then
  begin
    bbNew.Enabled := False;
    bbEdit.Enabled := False;
    bbDrop.Enabled := False;
    bbPost.Enabled := HasContent;

    // Name nur bei New änderbar, nicht bei Edit
    edConstraintName.Enabled := (FEditingConstraintName = '');
    meCheckExpression.Enabled := True;

    if edConstraintName.Enabled then
      edConstraintName.Color := clWindow
    else
      edConstraintName.Color := clBtnFace;
    meCheckExpression.Color := clWindow;
  end
  else
  begin
    bbNew.Enabled := True;
    bbEdit.Enabled := HasSelection;
    bbDrop.Enabled := HasSelection;
    bbPost.Enabled := False;
    edConstraintName.Enabled := False;
    meCheckExpression.Enabled := False;
    edConstraintName.Color := clBtnFace;
    meCheckExpression.Color := clBtnFace;
  end;
end;

procedure TfmCheckConstraints.LoadConstraintDetail(const ConstraintName: string);
var
  SQL: string;
  Qry: TIsolatedQuery;
  RawSource: string;
begin
  ClearEditFields;

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

      // "CHECK (...)" → "(...)" entfernen
      if Pos('CHECK', UpperCase(RawSource)) = 1 then
      begin
        RawSource := Trim(Copy(RawSource, Length('CHECK') + 1, MaxInt));
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

  // Ersten Eintrag selektieren, falls vorhanden
  if sgCheckConstraints.RowCount > 1 then
  begin
    sgCheckConstraints.Row := 1;
    sgCheckConstraintsSelection(nil, 0, 1);
  end
  else
  begin
    ClearEditFields;
  end;

  // Immer in den Normalmodus zurücksetzen
  FEditMode := False;
  FEditingConstraintName := '';
  UpdateButtonStates;
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
    ColonPos := Pos(':', Line);
    if ColonPos > 0 then
      ConstraintName := Trim(Copy(Line, 1, ColonPos - 1))
    else
      ConstraintName := '';

    LoadConstraintDetail(ConstraintName);

    FEditMode := False;
    FEditingConstraintName := '';
    UpdateButtonStates;
  end;
end;

procedure TfmCheckConstraints.bbNewClick(Sender: TObject);
begin
  ClearEditFields;
  edConstraintName.Text := 'CHK_' + FTableName + '_' + IntToStr(sgCheckConstraints.RowCount);
  meCheckExpression.Text := '-- Enter check condition, e.g. FIELD_NAME > 0';
  FEditMode := True;
  FEditingConstraintName := '';   // New = kein existierender Name
  UpdateButtonStates;
  edConstraintName.SetFocus;
end;

procedure TfmCheckConstraints.bbPostClick(Sender: TObject);
var
  QWindow: TfmQueryWindow;
  ConstraintName: string;
  IsNew: Boolean;
begin
  ConstraintName := Trim(edConstraintName.Text);
  if ConstraintName = '' then
  begin
    ShowMessage('Please enter a constraint name.');
    Exit;
  end;

  if Trim(meCheckExpression.Text) = '' then
  begin
    ShowMessage('Please enter a check expression.');
    Exit;
  end;

  IsNew := (FEditingConstraintName = '');

  if IsNew then
    QWindow := fmMain.ShowQueryWindow(FDBIndex, 'New Check Constraint: ' + ConstraintName)
  else
    QWindow := fmMain.ShowQueryWindow(FDBIndex, 'Edit Check Constraint: ' + ConstraintName);  QWindow.meQuery.Lines.Clear;

  QWindow.meQuery.Lines.Add('SET TERM ^;');
  QWindow.meQuery.Lines.Add('');

  if not IsNew then
  begin
    QWindow.meQuery.Lines.Add('-- Drop existing Check Constraint');
    QWindow.meQuery.Lines.Add('ALTER TABLE ' + MakeCaseSensitiveAuto(FTableName) +
      ' DROP CONSTRAINT ' + FEditingConstraintName + ' ^;');
    QWindow.meQuery.Lines.Add('');
  end;

  if IsNew then
    QWindow.meQuery.Lines.Add('-- Create new Check Constraint')
  else
    QWindow.meQuery.Lines.Add('-- Create updated Check Constraint');

  QWindow.meQuery.Lines.Add('ALTER TABLE ' + MakeCaseSensitiveAuto(FTableName) +
    ' ADD CONSTRAINT ' + ConstraintName + ' CHECK (');
  QWindow.meQuery.Lines.AddStrings(meCheckExpression.Lines);
  QWindow.meQuery.Lines.Add(') ^;');
  QWindow.meQuery.Lines.Add('');
  QWindow.meQuery.Lines.Add('SET TERM ;^');

  QWindow.OnCommit := @bbRefreshClick;
  QWindow.Show;

  // Zurücksetzen
  FEditMode := False;
  FEditingConstraintName := '';
  ClearEditFields;
  FillCheckConstraints;
end;

procedure TfmCheckConstraints.bbEditClick(Sender: TObject);
var
  Line: string;
  ColonPos: Integer;
begin
  if sgCheckConstraints.RowCount <= 1 then Exit;

  Line := sgCheckConstraints.Cells[0, sgCheckConstraints.Row];
  ColonPos := Pos(':', Line);
  if ColonPos > 0 then
    FEditingConstraintName := Trim(Copy(Line, 1, ColonPos - 1))
  else
    FEditingConstraintName := '';

  if FEditingConstraintName = '' then Exit;

  FEditMode := True;
  UpdateButtonStates;
  edConstraintName.Enabled := False;   // Name nicht änderbar
  meCheckExpression.SetFocus;
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
  //FillCheckConstraints;
end;

end.
