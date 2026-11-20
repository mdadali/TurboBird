unit CheckConstraints;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Grids,
  StdCtrls, Buttons, SynEdit, SynHighlighterSQL,

  IBDatabase, IBXScript,

  turbocommon,
  fbcommon,
  fsimpleobjextractor,
  uthemeselector,
  umessagedlg,
  confirm_query;

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
    function ExecuteSQL(const ASQL: string): Boolean;

  public
    procedure Init(ADBIndex: Integer; const ATableName: string;
                   ANodeInfos: TPNodeInfos; AExtractor: TSimpleObjExtractor);
    procedure FillCheckConstraints;

  end;

implementation

{$R *.lfm}

uses Main;

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
  RawSource: string;
begin
  ClearEditFields;

  if ConstraintName = '' then
    Exit;

  edConstraintName.Text := ConstraintName;

  // Ausdruck über den Extractor holen — funktioniert auf FB 1.5 bis 6
  RawSource := FExtractor.GetCheckConstraintSource(FTableName, ConstraintName);

  if RawSource = '' then
    Exit;

  // "CHECK (...)" → "(...)" entfernen, falls vorhanden
  if Pos('CHECK', UpperCase(RawSource)) = 1 then
  begin
    RawSource := Trim(Copy(RawSource, Length('CHECK') + 1, MaxInt));
    if (Length(RawSource) >= 2) and (RawSource[1] = '(') and (RawSource[Length(RawSource)] = ')') then
      RawSource := Trim(Copy(RawSource, 2, Length(RawSource) - 2));
  end;

  meCheckExpression.Text := RawSource;
end;

procedure TfmCheckConstraints.FillCheckConstraints;
var
  Items: TStringList;
  i: Integer;
begin
  sgCheckConstraints.RowCount := 1;

  Items := TStringList.Create;
  try
    FExtractor.Extract(otCheckConstraints, FTableName, [],
      AlwaysQuoteIdentifiers, TStrings(Items));

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
    ClearEditFields;

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
  FEditingConstraintName := '';
  UpdateButtonStates;
  edConstraintName.SetFocus;
end;

procedure TfmCheckConstraints.bbPostClick(Sender: TObject);
var
  ConstraintName: string;
  IsNew: Boolean;
  Script: string;
  DlgCaption: string;
  Expr: string;
begin
  ConstraintName := Trim(edConstraintName.Text);
  if ConstraintName = '' then
  begin
    MsgWarning('Please enter a constraint name.');
    Exit;
  end;

  Expr := Trim(meCheckExpression.Text);
  if Expr = '' then
  begin
    MsgWarning('Please enter a check expression.');
    Exit;
  end;

  IsNew := (FEditingConstraintName = '');

  Script := '';
  if not IsNew then
  begin
    Script := Script + '-- Drop existing Check Constraint' + LineEnding;
    Script := Script + 'ALTER TABLE ' + MakeCaseSensitiveAuto(FTableName) +
              ' DROP CONSTRAINT ' + MakeCaseSensitiveAuto(FEditingConstraintName) + ';' + LineEnding;
    Script := Script + LineEnding;
  end;

  if IsNew then
    Script := Script + '-- Create new Check Constraint' + LineEnding
  else
    Script := Script + '-- Create updated Check Constraint' + LineEnding;

  Script := Script +
    'ALTER TABLE ' + MakeCaseSensitiveAuto(FTableName) +
    ' ADD CONSTRAINT ' + MakeCaseSensitiveAuto(ConstraintName) +
    ' CHECK (' + LineEnding +
    meCheckExpression.Text + LineEnding +
    ');';

  if IsNew then
    DlgCaption := 'New Check Constraint: ' + ConstraintName
  else
    DlgCaption := 'Edit Check Constraint: ' + ConstraintName;

  if not TfrmConfirmQuery.Execute(DlgCaption, Script) then
    Exit;

  if ExecuteSQL(Script) then
  begin
    turbocommon.MetaDataChanged := True;
    FEditMode := False;
    FEditingConstraintName := '';
    ClearEditFields;
    FillCheckConstraints;
  end;
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
  edConstraintName.Enabled := False;
  meCheckExpression.SetFocus;
end;

procedure TfmCheckConstraints.bbDropClick(Sender: TObject);
var
  ConstraintName: string;
  Line: string;
  ColonPos: Integer;
  Script: string;
begin
  if sgCheckConstraints.RowCount <= 1 then
    Exit;

  Line := sgCheckConstraints.Cells[0, sgCheckConstraints.Row];
  ColonPos := Pos(':', Line);
  if ColonPos > 0 then
    ConstraintName := Trim(Copy(Line, 1, ColonPos - 1))
  else
    ConstraintName := Trim(Line);

  if ConstraintName = '' then
    Exit;

  if not MsgConfirm(
       'Are you sure you want to drop Check Constraint ' + ConstraintName + '?',
       mkWarning) then
    Exit;

  Script :=
    'ALTER TABLE ' + MakeCaseSensitiveAuto(FTableName) +
    ' DROP CONSTRAINT ' + MakeCaseSensitiveAuto(ConstraintName) + ';';

  if ExecuteSQL(Script) then
  begin
    turbocommon.MetaDataChanged := True;
    FillCheckConstraints;
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
  FillCheckConstraints;
  frmThemeSelector.btnApplyClick(Self);
end;

function TfmCheckConstraints.ExecuteSQL(const ASQL: string): Boolean;
var
  Tr: TIBTransaction;
  ScriptEngine: TIBXScript;
  Lines: TStringList;
begin
  Result := False;

  if not RegisteredDatabases[FDBIndex].IBDatabase.Connected then
  begin
    MsgError('Database is not connected.');
    Exit;
  end;

  Tr := TIBTransaction.Create(nil);
  ScriptEngine := TIBXScript.Create(nil);
  Lines := TStringList.Create;
  try
    Tr.DefaultDatabase := RegisteredDatabases[FDBIndex].IBDatabase;
    Tr.Params.Clear;
    Tr.Params.Assign(RegisteredDatabases[FDBIndex].IBTransaction.Params);

    ScriptEngine.Transaction := Tr;
    ScriptEngine.Database    := RegisteredDatabases[FDBIndex].IBDatabase;
    Lines.Text := ASQL;

    try
      if not Tr.InTransaction then
        Tr.StartTransaction;

      ScriptEngine.ExecSQLScript(Lines.Text);

      if Tr.InTransaction then
        Tr.Commit;

      Result := True;
    except
      on E: Exception do
      begin
        if Tr.InTransaction then
          Tr.Rollback;
        MsgError('Error while executing SQL:' + sLineBreak + sLineBreak + E.Message);
        Result := False;
      end;
    end;
  finally
    Lines.Free;
    ScriptEngine.Free;
    Tr.Free;
  end;
end;

end.
