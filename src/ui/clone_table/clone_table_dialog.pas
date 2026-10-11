unit clone_table_dialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Grids, CheckLst, Menus, IB, IBQuery, IBDatabase, IBDatabaseInfo,
  IBExtract, ibxscript,  IBSQL,

  SysTables,
  turbocommon,
  fbcommon,

  fsimpleobjextractor,

  uCopyTableDataLocal,
  uCopyTableDataCrossExecuteBlock,
  ucopytabledatarowbyrow,
  uCopyTableDataFBIntf,

  uFormulaPresets,
  uProblemFieldsDialog,

  uthemeselector,

  uReport,
  uSystemInfo
  ;


type

  TFieldInfo = record
    FieldName: string;
    FieldType: string;
    IsComputed: Boolean;
    Checked: Boolean;
    Formula: string;
  end;

  { TfrmCloneTable }

  TfrmCloneTable = class(TForm)
    btnAddToQueue: TButton;
    btnCancelGlobal: TButton;
    btnDeselectAll: TButton;
    btnExecute: TButton;
    btnExternalFile: TButton;
    btnGenTestFormulas: TButton;
    btnNewDB: TButton;
    btnOpenExternalFile: TButton;
    btnPreviewSQL: TButton;
    btnRefreshPresets: TButton;
    btnSelectAll: TButton;
    cbFormulaPreset: TComboBox;
    chkFBIntfForceRow: TCheckBox;
    chkboxExternalTable: TCheckBox;
    chkCopyData: TCheckBox;
    chkCreateTable: TCheckBox;
    chkLstFields: TCheckListBox;
    chkUseFormula: TCheckBox;
    comboxDestDB: TComboBox;
    comboxDestServer: TComboBox;
    comboxSourceDB: TComboBox;
    comboxSourceServer: TComboBox;
    comboxSourceTables: TComboBox;
    Destination: TGroupBox;
    edtFBIntfRowByRowCommit: TEdit;
    edtFBIntfMemory: TEdit;
    edtlRowByRowBatchSize: TEdit;
    edtExecBlockBatch: TEdit;
    edtLocalBatchSize: TEdit;
    edtDestTable: TEdit;
    edtExternalFile: TEdit;
    edtFrom: TEdit;
    edtTo: TEdit;
    grBoxCopyMethod: TGroupBox;
    grboxCopyOptions: TGroupBox;
    grboxFields: TGroupBox;
    grBoxFormulaFields: TGroupBox;
    grBoxFormulaPresets: TGroupBox;
    grBoxSource: TGroupBox;
    gbLocalEngine: TGroupBox;
    gbExecBlockEngine: TGroupBox;
    gbRowByRowEngine: TGroupBox;
    gbFBIntfEngine: TGroupBox;
    IBDBDest: TIBDatabase;
    IBDBSource: TIBDatabase;
    IBQueryDest: TIBQuery;
    IBQuerySource: TIBQuery;
    IBTransDest: TIBTransaction;
    IBTransSource: TIBTransaction;
    IBXScript1: TIBXScript;
    lbFBIntfRowByRowCommit: TLabel;
    lblFBIntfStrategy: TLabel;
    lblFBIntfMemoryRange: TLabel;
    lblFBIntfMemory: TLabel;
    lblRowByRowBatchSize: TLabel;
    lblRowByRowInfo: TLabel;
    lblExecBlockInfo: TLabel;
    lblExecBlockBatch: TLabel;
    lblLocalBatchInfo: TLabel;
    lblLocalBatchSize: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    lbSourceTable: TLabel;
    OpenDialog1: TOpenDialog;
    PageControlMain: TPageControl;
    Panel1: TPanel;
    pnlFieldsSelectButtons: TPanel;
    pnlSelector: TPanel;
    PopupMenu1: TPopupMenu;
    rbAllRows: TRadioButton;
    rbExecuteBlock: TRadioButton;
    rbFBIntf: TRadioButton;
    rbInsertSelect: TRadioButton;
    rbRange: TRadioButton;
    rbRowByRow: TRadioButton;
    sgFields: TStringGrid;
    StatusBar1: TStatusBar;
    tsSelection: TTabSheet;
    tsEngineSettings: TTabSheet;
    procedure btnAddToQueueClick(Sender: TObject);
    procedure btnCancelGlobalClick(Sender: TObject);
    procedure btnExecuteClick(Sender: TObject);
    procedure btnExternalFileClick(Sender: TObject);
    procedure btnGenTestFormulasClick(Sender: TObject);
    procedure btnRefreshPresetsClick(Sender: TObject);
    procedure btnSelectAllClick(Sender: TObject);
    procedure btnDeselectAllClick(Sender: TObject);
    procedure btnPreviewSQLClick(Sender: TObject);
    procedure cbFormulaPresetChange(Sender: TObject);
    procedure chkboxExternalTableChange(Sender: TObject);
    procedure chkLstFieldsClickCheck(Sender: TObject);
    procedure chkUseFormulaChange(Sender: TObject);
    procedure comboxDestDBChange(Sender: TObject);
    procedure comboxDestServerChange(Sender: TObject);
    procedure comboxSourceDBChange(Sender: TObject);
    procedure comboxSourceServerChange(Sender: TObject);
    procedure comboxSourceTablesChange(Sender: TObject);
    procedure edtExecBlockBatchExit(Sender: TObject);
    procedure edtLocalBatchSizeExit(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbAllRowsChange(Sender: TObject);
    procedure sgFieldsDblClick(Sender: TObject);

    procedure rbCopyMethodChange(Sender: TObject);
  private
    FNodeInfos: TPNodeInfos;
    FSourceDBIndex: Integer;
    FDestDBIndex: Integer;
    FFields: array of TFieldInfo;
    FUpdatingCombos: Boolean;

    FInitialTableName: string;
    FInitialDBIndex: Integer;

    FExtractor: TSimpleObjExtractor;
    FExtractorDBIndex: Integer;

    function GetLocalBatchSize: Integer;
    function GetExecBlockBatchSize: Integer;
    function GetFBIntfRowByRowCommit: Integer;
    function GetRowByRowCommitInterval: Integer;
    function GetFBIntfMemoryMB: Integer;


    procedure UpdateEngineStatus;
    procedure EnsureExtractor(ADBIndex: Integer);

    procedure ApplyFieldStates;

    function  CheckFieldCompatibility(AForCopy: Boolean = True): Boolean;
    function  ShowProblemDialog(const AMessage: string): Integer;

    procedure ApplyInitialSelection;
    function  FillSourceServerCombo: boolean;
    function  FillSourceDBCombo: boolean;
    function  FillSourceTableCombo: boolean;
    procedure FillSourceCombos;

    function  FillDestServerCombo: boolean;
    function  FillDestDBCombo: boolean;
    procedure FillDestCombos;

    function ConfigureSourceConnection: boolean;
    function ConfigureDestConnection: boolean;

    procedure LoadFields;
    function GetFieldTransforms: TFieldTransformArray;
    function GetProblemFieldTransforms: TFieldTransformArray;
    function GenerateCreateTableSQL: string;
    function GenerateInsertSQL: string;
    function GenerateCreateExternalTableSQL: string;
    function CreateExternalDestTable(DestDB: TIBDatabase;
                                     DestTrans: TIBTransaction; TableName: string): Boolean;

    function TableExists(DB: TIBDatabase; TableName: string): Boolean;
    function CreateDestTable(DestDB: TIBDatabase; DestTrans: TIBTransaction; TableName: string): Boolean;

    function GetMissingDestFields(DB: TIBDatabase;
      const ATableName: string): TStringList;
    procedure DeselectFields(AMissingFields: TStringList);
    function ShowStructureDriftDialog(const ATableName: string;
      AMissingFields: TStringList; AAllowDrop: Boolean): Integer;
    function DropDestTable(DB: TIBDatabase; Trans: TIBTransaction;
      const ATableName: string): Boolean;

    procedure HighlightActiveEngineGroup;
  public
    procedure Init(ANodeInfos: TPNodeInfos; const ATableName: string);
    procedure LoadFormulaPresets;
    procedure UpdateCopyMethodAvailability;
  end;

var
  frmCloneTable: TfrmCloneTable;

implementation

{$R *.lfm}

{ TfrmCloneTable }

function TfrmCloneTable.GetFBIntfRowByRowCommit: Integer;
begin
  Result := StrToIntDef(Trim(edtFBIntfRowByRowCommit.Text), 2000000);
end;

function TfrmCloneTable.GetRowByRowCommitInterval: Integer;
begin
  Result := StrToIntDef(Trim(edtlRowByRowBatchSize.Text), 2000000);
end;

function TfrmCloneTable.GetExecBlockBatchSize: Integer;
begin
  Result := StrToIntDef(Trim(edtExecBlockBatch.Text), 10000);
end;

function TfrmCloneTable.GetLocalBatchSize: Integer;
begin
  Result := StrToIntDef(Trim(edtLocalBatchSize.Text), 500000);
end;

function TfrmCloneTable.GetFBIntfMemoryMB: Integer;
begin
  Result := StrToIntDef(Trim(edtFBIntfMemory.Text), 256);
  if Result < 16  then Result := 16;    // FBIntf-Minimum
  if Result > 256 then Result := 256;   // FBIntf-Cap (MWA Guide 1.12)
end;

procedure TfrmCloneTable.UpdateEngineStatus;
begin
  if rbInsertSelect.Checked then
    StatusBar1.SimpleText := Format('INSERT...SELECT — Batch Size: %s rows',
      [FormatFloat('#,##0', GetLocalBatchSize)])

  else if rbExecuteBlock.Checked then
    StatusBar1.SimpleText := Format('EXECUTE BLOCK — Batch Size: %s rows',
      [FormatFloat('#,##0', GetExecBlockBatchSize)])

  else if rbRowByRow.Checked then
    StatusBar1.SimpleText := Format('Row-by-Row (IBX) — Commit every %s rows',
      [FormatFloat('#,##0', GetRowByRowCommitInterval)])

  else if rbFBIntf.Checked then
    StatusBar1.SimpleText := Format('Firebird API (FBIntf) — Batch Memory: %s MB',
      [FormatFloat('#,##0', GetFBIntfMemoryMB)]);
end;

procedure TfrmCloneTable.rbCopyMethodChange(Sender: TObject);
begin
  HighlightActiveEngineGroup;
end;

procedure TfrmCloneTable.HighlightActiveEngineGroup;
const
  ACTIVE_COLOR   = clSkyBlue;   // oder clBtnFace für neutral
  INACTIVE_COLOR = clBtnFace;
begin
  // Alle GroupBoxen zurück auf Neutral
  gbLocalEngine.Color   := INACTIVE_COLOR;
  gbExecBlockEngine.Color := INACTIVE_COLOR;
  gbRowByRowEngine.Color  := INACTIVE_COLOR;
  gbFBIntfEngine.Color    := INACTIVE_COLOR;

  // Aktive GroupBox hervorheben
  if rbInsertSelect.Checked then
    gbLocalEngine.Color := ACTIVE_COLOR
  else if rbExecuteBlock.Checked then
    gbExecBlockEngine.Color := ACTIVE_COLOR
  else if rbRowByRow.Checked then
    gbRowByRowEngine.Color := ACTIVE_COLOR
  else if rbFBIntf.Checked then
    gbFBIntfEngine.Color := ACTIVE_COLOR;
end;

// ============================================================
// Ermittelt Felder, die in der Zieltabelle fehlen.
// Nur Felder, die tatsächlich eingefügt werden (checked, nicht computed).
// ============================================================
function TfrmCloneTable.GetMissingDestFields(DB: TIBDatabase;
  const ATableName: string): TStringList;
var
  Q: TIBQuery;
  i: Integer;
  FieldName: string;
begin
  Result := TStringList.Create;

  // 1. Alle zu kopierenden Felder sammeln
  for i := 0 to High(FFields) do
  begin
    if i >= chkLstFields.Count then Break;
    if not chkLstFields.Checked[i] then Continue;
    if FFields[i].IsComputed then Continue;
    Result.Add(FFields[i].FieldName);
  end;

  // 2. Mit tatsächlichen Spalten der Zieltabelle abgleichen
  Q := TIBQuery.Create(nil);
  try
    Q.Database := DB;
    Q.Transaction := DB.DefaultTransaction;
    Q.AllowAutoActivateTransaction := True;
    Q.SQL.Text :=
      'SELECT RDB$FIELD_NAME FROM RDB$RELATION_FIELDS ' +
      'WHERE RDB$RELATION_NAME = :T';
    Q.ParamByName('T').AsString := StripIdentifierQuotes(ATableName);
    Q.Open;

    while not Q.EOF do
    begin
      FieldName := Trim(Q.FieldByName('RDB$FIELD_NAME').AsString);
      i := Result.IndexOf(FieldName);
      if i >= 0 then
        Result.Delete(i);
      Q.Next;
    end;
    Q.Close;
  finally
    Q.Free;
  end;
end;

// ============================================================
// Wählt fehlende Felder im Hauptformular ab.
// ApplyFieldStates muss danach vom Aufrufer kommen.
// ============================================================
procedure TfrmCloneTable.DeselectFields(AMissingFields: TStringList);
var
  i: Integer;
begin
  for i := 0 to chkLstFields.Count - 1 do
    if AMissingFields.IndexOf(chkLstFields.Items[i]) >= 0 then
      chkLstFields.Checked[i] := False;
end;

// ============================================================
// Dialog mit 4 Optionen: Drop / Deselect / Continue / Cancel
// mrYes    = Drop & Recreate
// mrNo     = Deselect Missing
// mrIgnore = Continue Anyway
// mrCancel = Cancel
// ============================================================
function TfrmCloneTable.ShowStructureDriftDialog(
  const ATableName: string;
  AMissingFields: TStringList;
  AAllowDrop: Boolean): Integer;
var
  Dlg: TForm;
  Memo: TMemo;
  BtnDrop, BtnDeselect, BtnContinue, btnCancel: TButton;
  i: Integer;
begin
  Dlg := TForm.Create(nil);
  try
    Dlg.Caption := 'Destination Table Structure Warning';
    Dlg.Width := 620;
    Dlg.Height := 420;
    Dlg.Position := poScreenCenter;
    Dlg.BorderStyle := bsDialog;

    Memo := TMemo.Create(Dlg);
    Memo.Parent := Dlg;
    Memo.Left := 16;
    Memo.Top := 16;
    Memo.Width := 572;
    Memo.Height := 280;
    Memo.ReadOnly := True;
    Memo.ScrollBars := ssVertical;
    Memo.Lines.Add('The destination table "' + ATableName + '" already exists,');
    Memo.Lines.Add('but the following columns are missing in the destination:');
    Memo.Lines.Add('');
    for i := 0 to AMissingFields.Count - 1 do
      Memo.Lines.Add('  • ' + AMissingFields[i]);
    Memo.Lines.Add('');
    Memo.Lines.Add('The copy would fail when trying to insert them.');
    Memo.Lines.Add('');
    Memo.Lines.Add('What do you want to do?');

    // Zeile 1 — Drop & Deselect
    BtnDrop := TButton.Create(Dlg);
    BtnDrop.Parent := Dlg;
    BtnDrop.Caption := 'Drop && Recreate';
    BtnDrop.Left := 16;
    BtnDrop.Top := 310;
    BtnDrop.Width := 280;
    BtnDrop.ModalResult := mrYes;
    BtnDrop.Enabled := AAllowDrop;
    if not AAllowDrop then
      BtnDrop.Hint := 'Enable "Create Destination Table" to use this option';

    BtnDeselect := TButton.Create(Dlg);
    BtnDeselect.Parent := Dlg;
    BtnDeselect.Caption := 'Deselect Missing';
    BtnDeselect.Left := 308;
    BtnDeselect.Top := 310;
    BtnDeselect.Width := 280;
    BtnDeselect.ModalResult := mrNo;

    // Zeile 2 — Continue & Cancel
    BtnContinue := TButton.Create(Dlg);
    BtnContinue.Parent := Dlg;
    BtnContinue.Caption := 'Continue Anyway';
    BtnContinue.Left := 16;
    BtnContinue.Top := 350;
    BtnContinue.Width := 280;
    BtnContinue.ModalResult := mrIgnore;

    btnCancelGlobal := TButton.Create(Dlg);
    btnCancelGlobal.Parent := Dlg;
    btnCancelGlobal.Caption := 'Cancel';
    btnCancelGlobal.Left := 308;
    btnCancelGlobal.Top := 350;
    btnCancelGlobal.Width := 280;
    btnCancelGlobal.ModalResult := mrCancel;

    Result := Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

{// ============================================================
// DROP TABLE — für „Drop & Recreate"
// ============================================================
function TfrmCloneTable.DropDestTable(DB: TIBDatabase;
  Trans: TIBTransaction; const ATableName: string): Boolean;
var
  Q: TIBSQL;
begin
  Result := False;
  Q := TIBSQL.Create(DB);
  try
    Q.Transaction := Trans;
    if not Trans.InTransaction then
      Trans.StartTransaction;
    Q.SQL.Text := 'DROP TABLE ' +
      MakeCaseSensitiveAuto(StripIdentifierQuotes(ATableName));
    Q.Open;
    Trans.Commit;
    Result := True;
  except
    on E: Exception do
    begin
      if Trans.InTransaction then
        Trans.Rollback;
      MessageDlg('Could not drop table:' + sLineBreak + E.Message,
        mtError, [mbOK], 0);
    end;
  end;
  Q.Free;
end;}

// ============================================================
// DROP TABLE — für „Drop & Recreate"
//
// Wichtig:
//   * DDL-Statements brauchen ExecQuery (nicht Open!)
//   * Nach dem Drop wird verifiziert, dass die Tabelle wirklich weg ist
//   * IBX kann DDL nur auf einer aktiven Transaktion ausführen
// ============================================================
function TfrmCloneTable.DropDestTable(DB: TIBDatabase;
  Trans: TIBTransaction; const ATableName: string): Boolean;
var
  Q: TIBSQL;
  CleanName: string;
begin
  Result := False;

  if (DB = nil) or (Trans = nil) then
  begin
    MessageDlg('DropDestTable: DB or Transaction is nil.',
      mtError, [mbOK], 0);
    Exit;
  end;

  CleanName := StripIdentifierQuotes(ATableName);
  if Trim(CleanName) = '' then
  begin
    MessageDlg('DropDestTable: Table name is empty.',
      mtError, [mbOK], 0);
    Exit;
  end;

  Q := TIBSQL.Create(nil);
  try
    Q.Database := DB;
    Q.Transaction := Trans;

    // Transaktion sicherstellen
    if not Trans.InTransaction then
      Trans.StartTransaction;

    Q.SQL.Text := 'DROP TABLE ' + MakeCaseSensitiveAuto(CleanName);

    // ============================================================
    // WICHTIG: ExecQuery (nicht Open!) — Open ist für SELECT
    // und führt DDL-Statements NICHT aus.
    // ============================================================
    Q.ExecQuery;

    Trans.Commit;

    // ============================================================
    // Verifikation: Ist die Tabelle wirklich weg?
    // ============================================================
    if TableExists(DB, ATableName) then
    begin
      MessageDlg(
        'Drop was reported as successful, but the table still exists:' +
        sLineBreak + sLineBreak +
        '  ' + ATableName + sLineBreak + sLineBreak +
        'The copy will be aborted to avoid data corruption.',
        mtError, [mbOK], 0);
      Exit;   // Result bleibt False
    end;

    Result := True;

  except
    on E: Exception do
    begin
      if Trans.InTransaction then
        Trans.Rollback;

      MessageDlg(
        'Could not drop table "' + ATableName + '":' +
        sLineBreak + sLineBreak +
        E.Message,
        mtError, [mbOK], 0);
    end;
  end;

  Q.Free;
end;

// ============================================================
// Synchronisiert FFields[i].Checked und sgFields.Cells[0, i+1]
// mit chkLstFields.Checked[] — eine Richtung, eine Quelle der Wahrheit.
// ============================================================
procedure TfrmCloneTable.ApplyFieldStates;
var
  i: Integer;
begin
  for i := 0 to High(FFields) do
  begin
    if i >= chkLstFields.Count then Break;

    FFields[i].Checked := chkLstFields.Checked[i];

    if FFields[i].Checked then
      sgFields.Cells[0, i + 1] := '1'
    else
      sgFields.Cells[0, i + 1] := '0';
  end;
end;

procedure TfrmCloneTable.EnsureExtractor(ADBIndex: Integer);
begin
  if Assigned(FExtractor) and (FExtractorDBIndex = ADBIndex) then
    Exit;

  if Assigned(FExtractor) then
    FreeAndNil(FExtractor);

  FExtractor := TSimpleObjExtractor.Create(ADBIndex);
  FExtractorDBIndex := ADBIndex;
end;

// ============================================================================
// FIELD COMPATIBILITY
// ============================================================================

function TfrmCloneTable.ShowProblemDialog(const AMessage: string): Integer;
var
  Dlg: TForm;
  Btn: TButton;
  i: Integer;
  YesBtn, NoBtn, CancelBtn: TButton;
begin
  Result := mrCancel;

  Dlg := CreateMessageDialog(AMessage, mtWarning, [mbYes, mbNo, mbCancel]);
  try
    Dlg.Caption := 'CloneTable — Problem Fields';
    Dlg.Width := Dlg.Width + 200;

    YesBtn := nil;
    NoBtn := nil;
    CancelBtn := nil;

    for i := 0 to Dlg.ComponentCount - 1 do
    begin
      if Dlg.Components[i] is TButton then
      begin
        Btn := TButton(Dlg.Components[i]);
        if SameText(Btn.Name, 'Yes') then YesBtn := Btn
        else if SameText(Btn.Name, 'No') then NoBtn := Btn
        else if SameText(Btn.Name, 'Cancel') then CancelBtn := Btn;
      end;
    end;

    if Assigned(YesBtn) then
    begin
      YesBtn.Caption := 'Deselect && Continue';
      YesBtn.Width := 160;
    end;
    if Assigned(NoBtn) then
    begin
      NoBtn.Caption := 'Continue Anyway';
      NoBtn.Width := 160;
    end;
    if Assigned(CancelBtn) then
    begin
      CancelBtn.Caption := 'Cancel';
      CancelBtn.Width := 100;
    end;

    Result := Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

function TfrmCloneTable.CheckFieldCompatibility(AForCopy: Boolean = True): Boolean;
var
  AMajor, AMinor: Word;
  AVersion: Word;
  SupportedTypes: TStringList;
  ProblemFields: TProblemFieldArray;
  KeptNames: TStringList;
  Dlg: TfrmProblemDialog;
  i, j: Integer;
  NeedsMethodCheck: Boolean;
  S, PrecisionStr: string;
  P1, P2, Precision: Integer;
  IsFB4Only: Boolean;
  IsPrecisionHigh: Boolean;
  Category: TProblemCategory;
  HasProblem: Boolean;
  HasAnyProblem: Boolean;
begin
  Result := False;

  SetLength(ProblemFields, 0);
  SupportedTypes := TStringList.Create;
  try
    ParseFBVersionString(
      RegisteredDatabases[FDestDBIndex].RegRec.ServerVersionString,
      AMajor, AMinor);
    AVersion := AMajor * 10 + AMinor;

    GetDataTypesByFBVersion(AVersion, SupportedTypes);

    NeedsMethodCheck := AForCopy and
                        (chkboxExternalTable.Checked or
                         rbExecuteBlock.Checked or
                         rbRowByRow.Checked);

    HasAnyProblem := False;

    for i := 0 to High(FFields) do
    begin
      if not chkLstFields.Checked[i] then Continue;

      HasProblem := False;
      Category := pcNotSupported;
      S := UpperCase(FFields[i].FieldType);

      // ============================================================
      // Sektion 1: Not supported (Array, BLOB bei bestimmten Methoden,
      //            Computed bei Execute Block / External)
      // ============================================================
      // ============================================================
      // Sektion 1: Not supported (Array, BLOB bei bestimmten Methoden,
      //            Computed bei Execute Block / External)
      // ============================================================
      if NeedsMethodCheck then
      begin
        if Pos('[', FFields[i].FieldType) > 0 then
        begin
          HasProblem := True;
          Category := pcNotSupported;
        end
        else if (chkboxExternalTable.Checked or rbExecuteBlock.Checked) and
                (FFields[i].IsComputed or (Pos('BLOB', S) > 0)) then
        begin
          HasProblem := True;
          Category := pcNotSupported;
        end;
      end;

      // ============================================================
      // Sektion 2: May overflow (nur bei Execute Block Cross-DB)
      // ============================================================
      if (not HasProblem) and
         ((FSourceDBIndex <> FDestDBIndex) and rbExecuteBlock.Checked) then
      begin
        IsFB4Only := False;
        IsPrecisionHigh := False;

        if (Pos('INT128', S) > 0) or (Pos('DECFLOAT', S) > 0) or
           (Pos('WITH TIME ZONE', S) > 0) then
        begin
          if AMajor < 4 then
            IsFB4Only := True;
          // AMajor >= 4: kein Problem
        end;

        if (Pos('NUMERIC', S) > 0) or (Pos('DECIMAL', S) > 0) then
        begin
          P1 := Pos('(', S);
          if P1 > 0 then
          begin
            P2 := Pos(',', S);
            if P2 > P1 then
              PrecisionStr := Trim(Copy(S, P1 + 1, P2 - P1 - 1))
            else
            begin
              P2 := Pos(')', S);
              if P2 > P1 then
                PrecisionStr := Trim(Copy(S, P1 + 1, P2 - P1 - 1))
              else
                PrecisionStr := '';
            end;

            if TryStrToInt(PrecisionStr, Precision) then
            begin
              if (AMajor < 4) and (Precision > 18) then
                IsFB4Only := True;
            end;
          end;
        end;

        if IsFB4Only then
        begin
          HasProblem := True;
          Category := pcNotSupported;
        end
        else if IsPrecisionHigh then
        begin
          HasProblem := True;
          Category := pcMayOverflow;
        end;
      end;

      // ============================================================
      // Sektion 3: IBX conversion (nur bei Row-by-Row)
      // ============================================================
      if (not HasProblem) and rbRowByRow.Checked then
      begin
        IsPrecisionHigh := False;

        if (Pos('INT128', S) > 0) or (Pos('DECFLOAT', S) > 0) then
          IsPrecisionHigh := True;

        if (Pos('NUMERIC', S) > 0) or (Pos('DECIMAL', S) > 0) then
        begin
          P1 := Pos('(', S);
          if P1 > 0 then
          begin
            P2 := Pos(',', S);
            if P2 > P1 then
              PrecisionStr := Trim(Copy(S, P1 + 1, P2 - P1 - 1))
            else
            begin
              P2 := Pos(')', S);
              if P2 > P1 then
                PrecisionStr := Trim(Copy(S, P1 + 1, P2 - P1 - 1))
              else
                PrecisionStr := '';
            end;

            if TryStrToInt(PrecisionStr, Precision) then
              if Precision > 16 then
                IsPrecisionHigh := True;
          end;
        end;

        if IsPrecisionHigh then
        begin
          HasProblem := True;
          Category := pcIBXConversion;
        end;
      end;

      // ============================================================
      // Sektion 4: Version-Check (immer)
      // ============================================================
      if (not HasProblem) and (SupportedTypes.Count > 0) then
      begin
        if not IsFieldTypeSupported(FFields[i].FieldType, AVersion,
                                    IBDBSource, IBTransSource) then
        begin
          HasProblem := True;
          Category := pcNotSupported;
        end;
      end;

      if HasProblem then
      begin
        HasAnyProblem := True;
        SetLength(ProblemFields, Length(ProblemFields) + 1);
        ProblemFields[High(ProblemFields)].FieldName := FFields[i].FieldName;
        ProblemFields[High(ProblemFields)].FieldType := FFields[i].FieldType;
        ProblemFields[High(ProblemFields)].Category := Category;
      end;
    end;

    // ============================================================
    // Keine Probleme → weiter
    // ============================================================
    if not HasAnyProblem then
    begin
      Result := True;
      Exit;
    end;

    // ============================================================
    // Dialog öffnen
    // ============================================================
    Dlg := TfrmProblemDialog.CreateNew(nil);
    try
      Dlg.Init(ProblemFields);

      if Dlg.ShowModal = mrOK then
      begin
        KeptNames := Dlg.GetKeptFieldNames;
        try
          for i := 0 to High(ProblemFields) do
          begin
            if KeptNames.IndexOf(ProblemFields[i].FieldName) < 0 then
            begin
              for j := 0 to chkLstFields.Count - 1 do
                if SameText(chkLstFields.Items[j], ProblemFields[i].FieldName) then
                begin
                  chkLstFields.Checked[j] := False;
                  Break;
                end;
            end;
          end;
        finally
          KeptNames.Free;
        end;

        ApplyFieldStates;
        Result := True;
      end
      else
        Result := False;
    finally
      Dlg.Free;
    end;

  finally
    SupportedTypes.Free;
  end;
end;

procedure TfrmCloneTable.ApplyInitialSelection;
var
  i: Integer;
  ServerName, DBTitle: string;
begin
  // Nur wenn eine Vorbelegung gewünscht ist
  if (FInitialDBIndex < 0) or (FInitialDBIndex >= Length(RegisteredDatabases)) then
    Exit;

  // Server + DB-Titel aus dem Registrierungseintrag holen
  ServerName := RegisteredDatabases[FInitialDBIndex].RegRec.ServerName;
  DBTitle    := RegisteredDatabases[FInitialDBIndex].RegRec.Title;

  // --- Source-Server setzen ---
  i := comboxSourceServer.Items.IndexOf(ServerName);
  if i >= 0 then
    comboxSourceServer.ItemIndex := i;

  // --- Source-DB-Combo neu füllen und DB setzen ---
  FillSourceDBCombo;
  i := comboxSourceDB.Items.IndexOf(DBTitle);
  if i >= 0 then
    comboxSourceDB.ItemIndex := i;

  // --- Dest-Server: gleicher Server wie Source (sinnvoller Default) ---
  i := comboxDestServer.Items.IndexOf(ServerName);
  if i >= 0 then
    comboxDestServer.ItemIndex := i;
end;

procedure TfrmCloneTable.UpdateCopyMethodAvailability;
var
  SameServer, SameDB: Boolean;
  ServerVersionMajor, ServerVersionMinor: Word;
  SupportsExecuteBlock: Boolean;
begin
  if FDestDBIndex < 0 then Exit;

  // FBIntf: immer verfügbar (Same-DB und Cross-DB)
    rbFBIntf.Enabled := True;

  SameServer := SameText(Trim(comboxSourceServer.Text), Trim(comboxDestServer.Text));
  SameDB     := SameServer and SameText(Trim(comboxSourceDB.Text), Trim(comboxDestDB.Text));

  ServerVersionMajor := RegisteredDatabases[FDestDBIndex].RegRec.ServerVersionMajor;
  ServerVersionMinor := RegisteredDatabases[FDestDBIndex].RegRec.ServerVersionMinor;

  // Execute Block mit ON EXTERNAL DATA SOURCE erst ab FB 2.1
  SupportsExecuteBlock := (ServerVersionMajor > 2) or
                          ((ServerVersionMajor = 2) and (ServerVersionMinor >= 1));

  // ============================================================
  // Enabled-Zustände pro Methode
  // ============================================================
  rbInsertSelect.Enabled := SameDB;

  if SameDB then
    rbExecuteBlock.Enabled := True
  else
    rbExecuteBlock.Enabled := SupportsExecuteBlock;

  rbRowByRow.Enabled := True;

  // ============================================================
  // Vorauswahl — immer die beste verfügbare Methode
  // ============================================================
  if SameDB then
  begin
    // Same-DB → Insert-Select ist immer die beste Wahl
    rbInsertSelect.Checked := True;

    StatusBar1.SimpleText := 'Same database: Insert Select (fastest) or Row-by-Row.';
  end
  else
  begin
    // Cross-DB → Execute Block (wenn verfügbar), sonst Row-by-Row
    if SupportsExecuteBlock then
    begin
      rbExecuteBlock.Checked := True;
      StatusBar1.SimpleText := 'Cross-database: Execute Block (default) or Row-by-Row.';
    end
    else
    begin
      rbRowByRow.Checked := True;
      StatusBar1.SimpleText := 'Cross-database: Row-by-Row only (Execute Block requires FB 2.1+).';
    end;
  end;
end;

procedure TfrmCloneTable.FormCreate(Sender: TObject);
begin
  // StringGrid initialisieren
  sgFields.RowCount := 1;
  sgFields.ColCount := 4;
  sgFields.Cells[0, 0] := 'Copy';
  sgFields.Cells[1, 0] := 'Source Field';
  sgFields.Cells[2, 0] := 'Field Type';
  sgFields.Cells[3, 0] := 'Formula ($1 = value)';
  //sgFields.ColWidths[0] := 45;
  sgFields.ColWidths[0] := 0; //invisible
  sgFields.ColWidths[1] := 150;
  sgFields.ColWidths[2] := 120;
  sgFields.ColWidths[3] := 250;

  // Guard AN – alle OnChange-Events blockieren während der Initialisierung
  FUpdatingCombos := True;

  try
    FillSourceCombos;
    FillDestCombos;

    if Trim(comboxSourceTables.Text) <> '' then
      edtDestTable.Text := Trim(comboxSourceTables.Text) + '_CLONED';
  finally
    // Guard bleibt AN bis FormShow!
    // FUpdatingCombos wird erst in FormShow auf False gesetzt,
    // damit die Kaskade einmalig und kontrolliert ausgelöst wird.
  end;

  LoadFormulaPresets;
  UpdateCopyMethodAvailability;
end;

procedure TfrmCloneTable.Init(ANodeInfos: TPNodeInfos; const ATableName: string);
begin
  FNodeInfos := ANodeInfos;
  FInitialTableName := Trim(ATableName);
  FInitialDBIndex := -1;

  if Assigned(ANodeInfos) then
    FInitialDBIndex := ANodeInfos^.dbIndex;
end;

procedure TfrmCloneTable.comboxSourceDBChange(Sender: TObject);
var
  i: Integer;
begin
  if FUpdatingCombos then Exit;
  FUpdatingCombos := True;
  try
    comboxSourceTables.Items.Clear;

    // FSourceDBIndex frisch ermitteln (falls noch -1)
    FSourceDBIndex := -1;
    for i := 0 to High(RegisteredDatabases) do
      if SameText(Trim(RegisteredDatabases[i].RegRec.ServerName), Trim(comboxSourceServer.Text)) and
         SameText(Trim(RegisteredDatabases[i].RegRec.Title), Trim(comboxSourceDB.Text)) then
      begin
        FSourceDBIndex := i;
        Break;
      end;

    // Wenn die geteilte DB nicht verbunden ist → Login-Flow auslösen
    if (FSourceDBIndex >= 0) and
       Assigned(RegisteredDatabases[FSourceDBIndex].IBDatabase) and
       (not RegisteredDatabases[FSourceDBIndex].IBDatabase.Connected) then
      ConnectToDBAs(FSourceDBIndex);

    if ConfigureSourceConnection then
    begin
      FillSourceTableCombo;
      edtDestTable.Text := Trim(comboxSourceTables.Text) + '_CLONED';
      LoadFields;
    end
    else StatusBar1.SimpleText := 'Source database not connected.';

  finally
    FUpdatingCombos := False;
  end;
  UpdateCopyMethodAvailability;
end;

procedure TfrmCloneTable.comboxSourceServerChange(Sender: TObject);
begin
  if FUpdatingCombos then Exit;
  FUpdatingCombos := True;
  try
    FillSourceDBCombo;
    // KEIN erzwungenes ItemIndex := 0 mehr — FillSourceDBCombo
    // setzt selbst einen Default, wenn keine valide Auswahl existiert.
  finally
    FUpdatingCombos := False;
  end;

  // Kaskade explizit auslösen
  if comboxSourceDB.Items.Count > 0 then
    comboxSourceDBChange(nil);

  UpdateCopyMethodAvailability;
end;

procedure TfrmCloneTable.comboxSourceTablesChange(Sender: TObject);
begin
  if FUpdatingCombos then Exit;
  if Trim(comboxSourceTables.Text) = '' then Exit;   // Schutz gegen leeren Namen

  edtDestTable.Text := Trim(comboxSourceTables.Text) + '_CLONED';
  LoadFields;
end;

procedure TfrmCloneTable.edtExecBlockBatchExit(Sender: TObject);
var
  V: Integer;
begin
  V := StrToIntDef(Trim(edtExecBlockBatch.Text), 10000);

  edtExecBlockBatch.Text := IntToStr(V);

  if rbExecuteBlock.Checked then
    UpdateEngineStatus;
end;

procedure TfrmCloneTable.edtLocalBatchSizeExit(Sender: TObject);
var
  V: Integer;
begin
  V := StrToIntDef(Trim(edtLocalBatchSize.Text), 500000);

  edtLocalBatchSize.Text := IntToStr(V);

  // StatusBar aktualisieren, falls Local aktiv ist
  if rbInsertSelect.Checked then
    UpdateEngineStatus;
end;

procedure TfrmCloneTable.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(FExtractor) then
    FreeAndNil(FExtractor);

  if Assigned(IBQuerySource) and IBQuerySource.Active then
    IBQuerySource.Close;
  if Assigned(IBQueryDest) and IBQueryDest.Active then
    IBQueryDest.Close;

  if Assigned(IBTransSource) and IBTransSource.InTransaction then
    IBTransSource.Rollback;
  if Assigned(IBTransDest) and IBTransDest.InTransaction then
    IBTransDest.Rollback;

  // Jetzt SICHER: das sind Form-eigene Verbindungen, nicht die geteilten!
  if Assigned(IBDBSource) and IBDBSource.Connected then
    IBDBSource.Connected := False;
  if Assigned(IBDBDest) and IBDBDest.Connected then
    IBDBDest.Connected := False;
end;

procedure TfrmCloneTable.FillSourceCombos;
begin
  FillSourceServerCombo;
  //FillSourceDBCombo;
  //FillSourceTableCombo;
end;

function TfrmCloneTable.FillSourceServerCombo: boolean;
var
  ServerList: TStringList;
begin
  Result := False;
  comboxSourceServer.Items.Clear;

  try
    ServerList := GetServerListFromTreeView;
    comboxSourceServer.Items.Assign(ServerList);
    if comboxSourceServer.Items.Count > 0 then
    begin
      comboxSourceServer.ItemIndex := 0;
      Result := True;
      FillSourceDBCombo;
    end;
  finally
    ServerList.Free;
  end;
end;

function TfrmCloneTable.FillSourceDBCombo: boolean;
var
  i, PreserveIdx: Integer;
  PreserveTitle: string;
begin
  Result := False;

  // Aktuelle Auswahl merken (falls vorhanden)
  PreserveTitle := Trim(comboxSourceDB.Text);

  comboxSourceDB.Items.Clear;

  for i := 0 to High(RegisteredDatabases) do
    if SameText(RegisteredDatabases[i].RegRec.ServerName, comboxSourceServer.Text) then
      comboxSourceDB.Items.Add(RegisteredDatabases[i].RegRec.Title);

  if comboxSourceDB.Items.Count > 0 then
  begin
    // Versuche die vorherige Auswahl zu erhalten
    PreserveIdx := comboxSourceDB.Items.IndexOf(PreserveTitle);
    if PreserveIdx >= 0 then
      comboxSourceDB.ItemIndex := PreserveIdx
    else
      comboxSourceDB.ItemIndex := 0;

    Result := True;
    if ConfigureSourceConnection then
    begin
      FillSourceTableCombo;
      LoadFields;
    end;
  end
  else
  begin
    chkLstFields.Items.Clear;
    sgFields.RowCount := 0;
    comboxSourceTables.Items.Clear;
  end;
end;

//IBX version
{function TfrmCloneTable.FillSourceTableCombo: boolean;
begin
  Result := False;
  comboxSourceTables.Items.Clear;

  try
    IBDBSource.GetTableNames(comboxSourceTables.Items);

    if comboxSourceTables.Items.Count > 0 then
    begin
      comboxSourceTables.ItemIndex := 0;
      //edtDestTable.Text := Trim(comboxSourceTables.Text) + '_COPY';
      Result := True;
    end else
    begin
      chkLstFields.Items.Clear;
      sgFields.RowCount := 0;
    end;
  except
  end;
end;}

function TfrmCloneTable.FillSourceTableCombo: boolean;
var
  Extractor: TSimpleObjExtractor;
  TableList: TStringList;
  i: Integer;
begin
  Result := False;
  comboxSourceTables.Items.Clear;

  if FSourceDBIndex < 0 then
    Exit;

  try
    Extractor := TSimpleObjExtractor.Create(FSourceDBIndex);
    TableList := TStringList.Create;
    try
      Extractor.ExtractTableNames(TableList, False, False);
      for i := 0 to TableList.Count - 1 do
        comboxSourceTables.Items.Add(TableList[i]);
    finally
      TableList.Free;
      Extractor.Free;
    end;

    if comboxSourceTables.Items.Count > 0 then
    begin
      comboxSourceTables.ItemIndex := 0;
      Result := True;
    end
    else
    begin
      chkLstFields.Items.Clear;
      sgFields.RowCount := 0;
    end;
  except
    on E: Exception do
    begin
      StatusBar1.SimpleText := 'Could not load table names: ' + E.Message;
    end;
  end;
end;

function TfrmCloneTable.ConfigureSourceConnection: boolean;
var
  i: Integer;
  DBRec: TDatabaseRec;
  Pwd: string;
begin
  Result := False;
  FSourceDBIndex := -1;
  Pwd := '';

  for i := 0 to High(RegisteredDatabases) do
    if SameText(Trim(RegisteredDatabases[i].RegRec.ServerName), Trim(comboxSourceServer.Text)) and
       SameText(Trim(RegisteredDatabases[i].RegRec.Title), Trim(comboxSourceDB.Text)) then
    begin
      FSourceDBIndex := i;
      Break;
    end;

  if FSourceDBIndex < 0 then Exit;

  try
    DBRec := RegisteredDatabases[FSourceDBIndex];

    // Trenne die Form-eigene Verbindung, falls sie noch offen ist
    if IBDBSource.Connected then
      IBDBSource.Connected := False;
    if IBTransSource.InTransaction then
      IBTransSource.Rollback;

    // --- Einstellungen von der geteilten DB in die Form-Component kopieren ---
    AssignIBDatabase(DBRec.IBDatabase, IBDBSource);
    SetDBInstanceIndex(IBDBSource, FSourceDBIndex);

    // --- Credentials nachreichen (RegRec → Session-Cache → Live) ---
    if Trim(IBDBSource.Params.Values['user_name']) = '' then
      IBDBSource.Params.Values['user_name'] := DBRec.RegRec.UserName;

    if Trim(IBDBSource.Params.Values['password']) = '' then
    begin
      Pwd := DBRec.RegRec.Password;

      if Pwd = '' then
        Pwd := GetDBSessionPassword(DBRec.RegRec.ServerName, DBRec.RegRec.DatabaseName);

      //if Pwd = '' then
        //Pwd := GetServerSessionPassword(DBRec.RegRec.ServerName);

      if (Pwd = '') and DBRec.RegRec.IsEmbedded then
        Pwd := 'embedded_local';

      if Pwd <> '' then
        IBDBSource.Params.Values['password'] := Pwd;
    end;

    // --- Transaktion: Params kopieren + validieren ---
    IBTransSource.Params.Clear;
    IBTransSource.Params.Assign(DBRec.IBTransaction.Params);

    // Defaults greifen, wenn die kopierten Params leer oder Müll sind
    EnsureTransactionParams(IBTransSource, DefTxFileName);

    if not IsTransactionParamsValid(IBTransSource) then
      // Falls Assign Müll geliefert hat → hart auf Default
      EnsureTransactionParams(IBTransSource, '');

    // DefaultDatabase explizit setzen (Pflicht für StartTransaction!)
    IBTransSource.DefaultDatabase := IBDBSource;


    // --- Jetzt verbinden (Form-eigene Verbindung!) ---
    if not IBDBSource.Connected then
      IBDBSource.Connected := True;

    CachePasswordAfterConnect(FSourceDBIndex, IBDBSource);

    if not IBTransSource.InTransaction then
      IBTransSource.StartTransaction;

    Result := True;
  except
    on E: Exception do
    begin
      StatusBar1.SimpleText := 'Could not configure source connection: ' + E.Message;
      Result := False;
    end;
  end;
end;

function TfrmCloneTable.ConfigureDestConnection: boolean;
var
  i: Integer;
  DBRec: TDatabaseRec;
  Pwd: string;
begin
  Result := False;
  FDestDBIndex := -1;
  Pwd := '';

  for i := 0 to High(RegisteredDatabases) do
    if SameText(Trim(RegisteredDatabases[i].RegRec.ServerName), Trim(comboxDestServer.Text)) and
       SameText(Trim(RegisteredDatabases[i].RegRec.Title), Trim(comboxDestDB.Text)) then
    begin
      FDestDBIndex := i;
      Break;
    end;

  if FDestDBIndex < 0 then Exit;

  try
    DBRec := RegisteredDatabases[FDestDBIndex];

    // Trenne die Form-eigene Verbindung, falls sie noch offen ist
    if IBDBDest.Connected then
      IBDBDest.Connected := False;
    if IBTransDest.InTransaction then
      IBTransDest.Rollback;

    // --- Einstellungen kopieren ---
    AssignIBDatabase(DBRec.IBDatabase, IBDBDest);
    SetDBInstanceIndex(IBDBDest, FDestDBIndex);

    // --- Credentials nachreichen (RegRec → Session-Cache → Live) ---
    if Trim(IBDBDest.Params.Values['user_name']) = '' then
      IBDBDest.Params.Values['user_name'] := DBRec.RegRec.UserName;

    if Trim(IBDBDest.Params.Values['password']) = '' then
    begin
      Pwd := DBRec.RegRec.Password;

      if Pwd = '' then
        Pwd := GetDBSessionPassword(DBRec.RegRec.ServerName, DBRec.RegRec.DatabaseName);

      //if Pwd = '' then
        //Pwd := GetServerSessionPassword(DBRec.RegRec.ServerName);

      if (Pwd = '') and DBRec.RegRec.IsEmbedded then
        Pwd := 'embedded_local';

      if Pwd <> '' then
        IBDBDest.Params.Values['password'] := Pwd;
    end;

    // --- Transaktion: Params kopieren + validieren ---
    IBTransDest.Params.Clear;
    IBTransDest.Params.Assign(DBRec.IBTransaction.Params);

    EnsureTransactionParams(IBTransDest, DefTxFileName);

    if not IsTransactionParamsValid(IBTransDest) then
      EnsureTransactionParams(IBTransDest, '');

    // DefaultDatabase VOR StartTransaction setzen!
    IBTransDest.DefaultDatabase := IBDBDest;

    // --- Query an die Form-eigenen Komponenten binden ---
    IBQueryDest.Database := IBDBDest;
    IBQueryDest.Transaction := IBTransDest;

    // --- Verbinden ---
    if not IBDBDest.Connected then
      IBDBDest.Connected := True;

    CachePasswordAfterConnect(FDestDBIndex, IBDBDest);

    if not IBTransDest.InTransaction then
      IBTransDest.StartTransaction;

    // --- Skript-Component an die Form-eigenen Komponenten binden ---
    IBXScript1.Database := IBDBDest;
    IBXScript1.Transaction := IBTransDest;

    Result := True;
  except
    on E: Exception do
    begin
      StatusBar1.SimpleText := 'Could not configure destination connection: ' + E.Message;
      Result := False;
    end;
  end;
end;

// ============================================================================
// DESTINATION
// ============================================================================

procedure TfrmCloneTable.comboxDestServerChange(Sender: TObject);
begin
  if FUpdatingCombos then Exit;
  FUpdatingCombos := True;
  try
    FillDestDBCombo;

    if comboxDestDB.Items.Count > 0 then
      comboxDestDB.ItemIndex := 0;
  finally
    FUpdatingCombos := False;
  end;

  // Explizit die Kaskade auslösen – auch bei nur einer DB
  if comboxDestDB.Items.Count > 0 then
    comboxDestDBChange(nil);

  UpdateCopyMethodAvailability;
end;

procedure TfrmCloneTable.comboxDestDBChange(Sender: TObject);
var
  i: Integer;
begin
  if FUpdatingCombos then Exit;
  FUpdatingCombos := True;
  try
    // FDestDBIndex frisch ermitteln (falls noch -1)
    FDestDBIndex := -1;
    for i := 0 to High(RegisteredDatabases) do
      if SameText(Trim(RegisteredDatabases[i].RegRec.ServerName), Trim(comboxDestServer.Text)) and
         SameText(Trim(RegisteredDatabases[i].RegRec.Title), Trim(comboxDestDB.Text)) then
      begin
        FDestDBIndex := i;
        Break;
      end;

    // Wenn die geteilte DB nicht verbunden ist → Login-Flow auslösen
    if (FDestDBIndex >= 0) and
       Assigned(RegisteredDatabases[FDestDBIndex].IBDatabase) and
       (not RegisteredDatabases[FDestDBIndex].IBDatabase.Connected) then
      ConnectToDBAs(FDestDBIndex);

    if not ConfigureDestConnection then
      StatusBar1.SimpleText := 'Destination database not connected.';

  finally
    FUpdatingCombos := False;
  end;
  UpdateCopyMethodAvailability;
end;

procedure TfrmCloneTable.FillDestCombos;
begin
  FillDestServerCombo;

end;

function TfrmCloneTable.FillDestServerCombo: boolean;
var
  ServerList: TStringList;
begin
  Result := False;
  comboxDestServer.Items.Clear;

  try
    ServerList := GetServerListFromTreeView;
    comboxDestServer.Items.Assign(ServerList);
    if comboxDestServer.Items.Count > 0 then
    begin
      comboxDestServer.ItemIndex := 0;
      FillDestDBCombo;
      Result := True;
    end;
  finally
    ServerList.Free;
  end;
end;

function TfrmCloneTable.FillDestDBCombo: boolean;
var
  i: Integer;
begin
  Result := False;
  comboxDestDB.Items.Clear;

  for i := 0 to High(RegisteredDatabases) do
    if SameText(RegisteredDatabases[i].RegRec.ServerName, comboxDestServer.Text) then
      comboxDestDB.Items.Add(RegisteredDatabases[i].RegRec.Title);

  if comboxDestDB.Items.Count > 0 then
  begin
    comboxDestDB.ItemIndex := 0;
    Result := True;
  end;
end;

// ============================================================================
// FIELDER
// ============================================================================
procedure TfrmCloneTable.LoadFields;
var
  RawFields: TFBFieldRawArray;
  i, idx: Integer;
  FieldType: string;
begin
  if FSourceDBIndex < 0 then Exit;
  if Trim(comboxSourceTables.Text) = '' then Exit;

  SetLength(FFields, 0);
  chkLstFields.Clear;
  sgFields.RowCount := 1;

  try
    EnsureExtractor(FSourceDBIndex);
    if not Assigned(FExtractor) then Exit;

    RawFields := FExtractor.GetTableFieldsRaw(Trim(comboxSourceTables.Text));

    for i := 0 to High(RawFields) do
    begin
      // Basis-Typ (nicht Domain-Name) — für Clone-Zwecke robust
      FieldType := GetFBTypeName(
        RawFields[i].FieldType,
        RawFields[i].FieldSubType,
        RawFields[i].FieldLength,
        RawFields[i].FieldPrecision,
        RawFields[i].FieldScale,
        RawFields[i].CharacterSetName,
        RawFields[i].CharacterLength
      );

      // Array-Dimension anhängen — CheckFieldCompatibility erkennt
      // Arrays am '['-Zeichen
      if RawFields[i].ArrayUpperBound <> 0 then
        FieldType := FieldType + ' [' + IntToStr(RawFields[i].ArrayUpperBound) + ']';

      idx := Length(FFields);
      SetLength(FFields, idx + 1);
      FFields[idx].FieldName := RawFields[i].FieldName;
      FFields[idx].FieldType := FieldType;
      FFields[idx].IsComputed := (Trim(RawFields[i].ComputedSource) <> '');
      FFields[idx].Checked := True;
      FFields[idx].Formula := '';

      chkLstFields.Items.Add(RawFields[i].FieldName);
      chkLstFields.Checked[idx] := True;

      sgFields.RowCount := idx + 2;
      sgFields.Cells[0, idx + 1] := '1';
      sgFields.Cells[1, idx + 1] := RawFields[i].FieldName;
      sgFields.Cells[2, idx + 1] := FieldType;
      sgFields.Cells[3, idx + 1] := '';

      if FFields[idx].IsComputed then
        sgFields.Cells[3, idx + 1] := '(computed)';
    end;
  except
    on E: Exception do
    begin
      StatusBar1.SimpleText := 'Could not load fields: ' + E.Message;
      SetLength(FFields, 0);
      chkLstFields.Clear;
      sgFields.RowCount := 1;
    end;
  end;
  // Initialer Button-Zustand (wenn min. 1 Feld vorhanden → Execute aktiv)
  btnExecute.Enabled := (chkLstFields.Count > 0);
end;

procedure TfrmCloneTable.btnSelectAllClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to chkLstFields.Count - 1 do
    chkLstFields.Checked[i] := True;

  ApplyFieldStates;
  btnExecute.Enabled := (chkLstFields.Count > 0);
end;

procedure TfrmCloneTable.btnDeselectAllClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to chkLstFields.Count - 1 do
    chkLstFields.Checked[i] := False;

  ApplyFieldStates;
  btnExecute.Enabled := False;
end;

procedure TfrmCloneTable.sgFieldsDblClick(Sender: TObject);
var
  Row: Integer;
  NewFormula: string;
begin
  Row := sgFields.Row;
  if (Row < 1) or (Row >= sgFields.RowCount) then Exit;

  NewFormula := sgFields.Cells[3, Row];
  if InputQuery('Formula for ' + sgFields.Cells[1, Row],
                'Enter SQL expression ($1 = field value):', NewFormula) then
  begin
    sgFields.Cells[3, Row] := NewFormula;
  end;
end;

function TfrmCloneTable.GetFieldTransforms: TFieldTransformArray;
var
  i: Integer;
begin
  SetLength(Result, chkLstFields.Count);
  for i := 0 to chkLstFields.Count - 1 do
  begin
    Result[i].SourceField := chkLstFields.Items[i];
    Result[i].DestField := chkLstFields.Items[i];
    Result[i].DestFieldType := FFields[i].FieldType;

    if FFields[i].IsComputed then
    begin
      Result[i].Formula := '';
      Result[i].CopyField := False;
    end
    else
    begin
      // Nur wenn Checkbox aktiv ist, die Formel aus dem Grid übernehmen
      if chkUseFormula.Checked then
        Result[i].Formula := sgFields.Cells[3, i + 1]
      else
        Result[i].Formula := '';
      Result[i].CopyField := chkLstFields.Checked[i];
    end;
  end;
end;

function TfrmCloneTable.GetProblemFieldTransforms: TFieldTransformArray;
var
  i, idx: Integer;
begin
  SetLength(Result, 0);
  idx := 0;

  for i := 0 to chkLstFields.Count - 1 do
  begin
    if not chkLstFields.Checked[i] then
      Continue;

    if FFields[i].IsComputed then
      Continue;

    // Nur ARRAY und BLOB aufnehmen
    if (Pos('[', FFields[i].FieldType) > 0) or
       (Pos('BLOB', UpperCase(FFields[i].FieldType)) > 0) then
    begin
      SetLength(Result, idx + 1);
      Result[idx].SourceField := chkLstFields.Items[i];
      Result[idx].DestField := chkLstFields.Items[i];
      Result[idx].DestFieldType := FFields[i].FieldType;
      Result[idx].Formula := '';
      Result[idx].CopyField := True;
      Inc(idx);
    end;
  end;
end;

function TfrmCloneTable.GenerateCreateTableSQL: string;
var
  SL: TStringList;
  i, j: Integer;
  RawFields: TFBFieldRawArray;
  FieldName, FieldType, DefaultSource, ComputedSource: string;
  Line: string;
begin
  SL := TStringList.Create;
  try
    SL.Add('CREATE TABLE ' + MakeCaseSensitiveAuto(Trim(edtDestTable.Text)) + ' (');

    EnsureExtractor(FSourceDBIndex);
    if not Assigned(FExtractor) then
    begin
      Result := '';
      Exit;
    end;

    RawFields := FExtractor.GetTableFieldsRaw(Trim(comboxSourceTables.Text));

    for i := 0 to High(RawFields) do
    begin
      if i >= chkLstFields.Count then Break;
      if not chkLstFields.Checked[i] then Continue;

      FieldName := MakeCaseSensitiveAuto(RawFields[i].FieldName);
      ComputedSource := Trim(RawFields[i].ComputedSource);

      if ComputedSource <> '' then
      begin
        Line := '  ' + FieldName + ' COMPUTED BY (' + ComputedSource + '),';
      end
      else
      begin
        FieldType := GetFBTypeName(
          RawFields[i].FieldType,
          RawFields[i].FieldSubType,
          RawFields[i].FieldLength,
          RawFields[i].FieldPrecision,
          RawFields[i].FieldScale,
          RawFields[i].CharacterSetName,
          RawFields[i].CharacterLength
        );

        // Nach GetFBTypeName — Versions-Fix für BLOB-Subtype
        if (RawFields[i].FieldType = 261) and          // BLOB
           (RegisteredDatabases[FDestDBIndex].RegRec.ServerVersionMajor < 2) then
        begin
          // Firebird 1.5 kennt den Alias 'BINARY' nicht → numerischer Subtyp
          FieldType := StringReplace(FieldType, ' SUB_TYPE BINARY',
                                     ' SUB_TYPE 0', [rfReplaceAll, rfIgnoreCase]);
          FieldType := StringReplace(FieldType, ' SUB_TYPE TEXT',
                                     ' SUB_TYPE 1', [rfReplaceAll, rfIgnoreCase]);
        end;

        if Length(RawFields[i].ArrayDims) > 0 then
        begin
          FieldType := FieldType + ' [';
          for j := 0 to High(RawFields[i].ArrayDims) do
          begin
            if j > 0 then FieldType := FieldType + ', ';
            with RawFields[i].ArrayDims[j] do
            begin
              if LowerBound = 1 then
                FieldType := FieldType + IntToStr(UpperBound)
              else
                FieldType := FieldType + IntToStr(LowerBound) + ':' +
                             IntToStr(UpperBound);
            end;
          end;
          FieldType := FieldType + ']';
        end;

        Line := '  ' + FieldName + ' ' + FieldType;

        DefaultSource := Trim(RawFields[i].DefaultSource);
        if DefaultSource <> '' then
        begin
          if UpperCase(Copy(DefaultSource, 1, 7)) = 'DEFAULT' then
            Line := Line + ' ' + DefaultSource
          else
            Line := Line + ' DEFAULT ' + DefaultSource;
        end;

        if RawFields[i].NotNull then
          Line := Line + ' NOT NULL';

        Line := Line + ',';
      end;

      SL.Add(Line);
    end;

    // Letztes Komma entfernen
    if SL.Count > 1 then
    begin
      i := SL.Count - 1;
      Line := SL[i];
      if (Length(Line) > 0) and (Line[Length(Line)] = ',') then
        SL[i] := Copy(Line, 1, Length(Line) - 1);
    end;

    SL.Add(');');
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function TfrmCloneTable.GenerateInsertSQL: string;
var
  SourceFields, DestFields: string;
  i: Integer;
  QFieldName: string;
begin
  SourceFields := '';
  DestFields := '';

  for i := 0 to chkLstFields.Count - 1 do
  begin
    if chkLstFields.Checked[i] and (not FFields[i].IsComputed) then
    begin
      QFieldName := MakeCaseSensitiveAuto(FFields[i].FieldName);

      if SourceFields <> '' then SourceFields := SourceFields + ', ';
      SourceFields := SourceFields + QFieldName;

      if DestFields <> '' then DestFields := DestFields + ', ';
      DestFields := DestFields + QFieldName;
    end;
  end;

  Result := 'INSERT INTO ' + MakeCaseSensitiveAuto(Trim(edtDestTable.Text)) +
            ' (' + DestFields + ')' + sLineBreak +
            'SELECT ' + SourceFields + sLineBreak +
            'FROM ' + MakeCaseSensitiveAuto(Trim(comboxSourceTables.Text)) + ';';
end;

function TfrmCloneTable.GenerateCreateExternalTableSQL: string;
var
  SL: TStringList;
  i: Integer;
  Line: string;
  FieldName, FieldType: string;
begin
  SL := TStringList.Create;
  try
    SL.Add('CREATE TABLE ' + MakeCaseSensitiveAuto(Trim(edtDestTable.Text)) +
           ' EXTERNAL FILE ''' + edtExternalFile.Text + ''' (');

    for i := 0 to chkLstFields.Count - 1 do
    begin
      if not chkLstFields.Checked[i] then Continue;

      FieldName := MakeCaseSensitiveAuto(chkLstFields.Items[i]);
      FieldType := FFields[i].FieldType;

      // Keine BLOBs, Arrays oder Computed Fields in externen Tabellen!
      if FFields[i].IsComputed then Continue;
      if Pos('BLOB', UpperCase(FieldType)) > 0 then Continue;
      if Pos('[', FieldType) > 0 then Continue;

      SL.Add('  ' + FieldName + ' ' + FieldType + ',');
    end;

    // Letztes Komma entfernen
    if SL.Count > 1 then
    begin
      Line := SL[SL.Count - 1];
      if (Length(Line) > 0) and (Line[Length(Line)] = ',') then
        SL[SL.Count - 1] := Copy(Line, 1, Length(Line) - 1);
    end;

    SL.Add(');');
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function TfrmCloneTable.CreateExternalDestTable(DestDB: TIBDatabase;
  DestTrans: TIBTransaction; TableName: string): Boolean;
var
  SQL: string;
begin
  Result := False;
  SQL := GenerateCreateExternalTableSQL;

  if SQL = '' then Exit;

  IBXScript1.Database := DestDB;
  IBXScript1.Transaction := DestTrans;

  try
    if not DestTrans.InTransaction then
      DestTrans.StartTransaction;

    IBXScript1.ExecSQLScript(SQL);
    DestTrans.Commit;
    Result := True;
  except
    on E: Exception do
    begin
      MessageDlg('Failed to create external table: ' + E.Message, mtError, [mbOK], 0);
      DestTrans.Rollback;
    end;
  end;
end;

// ============================================================================
// AKTIONEN
// ============================================================================

procedure TfrmCloneTable.btnPreviewSQLClick(Sender: TObject);
var
  SQL: string;
begin
  if chkCreateTable.Checked then
    SQL := GenerateCreateTableSQL + sLineBreak + sLineBreak;
  SQL := SQL + GenerateInsertSQL;
  ShowMessage(SQL);
end;

procedure TfrmCloneTable.btnExecuteClick(Sender: TObject);
var
  Fields: TFieldTransformArray;

  CopyEngineLocal: TCopyTableDataLocal;
  CopyEngineCrossExecuteBlock: TCopyTableDataCrossExecuteBlock;
  CopyEngineRowByRow: TCopyTableDataRowByRow;
  CopyEngineFBIntf: TCopyTableDataFBIntf;

  DestTable: string;
  FromRow, ToRow: Integer;
  i: integer;
  TableCreated: Boolean;
  IsProblemField: Boolean;
  ReportForm: TfrmReport;
  Stats: TTransferStatistic;
  FormulasText: string;
  MissingFields: TStringList;
  Answer: Integer;
begin
  if chkboxExternalTable.Checked and (Trim(edtExternalFile.Text) = '') then
  begin
    MessageDlg('Please select an external file for the external table.', mtWarning, [mbOK], 0);
    Exit;
  end;

  if FSourceDBIndex < 0 then
  begin
    MessageDlg('Please select a valid source database.', mtWarning, [mbOK], 0);
    Exit;
  end;

  if FDestDBIndex < 0 then
  begin
    MessageDlg('Please select a valid destination database.', mtWarning, [mbOK], 0);
    Exit;
  end;

  // Sicherheits-Check: mindestens ein Feld muss ausgewählt sein
  i := 0;
  while (i < chkLstFields.Count) and (not chkLstFields.Checked[i]) do
    Inc(i);

  if i >= chkLstFields.Count then
  begin
    MessageDlg('No fields selected.' + sLineBreak +
               'Please select at least one field to copy.',
               mtWarning, [mbOK], 0);
    Exit;
  end;

  // Quelle konfigurieren
  if not ConfigureSourceConnection then
  begin
    MessageDlg('Could not connect to source database:' + sLineBreak +
               sLineBreak +
               'Server: ' + comboxSourceServer.Text + sLineBreak +
               'Database: ' + comboxSourceDB.Text,
               mtError, [mbOK], 0);
    Exit;
  end;

  // Ziel konfigurieren
  if not ConfigureDestConnection then
  begin
    MessageDlg('Could not connect to destination database:' + sLineBreak +
               sLineBreak +
               'Server: ' + comboxDestServer.Text + sLineBreak +
               'Database: ' + comboxDestDB.Text,
               mtError, [mbOK], 0);
    Exit;
  end;

  DestTable := MakeCaseSensitiveAuto(Trim(edtDestTable.Text));
  if DestTable = '' then
  begin
    MessageDlg('Please enter a destination table name.', mtWarning, [mbOK], 0);
    Exit;
  end;

  // ============================================================
  // Prüfen ob mindestens eine Aktion gewählt wurde
  // ============================================================
  if not chkCreateTable.Checked and not chkCopyData.Checked then
  begin
    MessageDlg('No action selected.' + sLineBreak +
               'Please check at least one option:' + sLineBreak +
               '"Create Destination Table" or "Copy Data".', mtWarning, [mbOK], 0);
    Exit;
  end;

  // ============================================================
  // Feld-Kompatibilität prüfen
  //  - Version-Check: immer
  //  - Method-Check: nur wenn Copy Data aktiv
  // ============================================================
  if not CheckFieldCompatibility(chkCopyData.Checked) then
  begin
    StatusBar1.SimpleText := 'Copy cancelled by user.';
    Exit;
  end;

  // ============================================================
  // Structure-Drift-Check — nur wenn Daten kopiert werden
  // ============================================================
  if chkCopyData.Checked and TableExists(IBDBDest, DestTable) then
  begin
    MissingFields := GetMissingDestFields(IBDBDest, DestTable);
    try
      if MissingFields.Count > 0 then
      begin
        Answer := ShowStructureDriftDialog(DestTable, MissingFields,
                                           chkCreateTable.Checked);

        case Answer of
          mrYes:
            // Drop & Recreate
            begin
              if DropDestTable(IBDBDest, IBTransDest, DestTable) then
                chkCreateTable.Checked := True   // sicherstellen dass neu angelegt wird
              else
              begin
                StatusBar1.SimpleText := 'Drop failed — aborting copy.';
                Exit;
              end;
            end;

          mrNo:
            // Deselect Missing
            begin
              DeselectFields(MissingFields);
              ApplyFieldStates;
              StatusBar1.SimpleText := IntToStr(MissingFields.Count) +
                ' field(s) deselected.';
            end;

          mrIgnore:
            ; // Continue Anyway — nichts tun

          mrCancel:
            begin
              StatusBar1.SimpleText := 'Copy cancelled by user.';
              Exit;
            end;
        end;
      end;
    finally
      MissingFields.Free;
    end;
  end;

  // CREATE TABLE falls gewünscht
  if chkCreateTable.Checked then
  begin
    if not TableExists(IBDBDest, DestTable) then
    begin
      StatusBar1.SimpleText := 'Creating table ' + DestTable + '...';
      Application.ProcessMessages;

      if chkboxExternalTable.Checked then
        TableCreated := CreateExternalDestTable(IBDBDest, IBTransDest, DestTable)
      else
        TableCreated := CreateDestTable(IBDBDest, IBTransDest, DestTable);

      if not TableCreated then
      begin
        // Fehler ist bereits in CreateDestTable/CreateExternalDestTable
        // gemeldet worden (MessageDlg).
        StatusBar1.SimpleText := 'Table creation failed: ' + DestTable;
        Exit;   // ← Abbruch — KEIN Copy, KEIN Success-Dialog
      end;
    end;
  end;

  // ============================================================
  // Wenn chkCopyData NICHT angehakt ist → nur Tabelle, keine Daten
  // ============================================================
  if not chkCopyData.Checked then
  begin
    StatusBar1.SimpleText := 'Table created: ' + DestTable;
    ShowMessage('Table "' + DestTable + '" created successfully.' + sLineBreak +
                'No data copied (Copy Data is unchecked).');
    Exit;
  end;

  // From/To
  if rbRange.Checked then
  begin
    FromRow := StrToIntDef(edtFrom.Text, 1);
    ToRow := StrToIntDef(edtTo.Text, 0);
  end
  else
  begin
    FromRow := 1;
    ToRow := 0;
  end;

  // Felder aus Grid holen
  Fields := GetFieldTransforms;

  // Sicherstellen dass Transaktionen aktiv sind
  if Assigned(IBTransSource) and (not IBTransSource.InTransaction) then
    IBTransSource.StartTransaction;
  if Assigned(IBTransDest) and (not IBTransDest.InTransaction) then
    IBTransDest.StartTransaction;

  // ============================================================
  // SAME-DB: Insert Select (alle Feldtypen)
  // ============================================================
  if (FSourceDBIndex = FDestDBIndex) and rbInsertSelect.Checked then
  begin
    StatusBar1.SimpleText := 'Copying within same database (INSERT...SELECT)...';
    Application.ProcessMessages;

    CopyEngineLocal := TCopyTableDataLocal.Create(
      FSourceDBIndex, FDestDBIndex,
      MakeCaseSensitiveAuto(Trim(comboxSourceTables.Text)), DestTable,
      Fields,
      StrToIntDef(edtLocalBatchSize.Text, 500000),
      FromRow, ToRow
    );
    try
      CopyEngineLocal.Execute;
      Stats := CopyEngineLocal.Statistics;
    finally
      CopyEngineLocal.Free;
    end;

    if chkboxExternalTable.Checked then
      Stats.DestKind := 'External Table'
    else
      Stats.DestKind := 'Firebird Table';

    Stats.SystemInfo := GetSystemInfo(GetDBFileNameFromConnectionString(RegisteredDatabases[FDestDBIndex].RegRec.DatabaseName));

    // --- Client-Lib-Version (Server-Versionen kommen schon aus der Engine) ---
    if Assigned(RegisteredDatabases[FSourceDBIndex].IBDatabase) and
       Assigned(RegisteredDatabases[FSourceDBIndex].IBDatabase.FirebirdAPI) then
      Stats.ClientLibVersion := 'Firebird ' +
        RegisteredDatabases[FSourceDBIndex].IBDatabase.FirebirdAPI.GetImplementationVersion;

    // --- Formeln sammeln ---
    Stats.FormulasApplied := '';
    if chkUseFormula.Checked then
    begin
      FormulasText := '';
      for i := 0 to chkLstFields.Count - 1 do
      begin
        if chkLstFields.Checked[i] and (Trim(sgFields.Cells[3, i + 1]) <> '') then
          FormulasText := FormulasText +
            '  • ' + FFields[i].FieldName + ' = ' + sgFields.Cells[3, i + 1] + sLineBreak;
      end;
      Stats.FormulasApplied := FormulasText;
    end;

    // --- CREATE TABLE ---
    if chkCreateTable.Checked then
      Stats.CreateTableSQL := GenerateCreateTableSQL
    else
      Stats.CreateTableSQL := '';

    // --- Report anzeigen ---
    ReportForm := TfrmReport.Create(nil);
    try
      ReportForm.SetReportText(FormatTransferReport(Stats));
      ReportForm.ShowModal;
    finally
      ReportForm.Free;
    end;
  end

  // ============================================================
  // CROSS-DB: Execute Block (keine Arrays, BLOBs, Computed)
  // ============================================================
  else if rbExecuteBlock.Checked then
  begin
    StatusBar1.SimpleText := 'Copying across databases (Execute Block)...';
    Application.ProcessMessages;

    CopyEngineCrossExecuteBlock := TCopyTableDataCrossExecuteBlock.Create(
      FSourceDBIndex, FDestDBIndex,
      MakeCaseSensitiveAuto(Trim(comboxSourceTables.Text)), DestTable,
      Fields,
      StrToIntDef(edtExecBlockBatch.Text, 20000),
      FromRow, ToRow,
      IBDBSource, IBTransSource, IBDBDest, IBTransDest
    );
    try
      CopyEngineCrossExecuteBlock.Execute;
      Stats := CopyEngineCrossExecuteBlock.Statistics;
    finally
      CopyEngineCrossExecuteBlock.Free;
    end;

    if chkboxExternalTable.Checked then
      Stats.DestKind := 'External Table'
    else
      Stats.DestKind := 'Firebird Table';

    Stats.SystemInfo := GetSystemInfo(GetDBFileNameFromConnectionString(RegisteredDatabases[FDestDBIndex].RegRec.DatabaseName));

    // --- Client-Lib-Version (Server-Versionen kommen schon aus der Engine) ---
    if Assigned(RegisteredDatabases[FSourceDBIndex].IBDatabase) and
       Assigned(RegisteredDatabases[FSourceDBIndex].IBDatabase.FirebirdAPI) then
      Stats.ClientLibVersion := 'Firebird ' +
        RegisteredDatabases[FSourceDBIndex].IBDatabase.FirebirdAPI.GetImplementationVersion;


    // --- Formeln sammeln ---
    Stats.FormulasApplied := '';
    if chkUseFormula.Checked then
    begin
      FormulasText := '';
      for i := 0 to chkLstFields.Count - 1 do
      begin
        if chkLstFields.Checked[i] and (Trim(sgFields.Cells[3, i + 1]) <> '') then
          FormulasText := FormulasText +
            '  • ' + FFields[i].FieldName + ' = ' + sgFields.Cells[3, i + 1] + sLineBreak;
      end;
      Stats.FormulasApplied := FormulasText;
    end;

    // --- CREATE TABLE ---
    if chkCreateTable.Checked then
      Stats.CreateTableSQL := GenerateCreateTableSQL
    else
      Stats.CreateTableSQL := '';

    // --- Report anzeigen ---
    ReportForm := TfrmReport.Create(nil);
    try
      ReportForm.SetReportText(FormatTransferReport(Stats));
      ReportForm.ShowModal;
    finally
      ReportForm.Free;
    end;
  end

  // ============================================================
  // FBIntf (Cross-DB mit Arrays und BLOBs)
  // ============================================================
  else if rbFBIntf.Checked then
  begin
    StatusBar1.SimpleText := 'Copying via FBIntf (arrays + BLOBs supported)...';
    Application.ProcessMessages;

    CopyEngineFBIntf := TCopyTableDataFBIntf.Create(
      FSourceDBIndex, FDestDBIndex,
      Trim(comboxSourceTables.Text), Trim(edtDestTable.Text),
      Fields,
      StrToIntDef(edtFBIntfMemory.Text, 256),
      FromRow, ToRow,
      chkFBIntfForceRow.Checked, GetFBIntfRowByRowCommit
    );
    try
      CopyEngineFBIntf.Execute;
      Stats := CopyEngineFBIntf.Statistics;
    finally
      CopyEngineFBIntf.Free;
    end;

    if chkboxExternalTable.Checked then
      Stats.DestKind := 'External Table'
    else
      Stats.DestKind := 'Firebird Table';

    Stats.SystemInfo := GetSystemInfo(GetDBFileNameFromConnectionString(
      RegisteredDatabases[FDestDBIndex].RegRec.DatabaseName));

    if Assigned(RegisteredDatabases[FSourceDBIndex].IBDatabase) and
       Assigned(RegisteredDatabases[FSourceDBIndex].IBDatabase.FirebirdAPI) then
      Stats.ClientLibVersion := 'Firebird ' +
        RegisteredDatabases[FSourceDBIndex].IBDatabase.FirebirdAPI.GetImplementationVersion;

    // --- CREATE TABLE ---
    if chkCreateTable.Checked then
      Stats.CreateTableSQL := GenerateCreateTableSQL
    else
      Stats.CreateTableSQL := '';

    // --- Report ---
    ReportForm := TfrmReport.Create(nil);
    try
      ReportForm.SetReportText(FormatTransferReport(Stats));
      ReportForm.ShowModal;
    finally
      ReportForm.Free;
    end;
  end

  // ============================================================
  // CROSS-DB: Row-by-Row (Fallback, keine Arrays)
  // ============================================================
  else
  begin
    StatusBar1.SimpleText := 'Copying across databases (Row-by-Row)...';

    Application.ProcessMessages;

    CopyEngineRowByRow := TCopyTableDataRowByRow.Create(
      FSourceDBIndex, FDestDBIndex,
      MakeCaseSensitiveAuto(Trim(comboxSourceTables.Text)), DestTable,
      Fields,
      StrToIntDef(edtlRowByRowBatchSize.Text, 10000),
      FromRow, ToRow,
      IBDBSource, IBTransSource, IBDBDest, IBTransDest
    );

    try
      CopyEngineRowByRow.Execute;
      Stats := CopyEngineRowByRow.Statistics;
    finally
      CopyEngineRowByRow.Free;
    end;

    if chkboxExternalTable.Checked then
      Stats.DestKind := 'External Table'
    else
      Stats.DestKind := 'Firebird Table';

    Stats.SystemInfo := GetSystemInfo(GetDBFileNameFromConnectionString(RegisteredDatabases[FDestDBIndex].RegRec.DatabaseName));

    // --- Client-Lib-Version (Server-Versionen kommen schon aus der Engine) ---
    if Assigned(RegisteredDatabases[FSourceDBIndex].IBDatabase) and
       Assigned(RegisteredDatabases[FSourceDBIndex].IBDatabase.FirebirdAPI) then
      Stats.ClientLibVersion := 'Firebird ' +
        RegisteredDatabases[FSourceDBIndex].IBDatabase.FirebirdAPI.GetImplementationVersion;

    // --- Formeln sammeln ---
    Stats.FormulasApplied := '';
    if chkUseFormula.Checked then
    begin
      FormulasText := '';
      for i := 0 to chkLstFields.Count - 1 do
      begin
        if chkLstFields.Checked[i] and (Trim(sgFields.Cells[3, i + 1]) <> '') then
          FormulasText := FormulasText +
            '  • ' + FFields[i].FieldName + ' = ' + sgFields.Cells[3, i + 1] + sLineBreak;
      end;
      Stats.FormulasApplied := FormulasText;
    end;

    // --- CREATE TABLE ---
    if chkCreateTable.Checked then
      Stats.CreateTableSQL := GenerateCreateTableSQL
    else
      Stats.CreateTableSQL := '';

    // --- Report anzeigen ---
    ReportForm := TfrmReport.Create(nil);
    try
      ReportForm.SetReportText(FormatTransferReport(Stats));
      ReportForm.ShowModal;
    finally
      ReportForm.Free;
    end;
  end;

  StatusBar1.SimpleText := 'Copy completed: ' + Trim(comboxSourceTables.Text) + ' → ' + DestTable;
end;

procedure TfrmCloneTable.btnExternalFileClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    edtExternalFile.Text := OpenDialog1.FileName;
end;

procedure TfrmCloneTable.btnGenTestFormulasClick(Sender: TObject);
var
  i: Integer;
  FieldType: string;
begin
  for i := 0 to High(FFields) do
  begin
    if FFields[i].IsComputed then
      Continue;

    FieldType := UpperCase(FFields[i].FieldType);

    // Standard-Formel: Feldwert unverändert
    FFields[i].Formula := '$1';

    // Spezifische Testformeln basierend NUR auf Datentypen
    if Pos('VARCHAR', FieldType) > 0 then
      FFields[i].Formula := '$1 || ''_CLONED'''

    else if Pos('CHAR', FieldType) > 0 then
      FFields[i].Formula := 'UPPER($1)'

    else if Pos('BLOB', FieldType) > 0 then
      FFields[i].Formula := '$1 || '' (cloned)'''

    else if Pos('INTEGER', FieldType) > 0 then
      FFields[i].Formula := '$1 + 10000000'

    else if Pos('SMALLINT', FieldType) > 0 then
      FFields[i].Formula := '$1 * 2'

    else if Pos('BIGINT', FieldType) > 0 then
      FFields[i].Formula := '$1 + 10000000'

    else if Pos('NUMERIC', FieldType) > 0 then
      FFields[i].Formula := '$1 * 1.1'

    else if Pos('DECIMAL', FieldType) > 0 then
      FFields[i].Formula := '$1 * 1.1'

    else if Pos('FLOAT', FieldType) > 0 then
      FFields[i].Formula := '$1 * 1.15 + 500'

    else if Pos('DOUBLE', FieldType) > 0 then
      FFields[i].Formula := '$1 * 1.05'

    else if Pos('DATE', FieldType) > 0 then
      FFields[i].Formula := '$1 + 30'

    else if Pos('TIMESTAMP', FieldType) > 0 then
      FFields[i].Formula := '$1 + 365'

    else if Pos('TIME', FieldType) > 0 then
      FFields[i].Formula := '$1 + 3600'

    else if Pos('BOOLEAN', FieldType) > 0 then
      FFields[i].Formula := 'true';

    // Formel ins Grid schreiben
    sgFields.Cells[3, i + 1] := FFields[i].Formula;
  end;

  StatusBar1.SimpleText := IntToStr(Length(FFields)) + ' test formulas generated.';
end;

procedure TfrmCloneTable.btnAddToQueueClick(Sender: TObject);
begin
  MessageDlg('Queue feature coming soon!', mtInformation, [mbOK], 0);
end;

procedure TfrmCloneTable.btnCancelGlobalClick(Sender: TObject);
begin
  Close;
end;

// ============================================================================
// HILFSFUNKTIONEN
// ============================================================================

function TfrmCloneTable.TableExists(DB: TIBDatabase; TableName: string): Boolean;
var
  Q: TIBQuery;
begin
  Result := False;
  Q := TIBQuery.Create(nil);
  try
    Q.Database := DB;
    Q.Transaction := DB.DefaultTransaction;
    Q.AllowAutoActivateTransaction := True;
    Q.SQL.Text :=
      'SELECT RDB$RELATION_NAME FROM RDB$RELATIONS ' +
      'WHERE RDB$RELATION_NAME = :T AND RDB$VIEW_BLR IS NULL AND RDB$SYSTEM_FLAG = 0';
    Q.ParamByName('T').AsString := StripIdentifierQuotes(TableName);
    Q.Open;
    Result := (Q.RecordCount > 0);
    Q.Close;
  finally
    Q.Free;
  end;
end;

{// ============================================================
// TABLE EXISTS — prüft, ob eine Tabelle (kein View) in der DB existiert.
//
// Wichtig:
//   * Arbeitet mit einer eigenen, frischen Transaktion
//     → immer aktueller Snapshot, unabhängig vom Zustand der
//       aufrufenden Transaktion (z. B. direkt nach einem DROP)
//   * read_committed + rec_version + nowait → keine Blockaden
//   * not Q.EOF statt RecordCount (IBX-RecordCount ist unzuverlässig)
//   * Erkennt nur echte Tabellen, keine Views, keine System-Tabellen
// ============================================================
function TfrmCloneTable.TableExists(DB: TIBDatabase; TableName: string): Boolean;
var
  Q: TIBQuery;
  T: TIBTransaction;
  CleanName: string;
begin
  Result := False;

  if DB = nil then
    Exit;

  CleanName := StripIdentifierQuotes(TableName);
  if Trim(CleanName) = '' then
    Exit;

  Q := nil;
  T := nil;
  try
    // ---- Eigene, frische Transaktion ----
    T := TIBTransaction.Create(nil);
    try
      T.DefaultDatabase := DB;
      T.Params.Clear;
      T.Params.Add('read_committed');
      T.Params.Add('rec_version');
      T.Params.Add('nowait');
      T.StartTransaction;

      // ---- Query ----
      Q := TIBQuery.Create(nil);
      try
        Q.Database := DB;
        Q.Transaction := T;
        Q.SQL.Text :=
          'SELECT RDB$RELATION_NAME FROM RDB$RELATIONS ' +
          'WHERE RDB$RELATION_NAME = :T ' +
          '  AND RDB$VIEW_BLR IS NULL ' +
          '  AND RDB$SYSTEM_FLAG = 0';
        Q.ParamByName('T').AsString := CleanName;
        Q.Open;

        Q.First;
        Result := not Q.EOF;

        Q.Close;
      finally
        Q.Free;
        Q := nil;
      end;

      if T.InTransaction then
        T.Commit;

    finally
      T.Free;
      T := nil;
    end;

  except
    // Bei Fehler: Result bleibt False.
    // Stilles Schlucken ist hier OK, weil TableExists eine reine
    // Ja/Nein-Frage beantwortet — kein Grund, den User zu behelligen.
    if Assigned(Q) then
    begin
      try Q.Close; except end;
      FreeAndNil(Q);
    end;
    if Assigned(T) then
    begin
      try
        if T.InTransaction then T.Rollback;
      except end;
      FreeAndNil(T);
    end;
  end;
end;}

function TfrmCloneTable.CreateDestTable(DestDB: TIBDatabase; DestTrans: TIBTransaction; TableName: string): Boolean;
var
  SQL: string;
begin
  Result := False;
  SQL := GenerateCreateTableSQL;

  if SQL = '' then Exit;

  IBXScript1.Database := DestDB;
  IBXScript1.Transaction := DestTrans;

  try
    if not DestTrans.InTransaction then
      DestTrans.StartTransaction;

    IBXScript1.ExecSQLScript(SQL);
    DestTrans.Commit;
    Result := True;
  except
    on E: Exception do
    begin
      MessageDlg('Failed to create table: ' + E.Message, mtError, [mbOK], 0);
      DestTrans.Rollback;
    end;
  end;
end;

procedure TfrmCloneTable.FormShow(Sender: TObject);
var
  i: Integer;
begin
  frmThemeSelector.btnApplyClick(Self);

  // Guard AUS – ab jetzt dürfen die Change-Handler feuern
  FUpdatingCombos := False;

  // Vorbelegung anwenden, falls ein Tabellen-Node übergeben wurde
  ApplyInitialSelection;

  // Kaskade explizit auslösen
  if comboxSourceServer.Items.Count > 0 then
    comboxSourceServerChange(nil);

  // Wenn eine konkrete Tabelle vorgegeben ist → auswählen
  if FInitialTableName <> '' then
  begin
    i := comboxSourceTables.Items.IndexOf(FInitialTableName);
    if i >= 0 then
    begin
      comboxSourceTables.ItemIndex := i;
      comboxSourceTablesChange(nil);   // löst LoadFields + edtDestTable-Update aus
    end;
  end;

  if comboxDestServer.Items.Count > 0 then
    comboxDestServerChange(nil);
end;

procedure TfrmCloneTable.rbAllRowsChange(Sender: TObject);
begin
  edtFrom.Enabled := not rbAllRows.Checked;
  edtTo.Enabled := not rbAllRows.Checked;
end;

procedure TfrmCloneTable.LoadFormulaPresets;
var
  i: Integer;
begin
  cbFormulaPreset.Items.Clear;
  cbFormulaPreset.Items.Add('None');

  for i := 0 to FormulaPresetManager.PresetCount - 1 do
    cbFormulaPreset.Items.Add(FormulaPresetManager.PresetName(i));

  cbFormulaPreset.ItemIndex := 0;
end;

procedure TfrmCloneTable.cbFormulaPresetChange(Sender: TObject);
var
  Preset: TFormulaPreset;
  i: Integer;
  Formula: string;
begin
  // "None" ausgewählt → alle Formeln löschen
  if cbFormulaPreset.ItemIndex <= 0 then
  begin
    for i := 0 to High(FFields) do
    begin
      if FFields[i].IsComputed then Continue;

      FFields[i].Formula := '';
      sgFields.Cells[3, i + 1] := '';
    end;

    StatusBar1.SimpleText := 'All formulas cleared.';
    Exit;
  end;

  // Preset ausgewählt → Formeln anwenden
  Preset := FormulaPresetManager.GetPreset(cbFormulaPreset.Text);
  if Preset = nil then Exit;

  for i := 0 to High(FFields) do
  begin
    if FFields[i].IsComputed then Continue;

    Formula := Preset.GetFormulaForFieldType(FFields[i].FieldType);
    FFields[i].Formula := Formula;
    sgFields.Cells[3, i + 1] := Formula;
  end;

  StatusBar1.SimpleText := 'Preset "' + Preset.Name + '" applied to ' +
                           IntToStr(Length(FFields)) + ' fields.';
end;

procedure TfrmCloneTable.chkboxExternalTableChange(Sender: TObject);
begin
  edtExternalFile.Enabled := chkboxExternalTable.Checked;
  btnExternalFile.Enabled := chkboxExternalTable.Checked;
end;

// ============================================================
// Sofort-Sync bei jedem Klick im Hauptformular.
// Aktualisiert FFields/sgFields und deaktiviert btnExecute,
// wenn kein Feld mehr ausgewählt ist.
// ============================================================
procedure TfrmCloneTable.chkLstFieldsClickCheck(Sender: TObject);
var
  i: Integer;
  AnyChecked: Boolean;
begin
  ApplyFieldStates;

  AnyChecked := False;
  for i := 0 to chkLstFields.Count - 1 do
    if chkLstFields.Checked[i] then
    begin
      AnyChecked := True;
      Break;
    end;

  btnExecute.Enabled := AnyChecked;
end;

procedure TfrmCloneTable.chkUseFormulaChange(Sender: TObject);
begin
  // Optional: Formel-Spalte im Grid ausgrauen, wenn deaktiviert
  if sgFields.Columns.Count < 4 then Exit;

  if chkUseFormula.Checked then
    sgFields.Columns[3].Color := clWindow
  else
    sgFields.Columns[3].Color := clBtnFace;
end;

procedure TfrmCloneTable.btnRefreshPresetsClick(Sender: TObject);
begin
  FormulaPresetManager.Reload;
  LoadFormulaPresets;
  StatusBar1.SimpleText := IntToStr(FormulaPresetManager.PresetCount) + ' presets loaded.';
end;

end.
