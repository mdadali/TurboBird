unit clone_table_dialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Grids, CheckLst, Menus, IB, IBQuery, IBDatabase, IBDatabaseInfo,
  IBExtract, ibxscript,

  SysTables,
  turbocommon,

  uCopyTableDataLocal,
  uCopyTableDataCrossExecuteBlock,
  uCopyTableDataCrossRowByRow,
  uFormulaPresets,

  fmetaquerys,
  uthemeselector,

  uCopyStatistics,
  frmCopyReport
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
    btnCancel: TButton;
    btnDeselectAll: TButton;
    btnExecute: TButton;
    btnNewDB: TButton;
    btnOpenExternalFile: TButton;
    btnPreviewSQL: TButton;
    btnAddToQueue: TButton;
    btnRefreshPresets: TButton;
    btnExternalFile: TButton;
    btnGenTestFormulas: TButton;
    btnSelectAll: TButton;
    chkCopyData: TCheckBox;
    chkLstFields: TCheckListBox;
    chkUseFormula: TCheckBox;
    chkboxExternalTable: TCheckBox;
    chkCreateTable: TCheckBox;
    cbFormulaPreset: TComboBox;
    comboxDestDB: TComboBox;
    comboxSourceDB: TComboBox;
    comboxDestServer: TComboBox;
    comboxSourceServer: TComboBox;
    comboxSourceTables: TComboBox;
    Destination: TGroupBox;
    edtExternalFile: TEdit;
    edtDestTable: TEdit;
    edtBatchSize: TEdit;
    edtFrom: TEdit;
    edtTo: TEdit;
    grboxCopyOptions: TGroupBox;
    grBoxFormulaFields: TGroupBox;
    grBoxSource: TGroupBox;
    grBoxFormulaPresets: TGroupBox;
    grBoxCopyMethod: TGroupBox;
    grboxFields: TGroupBox;
    IBDBDest: TIBDatabase;
    IBDBSource: TIBDatabase;
    IBQueryDest: TIBQuery;
    IBQuerySource: TIBQuery;
    IBTransDest: TIBTransaction;
    IBTransSource: TIBTransaction;
    IBXScript1: TIBXScript;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    lbSourceTable: TLabel;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    pnlSelector: TPanel;
    pnlFieldsSelectButtons: TPanel;
    pnlTop: TPanel;
    PopupMenu1: TPopupMenu;
    rbInsertSelect: TRadioButton;
    rbRowByRow: TRadioButton;
    rbExecuteBlock: TRadioButton;
    rbAllRows: TRadioButton;
    rbRange: TRadioButton;
    sgFields: TStringGrid;
    StatusBar1: TStatusBar;
    procedure btnAddToQueueClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnExecuteClick(Sender: TObject);
    procedure btnExternalFileClick(Sender: TObject);
    procedure btnGenTestFormulasClick(Sender: TObject);
    procedure btnRefreshPresetsClick(Sender: TObject);
    procedure btnSelectAllClick(Sender: TObject);
    procedure btnDeselectAllClick(Sender: TObject);
    procedure btnPreviewSQLClick(Sender: TObject);
    procedure cbFormulaPresetChange(Sender: TObject);
    procedure chkboxExternalTableChange(Sender: TObject);
    procedure chkCreateTableChange(Sender: TObject);
    procedure chkUseFormulaChange(Sender: TObject);
    procedure comboxDestDBChange(Sender: TObject);
    procedure comboxDestServerChange(Sender: TObject);
    procedure comboxSourceDBChange(Sender: TObject);
    procedure comboxSourceServerChange(Sender: TObject);
    procedure comboxSourceTablesChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbAllRowsChange(Sender: TObject);
    procedure sgFieldsDblClick(Sender: TObject);
  private
    FNodeInfos: TPNodeInfos;
    FSourceDBIndex: Integer;
    FDestDBIndex: Integer;
    FFields: array of TFieldInfo;
    FUpdatingCombos: Boolean;

    FInitialTableName: string;
    FInitialDBIndex: Integer;

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
begin
  SameServer := SameText(Trim(comboxSourceServer.Text), Trim(comboxDestServer.Text));
  SameDB := SameServer and SameText(Trim(comboxSourceDB.Text), Trim(comboxDestDB.Text));

  // GroupBox ist IMMER enabled
  grBoxCopyMethod.Enabled := True;

  // Insert Select: nur bei SameDB möglich
  rbInsertSelect.Enabled := SameDB;

  // Execute Block: nur bei Cross-DB sinnvoll
  rbExecuteBlock.Enabled := not SameDB;

  // Row-by-Row: immer möglich
  rbRowByRow.Enabled := True;

  // Sicherstellen, dass eine gültige Option ausgewählt ist
  if SameDB then
  begin
    // Insert Select ist der Default
    if not rbInsertSelect.Checked and not rbRowByRow.Checked then
      rbInsertSelect.Checked := True;

    // Falls eine ungültige Option ausgewählt war → korrigieren
    if rbExecuteBlock.Checked then
    begin
      rbExecuteBlock.Checked := False;
      rbInsertSelect.Checked := True;
    end;

    StatusBar1.SimpleText := 'Same database: Insert Select (default) or Row-by-Row.';
  end
  else
  begin
    // Cross-DB: Default = Execute Block
    if not rbExecuteBlock.Checked and not rbRowByRow.Checked then
      rbExecuteBlock.Checked := True;

    // Falls Insert Select ausgewählt war → korrigieren
    if rbInsertSelect.Checked then
    begin
      rbInsertSelect.Checked := False;
      rbExecuteBlock.Checked := True;
    end;

    StatusBar1.SimpleText := 'Cross-database: Execute Block (default) or Row-by-Row.';
  end;
end;

procedure TfrmCloneTable.FormCreate(Sender: TObject);
begin
  edtBatchSize.Text := IntToStr(DefaultBatchSize);

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

// ============================================================================
// SOURCE
// ============================================================================

{procedure TfrmCloneTable.comboxSourceServerChange(Sender: TObject);
begin
  if FUpdatingCombos then Exit;
  FUpdatingCombos := True;
  try
    FillSourceDBCombo;
  finally
    FUpdatingCombos := False;
  end;
  UpdateCopyMethodAvailability;
end;}

{procedure TfrmCloneTable.comboxSourceDBChange(Sender: TObject);
begin
  if FUpdatingCombos then Exit;
  FUpdatingCombos := True;
  try
    comboxSourceTables.Items.Clear;
    if ConfigureSourceConnection then
    begin
      FillSourceTableCombo;
      edtDestTable.Text := Trim(comboxSourceTables.Text) + '_CLONED';
      LoadFields;
    end
    else
      grBoxCopyMethod.Enabled := False;
  finally
    FUpdatingCombos := False;
  end;
  UpdateCopyMethodAvailability;
end;}

procedure TfrmCloneTable.comboxSourceServerChange(Sender: TObject);
begin
  if FUpdatingCombos then Exit;
  FUpdatingCombos := True;
  try
    FillSourceDBCombo;

    // Nach dem Füllen explizit die Kaskade auslösen,
    // damit auch bei nur einer DB der Login-/Lade-Flow läuft.
    if comboxSourceDB.Items.Count > 0 then
    begin
      comboxSourceDB.ItemIndex := 0;
      // Handler explizit aufrufen, weil OnChange durch den Guard blockiert wird
    end;
  finally
    FUpdatingCombos := False;
  end;

  // Jetzt Guard ist aus → explizit auslösen
  if comboxSourceDB.Items.Count > 0 then
    comboxSourceDBChange(nil);

  UpdateCopyMethodAvailability;
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
    else
    begin
      grBoxCopyMethod.Enabled := False;
      StatusBar1.SimpleText := 'Source database not connected.';
    end;
  finally
    FUpdatingCombos := False;
  end;
  UpdateCopyMethodAvailability;
end;

procedure TfrmCloneTable.comboxSourceTablesChange(Sender: TObject);
begin
  if FUpdatingCombos then Exit;
  if Trim(comboxSourceTables.Text) = '' then Exit;   // Schutz gegen leeren Namen

  edtDestTable.Text := Trim(comboxSourceTables.Text) + '_CLONED';
  LoadFields;
end;

procedure TfrmCloneTable.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
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
  i: Integer;
begin
  Result := False;
  comboxSourceDB.Items.Clear;

  for i := 0 to High(RegisteredDatabases) do
    if SameText(RegisteredDatabases[i].RegRec.ServerName, comboxSourceServer.Text) then
      comboxSourceDB.Items.Add(RegisteredDatabases[i].RegRec.Title);

  if comboxSourceDB.Items.Count > 0 then
  begin
    comboxSourceDB.ItemIndex := 0;
    Result := True;
    if ConfigureSourceConnection then
    begin
      FillSourceTableCombo;
      LoadFields;
    end;
  end else
  begin
    chkLstFields.Items.Clear;
    sgFields.RowCount := 0;
    comboxSourceTables.Items.Clear;
  end;
end;

function TfrmCloneTable.FillSourceTableCombo: boolean;
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

    // --- Credentials nachreichen (RegRec → Session-Cache → Live) ---
    if Trim(IBDBSource.Params.Values['user_name']) = '' then
      IBDBSource.Params.Values['user_name'] := DBRec.RegRec.UserName;

    if Trim(IBDBSource.Params.Values['password']) = '' then
    begin
      Pwd := DBRec.RegRec.Password;

      if Pwd = '' then
        Pwd := GetDBSessionPassword(DBRec.RegRec.ServerName, DBRec.RegRec.DatabaseName);

      if Pwd = '' then
        Pwd := GetServerSessionPassword(DBRec.RegRec.ServerName);

      if (Pwd = '') and DBRec.RegRec.IsEmbedded then
        Pwd := 'embedded_local';

      if Pwd <> '' then
        IBDBSource.Params.Values['password'] := Pwd;
    end;

    // --- Transaktion: Params kopieren, DefaultDatabase auf Form-DB setzen ---
    if DBRec.IBTransaction.Params.Count > 0 then
      IBTransSource.Params.Assign(DBRec.IBTransaction.Params)
    else
    begin
      IBTransSource.Params.Clear;
      IBTransSource.Params.Add('read_committed');
      IBTransSource.Params.Add('rec_version');
      IBTransSource.Params.Add('nowait');
    end;

    // --- Jetzt verbinden (Form-eigene Verbindung!) ---
    if not IBDBSource.Connected then
      IBDBSource.Connected := True;
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

    // --- Credentials nachreichen (RegRec → Session-Cache → Live) ---
    if Trim(IBDBDest.Params.Values['user_name']) = '' then
      IBDBDest.Params.Values['user_name'] := DBRec.RegRec.UserName;

    if Trim(IBDBDest.Params.Values['password']) = '' then
    begin
      Pwd := DBRec.RegRec.Password;

      if Pwd = '' then
        Pwd := GetDBSessionPassword(DBRec.RegRec.ServerName, DBRec.RegRec.DatabaseName);

      if Pwd = '' then
        Pwd := GetServerSessionPassword(DBRec.RegRec.ServerName);

      if (Pwd = '') and DBRec.RegRec.IsEmbedded then
        Pwd := 'embedded_local';

      if Pwd <> '' then
        IBDBDest.Params.Values['password'] := Pwd;
    end;

    if DBRec.IBTransaction.Params.Count > 0 then
      IBTransDest.Params.Assign(DBRec.IBTransaction.Params)
    else
    begin
      IBTransDest.Params.Clear;
      IBTransDest.Params.Add('read_committed');
      IBTransDest.Params.Add('rec_version');
      IBTransDest.Params.Add('nowait');
    end;
    IBTransDest.DefaultDatabase := IBDBDest;

    // --- Query an die Form-eigenen Komponenten binden ---
    IBQueryDest.Database := IBDBDest;
    IBQueryDest.Transaction := IBTransDest;

    // --- Verbinden ---
    if not IBDBDest.Connected then
      IBDBDest.Connected := True;
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
    begin
      grBoxCopyMethod.Enabled := False;
      StatusBar1.SimpleText := 'Destination database not connected.';
    end;
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
  Iso: TIsolatedQuery;
  i: Integer;
  FieldName, FieldType, ComputedSource: string;
  FSize: Integer;
begin
  // === Absicherung: ungültige Ausgangslage verhindern ===
  if FSourceDBIndex < 0 then Exit;
  if Trim(comboxSourceTables.Text) = '' then Exit;
  if not Assigned(RegisteredDatabases[FSourceDBIndex].IBDatabase) then Exit;

  SetLength(FFields, 0);
  chkLstFields.Clear;
  sgFields.RowCount := 1;

  try
    Iso := GetFieldsIsolated(RegisteredDatabases[FSourceDBIndex].IBDatabase,
                             Trim(comboxSourceTables.Text));
    try
      while not Iso.Query.EOF do
      begin
        FieldName := Trim(Iso.Query.FieldByName('field_name').AsString);
        GetFieldType(Iso.Query, FieldType, FSize);
        ComputedSource := Trim(Iso.Query.FieldByName('computed_source').AsString);

        i := Length(FFields);
        SetLength(FFields, i + 1);
        FFields[i].FieldName := FieldName;
        FFields[i].FieldType := FieldType;
        FFields[i].IsComputed := (ComputedSource <> '');
        FFields[i].Checked := True;
        FFields[i].Formula := '';

        chkLstFields.Items.Add(FieldName);
        chkLstFields.Checked[i] := True;

        sgFields.RowCount := i + 2;
        sgFields.Cells[0, i + 1] := '1';
        sgFields.Cells[1, i + 1] := FieldName;
        sgFields.Cells[2, i + 1] := FieldType;
        sgFields.Cells[3, i + 1] := '';

        if FFields[i].IsComputed then
          sgFields.Cells[3, i + 1] := '(computed)';

        Iso.Query.Next;
      end;
    finally
      Iso.Free;
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

  //btnGenTestFormulasClick(nil);
end;

procedure TfrmCloneTable.btnSelectAllClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to chkLstFields.Count - 1 do
  begin
    chkLstFields.Checked[i] := True;
    sgFields.Cells[0, i + 1] := '1';
  end;
end;

procedure TfrmCloneTable.btnDeselectAllClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to chkLstFields.Count - 1 do
  begin
    chkLstFields.Checked[i] := False;
    sgFields.Cells[0, i + 1] := '0';
  end;
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
  i: Integer;
  Iso: TIsolatedQuery;
  FieldName, FieldType, DefaultSource, NullFlag, ComputedSource: string;
  Line: string;
  FSize: Integer;
begin
  SL := TStringList.Create;
  try
    SL.Add('CREATE TABLE ' + edtDestTable.Text + ' (');

    Iso := GetFieldsIsolated(RegisteredDatabases[FSourceDBIndex].IBDatabase, Trim(comboxSourceTables.Text));
    try
      i := 0;
      while not Iso.Query.EOF do
      begin
        FieldName := Trim(Iso.Query.FieldByName('field_name').AsString);

        if chkLstFields.Checked[i] then
        begin
          GetFieldType(Iso.Query, FieldType, FSize);
          DefaultSource := Trim(Iso.Query.FieldByName('field_default_source').AsString);
          NullFlag := Iso.Query.FieldByName('field_not_null_constraint').AsString;
          ComputedSource := Trim(Iso.Query.FieldByName('computed_source').AsString);

          if ComputedSource <> '' then
          begin
            // COMPUTED BY: Immer anlegen bei normalen Tabellen
            Line := '  ' + FieldName + ' COMPUTED BY (' + ComputedSource + '),';
          end
          else
          begin
            Line := '  ' + FieldName + ' ' + FieldType;

            if DefaultSource <> '' then
            begin
              // Firebird liefert DefaultSource MIT dem Wort "DEFAULT" davor.
              // Nur ein "DEFAULT" anhängen, wenn noch nicht vorhanden.
              if UpperCase(Copy(DefaultSource, 1, 7)) = 'DEFAULT' then
                Line := Line + ' ' + DefaultSource
              else
                Line := Line + ' DEFAULT ' + DefaultSource;
            end;

            if NullFlag = '1' then
              Line := Line + ' NOT NULL';
            Line := Line + ',';
          end;

          SL.Add(Line);
        end;

        Iso.Query.Next;
        Inc(i);
      end;
    finally
      Iso.Free;
    end;

    // Letztes Komma entfernen
    if SL.Count > 1 then
    begin
      i := SL.Count - 1;
      Line := SL[i];
      if Line[Length(Line)] = ',' then
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
begin
  SourceFields := '';
  DestFields := '';

  for i := 0 to chkLstFields.Count - 1 do
  begin
    if chkLstFields.Checked[i] and (not FFields[i].IsComputed) then
    begin
      if SourceFields <> '' then SourceFields := SourceFields + ', ';
      SourceFields := SourceFields + FFields[i].FieldName;

      if DestFields <> '' then DestFields := DestFields + ', ';
      DestFields := DestFields + FFields[i].FieldName;
    end;
  end;

  Result := 'INSERT INTO ' + edtDestTable.Text + ' (' + DestFields + ')' + sLineBreak +
            'SELECT ' + SourceFields + sLineBreak +
            'FROM ' + Trim(comboxSourceTables.Text) + ';';
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
    SL.Add('CREATE TABLE ' + edtDestTable.Text + ' EXTERNAL FILE ''' +
           edtExternalFile.Text + ''' (');

    for i := 0 to chkLstFields.Count - 1 do
    begin
      if not chkLstFields.Checked[i] then Continue;

      FieldName := chkLstFields.Items[i];
      FieldType := FFields[i].FieldType;   // Firebird-Typ, z. B. 'VARCHAR(50)'

      // Keine BLOBs, Arrays oder Computed Fields in externen Tabellen!
      if FFields[i].IsComputed then Continue;
      if Pos('BLOB', UpperCase(FieldType)) > 0 then Continue;
      if Pos('[', FieldType) > 0 then Continue;  // Arrays

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
  CopyEngineCrossRowByRow: TCopyTableDataCrossRowByRow;
  DestTable: string;
  SkippedFields: string;
  FromRow, ToRow: Integer;
  i: integer;

  NeedsProblemCheck: Boolean;
  IsProblemField: Boolean;
  TmpMethodName: string;

  ReportForm: TfrmCopyReport;
  Stats: TCopyStatistics;
  FormulasText: string;
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

  // CREATE TABLE falls gewünscht
  if chkCreateTable.Checked then
  begin
    if not TableExists(IBDBDest, DestTable) then
    begin
      StatusBar1.SimpleText := 'Creating table ' + DestTable + '...';
      Application.ProcessMessages;

      if chkboxExternalTable.Checked then
        CreateExternalDestTable(IBDBDest, IBTransDest, DestTable)
      else
        CreateDestTable(IBDBDest, IBTransDest, DestTable);
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

  // ============================================================
  // Problemfelder prüfen – abhängig von der Engine
  //   External / Execute Block: BLOB, Array, Computed
  //   Row-by-Row:               nur Array
  // ============================================================
  NeedsProblemCheck := chkboxExternalTable.Checked or
                       ((FSourceDBIndex <> FDestDBIndex) and rbExecuteBlock.Checked) or
                       rbRowByRow.Checked;

  if NeedsProblemCheck then
  begin
    SkippedFields := '';
    for i := 0 to chkLstFields.Count - 1 do
    begin
      if chkLstFields.Checked[i] then
      begin
        IsProblemField := False;

        // Arrays sind immer problematisch (außer bei Insert Select)
        if Pos('[', FFields[i].FieldType) > 0 then
          IsProblemField := True;

        // BLOB und Computed nur bei External/Execute Block
        if chkboxExternalTable.Checked or
           ((FSourceDBIndex <> FDestDBIndex) and rbExecuteBlock.Checked) then
        begin
          if FFields[i].IsComputed or
             (Pos('BLOB', UpperCase(FFields[i].FieldType)) > 0) then
            IsProblemField := True;
        end;

        if IsProblemField then
        begin
          chkLstFields.Checked[i] := False;
          sgFields.Cells[0, i + 1] := '0';
          if SkippedFields <> '' then SkippedFields := SkippedFields + sLineBreak;
          SkippedFields := SkippedFields + '  • ' + FFields[i].FieldName +
                           '  (' + FFields[i].FieldType + ')';
        end;
      end;
    end;

    if SkippedFields <> '' then
    begin
      // Methode ermitteln für die Meldung
      if chkboxExternalTable.Checked then
        TmpMethodName := 'External Table'
      else if (FSourceDBIndex <> FDestDBIndex) and rbExecuteBlock.Checked then
        TmpMethodName := 'Execute Block'
      else if rbRowByRow.Checked then
        TmpMethodName := 'Row-by-Row'
      else
        TmpMethodName := 'The selected method';

      if MessageDlg(
           TmpMethodName + ' cannot copy the following field types:' + sLineBreak +
           sLineBreak +
           SkippedFields + sLineBreak +
           sLineBreak +
           'These fields have been deselected.' + sLineBreak +
           sLineBreak +
           'Hint:' + sLineBreak +
           '  • Arrays       → use Insert Select (same DB) or gbak' + sLineBreak +
           '  • BLOB/Computed → use Insert Select or Row-by-Row' + sLineBreak +
           sLineBreak +
           'Start the copy operation anyway?',
           mtWarning, [mbYes, mbNo], 0) <> mrYes then
      begin
        StatusBar1.SimpleText := 'Copy cancelled by user.';
        Exit;
      end;
    end;
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
      StrToIntDef(edtBatchSize.Text, 500000),
      FromRow, ToRow
    );
    try
      CopyEngineLocal.Execute;
      Stats := CopyEngineLocal.Statistics;
    finally
      CopyEngineLocal.Free;
    end;

    Stats.DestIsExternal := chkboxExternalTable.Checked;

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
    ReportForm := TfrmCopyReport.Create(nil);
    try
      ReportForm.SetReportText(FormatCopyReport(Stats));
      ReportForm.ShowModal;
    finally
      ReportForm.Free;
    end;
  end

  // ============================================================
  // CROSS-DB: Execute Block (keine Arrays, BLOBs, Computed)
  // ============================================================
  else if (FSourceDBIndex <> FDestDBIndex) and rbExecuteBlock.Checked then
  begin
    StatusBar1.SimpleText := 'Copying across databases (Execute Block)...';
    Application.ProcessMessages;

    CopyEngineCrossExecuteBlock := TCopyTableDataCrossExecuteBlock.Create(
      FSourceDBIndex, FDestDBIndex,
      MakeCaseSensitiveAuto(Trim(comboxSourceTables.Text)), DestTable,
      Fields,
      StrToIntDef(edtBatchSize.Text, 10000),
      FromRow, ToRow,
      IBDBSource, IBTransSource, IBDBDest, IBTransDest
    );
    try
      CopyEngineCrossExecuteBlock.Execute;
      Stats := CopyEngineCrossExecuteBlock.Statistics;
    finally
      CopyEngineCrossExecuteBlock.Free;
    end;

    Stats.DestIsExternal := chkboxExternalTable.Checked;

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
    ReportForm := TfrmCopyReport.Create(nil);
    try
      ReportForm.SetReportText(FormatCopyReport(Stats));
      ReportForm.ShowModal;
    finally
      ReportForm.Free;
    end;
  end

  // ============================================================
  // CROSS-DB: Row-by-Row (keine Arrays)
  // ============================================================
  else
  begin
    StatusBar1.SimpleText := 'Copying across databases (Row-by-Row)...';
    Application.ProcessMessages;

    CopyEngineCrossRowByRow := TCopyTableDataCrossRowByRow.Create(
      FSourceDBIndex, FDestDBIndex,
      MakeCaseSensitiveAuto(Trim(comboxSourceTables.Text)), DestTable,
      Fields,
      StrToIntDef(edtBatchSize.Text, 10000),
      FromRow, ToRow,
      IBDBSource, IBTransSource, IBDBDest, IBTransDest
    );

    try
      CopyEngineCrossRowByRow.Execute;
      Stats := CopyEngineCrossRowByRow.Statistics;
    finally
      CopyEngineCrossRowByRow.Free;
    end;

    Stats.DestIsExternal := chkboxExternalTable.Checked;

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
    ReportForm := TfrmCopyReport.Create(nil);
    try
      ReportForm.SetReportText(FormatCopyReport(Stats));
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

procedure TfrmCloneTable.btnCancelClick(Sender: TObject);
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
      'WHERE UPPER(RDB$RELATION_NAME) = :T AND RDB$VIEW_BLR IS NULL AND RDB$SYSTEM_FLAG = 0';
    Q.ParamByName('T').AsString := UpperCase(TableName);
    Q.Open;
    Result := not Q.EOF;
    Q.Close;
  finally
    Q.Free;
  end;
end;

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

procedure TfrmCloneTable.chkCreateTableChange(Sender: TObject);
begin

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
