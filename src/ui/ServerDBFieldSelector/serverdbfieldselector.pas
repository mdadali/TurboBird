unit ServerDBFieldSelector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, CheckLst, ComCtrls, Buttons, IBDatabase, IBQuery,
  IBCustomDataSet, Variants, DB,

  turbocommon,

  fmetaquerys;  // Für GetFieldsIsolated



type
  TSelectorFieldInfo = record
    Name: string;
    FieldType: string;        // z.B. 'VARCHAR(100)', 'INTEGER', 'BLOB'
    IsComputed: Boolean;
    IsArray: Boolean;
    IsBlob: Boolean;
    IsBoolean: Boolean;       //Für BOOLEAN → SMALLINT Konvertierung
  end;

  { TfrmServerDBFieldSelector }

  TfrmServerDBFieldSelector = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    // ===== SOURCE =====
    grBoxSource: TGroupBox;
    Label7: TLabel;
    comboxSourceServer: TComboBox;
    Label6: TLabel;
    comboxSourceDB: TComboBox;
    lbSourceTable: TLabel;
    comboxSourceTables: TComboBox;

    // ===== DESTINATION =====
    Destination: TGroupBox;
    Label4: TLabel;
    comboxDestServer: TComboBox;
    Label5: TLabel;
    comboxDestDB: TComboBox;
    btnNewDB: TButton;
    Label2: TLabel;
    edtDestTable: TEdit;
    chkCreateTable: TCheckBox;
    chkboxExternalTable: TCheckBox;
    edtExternalFile: TEdit;
    btnExternalFile: TButton;

    // ===== FIELDS =====
    grboxFields: TGroupBox;
    chkLstFields: TCheckListBox;
    pnlFieldsSelectButtons: TPanel;
    btnSelectAll: TButton;
    btnDeselectAll: TButton;

    // ===== CONTAINER =====
    pnlSelector: TPanel;

  private
    FSourceDBIndex: Integer;
    FDestDBIndex: Integer;
    FShowFieldSelection: Boolean;
    FFieldList: TStringList;
    FSelectedFields: TStringList;

    FFieldInfos: array of TSelectorFieldInfo;

    FShowSource: Boolean;

    function GetSelectedFieldsList: TStringList;

    procedure SetShowSource(AValue: Boolean);
    // ---- SOURCE ----
    function FillSourceServerCombo: Boolean;
    function FillSourceDBCombo: Boolean;
    function FillSourceTableCombo: Boolean;
    procedure FillSourceCombos;
    function ConfigureSourceConnection: Boolean;

    // ---- DESTINATION ----
    function FillDestServerCombo: Boolean;
    function FillDestDBCombo: Boolean;
    procedure FillDestCombos;
    function ConfigureDestConnection: Boolean;

    // ---- FIELDS ----
    procedure LoadFields;
    procedure SelectAllFields;
    procedure DeselectAllFields;
    procedure UpdateExternalFileState;

    function IsProblemFieldForExternal(const AFieldName: string): Boolean;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Initialize;
    procedure SetFieldList(AFieldList: TStringList; AFieldInfos: array of TSelectorFieldInfo);
    procedure SetShowFieldSelection(AVisible: Boolean);
    procedure ClearAll;

    // ===== DIALOG FUNCTIONS =====
    function ShowDialog(out ASourceServer, ASourceDB, ASourceTable: string;
                        out ADestServer, ADestDB, ADestTable, AExternalFile: string;
                        out CreateTable, ExternalTable: Boolean;
                        out SelectedFields: TStringList): Boolean; overload;

    function ShowDialog(out ASourceServer, ASourceDB, ASourceTable: string;
                        out ADestServer, ADestDB, ADestTable, AExternalFile: string;
                        out CreateTable, ExternalTable: Boolean): Boolean; overload;

    // ===== PROPERTIES =====
    property ShowFieldSelection: Boolean read FShowFieldSelection write SetShowFieldSelection;
    property FieldList: TStringList read FFieldList;
    property SelectedFields: TStringList read FSelectedFields;

    property ShowSource: Boolean read FShowSource write SetShowSource;

  published
    // ===== EVENT HANDLER =====
    procedure comboxSourceServerChange(Sender: TObject);
    procedure comboxSourceDBChange(Sender: TObject);
    procedure comboxSourceTablesChange(Sender: TObject);
    procedure comboxDestServerChange(Sender: TObject);
    procedure comboxDestDBChange(Sender: TObject);
    procedure chkboxExternalTableChange(Sender: TObject);
    procedure btnSelectAllClick(Sender: TObject);
    procedure btnDeselectAllClick(Sender: TObject);
    procedure btnExternalFileClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  end;

var
  frmServerDBFieldSelector: TfrmServerDBFieldSelector;

implementation

{$R *.lfm}

{ TfrmServerDBFieldSelector }


procedure TfrmServerDBFieldSelector.SetShowSource(AValue: Boolean);
begin
  FShowSource := AValue;
  grBoxSource.Visible := AValue;
  if not AValue then
    Self.Width := Self.Width - 400;

end;

constructor TfrmServerDBFieldSelector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFieldList := TStringList.Create;
  FSelectedFields := TStringList.Create;
  FShowFieldSelection := True;
  FSourceDBIndex := -1;
  FDestDBIndex := -1;

  // Standardwerte setzen
  chkCreateTable.Checked := True;
  chkboxExternalTable.Checked := False;
  UpdateExternalFileState;
end;

destructor TfrmServerDBFieldSelector.Destroy;
begin
  FFieldList.Free;
  FSelectedFields.Free;
  inherited;
end;

// ============================================================================
// INITIALISIERUNG
// ============================================================================

procedure TfrmServerDBFieldSelector.Initialize;
begin
  FillSourceCombos;
  FillDestCombos;
  UpdateExternalFileState;
end;

procedure TfrmServerDBFieldSelector.ClearAll;
begin
  comboxSourceServer.ItemIndex := -1;
  comboxSourceDB.ItemIndex := -1;
  comboxSourceTables.ItemIndex := -1;
  comboxDestServer.ItemIndex := -1;
  comboxDestDB.ItemIndex := -1;
  edtDestTable.Text := '';
  edtExternalFile.Text := '';
  chkCreateTable.Checked := True;
  chkboxExternalTable.Checked := False;
  chkLstFields.Clear;
  FFieldList.Clear;
  FSelectedFields.Clear;
end;

procedure TfrmServerDBFieldSelector.SetFieldList(AFieldList: TStringList; AFieldInfos: array of TSelectorFieldInfo);
var
  i: Integer;
begin
  FFieldList.Assign(AFieldList);
  chkLstFields.Clear;
  FSelectedFields.Clear;

  // Feld-Infos speichern (prüfen ob Länge > 0)
  if Length(AFieldInfos) > 0 then
  begin
    SetLength(FFieldInfos, Length(AFieldInfos));
    for i := 0 to High(AFieldInfos) do
      FFieldInfos[i] := AFieldInfos[i];
  end
  else
    SetLength(FFieldInfos, 0);

  for i := 0 to FFieldList.Count - 1 do
  begin
    chkLstFields.Items.Add(FFieldList[i]);
    chkLstFields.Checked[i] := True;
    FSelectedFields.Add(FFieldList[i]);
  end;
end;

procedure TfrmServerDBFieldSelector.SetShowFieldSelection(AVisible: Boolean);
begin
  FShowFieldSelection := AVisible;
  grboxFields.Visible := AVisible;
  if not AVisible then
  begin
    chkLstFields.Clear;
    FSelectedFields.Clear;
  end;
end;

// ============================================================================
// SOURCE (wie in CloneTable)
// ============================================================================

procedure TfrmServerDBFieldSelector.FillSourceCombos;
begin
  FillSourceServerCombo;
  FillSourceDBCombo;
  FillSourceTableCombo;
end;

function TfrmServerDBFieldSelector.FillSourceServerCombo: Boolean;
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

function TfrmServerDBFieldSelector.FillSourceDBCombo: Boolean;
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
      FillSourceTableCombo;
  end;
end;

function TfrmServerDBFieldSelector.FillSourceTableCombo: Boolean;
begin
  Result := False;
  comboxSourceTables.Items.Clear;
  comboxSourceTables.Items.Add(''); // Leerer Eintrag

  try
    // Verbindung muss bereits konfiguriert sein
    if FSourceDBIndex >= 0 then
    begin
      // Tabellen aus der Datenbank holen
      RegisteredDatabases[FSourceDBIndex].IBDatabase.GetTableNames(comboxSourceTables.Items);
      if comboxSourceTables.Items.Count > 0 then
      begin
        comboxSourceTables.ItemIndex := 0;
        Result := True;
      end;
    end;
  except
  end;
end;

function TfrmServerDBFieldSelector.ConfigureSourceConnection: Boolean;
var
  i: Integer;
  DBRec: TDatabaseRec;
begin
  Result := False;
  FSourceDBIndex := -1;

  for i := 0 to High(RegisteredDatabases) do
    if SameText(RegisteredDatabases[i].RegRec.ServerName, comboxSourceServer.Text) and
       SameText(RegisteredDatabases[i].RegRec.Title, comboxSourceDB.Text) then
    begin
      FSourceDBIndex := i;
      Break;
    end;

  if FSourceDBIndex < 0 then Exit;

  try
    // Verbindung sicherstellen (ohne Komponenten zu ersetzen)
    DBRec := RegisteredDatabases[FSourceDBIndex];
    if not DBRec.IBDatabase.Connected then
      DBRec.IBDatabase.Connected := True;
    Result := True;
  except
    Result := False;
  end;
end;

// ============================================================================
// DESTINATION (wie in CloneTable)
// ============================================================================

procedure TfrmServerDBFieldSelector.FillDestCombos;
begin
  FillDestServerCombo;
  FillDestDBCombo;
end;

function TfrmServerDBFieldSelector.FillDestServerCombo: Boolean;
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
      Result := True;
    end;
  finally
    ServerList.Free;
  end;
end;

function TfrmServerDBFieldSelector.FillDestDBCombo: Boolean;
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
    ConfigureDestConnection;
  end;
end;

function TfrmServerDBFieldSelector.ConfigureDestConnection: Boolean;
var
  i: Integer;
  DBRec: TDatabaseRec;
begin
  Result := False;
  FDestDBIndex := -1;

  for i := 0 to High(RegisteredDatabases) do
    if SameText(RegisteredDatabases[i].RegRec.ServerName, comboxDestServer.Text) and
       SameText(RegisteredDatabases[i].RegRec.Title, comboxDestDB.Text) then
    begin
      FDestDBIndex := i;
      Break;
    end;

  if FDestDBIndex < 0 then Exit;

  try
    DBRec := RegisteredDatabases[FDestDBIndex];
    if not DBRec.IBDatabase.Connected then
      DBRec.IBDatabase.Connected := True;
    Result := True;
  except
    Result := False;
  end;
end;

// ============================================================================
// FIELDS (wie in CloneTable)
// ============================================================================

procedure TfrmServerDBFieldSelector.LoadFields;
var
  Iso: TIsolatedQuery;
  i: Integer;
  FieldName: string;
begin
  if FSourceDBIndex < 0 then Exit;

  chkLstFields.Clear;
  FFieldList.Clear;
  FSelectedFields.Clear;

  Iso := GetFieldsIsolated(RegisteredDatabases[FSourceDBIndex].IBDatabase,
                           Trim(comboxSourceTables.Text));
  try
    while not Iso.Query.EOF do
    begin
      FieldName := Trim(Iso.Query.FieldByName('field_name').AsString);
      FFieldList.Add(FieldName);
      Iso.Query.Next;
    end;
  finally
    Iso.Free;
  end;

  // Felder in die CheckListBox laden
  for i := 0 to FFieldList.Count - 1 do
  begin
    chkLstFields.Items.Add(FFieldList[i]);
    chkLstFields.Checked[i] := True;
    FSelectedFields.Add(FFieldList[i]);
  end;
end;

procedure TfrmServerDBFieldSelector.SelectAllFields;
var
  i: Integer;
begin
  FSelectedFields.Clear;
  for i := 0 to chkLstFields.Items.Count - 1 do
  begin
    chkLstFields.Checked[i] := True;
    FSelectedFields.Add(chkLstFields.Items[i]);
  end;
end;

procedure TfrmServerDBFieldSelector.DeselectAllFields;
var
  i: Integer;
begin
  FSelectedFields.Clear;
  for i := 0 to chkLstFields.Items.Count - 1 do
    chkLstFields.Checked[i] := False;
end;

procedure TfrmServerDBFieldSelector.UpdateExternalFileState;
begin
  edtExternalFile.Enabled := chkboxExternalTable.Checked;
  btnExternalFile.Enabled := chkboxExternalTable.Checked;
end;

// ============================================================================
// EVENT HANDLER
// ============================================================================

procedure TfrmServerDBFieldSelector.comboxSourceServerChange(Sender: TObject);
begin
  FillSourceDBCombo;
end;

procedure TfrmServerDBFieldSelector.comboxSourceDBChange(Sender: TObject);
begin
  comboxSourceTables.Items.Clear;
  if ConfigureSourceConnection then
  begin
    FillSourceTableCombo;
    comboxSourceTablesChange(nil);
  end;
end;

procedure TfrmServerDBFieldSelector.comboxSourceTablesChange(Sender: TObject);
begin
  if FShowFieldSelection then
    LoadFields;
end;

procedure TfrmServerDBFieldSelector.comboxDestServerChange(Sender: TObject);
begin
  FillDestDBCombo;
end;

procedure TfrmServerDBFieldSelector.comboxDestDBChange(Sender: TObject);
begin
  ConfigureDestConnection;
end;

procedure TfrmServerDBFieldSelector.chkboxExternalTableChange(Sender: TObject);
var
  i: Integer;
  Index: Integer;
begin
  UpdateExternalFileState;

  if chkboxExternalTable.Checked then
  begin
    // External Table aktiviert → Problemfelder abwählen
    for i := 0 to chkLstFields.Items.Count - 1 do
    begin
      if IsProblemFieldForExternal(chkLstFields.Items[i]) then
      begin
        chkLstFields.Checked[i] := False;
        Index := FSelectedFields.IndexOf(chkLstFields.Items[i]);
        if Index >= 0 then
          FSelectedFields.Delete(Index);
      end;
    end;
    // ============================================================
    // Hinweis: Computed-Felder manuell abwählen
    // ============================================================
    ShowMessageDialog(
      'External Table Limitations' + sLineBreak + sLineBreak +
      'External Tables do NOT support the following field types:' + sLineBreak +
      '- BLOB fields' + sLineBreak +
      '- ARRAY fields' + sLineBreak +
      '- COMPUTED fields' + sLineBreak + sLineBreak +
      'DBReader automatically detects and deselects BLOB and ARRAY fields.' + sLineBreak + sLineBreak +
      'However, COMPUTED fields cannot be automatically detected by DBReader.' + sLineBreak +
      'Please manually deselect any COMPUTED fields from the field list before proceeding.' + sLineBreak + sLineBreak +
      'Fields that may be COMPUTED (example):' + sLineBreak +
      '- FULL_NAME' + sLineBreak +
      '- GENERATED_COL' + sLineBreak +
      '- Any field with "COMPUTED BY" in its definition',
      mtInformation, [mbOK]
    );
  end
  else
  begin
    // External Table deaktiviert → Alle Felder wieder aktivieren
    for i := 0 to chkLstFields.Items.Count - 1 do
    begin
      chkLstFields.Checked[i] := True;
      if FSelectedFields.IndexOf(chkLstFields.Items[i]) < 0 then
        FSelectedFields.Add(chkLstFields.Items[i]);
    end;
  end;
end;

// Hilfsfunktion
procedure TfrmServerDBFieldSelector.btnSelectAllClick(Sender: TObject);
begin
  SelectAllFields;
end;

procedure TfrmServerDBFieldSelector.btnDeselectAllClick(Sender: TObject);
begin
  DeselectAllFields;
end;

procedure TfrmServerDBFieldSelector.btnExternalFileClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(nil);
  try
    OpenDialog.Filter := 'External Table files (*.ext)|*.ext|All files (*.*)|*.*';
    OpenDialog.DefaultExt := 'ext';
    if OpenDialog.Execute then
      edtExternalFile.Text := OpenDialog.FileName;
  finally
    OpenDialog.Free;
  end;
end;

procedure TfrmServerDBFieldSelector.btnOKClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmServerDBFieldSelector.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

// ============================================================================
// DIALOG FUNCTIONS
// ============================================================================
function TfrmServerDBFieldSelector.ShowDialog(
  out ASourceServer, ASourceDB, ASourceTable: string;
  out ADestServer, ADestDB, ADestTable, AExternalFile: string;
  out CreateTable, ExternalTable: Boolean;
  out SelectedFields: TStringList): Boolean;
begin
  Initialize;

  // Sichtbarkeit der Panels steuern
  grBoxSource.Visible := FShowSource;
  //Destination.Visible := FShowDestination;
  grboxFields.Visible := FShowFieldSelection;

  Result := ShowModal = mrOk;

  if Result then
  begin
    ASourceServer := comboxSourceServer.Text;
    ASourceDB := comboxSourceDB.Text;
    ASourceTable := comboxSourceTables.Text;
    ADestServer := comboxDestServer.Text;
    ADestDB := comboxDestDB.Text;
    ADestTable := edtDestTable.Text;
    AExternalFile := edtExternalFile.Text;
    CreateTable := chkCreateTable.Checked;
    ExternalTable := chkboxExternalTable.Checked;

    // Ausgewählte Felder aus der CheckListBox holen
    SelectedFields := GetSelectedFieldsList;

    // Falls keine Felder ausgewählt sind, leere Liste zurückgeben
    if SelectedFields.Count = 0 then
    begin
      SelectedFields.Free;
      SelectedFields := TStringList.Create;
    end;
  end
  else
  begin
    // Bei Abbruch leere Liste
    SelectedFields := TStringList.Create;
  end;
end;

function TfrmServerDBFieldSelector.ShowDialog(
  out ASourceServer, ASourceDB, ASourceTable: string;
  out ADestServer, ADestDB, ADestTable, AExternalFile: string;
  out CreateTable, ExternalTable: Boolean): Boolean;
var
  DummyFields: TStringList;
begin
  // Felder-Auswahl ausblenden
  grboxFields.Visible := False;
  FShowFieldSelection := False;

  // Erste ShowDialog-Variante aufrufen
  Result := ShowDialog(ASourceServer, ASourceDB, ASourceTable,
                       ADestServer, ADestDB, ADestTable, AExternalFile,
                       CreateTable, ExternalTable, DummyFields);

  DummyFields.Free;
end;

function TfrmServerDBFieldSelector.GetSelectedFieldsList: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  for i := 0 to chkLstFields.Items.Count - 1 do
    if chkLstFields.Checked[i] then
      Result.Add(chkLstFields.Items[i]);
end;

function TfrmServerDBFieldSelector.IsProblemFieldForExternal(const AFieldName: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to High(FFieldInfos) do
  begin
    if SameText(FFieldInfos[i].Name, AFieldName) then
    begin
      Result := FFieldInfos[i].IsComputed or
                FFieldInfos[i].IsArray or
                FFieldInfos[i].IsBlob;
      Break;
    end;
  end;

  //Fallback: Wenn keine Infos vorhanden, am Feldnamen erkennen
  if not Result then
  begin
    if (Pos('COMPUTED', UpperCase(AFieldName)) > 0) or
       (Pos('[', AFieldName) > 0) or
       (Pos('ARRAY', UpperCase(AFieldName)) > 0) then
      Result := True;
  end;
end;

end.
