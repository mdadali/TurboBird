unit fCheckDBIntegrity;

{$mode ObjFPC}{$H+}

interface

uses
  Forms, Classes, SysUtils, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, CheckLst, ComCtrls, TypInfo, DB, IBDatabase, IBQuery, DateUtils,
  turbocommon, CheckDBIntegrity,
  uthemeselector;

type

  { TfmCheckDBIntegrity }

  TfmCheckDBIntegrity = class(TForm)
    bbStart: TBitBtn;
    bbExport: TBitBtn;
    chklboxChecks: TCheckListBox;
    cmBoxDatabase: TComboBox;
    ImageList1: TImageList;
    Label1: TLabel;
    lDBInfo: TLabel;
    MemoResults: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    tsStart: TTabSheet;
    tsCharSet: TTabSheet;
    tsIndicies: TTabSheet;
    tsNotNull: TTabSheet;
    tsFielType: TTabSheet;
    tsPrimKeys: TTabSheet;
    tsForeignKeys: TTabSheet;
    tsViews: TTabSheet;
    tsTrigers: TTabSheet;
    SaveDialog1: TSaveDialog;

    procedure bbStartClick(Sender: TObject);
    procedure bbExportClick(Sender: TObject);
    procedure chklboxChecksClick(Sender: TObject);
    procedure chklboxChecksClickCheck(Sender: TObject);
    procedure cmBoxDatabaseChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FIBDatabase: TIBDatabase;
    FIBTransaction: TIBTransaction;
    FIBQuery: TIBQuery;

    FCheckResult: TDBCheckResult;
    FResultInitialized: Boolean;

    procedure FillDBTitlesComboBox;
    procedure SetCurrentDB(ADBTitle: string);
    function GetDBRecord(SearchField: TDBField; const SearchValue: string): TDBSearchResult;
    procedure RunChecks;
    procedure UpdateDBInfo;
    procedure ExportResultsToFile(const FileName: string);
  public
    procedure Init(ADBTitle: string);
 end;

implementation

{$R *.lfm}

uses
  FileUtil, LCLIntf, LCLType;

procedure TfmCheckDBIntegrity.FormCreate(Sender: TObject);
begin
  FIBDatabase := TIBDatabase.Create(nil);
  FIBTransaction := TIBTransaction.Create(nil);
  FIBQuery := TIBQuery.Create(nil);
  FIBDatabase.DefaultTransaction := FIBTransaction;
  FIBQuery.Transaction := FIBTransaction;
  FIBQuery.Database := FIBDatabase;

  FResultInitialized := False;

  // Initialisiere Check-Liste
  chklboxChecks.Items.Clear;
  chklboxChecks.Items.Add('Alle auswählen');
  chklboxChecks.Items.Add('Zeichensatz prüfen');
  chklboxChecks.Items.Add('Indizes/Unique prüfen');
  chklboxChecks.Items.Add('Feldlängen prüfen');
  chklboxChecks.Items.Add('NOT NULL prüfen');
  chklboxChecks.Items.Add('Datentypen prüfen');
  chklboxChecks.Items.Add('Primary Keys prüfen');
  chklboxChecks.Items.Add('Foreign Keys prüfen');
  chklboxChecks.Items.Add('Views prüfen');
  chklboxChecks.Items.Add('Trigger/Defaults prüfen');

  // Standardmäßig alle aktivieren
  chklboxChecks.CheckAll(cbChecked, True, False);
end;

procedure TfmCheckDBIntegrity.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(Self);
end;

procedure TfmCheckDBIntegrity.Init(ADBTitle: string);
begin
  FillDBTitlesComboBox;
  if ADBTitle <> '' then
  begin
    cmBoxDatabase.ItemIndex := cmBoxDatabase.Items.IndexOf(ADBTitle);
    if cmBoxDatabase.ItemIndex >= 0 then
      SetCurrentDB(ADBTitle);
  end;
  UpdateDBInfo;
end;

procedure TfmCheckDBIntegrity.FillDBTitlesComboBox;
var
  i: Integer;
begin
  cmBoxDatabase.Items.Clear;
  for i := 0 to Length(RegisteredDatabases) - 1 do
    cmBoxDatabase.Items.Add(RegisteredDatabases[i].RegRec.Title);

  if cmBoxDatabase.Items.Count > 0 then
    cmBoxDatabase.ItemIndex := 0;
end;

procedure TfmCheckDBIntegrity.SetCurrentDB(ADBTitle: string);
var
  DBRes: TDBSearchResult;
begin
  try
    DBRes := GetDBRecord(dbfTitle, ADBTitle);
    // Aktuell ausgewählte DB merken wir uns für spätere Verwendung
    cmBoxDatabase.Text := DBRes.Rec.Title;
    UpdateDBInfo;
  except
    on E: Exception do
      ShowMessage('Fehler beim Setzen der Datenbank: ' + E.Message);
  end;
end;

function TfmCheckDBIntegrity.GetDBRecord(SearchField: TDBField; const SearchValue: string): TDBSearchResult;
var
  i: Integer;
  function GetFieldAsString(const Rec: TRegisteredDatabase; AField: TDBField): string;
  begin
    case AField of
      dbfTitle:        Result := Rec.Title;
      dbfDatabaseName: Result := Rec.DatabaseName;
      dbfUserName:     Result := Rec.UserName;
      dbfPassword:     Result := Rec.Password;
      dbfCharset:      Result := Rec.Charset;
      dbfRole:         Result := Rec.Role;
      dbfLastOpened:   Result := DateTimeToStr(Rec.LastOpened);
      dbfDeleted:      Result := BoolToStr(Rec.Deleted, True);
      dbfSavePassword: Result := BoolToStr(Rec.SavePassword, True);
    else
      Result := '';
    end;
  end;
begin
  for i := Low(RegisteredDatabases) to High(RegisteredDatabases) do
  begin
    if SameText(GetFieldAsString(RegisteredDatabases[i].RegRec, SearchField), SearchValue) then
    begin
      Result.Index := i;
      Result.Rec   := RegisteredDatabases[i].RegRec;
      Exit;
    end;
  end;

  raise EDatabaseError.CreateFmt('No record found where %s = "%s".',
    [GetEnumName(TypeInfo(TDBField), Ord(SearchField)), SearchValue]);
end;

procedure TfmCheckDBIntegrity.UpdateDBInfo;
var
  DBRes: TDBSearchResult;
  DBVersion: string;
  TableCount, FieldCount: Integer;
begin
  // Standard-Werte setzen
  DBVersion := 'Unknown';
  TableCount := 0;
  FieldCount := 0;

  try
    if cmBoxDatabase.ItemIndex < 0 then
    begin
      lDBInfo.Caption := 'Keine Datenbank ausgewählt';
      Exit;
    end;

    DBRes := GetDBRecord(dbfTitle, cmBoxDatabase.Items[cmBoxDatabase.ItemIndex]);

    // Verbinde zur DB für Info
    if FIBDatabase.Connected then
      FIBDatabase.Connected := False;

    FIBDatabase.DatabaseName := DBRes.Rec.DatabaseName;
    FIBDatabase.Params.Clear;
    FIBDatabase.Params.Add('user_name=' + DBRes.Rec.UserName);
    FIBDatabase.Params.Add('password=' + DBRes.Rec.Password);
    if DBRes.Rec.Charset <> '' then
      FIBDatabase.Params.Add('lc_ctype=' + DBRes.Rec.Charset);
    if DBRes.Rec.Role <> '' then
      FIBDatabase.Params.Add('sql_role_name=' + DBRes.Rec.Role);
    FIBDatabase.LoginPrompt := False;

    FIBDatabase.Connected := True;
    if not FIBDatabase.DefaultTransaction.InTransaction then
      FIBDatabase.DefaultTransaction.StartTransaction;

    DBVersion := GetDBServerVersion(FIBDatabase);
    TableCount := GetTableCount(FIBDatabase);
    FieldCount := GetFieldCount(FIBDatabase);

    // SICHER: Nur zuweisen wenn alles geklappt hat
    lDBInfo.Caption := Format('Server: %s | Tables: %d | Fields: %d',
      [DBVersion, TableCount, FieldCount]);

    FIBDatabase.Connected := False;

  except
    on E: Exception do
    begin
      // SICHER: Fehlerbehandlung ohne Zugriff auf ungültige Variablen
      lDBInfo.Caption := 'Fehler beim Verbinden: ' + E.Message;

      // Versuche Datenbank zu schließen
      try
        if FIBDatabase.Connected then
          FIBDatabase.Connected := False;
      except
        // Ignorieren
      end;
    end;
  end;
end;

procedure TfmCheckDBIntegrity.cmBoxDatabaseChange(Sender: TObject);
begin
  if cmBoxDatabase.ItemIndex >= 0 then
    SetCurrentDB(cmBoxDatabase.Items[cmBoxDatabase.ItemIndex]);
end;

procedure TfmCheckDBIntegrity.chklboxChecksClick(Sender: TObject);
begin
  // "Alle auswählen" Checkbox-Logik
  if (chklboxChecks.ItemIndex = 0) and (chklboxChecks.State[0] = cbChecked) then
    chklboxChecks.CheckAll(cbChecked, True, False)
  else if (chklboxChecks.ItemIndex = 0) and (chklboxChecks.State[0] = cbUnchecked) then
    chklboxChecks.CheckAll(cbUnchecked, True, False);
end;

procedure TfmCheckDBIntegrity.chklboxChecksClickCheck(Sender: TObject);
var
  AllChecked: Boolean;
  i: Integer;
begin
  // Wenn ein einzelner Check deaktiviert wird, "Alle" deaktivieren
  if (chklboxChecks.ItemIndex > 0) and (chklboxChecks.State[chklboxChecks.ItemIndex] = cbUnchecked) then
    chklboxChecks.State[0] := cbUnchecked;

  // Wenn alle Checkboxen aktiviert sind, "Alle" aktivieren
  if (chklboxChecks.ItemIndex > 0) and (chklboxChecks.State[chklboxChecks.ItemIndex] = cbChecked) then
  begin
    AllChecked := True;
    for i := 1 to chklboxChecks.Count - 1 do
    begin
      if chklboxChecks.State[i] <> cbChecked then
      begin
        AllChecked := False;
        Break;
      end;
    end;
    if AllChecked then
      chklboxChecks.State[0] := cbChecked;
  end;
end;

procedure TfmCheckDBIntegrity.bbStartClick(Sender: TObject);
var
  AnySelected: Boolean;
  i: Integer;
begin
  // Prüfe ob mindestens ein Check ausgewählt ist
  AnySelected := False;
  for i := 1 to chklboxChecks.Count - 1 do
    if chklboxChecks.State[i] = cbChecked then
    begin
      AnySelected := True;
      Break;
    end;

  if not AnySelected then
  begin
    MessageDlg('Keine Checks ausgewählt', 'Bitte wählen Sie mindestens einen Check aus.', mtWarning, [mbOK], 0);
    Exit;
  end;

  bbStart.Enabled := False;
  bbStart.Caption := 'Prüfe...';
  Screen.Cursor := crHourGlass;
  Application.ProcessMessages;

  try
    RunChecks;
  finally
    Screen.Cursor := crDefault;
    bbStart.Caption := 'Start';
    bbStart.Enabled := True;
  end;
end;

procedure TfmCheckDBIntegrity.bbExportClick(Sender: TObject);
begin
  if MemoResults.Lines.Count = 0 then
  begin
    MessageDlg('Keine Ergebnisse', 'Führen Sie zuerst einen Check durch.', mtInformation, [mbOK], 0);
    Exit;
  end;

  SaveDialog1.Filter := 'Text-Dateien (*.txt)|*.txt|CSV-Dateien (*.csv)|*.csv|Alle Dateien (*.*)|*.*';
  SaveDialog1.DefaultExt := 'txt';
  SaveDialog1.FileName := Format('DBIntegrity_%s_%s',
    [FormatDateTime('YYYYMMDD', Now), FormatDateTime('HHMMSS', Now)]);

  if SaveDialog1.Execute then
    ExportResultsToFile(SaveDialog1.FileName);
end;

procedure TfmCheckDBIntegrity.ExportResultsToFile(const FileName: string);
begin
  try
    MemoResults.Lines.SaveToFile(FileName);
    MessageDlg('Export erfolgreich', Format('Ergebnisse wurden in "%s" gespeichert.', [FileName]),
      mtInformation, [mbOK], 0);
  except
    on E: Exception do
      MessageDlg('Export fehlgeschlagen', 'Fehler: ' + E.Message, mtError, [mbOK], 0);
  end;
end;

procedure TfmCheckDBIntegrity.RunChecks;
var
  DBRes: TDBSearchResult;
  StartTime: TDateTime;
  TotalIssues: Integer;
begin
  if cmBoxDatabase.ItemIndex < 0 then
  begin
    MessageDlg('Keine Datenbank', 'Bitte wählen Sie eine Datenbank aus.', mtWarning, [mbOK], 0);
    Exit;
  end;

  MemoResults.Clear;
  StartTime := Now;

  // Altes Result freigeben
  if FResultInitialized then
    FreeCheckResult(FCheckResult);
  FResultInitialized := False;

  try
    DBRes := GetDBRecord(dbfTitle, cmBoxDatabase.Items[cmBoxDatabase.ItemIndex]);

    if FIBDatabase.Connected then
      FIBDatabase.Connected := False;

    FIBDatabase.DatabaseName := DBRes.Rec.DatabaseName;
    FIBDatabase.Params.Clear;
    FIBDatabase.Params.Add('user_name=' + DBRes.Rec.UserName);
    FIBDatabase.Params.Add('password=' + DBRes.Rec.Password);
    if DBRes.Rec.Charset <> '' then
      FIBDatabase.Params.Add('lc_ctype=' + DBRes.Rec.Charset);
    if DBRes.Rec.Role <> '' then
      FIBDatabase.Params.Add('sql_role_name=' + DBRes.Rec.Role);
    FIBDatabase.LoginPrompt := False;

    FIBDatabase.Connected := True;
    if not FIBDatabase.DefaultTransaction.InTransaction then
      FIBDatabase.DefaultTransaction.StartTransaction;

    // Result initialisieren
    InitCheckResult(FCheckResult, DBRes.Rec.Title, FIBDatabase.DatabaseName);
    FResultInitialized := True;

    // Server-Version und Metadaten abrufen
    FCheckResult.DBServerVersion := GetDBServerVersion(FIBDatabase);
    FCheckResult.TotalTables := GetTableCount(FIBDatabase);
    FCheckResult.TotalFields := GetFieldCount(FIBDatabase);

    // Checks durchführen
    if chklboxChecks.State[1] = cbChecked then
      CheckFieldsCharset(FIBDatabase, FCheckResult.CharsetIssues);

    if chklboxChecks.State[2] = cbChecked then
      CheckIndicesUnique(FIBDatabase, FCheckResult.IndexUniqueIssues);

    if chklboxChecks.State[3] = cbChecked then
      CheckFieldsLength(FIBDatabase, FCheckResult.LengthIssues);

    if chklboxChecks.State[4] = cbChecked then
      CheckFieldsNotNull(FIBDatabase, FCheckResult.NotNullIssues);

    if chklboxChecks.State[5] = cbChecked then
      CheckFieldsDataType(FIBDatabase, FCheckResult.DataTypeIssues);

    if chklboxChecks.State[6] = cbChecked then
      CheckPrimaryKeys(FIBDatabase, FCheckResult.PKIssues);

    if chklboxChecks.State[7] = cbChecked then
      CheckForeignKeys(FIBDatabase, FCheckResult.FKIssues);

    if chklboxChecks.State[8] = cbChecked then
      CheckViews(FIBDatabase, FCheckResult.ViewsIssues);

    if chklboxChecks.State[9] = cbChecked then
      CheckTriggerDefaults(FIBDatabase, FCheckResult.TriggerDefaultIssues);

    // Zusammenfassung erstellen
    FCheckResult.Summary.Clear;
    FCheckResult.Summary.Add(Format('Check durchgeführt am: %s', [DateTimeToStr(FCheckResult.DateTimeChecked)]));
    FCheckResult.Summary.Add(Format('Dauer: %d Sekunden', [SecondsBetween(StartTime, Now)]));
    FCheckResult.Summary.Add(Format('Datenbank: %s', [FCheckResult.DBName]));
    FCheckResult.Summary.Add(Format('Server Version: %s', [FCheckResult.DBServerVersion]));
    FCheckResult.Summary.Add(Format('Tabellen: %d | Felder: %d', [FCheckResult.TotalTables, FCheckResult.TotalFields]));
    FCheckResult.Summary.Add('---');
    FCheckResult.Summary.Add(Format('Charset Issues: %d', [FCheckResult.CharsetIssues.Count]));
    FCheckResult.Summary.Add(Format('Length Issues: %d', [FCheckResult.LengthIssues.Count]));
    FCheckResult.Summary.Add(Format('NOT NULL Issues: %d', [FCheckResult.NotNullIssues.Count]));
    FCheckResult.Summary.Add(Format('Data Type Issues: %d', [FCheckResult.DataTypeIssues.Count]));
    FCheckResult.Summary.Add(Format('PK Issues: %d', [FCheckResult.PKIssues.Count]));
    FCheckResult.Summary.Add(Format('FK Issues: %d', [FCheckResult.FKIssues.Count]));
    FCheckResult.Summary.Add(Format('View Issues: %d', [FCheckResult.ViewsIssues.Count]));
    FCheckResult.Summary.Add(Format('Index Unique Issues: %d', [FCheckResult.IndexUniqueIssues.Count]));
    FCheckResult.Summary.Add(Format('Trigger Issues: %d', [FCheckResult.TriggerDefaultIssues.Count]));

    // Ergebnisse anzeigen
    MemoResults.Lines.Add(StringOfChar('=', 80));
    MemoResults.Lines.Add('  DB INTEGRITY CHECK REPORT');
    MemoResults.Lines.Add(StringOfChar('=', 80));
    MemoResults.Lines.Add('');
    MemoResults.Lines.AddStrings(FCheckResult.Summary);
    MemoResults.Lines.Add('');
    MemoResults.Lines.Add(StringOfChar('=', 80));
    MemoResults.Lines.Add('  DETAILS');
    MemoResults.Lines.Add(StringOfChar('=', 80));
    MemoResults.Lines.Add('');

    if FCheckResult.CharsetIssues.Count > 0 then
    begin
      MemoResults.Lines.Add('--- CHARSET ISSUES (' + IntToStr(FCheckResult.CharsetIssues.Count) + ') ---');
      MemoResults.Lines.AddStrings(FCheckResult.CharsetIssues);
      MemoResults.Lines.Add('');
    end;

    if FCheckResult.IndexUniqueIssues.Count > 0 then
    begin
      MemoResults.Lines.Add('--- INDEX/UNIQUE ISSUES (' + IntToStr(FCheckResult.IndexUniqueIssues.Count) + ') ---');
      MemoResults.Lines.AddStrings(FCheckResult.IndexUniqueIssues);
      MemoResults.Lines.Add('');
    end;

    if FCheckResult.LengthIssues.Count > 0 then
    begin
      MemoResults.Lines.Add('--- LENGTH ISSUES (' + IntToStr(FCheckResult.LengthIssues.Count) + ') ---');
      MemoResults.Lines.AddStrings(FCheckResult.LengthIssues);
      MemoResults.Lines.Add('');
    end;

    if FCheckResult.NotNullIssues.Count > 0 then
    begin
      MemoResults.Lines.Add('--- NOT NULL ISSUES (' + IntToStr(FCheckResult.NotNullIssues.Count) + ') ---');
      MemoResults.Lines.AddStrings(FCheckResult.NotNullIssues);
      MemoResults.Lines.Add('');
    end;

    if FCheckResult.DataTypeIssues.Count > 0 then
    begin
      MemoResults.Lines.Add('--- DATA TYPE ISSUES (' + IntToStr(FCheckResult.DataTypeIssues.Count) + ') ---');
      MemoResults.Lines.AddStrings(FCheckResult.DataTypeIssues);
      MemoResults.Lines.Add('');
    end;

    if FCheckResult.PKIssues.Count > 0 then
    begin
      MemoResults.Lines.Add('--- PRIMARY KEY ISSUES (' + IntToStr(FCheckResult.PKIssues.Count) + ') ---');
      MemoResults.Lines.AddStrings(FCheckResult.PKIssues);
      MemoResults.Lines.Add('');
    end;

    if FCheckResult.FKIssues.Count > 0 then
    begin
      MemoResults.Lines.Add('--- FOREIGN KEY ISSUES (' + IntToStr(FCheckResult.FKIssues.Count) + ') ---');
      MemoResults.Lines.AddStrings(FCheckResult.FKIssues);
      MemoResults.Lines.Add('');
    end;

    if FCheckResult.ViewsIssues.Count > 0 then
    begin
      MemoResults.Lines.Add('--- VIEW ISSUES (' + IntToStr(FCheckResult.ViewsIssues.Count) + ') ---');
      MemoResults.Lines.AddStrings(FCheckResult.ViewsIssues);
      MemoResults.Lines.Add('');
    end;

    if FCheckResult.TriggerDefaultIssues.Count > 0 then
    begin
      MemoResults.Lines.Add('--- TRIGGER ISSUES (' + IntToStr(FCheckResult.TriggerDefaultIssues.Count) + ') ---');
      MemoResults.Lines.AddStrings(FCheckResult.TriggerDefaultIssues);
      MemoResults.Lines.Add('');
    end;

    // Abschluss
    TotalIssues := FCheckResult.CharsetIssues.Count +
                   FCheckResult.LengthIssues.Count +
                   FCheckResult.NotNullIssues.Count +
                   FCheckResult.DataTypeIssues.Count +
                   FCheckResult.PKIssues.Count +
                   FCheckResult.FKIssues.Count +
                   FCheckResult.ViewsIssues.Count +
                   FCheckResult.IndexUniqueIssues.Count +
                   FCheckResult.TriggerDefaultIssues.Count;

    MemoResults.Lines.Add(StringOfChar('=', 80));
    if TotalIssues = 0 then
      MemoResults.Lines.Add('  ✅ KEINE PROBLEME GEFUNDEN - Datenbank ist in Ordnung!')
    else
      MemoResults.Lines.Add(Format('  ⚠️ %d PROBLEM(E) GEFUNDEN - Bitte überprüfen!', [TotalIssues]));
    MemoResults.Lines.Add(StringOfChar('=', 80));

    // Transaktion committen
    if FIBTransaction.InTransaction then
      FIBTransaction.Commit;

  except
    on E: Exception do
    begin
      if FIBTransaction.InTransaction then
        FIBTransaction.Rollback;

      MessageDlg('Fehler bei der DB-Integritätsprüfung',
        'Fehler: ' + E.Message + #13#10#13#10 +
        'Bitte überprüfen Sie die Datenbankverbindung.',
        mtError, [mbOK], 0);

      MemoResults.Lines.Add('FEHLER: ' + E.Message);
    end;
  end;
end;

procedure TfmCheckDBIntegrity.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if FResultInitialized then
    FreeCheckResult(FCheckResult);
  FResultInitialized := False;

  try
    if Assigned(FIBQuery) then
    begin
      if FIBQuery.Active then
        FIBQuery.Close;
      FreeAndNil(FIBQuery);
    end;

    if Assigned(FIBTransaction) then
    begin
      if FIBTransaction.InTransaction then
        FIBTransaction.Commit;
      FreeAndNil(FIBTransaction);
    end;

    if Assigned(FIBDatabase) then
    begin
      if FIBDatabase.Connected then
        FIBDatabase.Close;
      FreeAndNil(FIBDatabase);
    end;
  except
    on E: Exception do
      ShowMessage('Fehler beim Freigeben der Datenbank-Objekte: ' + E.Message);
  end;
end;

end.
