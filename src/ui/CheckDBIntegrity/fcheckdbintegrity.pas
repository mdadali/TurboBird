unit fCheckDBIntegrity;

{$mode ObjFPC}{$H+}

interface

uses
  Forms, Classes, SysUtils, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, CheckLst, ComCtrls, TypInfo, DB, IBDatabase, IBQuery, turbocommon,
  CheckDBIntegrity;

type

  { TfmCheckDBIntegrity }

  TfmCheckDBIntegrity = class(TForm)
    bbStart: TBitBtn;
    chklboxChecks: TCheckListBox;
    cmBoxDatabase: TComboBox;
    ImageList1: TImageList;
    Label1: TLabel;
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
    procedure bbStartClick(Sender: TObject);
    procedure chklboxChecksClick(Sender: TObject);
    procedure chklboxChecksClickCheck(Sender: TObject);
    procedure cmBoxDatabaseChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FIBDatabase: TIBDatabase;
    FIBTransaction: TIBTransaction;
    FIBQuery: TIBQuery;

    FCharsetIssues, IndexUniqueIssues, FLengthIssues, FNotNullIssues,
    DataTypeIssues, PKIssues, FKIssues, ViewsIssues,
    TriggerDefaultIssues: TStringList;

    DBRes: TDBSearchResult;
    procedure FillDBTitlesComboBox;
    procedure SetCurrentDB(ADBTitle: string);
    function GetDBRecord(SearchField: TDBField; const SearchValue: string): TDBSearchResult;
    procedure RunChecks;
  public
    procedure Init(ADBTitle: string);
 end;

  //var
    //fmCheckDBIntegrity: TfmCheckDBIntegrity;


{ TfmCheckDBIntegrity }

implementation
{$R *.lfm}

procedure TfmCheckDBIntegrity.chklboxChecksClick(Sender: TObject);
begin
  if chklboxChecks.Checked[0] then
   chklboxChecks.CheckAll(cbChecked, false, false);
end;

procedure TfmCheckDBIntegrity.chklboxChecksClickCheck(Sender: TObject);
begin
  if not chklboxChecks.Selected[0] then
    chklboxChecks.Checked[0] := false;
end;

procedure TfmCheckDBIntegrity.bbStartClick(Sender: TObject);
begin
  bbStart.Enabled := false;
  Screen.Cursor := crHourGlass;
  Application.ProcessMessages;
  RunChecks;
  Screen.Cursor := crDefault;
  bbStart.Enabled := true;
end;

procedure TfmCheckDBIntegrity.FormCreate(Sender: TObject);
begin
  FIBDatabase := TIBDatabase.Create(nil);
  FIBTransaction := TIBTransaction.Create(nil);
  FIBQuery := TIBQuery.Create(nil);
  FIBDatabase.DefaultTransaction := FIBTransaction;
  FIBQuery.Transaction := FIBTransaction;
end;

procedure TfmCheckDBIntegrity.Init(ADBTitle: string);
begin
  SetCurrentDB(ADBTitle);
  cmBoxDatabase.Items.Clear;
  FillDBTitlesComboBox;

  FCharsetIssues := TStringList.Create;
  IndexUniqueIssues := TStringList.Create;
  FLengthIssues := TStringList.Create;
  FNotNullIssues := TStringList.Create;
  DataTypeIssues := TStringList.Create;
  PKIssues := TStringList.Create;
  FKIssues := TStringList.Create;
  ViewsIssues := TStringList.Create;
  TriggerDefaultIssues := TStringList.Create;
end;

procedure TfmCheckDBIntegrity.FillDBTitlesComboBox;
var i: integer;
begin
  cmBoxDatabase.Items.Clear;
  for i := 0 to Length(RegisteredDatabases) - 1 do
    cmBoxDatabase.Items.Add(RegisteredDatabases[i].RegRec.Title);
  cmBoxDatabase.ItemIndex := cmBoxDatabase.Items.IndexOf(RegisteredDatabases[DBRes.Index].RegRec.Title);
end;

procedure TfmCheckDBIntegrity.cmBoxDatabaseChange(Sender: TObject);
begin
  SetCurrentDB(cmBoxDatabase.Items[cmBoxDatabase.ItemIndex]);
end;

procedure TfmCheckDBIntegrity.SetCurrentDB(ADBTitle: string);
begin
  DBRes := GetDBRecord(dbfTitle, ADBTitle);
end;

procedure TfmCheckDBIntegrity.FormClose(Sender: TObject;
var CloseAction: TCloseAction);
begin
  FreeAndNil(FCharsetIssues);
  FreeAndNil(IndexUniqueIssues);
  FreeAndNil(FLengthIssues);
  FreeAndNil(FNotNullIssues);
  FreeAndNil(DataTypeIssues);
  FreeAndNil(PKIssues);
  FreeAndNil(FKIssues);
  FreeAndNil(ViewsIssues);
  FreeAndNil(TriggerDefaultIssues);

  try
    // Close query if still active
    if Assigned(FIBQuery) then
    begin
      if FIBQuery.Active then
        FIBQuery.Close;
      FreeAndNil(FIBQuery);
    end;
      // Rollback or commit if transaction still active
    if Assigned(FIBTransaction) then
    begin
      if FIBTransaction.InTransaction then
        FIBTransaction.Commit; // safer than Commit on close
      FreeAndNil(FIBTransaction);
    end;

    // Disconnect database if still connected
    if Assigned(FIBDatabase) then
    begin
      if FIBDatabase.Connected then
        FIBDatabase.Close;
      FreeAndNil(FIBDatabase);
    end;

  except
    on E: Exception do
      ShowMessage('Error while closing database objects: ' + E.Message);
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

procedure TfmCheckDBIntegrity.RunChecks;
var
  DB: TIBDatabase;
  Res: TDBCheckResult;
  Report: TStringList;
begin
  DB := FIBDatabase;

  if DB.Connected then
    DB.Connected := False;

  DB.DatabaseName := DBRes.Rec.DatabaseName;
  DB.Params.Clear;
  DB.Params.Add('user_name=' + DBRes.Rec.UserName);
  DB.Params.Add('password=' + DBRes.Rec.Password);
  if DBRes.Rec.Charset <> '' then
    DB.Params.Add('lc_ctype=' + DBRes.Rec.Charset);
  if DBRes.Rec.Role <> '' then
    DB.Params.Add('sql_role_name=' + DBRes.Rec.Role);

  DB.LoginPrompt := False;

  try
    DB.Connected := True;

    // Init Ergebnis-Container
    InitCheckResult(Res, DBRes.Rec.Title, DB.DatabaseName);

    if not DB.DefaultTransaction.InTransaction then
      DB.DefaultTransaction.StartTransaction;

    // Checks aufrufen
    if chklboxChecks.Checked[1] then
      CheckFieldsCharset(DB, Res.CharsetIssues);
    if chklboxChecks.Checked[2] then
      CheckIndicesUnique(DB, Res.IndexUniqueIssues);
    if chklboxChecks.Checked[3] then
      CheckFieldsLength(DB, Res.LengthIssues);
    if chklboxChecks.Checked[4] then
      CheckFieldsNotNull(DB, Res.NotNullIssues);
    if chklboxChecks.Checked[5] then
      CheckFieldsDataType(DB, Res.DataTypeIssues);
    if chklboxChecks.Checked[6] then
      CheckPrimaryKeys(DB, Res.PKIssues);
    if chklboxChecks.Checked[7] then
      CheckForeignKeys(DB, Res.FKIssues);
    if chklboxChecks.Checked[7] then
      CheckViews(DB, Res.ViewsIssues);
    if chklboxChecks.Checked[8] then
      CheckTriggerDefaults(DB, Res.TriggerDefaultIssues);

    // Ausgabe
    MemoResults.Clear;

    {Report := TStringList.Create;
    try
      TDBCharsetMigrator.CheckCharsetCompatibility(DB, DBRes.Rec.Charset, Report);
      MemoResults.Lines.Assign(Report);
      MemoResults.Lines.Add(' ');
    finally
      Report.Free;
    end;}

    MemoResults.Lines.Add(Format(
      '--- DB Integrity Check ---'#13#10 +
      'Title: %s'#13#10 +
      'Database: %s'#13#10 +
      'Checked: %s',
      [Res.Title, Res.DBName, DateTimeToStr(Res.DateTimeChecked)]
    ));
    MemoResults.Lines.Add('');

    MemoResults.Lines.Add('--- Charset Issues ---');
    MemoResults.Lines.AddStrings(Res.CharsetIssues);
    MemoResults.Lines.Add('');

    MemoResults.Lines.Add('--- Indices/Unique Issues ---');
    MemoResults.Lines.AddStrings(Res.IndexUniqueIssues);
    MemoResults.Lines.Add('');

    MemoResults.Lines.Add('--- Length Issues ---');
    MemoResults.Lines.AddStrings(Res.LengthIssues);
    MemoResults.Lines.Add('');

    MemoResults.Lines.Add('--- NotNull Issues ---');
    MemoResults.Lines.AddStrings(Res.NotNullIssues);
    MemoResults.Lines.Add('');

    MemoResults.Lines.Add('--- DataType Issues ---');
    MemoResults.Lines.AddStrings(Res.DataTypeIssues);
    MemoResults.Lines.Add('');

    MemoResults.Lines.Add('--- PK Issues ---');
    MemoResults.Lines.AddStrings(Res.PKIssues);
    MemoResults.Lines.Add('');

    MemoResults.Lines.Add('--- FK Issues ---');
    MemoResults.Lines.AddStrings(Res.FKIssues);
    MemoResults.Lines.Add('');

    MemoResults.Lines.Add('--- Views Issues ---');
    MemoResults.Lines.AddStrings(Res.ViewsIssues);
    MemoResults.Lines.Add('');

    MemoResults.Lines.Add('--- Trigger/Default Issues ---');
    MemoResults.Lines.AddStrings(Res.TriggerDefaultIssues);

  except
    on E: Exception do
      ShowMessage('Error during DB integrity check: ' + E.Message);
  end;
end;

end.

