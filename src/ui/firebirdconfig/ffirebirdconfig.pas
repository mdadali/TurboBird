unit fFirebirdConfig;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, BufDataset, IBConnection, SQLDB, Forms, Controls,
  IniFiles, Graphics, Dialogs, StdCtrls, DBCtrls, DBGrids, ExtCtrls, Buttons,
  ComCtrls, FileUtil, StrUtils, SynEdit, synhighlighterunixshellscript, SynHighlighterIni,
  SynHighlighterJScript, SysTables, fbcommon, turbocommon;

type

  { TfmFirebirdConfig }

  TfmFirebirdConfig = class(TForm)
    bbClose: TSpeedButton;
    bbExport: TButton;
    bbImport: TBitBtn;
    bbLoadFB3Config: TBitBtn;
    bbRestoreDefaults: TBitBtn;
    bbRestoreFB3Defaults: TBitBtn;
    bbSave: TBitBtn;
    bbSaveFB3Config: TBitBtn;
    bbSearch: TBitBtn;
    BufDataset1: TBufDataset;
    DataSource1: TDataSource;
    dbedtActiveValue: TDBEdit;
    dbedtFileValue: TDBEdit;
    dbedtSource: TDBEdit;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    DBNavigator2: TDBNavigator;
    edtsearch: TEdit;
    GroupBox1: TGroupBox;
    IBConnection1: TIBConnection;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lbServerMode: TLabel;
    lbServer: TLabel;
    lbServerVersion: TLabel;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    Panel13: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    SaveDialog1: TSaveDialog;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    SynEdit1: TSynEdit;
    SynIniSyn1: TSynIniSyn;
    SynUNIXShellScriptSyn1: TSynUNIXShellScriptSyn;
    tsFB3Config: TTabSheet;
    tsFB4Config: TTabSheet;
    procedure bbCloseClick(Sender: TObject);
    procedure bbExportClick(Sender: TObject);
    procedure bbImportClick(Sender: TObject);
    procedure bbLoadFB3ConfigClick(Sender: TObject);
    procedure bbRestoreDefaultsClick(Sender: TObject);
    procedure bbRestoreFB3DefaultsClick(Sender: TObject);
    procedure bbSaveClick(Sender: TObject);
    procedure bbSaveFB3ConfigClick(Sender: TObject);
    procedure bbSearchClick(Sender: TObject);
    procedure DataSource1UpdateData(Sender: TObject);
    procedure DBNavigator1Click(Sender: TObject; Button: TDBNavButtonType);
    procedure edtsearchKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure SynEdit1Change(Sender: TObject);
  private
    FdbIndex: Integer;
    FFirebirdConfPath: string;
    FFirebirdConfLines: TStringList;
  public
    constructor create(AOwner: TComponent); override;
    destructor  destroy; override;
    procedure init(AdbIndex: integer);
    procedure InitForFB3;
    function GetOptionValue(AOption: string): string;
  end;

//var
  //fmFirebirdConfig: TfmFirebirdConfig;

var
  IniF: TIniFile;
  DataSetLoaded: boolean;

implementation

uses main;

procedure TfmFirebirdConfig.bbCloseClick(Sender: TObject);
begin
  close;
  self.Parent.Free;
end;

procedure TfmFirebirdConfig.bbExportClick(Sender: TObject);
begin
  if BufDataset1.RecordCount > 0 then
    ExportDataSet(BufDataset1);
end;

procedure TfmFirebirdConfig.bbImportClick(Sender: TObject);
var
  SL: TStringList;
  Line, FileName, BackupName: string;
  CSVFields: TStringList;
  i, RowID: Integer;
  FieldCount: Integer;
begin
  if not OpenDialog1.Execute then
    Exit;

  FileName := OpenDialog1.FileName;

  if not FileExists(FileName) then
  begin
    ShowMessage('Selected file does not exist.');
    Exit;
  end;

  // Backup der aktuellen firebird.conf anlegen
  if not FileExists(FFirebirdConfPath) then
  begin
    ShowMessage('firebird.conf not found: ' + FFirebirdConfPath);
    Exit;
  end;

  BackupName := FFirebirdConfPath + '.bak';
  try
    CopyFile(PChar(FFirebirdConfPath), PChar(BackupName), False);
  except
    on E: Exception do
    begin
      ShowMessage('Could not create backup of firebird.conf: ' + E.Message);
      Exit;
    end;
  end;

  SL := TStringList.Create;
  CSVFields := TStringList.Create;
  try
    SL.LoadFromFile(FileName);

    // Skip Header (first row)
    for i := 1 to SL.Count - 1 do
    begin
      Line := Trim(SL[i]);
      if Line = '' then Continue;

      CSVFields.Delimiter := ';';
      CSVFields.StrictDelimiter := True;
      CSVFields.DelimitedText := Line;

      FieldCount := CSVFields.Count;
      if FieldCount < 7 then Continue; // mindestens 7 Spalten erwartet

      if not TryStrToInt(CSVFields[0], RowID) then Continue;

      // Datensatz mit passender ID finden
      BufDataset1.First;
      while not BufDataset1.EOF do
      begin
        if BufDataset1.FieldByName('ID').AsInteger = RowID then
        begin
          if BufDataset1.FieldByName('FileValue').AsString <> CSVFields[3] then
          begin
            BufDataset1.Edit;
            BufDataset1.FieldByName('FileValue').AsString := CSVFields[3];
            BufDataset1.FieldByName('Changed').AsBoolean := True;
            BufDataset1.Post;
          end;
          Break;
        end;
        BufDataset1.Next;
      end;
    end;
    ShowMessage('Configuration import completed. Backup created: ' + BackupName);
  finally
    CSVFields.Free;
    SL.Free;
  end;
end;

procedure TfmFirebirdConfig.bbLoadFB3ConfigClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    try
      FFirebirdConfPath := OpenDialog1.FileName;
      synEdit1.Lines.LoadFromFile(FFirebirdConfPath);
      lbServerMode.Caption := GetOptionValue('ServerMode');
      fIniFile.WriteString('FireBird', 'ConfPath', FFirebirdConfPath);
    finally
    end;
  end;
end;

procedure TfmFirebirdConfig.bbRestoreDefaultsClick(Sender: TObject);
begin
  BufDataset1.DisableControls;
  try
    BufDataset1.First;
    while not BufDataset1.EOF do
    begin
      // Nur zurücksetzen, wenn DefaultValue vorhanden ist
      if (Trim(BufDataset1.FieldByName('DefaultValue').AsString) <> '') then
      begin
        BufDataset1.Edit;
        BufDataset1.FieldByName('FileValue').AsString := BufDataset1.FieldByName('DefaultValue').AsString;
        BufDataset1.FieldByName('Changed').AsBoolean := True;
        BufDataset1.Post;
      end;
      BufDataset1.Next;
    end;
  finally
    BufDataset1.EnableControls;
  end;

  ShowMessage('All values have been reset to their defaults.');
end;

procedure TfmFirebirdConfig.bbRestoreFB3DefaultsClick(Sender: TObject);
begin
  //
end;

procedure TfmFirebirdConfig.bbSaveClick(Sender: TObject);
var
  ConfList: TStringList;
  i: Integer;
  OptionName, FileValue: string;
  ConfIndex: Integer;
begin
  if not FileExists(FFirebirdConfPath) then
  begin
    ShowMessage('firebird.conf not found: ' + FFirebirdConfPath);
    Exit;
  end;

  // Create backup of current firebird.conf
  try
    CopyFile(FFirebirdConfPath, FFirebirdConfPath + '.bak');
  except
    on E: Exception do
    begin
      ShowMessage('Error creating backup: ' + E.Message);
      Exit;
    end;
  end;

  // Load firebird.conf into memory
  ConfList := TStringList.Create;
  try
    ConfList.LoadFromFile(FFirebirdConfPath);

    BufDataset1.DisableControls;
    try
      BufDataset1.First;
      while not BufDataset1.EOF do
      begin
        if BufDataset1.FieldByName('Changed').AsBoolean then
        begin
          OptionName := BufDataset1.FieldByName('Name').AsString;
          FileValue := BufDataset1.FieldByName('FileValue').AsString;

          // Look for existing option
          ConfIndex := -1;
          for i := 0 to ConfList.Count - 1 do
          begin
            if Trim(ConfList.Names[i]) = OptionName then
            begin
              ConfIndex := i;
              Break;
            end;
          end;

          // Update existing line or add new entry
          if ConfIndex >= 0 then
            ConfList[ConfIndex] := OptionName + ' = ' + FileValue
          else
            ConfList.Add(OptionName + ' = ' + FileValue);
        end;

        BufDataset1.Next;
      end;

      // Save back to firebird.conf
      try
        ConfList.SaveToFile(FFirebirdConfPath);
        ShowMessage('firebird.conf saved successfully.' + LineEnding +
                    'Changes will take effect after the next Firebird server restart.');
      except
        on E: Exception do
        begin
          ShowMessage('Error saving firebird.conf: ' + E.Message);
          Exit;
        end;
      end;

      // Reset Changed flag after saving
      BufDataset1.First;
      while not BufDataset1.EOF do
      begin
        if BufDataset1.FieldByName('Changed').AsBoolean then
        begin
          BufDataset1.Edit;
          BufDataset1.FieldByName('Changed').AsBoolean := False;
          BufDataset1.Post;
        end;
        BufDataset1.Next;
      end;

    finally
      BufDataset1.EnableControls;
    end;

  finally
    ConfList.Free;
  end;
end;

procedure TfmFirebirdConfig.bbSaveFB3ConfigClick(Sender: TObject);
begin
  try
    SynEdit1.Lines.SaveToFile(FFirebirdConfPath);
    //lbServerMode.Caption := GetOptionValue('ServerMode');
    ShowMessage('Configuration file saved successfully.' + sLineBreak +
                'Note: Changes will not take effect until the Firebird server is restarted.');
    bbSaveFB3Config.Enabled := false;
  except
    on E: Exception do
      ShowMessage('Error saving configuration file: ' + E.Message);
  end;
end;

procedure TfmFirebirdConfig.bbSearchClick(Sender: TObject);
var
  SearchText, FullText: string;
  FoundPos: Integer;
  StartPoint, EndPoint: TPoint;
begin
  SearchText := edtsearch.Text;
  if SearchText = '' then Exit;

  // Convert both to lowercase for case-insensitive search
  FullText := LowerCase(SynEdit1.Text);
  SearchText := LowerCase(SearchText);

  FoundPos := Pos(SearchText, FullText);

  if FoundPos > 0 then
  begin
    // Convert flat index to (X,Y) position
    StartPoint := SynEdit1.CharIndexToRowCol(FoundPos);
    EndPoint := SynEdit1.CharIndexToRowCol(FoundPos + Length(SearchText));

    StartPoint.X := StartPoint.X - 1;
    EndPoint.X := EndPoint.X - 1;

    // Set selection
    SynEdit1.BlockBegin := StartPoint;
    SynEdit1.BlockEnd := EndPoint;

    SynEdit1.CaretXY := EndPoint;
    SynEdit1.SetFocus;
  end
  else
    ShowMessage('Text not found.');
end;

procedure TfmFirebirdConfig.DataSource1UpdateData(Sender: TObject);
begin
  if DataSetLoaded then
  begin
    Datasource1.DataSet.FieldByName('Changed').AsBoolean := true;
    Datasource1.DataSet.FieldByName('Config_Is_Set').AsBoolean := true;
  end;
end;

procedure TfmFirebirdConfig.DBNavigator1Click(Sender: TObject;
  Button: TDBNavButtonType);
begin
  if Button = nbRefresh then
    BufDataset1.ApplyUpdates;
end;

procedure TfmFirebirdConfig.edtsearchKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then // Enter key
  begin
    Key := #0; // prevent beep sound
    bbSearchClick(nil);
  end;
end;

procedure TfmFirebirdConfig.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TfmFirebirdConfig.SynEdit1Change(Sender: TObject);
begin
  bbSaveFB3Config.Enabled := true;
end;

constructor TfmFirebirdConfig.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFirebirdConfLines := TStringList.Create;
end;

destructor TfmFirebirdConfig.Destroy;
begin
  FFirebirdConfLines.Free;
  if SQLQuery1.Active then SQLQuery1.Close;
  if IBConnection1.Connected then IBConnection1.Connected := False;
  inherited;
end;

function GuessFirebirdConfPath(const SecurityDbPath: string): string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(SecurityDbPath)) + 'firebird.conf';
end;

function LoadFirebirdConfToList(const AFileName: string): TStringList;
var
  Lines: TStringList;
  i, SepPos: Integer;
  Line, Key, Value: string;
begin
  Result := TStringList.Create;
  Result.NameValueSeparator := '=';
  Result.CaseSensitive := False;

  if not FileExists(AFileName) then Exit;

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(AFileName);
    for i := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[i]);
      if Line = '' then Continue;

      // Suche nach einfachem '='
      SepPos := Pos('=', Line);
      if SepPos = 0 then Continue;

      // Prüfen, ob direkt nach '=' noch ein '=' folgt (== vermeiden)
      if (SepPos < Length(Line)) and (Line[SepPos + 1] = '=') then
        Continue; // Zeile überspringen, weil '==' vorhanden

      Key := Trim(Copy(Line, 1, SepPos - 1));
      Value := Trim(Copy(Line, SepPos + 1, MaxInt));

      // Prüfen, ob Schlüssel schon vorhanden
      if Result.IndexOfName(Key) < 0 then
        Result.Values[Key] := Value;
    end;
  finally
    Lines.Free;
  end;
end;

procedure TfmFirebirdConfig.InitForFB3;
begin
  try
    FFirebirdConfPath := fIniFile.ReadString('FireBird', 'ConfPath', '');
    if FileExists(FFirebirdConfPath) then
    begin
      SynEdit1.Lines.LoadFromFile(FFirebirdConfPath);
      lbServerMode.Caption := GetOptionValue('ServerMode');
    end
    else
    begin
      ShowMessage('The file specified in the ini file does not exist. Please select a valid configuration file in the next dialog.');

      // Open file dialog to select a valid Firebird config file
      if OpenDialog1.Execute then
      begin
        FFirebirdConfPath := OpenDialog1.FileName;
        SynEdit1.Lines.LoadFromFile(FFirebirdConfPath);
        lbServerMode.Caption := GetOptionValue('ServerMode');
        // Optionally save the selected path back to the ini file
        fIniFile.WriteString('FireBird', 'ConfPath', FFirebirdConfPath);
      end
      else
      begin
        ShowMessage('No valid file was selected.');
      end;
    end;
  finally
  end;
end;

function TfmFirebirdConfig.GetOptionValue(AOption: string): string;
var
  i, PosEqual: Integer;
  Line, OptionName, OptionValue: string;
begin
  Result := 'Default';
  for i := 0 to SynEdit1.Lines.Count - 1 do
  begin
    Line := Trim(SynEdit1.Lines[i]);

    // Ignore comments and empty lines
    if (Line = '') or (Line[1] = '#') then
      Continue;

    // Find position of '='
    PosEqual := Pos('=', Line);
    if PosEqual = 0 then Continue;

    OptionName := Trim(Copy(Line, 1, PosEqual - 1));
    OptionValue := Trim(Copy(Line, PosEqual + 1, MaxInt));

    if SameText(OptionName, AOption) then
    begin
      Result := OptionValue;
      Exit;
    end;
  end;
end;

procedure TfmFirebirdConfig.Init(AdbIndex: Integer);
var
  Rec: TDatabaseRec;
  ConfList: TStringList;
  ConfName, ConfVal, ConfSource, SecDbPath: string;
begin
  FdbIndex := AdbIndex;
  Rec := RegisteredDatabases[FdbIndex];
  AssignIBConnection(IBConnection1, Rec.IBConnection);
  IBConnection1.Connected := True;

  lbServerVersion.Caption := IntToStr(FBVersionMajor) + '.' + IntToStr(FBVersionMinor);
  if  FBVersionMajor < 4 then
  begin
    PageControl1.ActivePage := tsFB3Config;
    tsFB3Config.TabVisible  := true;
    tsFB3Config.Visible     := true;
    InitForFB3;
  end
  else begin
    PageControl1.ActivePage := tsFB4Config;
    tsFB4Config.TabVisible := true;
    tsFB4Config.Visible := true;

    if SQLQuery1.Active then SQLQuery1.Close;
    if IBConnection1.Connected then IBConnection1.Connected := False;

    // RDB$CONFIG lesen
    //SQLQuery1.SQL.Text := 'SELECT RDB$CONFIG_NAME, RDB$CONFIG_VALUE, RDB$CONFIG_SOURCE FROM RDB$CONFIG';
    SQLQuery1.SQL.Text := 'SELECT * FROM RDB$CONFIG';
    SQLQuery1.Open;

    // firebird.conf Pfad finden
    SQLQuery1.First;
    while not SQLQuery1.EOF do
    begin
      if Trim(SQLQuery1.FieldByName('RDB$CONFIG_NAME').AsString) = 'SecurityDatabase' then
      begin
        SecDbPath := Trim(SQLQuery1.FieldByName('RDB$CONFIG_VALUE').AsString);
        FFirebirdConfPath := GuessFirebirdConfPath(SecDbPath);
        Break;
      end;
      SQLQuery1.Next;
    end;

    // file lesen
    FFirebirdConfLines.Clear;
    if FileExists(FFirebirdConfPath) then
      FFirebirdConfLines.LoadFromFile(FFirebirdConfPath);

    ConfList := LoadFirebirdConfToList(FFirebirdConfPath);

    // Dataset füllen
    BufDataset1.DisableControls;

    BufDataset1.Close;
    BufDataset1.Clear;

    BufDataset1.FieldDefs.Add('ID', ftInteger);
    BufDataset1.FieldDefs.Add('Name', ftString, 200);
    BufDataset1.FieldDefs.Add('ActiveValue', ftString, 250);
    BufDataset1.FieldDefs.Add('FileValue', ftString, 250);
    BufDataset1.FieldDefs.Add('DefaultValue', ftString, 250);
    BufDataset1.FieldDefs.Add('Source', ftString, 250);
    BufDataset1.FieldDefs.Add('Config_is_set', ftBoolean);
    BufDataset1.FieldDefs.Add('Changed', ftBoolean);

    BufDataset1.CreateDataset;

    dbedtActiveValue.DataField  := 'ActiveValue';
    dbedtsource.DataField       := 'Source';
    dbedtFileValue.DataField    := 'FileValue';

    SQLQuery1.First;

    while not SQLQuery1.EOF do
    begin
      ConfName := Trim(SQLQuery1.FieldByName('RDB$CONFIG_NAME').AsString);

      if UpperCASE(ConfName) = 'SERVERMODE' then
        lbServerMode.Caption := SQLQuery1.FieldByName('RDB$CONFIG_VALUE').AsString;

      ConfVal := Trim(SQLQuery1.FieldByName('RDB$CONFIG_VALUE').AsString);
      ConfSource := Trim(SQLQuery1.FieldByName('RDB$CONFIG_SOURCE').AsString);

      BufDataset1.Append;

      BufDataset1.FieldByName('ID').AsInteger := SQLQuery1.FieldByName('RDB$CONFIG_ID').AsInteger;
      BufDataset1.FieldByName('Name').AsString := ConfName;
      BufDataset1.FieldByName('ActiveValue').AsString := ConfVal;
      BufDataset1.FieldByName('DefaultValue').AsString := SQLQuery1.FieldByName('RDB$CONFIG_DEFAULT').AsString;
      BufDataset1.FieldByName('FileValue').AsString := ConfList.Values[ConfName];
      BufDataset1.FieldByName('Source').AsString := ConfSource;
      BufDataset1.FieldByName('Changed').AsBoolean := False;
      BufDataset1.FieldByName('Config_is_set').AsBoolean := SQLQuery1.FieldByName('RDB$CONFIG_IS_SET').AsBoolean;

      BufDataset1.FieldByName('Changed').Visible := false;

      BufDataset1.Post;
      SQLQuery1.Next;
    end;

    ConfList.Free;
    BufDataset1.First;
    BufDataset1.EnableControls;

    DBGrid1.Columns[0].Width := 30;
    DBGrid1.Columns[1].Width := 250;
    DBGrid1.Columns[2].Width := 300;
    DBGrid1.Columns[3].Width := 100;
    DBGrid1.Columns[4].Width := 100;
    DBGrid1.Columns[5].Width := 100;

    DBGrid1.Columns[0].ReadOnly := true;
    DBGrid1.Columns[1].ReadOnly := true;
    DBGrid1.Columns[2].ReadOnly := true;
    //DBGrid1.Columns[3].ReadOnly := true;
    DBGrid1.Columns[4].ReadOnly := true;
    DBGrid1.Columns[5].ReadOnly := true;
    DBGrid1.Columns[6].ReadOnly := true;

    DataSetLoaded := true;
  end;
end;

{$R *.lfm}

end.

