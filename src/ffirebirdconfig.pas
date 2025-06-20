unit fFirebirdConfig;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, BufDataset, IBConnection, SQLDB, Forms, Controls,
  Graphics, Dialogs, StdCtrls, DBCtrls, DBGrids, ExtCtrls, Buttons,  ComCtrls, FileUtil,
  SysTables, turbocommon, fbcommon;

type

  { TfmFirebirdConfig }

  TfmFirebirdConfig = class(TForm)
    bbClose: TSpeedButton;
    bbSave: TBitBtn;
    BufDataset1: TBufDataset;
    DataSource1: TDataSource;
    dbcboxChanged: TDBCheckBox;
    dbcboxConfigIsSet: TDBCheckBox;
    dbedtActiveValue: TDBEdit;
    dbedtSource: TDBEdit;
    dbedtFileValue: TDBEdit;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    GroupBox1: TGroupBox;
    IBConnection1: TIBConnection;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lbServerMode: TLabel;
    lbServer: TLabel;
    lbServerVersion: TLabel;
    Panel13: TPanel;
    Panel2: TPanel;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    procedure bbCloseClick(Sender: TObject);
    procedure bbSaveClick(Sender: TObject);
    procedure bbtCloseClick(Sender: TObject);
    procedure DBNavigator1Click(Sender: TObject; Button: TDBNavButtonType);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    FdbIndex: Integer;
    FFirebirdConfPath: string;
    FFirebirdConfLines: TStringList;
  public
    constructor create(AOwner: TComponent); override;
    destructor  destroy; override;
    procedure init(AdbIndex: integer);
  end;

//var
  //fmFirebirdConfig: TfmFirebirdConfig;

implementation

uses main;

function GuessFirebirdConfPath(const SecurityDbPath: string): string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(SecurityDbPath)) + 'firebird.conf';
end;

function LoadFirebirdConfToList(const AFileName: string): TStringList;
var
  Lines: TStringList;
  i: Integer;
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
      if (Line = '') or (Line[1] = '#') then Continue;
      if Pos('=', Line) > 0 then
      begin
        Key := Trim(Copy(Line, 1, Pos('=', Line) - 1));
        Value := Trim(Copy(Line, Pos('=', Line) + 1, MaxInt));
        Result.Values[Key] := Value;
      end;
    end;
  finally
    Lines.Free;
  end;
end;



procedure TfmFirebirdConfig.bbCloseClick(Sender: TObject);
begin
  close;
  self.Parent.Free;
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

procedure TfmFirebirdConfig.bbtCloseClick(Sender: TObject);
begin

end;

procedure TfmFirebirdConfig.DBNavigator1Click(Sender: TObject;
  Button: TDBNavButtonType);
begin
  if Button = nbRefresh then
    BufDataset1.ApplyUpdates;
end;

procedure TfmFirebirdConfig.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
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

procedure TfmFirebirdConfig.Init(AdbIndex: Integer);
var
  Rec: TDatabaseRec;
  ConfList: TStringList;
  ConfName, ConfVal, ConfSource, SecDbPath: string;
begin
  if SQLQuery1.Active then SQLQuery1.Close;
  if IBConnection1.Connected then IBConnection1.Connected := False;

  FdbIndex := AdbIndex;
  Rec := fmMain.RegisteredDatabases[FdbIndex];
  AssignIBConnection(IBConnection1, Rec.IBConnection);
  IBConnection1.Connected := True;
  DetectFBVersion(IBConnection1);
  lbServerVersion.Caption := IntToStr(FBVersionMajor) + '.' + IntToStr(FBVersionMinor);

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
  BufDataset1.FieldDefs.Add('Source', ftString, 250);
  BufDataset1.FieldDefs.Add('FileValue', ftString, 250);
  BufDataset1.FieldDefs.Add('Changed', ftBoolean);
  BufDataset1.FieldDefs.Add('Config_is_set', ftBoolean);

  BufDataset1.CreateDataset;

  dbedtActiveValue.DataField  := 'ActiveValue';
  dbedtsource.DataField       := 'Source';
  dbedtFileValue.DataField    := 'FileValue';
  dbcboxChanged.DataField     := 'Changed';
  dbcboxConfigIsSet.DataField := 'Config_is_set';

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
    BufDataset1.FieldByName('Source').AsString := ConfSource;
    BufDataset1.FieldByName('FileValue').AsString := ConfList.Values[ConfName];
    BufDataset1.FieldByName('Changed').AsBoolean := False;
    BufDataset1.FieldByName('Config_is_set').AsBoolean := SQLQuery1.FieldByName('RDB$CONFIG_IS_SET').AsBoolean;

    BufDataset1.Post;

    SQLQuery1.Next;
  end;

  BufDataset1.FieldByName('ID').ReadOnly := true;
  BufDataset1.FieldByName('Name').ReadOnly := true;
  BufDataset1.FieldByName('ActiveValue').ReadOnly := true;
  BufDataset1.FieldByName('Source').ReadOnly := true;

  ConfList.Free;
  BufDataset1.First;
  BufDataset1.EnableControls;

  DBGrid1.Columns[0].Width := 30;
  DBGrid1.Columns[1].Width := 250;
  DBGrid1.Columns[2].Width := 300;
  DBGrid1.Columns[3].Width := 100;
  DBGrid1.Columns[4].Width := 100;

end;

{$R *.lfm}

end.

