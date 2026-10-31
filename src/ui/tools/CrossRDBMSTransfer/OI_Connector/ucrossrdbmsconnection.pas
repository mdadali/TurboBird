unit uCrossRDBMSConnection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, PQConnection, IBConnection, SQLDBLib, mysql40conn,
  mysql41conn, mysql50conn, mysql51conn, mysql55conn, mysql56conn, mysql57conn,
  mysql80conn, odbcconn, oracleconnection, MSSQLConn, SQLite3Conn, Forms,
  Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, IBQuery, IBDatabase,
  RTTIGrids, PropEdits, IniFiles,

  turbocommon,
  uCrossRDBMSTypes,
  uCrossRDBMSTypeMapping,
  uCrossRDBMSConnectionWrapper, DB;

type
  { TfrmCrossRDBMSConnection }

  TfrmCrossRDBMSConnection = class(TForm)
    cmbboxConnection: TComboBox;
    GroupBoxConnection: TGroupBox;
    IBDatabase1: TIBDatabase;
    IBQuery1: TIBQuery;
    IBTransaction1: TIBTransaction;
    pnlInspector: TPanel;
    InspectorGrid: TTIPropertyGrid;
    SQLConnector: TSQLConnector;
    MySQL80Conn: TMySQL80Connection;
    SQLDBLibraryLoader: TSQLDBLibraryLoader;
    SQLQuery: TSQLQuery;
    SQLTransaction: TSQLTransaction;
    procedure cmbboxConnectionChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FCurrentWrapper: TConnectionWrapper;
    FDetectedBackend: TCrossDBMS;
    FOnAfterConnect: TNotifyEvent;
    FOnAfterDisconnect: TNotifyEvent;
    FIBXActive: Boolean;

    function  GetActiveConnection: TSQLConnection;
    function  IsMySQLDriver(const ADriver: string): Boolean;
    procedure SQLConnectorAfterConnect(Sender: TObject);
    procedure SQLConnectorAfterDisconnect(Sender: TObject);
    procedure WrapperBeforeDisconnect(Sender: TObject);

    procedure HardResetConnection;
  public
    function  GetConnectionsRootDir: string;
    function  GetConnectionTypeFolder: string;
    procedure SaveConnectionToIni(const AFileName: string);
    procedure LoadConnectionFromIni(const AFileName: string);
    function  ExecuteLoadDialog: Boolean;
    function  ExecuteSaveDialog: Boolean;

    property Conn: TSQLConnection read GetActiveConnection;
    property Loader: TSQLDBLibraryLoader read SQLDBLibraryLoader;
    property Trans: TSQLTransaction read SQLTransaction;
    property Query: TSQLQuery read SQLQuery;

    property OnAfterConnect: TNotifyEvent read FOnAfterConnect write FOnAfterConnect;
    property OnAfterDisconnect: TNotifyEvent read FOnAfterDisconnect write FOnAfterDisconnect;

    property IsIBX: Boolean read FIBXActive;
    property IBXConn: TIBDatabase read IBDatabase1;
    property IBXTrans: TIBTransaction read IBTransaction1;
    property IBXQuery: TIBQuery read IBQuery1;
    property ConnDBMS: TCrossDBMS read FDetectedBackend;

  end;

implementation

{$R *.lfm}

const
  SQLDB_DRIVERS: array[0..14] of string = (
    'SQLite3',
    'PostgreSQL',
    'Firebird',
    'MySQL 4.0',
    'MySQL 4.1',
    'MySQL 5.0',
    'MySQL 5.1',
    'MySQL 5.5',
    'MySQL 5.6',
    'MySQL 5.7',
    'MySQL 8.0',
    'ODBC',
    'Oracle',
    'MSSQLServer',
    'Sybase'
  );

procedure TfrmCrossRDBMSConnection.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  RegisterPropertyEditor(TypeInfo(String), TConnectionWrapper, 'DatabaseName',
    TFileNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TConnectionWrapper, 'LibraryLocation',
    TFileNamePropertyEditor);

  FCurrentWrapper := nil;
  FDetectedBackend := cdUnknown;
  FIBXActive := False;

  MySQL80Conn.SkipLibraryVersionCheck := True;
  MySQL80Conn.Transaction := SQLTransaction;

  SQLConnector.AfterConnect := @SQLConnectorAfterConnect;
  SQLConnector.AfterDisconnect := @SQLConnectorAfterDisconnect;
  MySQL80Conn.AfterConnect := @SQLConnectorAfterConnect;
  MySQL80Conn.AfterDisconnect := @SQLConnectorAfterDisconnect;

  cmbboxConnection.Items.Clear;
  for i := Low(SQLDB_DRIVERS) to High(SQLDB_DRIVERS) do
    cmbboxConnection.Items.Add(SQLDB_DRIVERS[i]);

  if cmbboxConnection.Items.Count > 0 then
  begin
    cmbboxConnection.ItemIndex := 0;
    cmbboxConnectionChange(nil);
  end;
end;

procedure TfrmCrossRDBMSConnection.FormDestroy(Sender: TObject);
begin
  if Assigned(FCurrentWrapper) then
    FreeAndNil(FCurrentWrapper);
end;

function TfrmCrossRDBMSConnection.IsMySQLDriver(const ADriver: string): Boolean;
begin
  Result := Pos('MySQL', ADriver) > 0;
end;

function TfrmCrossRDBMSConnection.GetActiveConnection: TSQLConnection;
begin
  if FIBXActive then
    Result := nil          // Firebird läuft über IBX, nicht SQLDB
  else if IsMySQLDriver(cmbboxConnection.Text) then
    Result := MySQL80Conn
  else
    Result := SQLConnector;
end;

procedure TfrmCrossRDBMSConnection.cmbboxConnectionChange(Sender: TObject);
var
  SelectedDriver: string;
  NewConn: TSQLConnection;
begin
  InspectorGrid.TIObject := nil;
  if Assigned(FCurrentWrapper) then
  begin
    FCurrentWrapper.Free;
    FCurrentWrapper := nil;
  end;

  { Alte Connections trennen }
  if Assigned(SQLConnector) and SQLConnector.Connected then
    SQLConnector.Close;
  if Assigned(MySQL80Conn) and MySQL80Conn.Connected then
    MySQL80Conn.Close;
  if Assigned(IBDatabase1) and IBDatabase1.Connected then
    IBDatabase1.Close;
  if SQLDBLibraryLoader.Enabled then
    SQLDBLibraryLoader.Enabled := False;

  if cmbboxConnection.ItemIndex < 0 then Exit;

  SelectedDriver := SQLDB_DRIVERS[cmbboxConnection.ItemIndex];

  FIBXActive := SameText(SelectedDriver, 'Firebird');
  FDetectedBackend := cdUnknown;

  if FIBXActive then
  begin
    { ---- IBX-Modus (Firebird) ---- }
    IBDatabase1.DefaultTransaction := IBTransaction1;
    IBDatabase1.LoginPrompt := False;
    IBDatabase1.Params.Clear;

    IBQuery1.Database := IBDatabase1;
    IBQuery1.Transaction := IBTransaction1;

    IBTransaction1.DefaultDatabase := IBDatabase1;

    IBDatabase1.AfterConnect := @SQLConnectorAfterConnect;
    IBDatabase1.AfterDisconnect := @SQLConnectorAfterDisconnect;

    FCurrentWrapper := TConnectionWrapper.Create(nil, nil, IBDatabase1);
    FCurrentWrapper.OnBeforeDisconnect := @WrapperBeforeDisconnect;
    InspectorGrid.TIObject := FCurrentWrapper;
  end
  else
  begin
    { ---- SQLDB-Modus ---- }
    if IsMySQLDriver(SelectedDriver) then
      NewConn := MySQL80Conn
    else
    begin
      NewConn := SQLConnector;
      SQLConnector.ConnectorType := SelectedDriver;
    end;

    SQLDBLibraryLoader.ConnectionType := SelectedDriver;

    SQLTransaction.Database := NewConn;
    SQLQuery.Database := NewConn;
    SQLQuery.Transaction := SQLTransaction;

    FCurrentWrapper := TConnectionWrapper.Create(NewConn, SQLDBLibraryLoader);
    FCurrentWrapper.OnBeforeDisconnect := @WrapperBeforeDisconnect;
    InspectorGrid.TIObject := FCurrentWrapper;
  end;
end;

procedure TfrmCrossRDBMSConnection.SQLConnectorAfterConnect(Sender: TObject);
var
  RawDBMS: TCrossDBMS;
begin
  if FIBXActive then
  begin
    { ---- IBX-Modus ---- }
    if Assigned(IBTransaction1) and not IBTransaction1.Active then
    begin
      try
        IBTransaction1.StartTransaction;
      except
      end;
    end;
    FDetectedBackend := cdFirebird;
  end
  else
  begin
    { ---- SQLDB-Modus ---- }
    if Assigned(SQLTransaction) and not SQLTransaction.Active then
    begin
      try
        SQLTransaction.StartTransaction;
      except
      end;
    end;

    RawDBMS := DBMSFromConnection(GetActiveConnection);
    if RawDBMS = cdODBC then
      FDetectedBackend := DetectBackendDBMS(SQLQuery, SQLTransaction)
    else
      FDetectedBackend := RawDBMS;
  end;

  if Assigned(FOnAfterConnect) then
    FOnAfterConnect(Self);
end;

procedure TfrmCrossRDBMSConnection.SQLConnectorAfterDisconnect(Sender: TObject);
begin
  FDetectedBackend := cdUnknown;

  if Assigned(FOnAfterDisconnect) then
    FOnAfterDisconnect(Self);
end;

procedure TfrmCrossRDBMSConnection.WrapperBeforeDisconnect(Sender: TObject);
begin
  { ---- SQLDB: Prepared Statements finalisieren ---- }
  try
    if Assigned(SQLQuery) then
    begin
      if SQLQuery.Active then
        SQLQuery.Close;
      // WICHTIG: Prepared Statement freigeben (finalisiert den Cursor)
      SQLQuery.UnPrepare;
    end;
  except
  end;

  try
    if Assigned(SQLTransaction) and SQLTransaction.Active then
    begin
      // Schließt alle mit dieser Transaktion verbundenen Datasets
      SQLTransaction.CloseDataSets;
      SQLTransaction.Rollback;
    end;
  except
  end;

  { ---- IBX: Queries schließen und Transaktion zurückrollen ---- }
  try
    if Assigned(IBQuery1) and IBQuery1.Active then
      IBQuery1.Close;
  except
  end;

  try
    if Assigned(IBTransaction1) and IBTransaction1.Active then
      IBTransaction1.Rollback;
  except
  end;
end;

function TfrmCrossRDBMSConnection.GetConnectionsRootDir: string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) +
            'data' + PathDelim + 'connections' + PathDelim;
end;

function TfrmCrossRDBMSConnection.GetConnectionTypeFolder: string;
var
  Driver: string;
begin
  Driver := Trim(cmbboxConnection.Text);

  if SameText(Driver, 'SQLite3') then
    Result := 'SQLite'
  else if SameText(Driver, 'PostgreSQL') then
    Result := 'PostgreSQL'
  else if SameText(Driver, 'Firebird') then
    Result := 'Firebird'
  else if Pos('MySQL', Driver) > 0 then
    Result := 'MySQL'
  else if SameText(Driver, 'MSSQLServer') then
    Result := 'MSSQLServer'
  else if SameText(Driver, 'Oracle') then
    Result := 'Oracle'
  else if SameText(Driver, 'Sybase') then
    Result := 'Sybase'
  else if SameText(Driver, 'ODBC') then
    Result := 'ODBC'
  else
    Result := 'Other';
end;

procedure TfrmCrossRDBMSConnection.SaveConnectionToIni(const AFileName: string);
var
  Ini: TIniFile;
  LocalConn: TSQLConnection;
begin
  ForceDirectories(ExtractFilePath(AFileName));

  Ini := TIniFile.Create(AFileName);
  try
    Ini.WriteString('Connection', 'Driver', cmbboxConnection.Text);

    if FIBXActive then
    begin
      if not Assigned(FCurrentWrapper) then Exit;

      Ini.WriteString('Connection', 'HostName',     FCurrentWrapper.HostName);
      Ini.WriteString('Connection', 'Port',         IntToStr(FCurrentWrapper.Port));
      Ini.WriteString('Connection', 'DatabaseName', FCurrentWrapper.DatabaseName);
      Ini.WriteString('Connection', 'UserName',     FCurrentWrapper.UserName);
      Ini.WriteString('Connection', 'Password',     FCurrentWrapper.Password);
      Ini.WriteString('Connection', 'LibraryLocation', '');
    end
    else
    begin
      LocalConn := GetActiveConnection;
      if not Assigned(LocalConn) then Exit;

      if Assigned(FCurrentWrapper) then
      begin
        Ini.WriteString('Connection', 'HostName',     FCurrentWrapper.HostName);
        Ini.WriteString('Connection', 'Port',         IntToStr(FCurrentWrapper.Port));
        Ini.WriteString('Connection', 'DatabaseName', FCurrentWrapper.DatabaseName);
        Ini.WriteString('Connection', 'UserName',     FCurrentWrapper.UserName);
        Ini.WriteString('Connection', 'Password',     FCurrentWrapper.Password);
      end
      else
      begin
        Ini.WriteString('Connection', 'HostName',     LocalConn.HostName);
        Ini.WriteString('Connection', 'Port',         '0');
        Ini.WriteString('Connection', 'DatabaseName', LocalConn.DatabaseName);
        Ini.WriteString('Connection', 'UserName',     LocalConn.UserName);
        Ini.WriteString('Connection', 'Password',     LocalConn.Password);
      end;

      Ini.WriteString('Connection', 'LibraryLocation', SQLDBLibraryLoader.LibraryName);
    end;
  finally
    Ini.Free;
  end;
end;

procedure TfrmCrossRDBMSConnection.LoadConnectionFromIni(const AFileName: string);
var
  Ini: TIniFile;
  Driver, Host, PortStr, DBName, User, Pwd, Lib: string;
  PortNum: Integer;
  i: Integer;
  LocalConn: TSQLConnection;
begin
  if not FileExists(AFileName) then
    raise Exception.Create('Connection file not found: ' + AFileName);

  Ini := TIniFile.Create(AFileName);
  try
    Driver  := Ini.ReadString('Connection', 'Driver', '');
    Host    := Ini.ReadString('Connection', 'HostName', '');
    PortStr := Ini.ReadString('Connection', 'Port', '0');
    DBName  := Ini.ReadString('Connection', 'DatabaseName', '');
    User    := Ini.ReadString('Connection', 'UserName', '');
    Pwd     := Ini.ReadString('Connection', 'Password', '');
    Lib     := Ini.ReadString('Connection', 'LibraryLocation', '');
  finally
    Ini.Free;
  end;

  PortNum := StrToIntDef(PortStr, 0);

  { Treiber setzen → triggert cmbboxConnectionChange, das FIBXActive setzt
    und den Wrapper erstellt. }
  if Driver <> '' then
  begin
    i := cmbboxConnection.Items.IndexOf(Driver);
    if i >= 0 then
    begin
      cmbboxConnection.ItemIndex := i;
      cmbboxConnectionChange(nil);
    end;
  end;

  { ---- IBX-Modus (Firebird) ---- }
  if FIBXActive then
  begin
    if not Assigned(FCurrentWrapper) then Exit;

    if IBDatabase1.Connected then
      IBDatabase1.Close;

    { Alle Werte über den Wrapper setzen — er baut den Connection-String
      beim Verbinden (HostName/Port/DatabaseName → 'host/port:path'). }
    FCurrentWrapper.HostName     := Host;
    FCurrentWrapper.Port         := PortNum;
    FCurrentWrapper.DatabaseName := DBName;
    FCurrentWrapper.UserName     := User;
    FCurrentWrapper.Password     := Pwd;

    InspectorGrid.TIObject := FCurrentWrapper;
    Exit;
  end;

  { ---- SQLDB-Modus ---- }
  LocalConn := GetActiveConnection;
  if not Assigned(LocalConn) then Exit;

  if LocalConn.Connected then
    LocalConn.Close;

  { Über den Wrapper setzen — er kennt die RDBMS-spezifische
    Port-Anwendung (ApplyPortToSQLDB, wird beim Verbinden ausgeführt) }
  if Assigned(FCurrentWrapper) then
  begin
    FCurrentWrapper.HostName     := Host;
    FCurrentWrapper.Port         := PortNum;
    FCurrentWrapper.DatabaseName := DBName;
    FCurrentWrapper.UserName     := User;
    FCurrentWrapper.Password     := Pwd;

    InspectorGrid.TIObject := FCurrentWrapper;
  end
  else
  begin
    { Fallback ohne Wrapper }
    LocalConn.HostName     := Host;
    LocalConn.DatabaseName := DBName;
    LocalConn.UserName     := User;
    LocalConn.Password     := Pwd;
    if PortNum > 0 then
      LocalConn.Params.Values['Port'] := IntToStr(PortNum);
  end;

  if Lib <> '' then
    SQLDBLibraryLoader.LibraryName := Lib;
end;

function TfrmCrossRDBMSConnection.ExecuteLoadDialog: Boolean;
var
  Dlg: TOpenDialog;
  Dir: string;
begin
  Result := False;
  Dlg := TOpenDialog.Create(nil);
  try
    Dlg.Title := 'Load Connection';
    Dlg.Filter := 'Connection files (*.ini)|*.ini|All files (*.*)|*.*';
    Dlg.DefaultExt := 'ini';
    Dlg.Options := [ofFileMustExist, ofEnableSizing];

    Dir := GetConnectionsRootDir + GetConnectionTypeFolder + PathDelim;
    ForceDirectories(Dir);
    Dlg.InitialDir := Dir;

    if Dlg.Execute then
    begin
      LoadConnectionFromIni(Dlg.FileName);
      Result := True;
    end;
  finally
    Dlg.Free;
  end;
end;

function TfrmCrossRDBMSConnection.ExecuteSaveDialog: Boolean;
var
  Dlg: TSaveDialog;
  Dir: string;
begin
  Result := False;
  Dlg := TSaveDialog.Create(nil);
  try
    Dlg.Title := 'Save Connection';
    Dlg.Filter := 'Connection files (*.ini)|*.ini|All files (*.*)|*.*';
    Dlg.DefaultExt := 'ini';
    Dlg.Options := [ofOverwritePrompt, ofPathMustExist, ofEnableSizing];

    Dir := GetConnectionsRootDir + GetConnectionTypeFolder + PathDelim;
    ForceDirectories(Dir);
    Dlg.InitialDir := Dir;

    if Dlg.Execute then
    begin
      SaveConnectionToIni(Dlg.FileName);
      Result := True;
    end;
  finally
    Dlg.Free;
  end;
end;

procedure TfrmCrossRDBMSConnection.HardResetConnection;
var
  LocalConn: TSQLConnection;
begin
  LocalConn := GetActiveConnection;
  if not Assigned(Conn) then Exit;

  try
    if Assigned(SQLQuery) and SQLQuery.Active then
      SQLQuery.Close;
  except
  end;

  try
    if Assigned(SQLTransaction) and SQLTransaction.Active then
    begin
      try
        SQLTransaction.Rollback;
      except
      end;
    end;
  except
  end;

  try
    if LocalConn.Connected then
      LocalConn.Close;
  except
  end;

  if Assigned(SQLDBLibraryLoader) then
  begin
    try
      if SQLDBLibraryLoader.ConnectionType <> 'Firebird' then
        //SQLDBLibraryLoader.Enabled := False;
    except
    end;
  end;
end;

end.
