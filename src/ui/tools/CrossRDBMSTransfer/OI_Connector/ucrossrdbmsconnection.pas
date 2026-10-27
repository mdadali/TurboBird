unit uCrossRDBMSConnection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, PQConnection, IBConnection, SQLDBLib,
  mysql40conn, mysql41conn, mysql50conn, mysql51conn, mysql55conn,
  mysql56conn, mysql57conn, mysql80conn, odbcconn, oracleconnection,
  MSSQLConn, SQLite3Conn,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, RTTIGrids,
  PropEdits,
  uCrossRDBMSConnectionWrapper;

type
  { TfrmCrossRDBMSConnection }

  TfrmCrossRDBMSConnection = class(TForm)
    cmbboxConnection: TComboBox;
    GroupBoxConnection: TGroupBox;
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
    FOnAfterConnect: TNotifyEvent;
    FOnAfterDisconnect: TNotifyEvent;

    function  GetActiveConnection: TSQLConnection;
    function  IsMySQLDriver(const ADriver: string): Boolean;
    procedure SQLConnectorAfterConnect(Sender: TObject);
    procedure SQLConnectorAfterDisconnect(Sender: TObject);
  public
    property Conn: TSQLConnection read GetActiveConnection;
    property Loader: TSQLDBLibraryLoader read SQLDBLibraryLoader;
    property Trans: TSQLTransaction read SQLTransaction;
    property Query: TSQLQuery read SQLQuery;

    property OnAfterConnect: TNotifyEvent read FOnAfterConnect write FOnAfterConnect;
    property OnAfterDisconnect: TNotifyEvent read FOnAfterDisconnect write FOnAfterDisconnect;
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
  // Datei-Dialoge für PropertyGrid registrieren
  RegisterPropertyEditor(TypeInfo(String), TConnectionWrapper, 'DatabaseName',
    TFileNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TConnectionWrapper, 'LibraryLocation',
    TFileNamePropertyEditor);

  FCurrentWrapper := nil;

  // MySQL80 spezielle Einstellungen
  MySQL80Conn.SkipLibraryVersionCheck := True;
  MySQL80Conn.Transaction := SQLTransaction;

  // Events
  SQLConnector.AfterConnect := @SQLConnectorAfterConnect;
  SQLConnector.AfterDisconnect := @SQLConnectorAfterDisconnect;
  MySQL80Conn.AfterConnect := @SQLConnectorAfterConnect;
  MySQL80Conn.AfterDisconnect := @SQLConnectorAfterDisconnect;

  // Treiberliste füllen
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
  if IsMySQLDriver(cmbboxConnection.Text) then
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

  if Assigned(SQLConnector) and SQLConnector.Connected then
    SQLConnector.Close;
  if Assigned(MySQL80Conn) and MySQL80Conn.Connected then
    MySQL80Conn.Close;

  if cmbboxConnection.ItemIndex < 0 then Exit;

  SelectedDriver := SQLDB_DRIVERS[cmbboxConnection.ItemIndex];

  if SQLDBLibraryLoader.Enabled then
    SQLDBLibraryLoader.Enabled := False;

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
  InspectorGrid.TIObject := FCurrentWrapper;
end;

procedure TfrmCrossRDBMSConnection.SQLConnectorAfterConnect(Sender: TObject);
begin
  if Assigned(SQLTransaction) and not SQLTransaction.Active then
  begin
    try
      SQLTransaction.StartTransaction;
    except
    end;
  end;

  if Assigned(FOnAfterConnect) then
    FOnAfterConnect(Self);
end;

procedure TfrmCrossRDBMSConnection.SQLConnectorAfterDisconnect(Sender: TObject);
begin
  if Assigned(FOnAfterDisconnect) then
    FOnAfterDisconnect(Self);
end;

end.
