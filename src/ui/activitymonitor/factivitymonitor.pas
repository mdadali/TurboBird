unit fActivityMonitor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  DBGrids, Grids, ExtCtrls, StdCtrls, DB,
  IBDatabase, IBQuery,
  turbocommon;

type

  { TfrmActivityMonitor }

  TfrmActivityMonitor = class(TForm)

    btnKillAttachment: TButton;
    btnKillStatement: TButton;
    btnRefresh: TButton;
    Panel1: TPanel;
    lbStatements: TPanel;

    pnlTop: TPanel;
    pnlMain: TPanel;
    pnlBottom: TPanel;

    Splitter1: TSplitter;
    Splitter2: TSplitter;

    grdAttachments: TDBGrid;
    grdTransactions: TDBGrid;
    grdStatements: TDBGrid;

    IBDatabase: TIBDatabase;
    trRead: TIBTransaction;
    trExec: TIBTransaction;

    qryAttachments: TIBQuery;
    qryTransactions: TIBQuery;
    qryStatements: TIBQuery;

    dsAttachments: TDataSource;
    dsTransactions: TDataSource;
    dsStatements: TDataSource;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);

    procedure btnRefreshClick(Sender: TObject);
    procedure btnKillAttachmentClick(Sender: TObject);
    procedure btnKillStatementClick(Sender: TObject);

    procedure grdAttachmentsCellClick(Column: TColumn);

    procedure grdAttachmentsDrawColumnCell(
      Sender: TObject;
      const Rect: TRect;
      DataCol: Integer;
      Column: TColumn;
      State: TGridDrawState);
    procedure Splitter2CanOffset(Sender: TObject; var NewOffset: Integer;
      var Accept: Boolean);

  private

    FNodeInfos: TPNodeInfos;
    FDBIndex: Integer;
    FMyAttachmentID: Int64;

    procedure ConnectDatabase;
    procedure DisconnectDatabase;

    procedure RefreshSnapshot;

    function FetchMyAttachmentID: Int64;

    procedure LoadAttachments;
    procedure LoadTransactions;
    procedure LoadStatements;

    function SelectedID(
      DataSet: TDataSet;
      const FieldName: String): Int64;

    procedure ExecSQL(
      const SQL: String;
      const ID: Int64);

    function IsMyAttachment(
      const ID: Int64): Boolean;

  public

    procedure Init(
      ANodeInfos: TPNodeInfos;
      dbIndex: Integer);

  end;

implementation

{$R *.lfm}


procedure TfrmActivityMonitor.DisconnectDatabase;
begin
  if qryAttachments.Active then
    qryAttachments.Close;

  if qryTransactions.Active then
    qryTransactions.Close;

  if qryStatements.Active then
    qryStatements.Close;

  if trRead.InTransaction then
    trRead.Commit;

  if trExec.InTransaction then
    trExec.Commit;

  if IBDatabase.Connected then
    IBDatabase.Connected := false;
end;

{ ============================================= }
{ Database Connection }
{ ============================================= }

procedure TfrmActivityMonitor.ConnectDatabase;
begin
  DisconnectDatabase;

  IBDatabase.DatabaseName :=
    RegisteredDatabases[FDBIndex].
    IBDatabase.DatabaseName;

  IBDatabase.LoginPrompt := False;

  IBDatabase.Params.Clear;

  IBDatabase.Params.Add(
    'user_name=' +
    RegisteredDatabases[FDBIndex].
    RegRec.UserName);

  IBDatabase.Params.Add(
    'password=' +
    RegisteredDatabases[FDBIndex].
    RegRec.Password);

  IBDatabase.Connected := True;

  trRead.DefaultDatabase := IBDatabase;
  trExec.DefaultDatabase := IBDatabase;

  trRead.Params.Text :=
    'read_committed'#13+
    'rec_version'#13+
    'nowait';

  trExec.Params.Text :=
    'read_committed'#13+
    'rec_version'#13+
    'nowait';

  trRead.StartTransaction;
  trExec.StartTransaction;

end;

{ ============================================= }
{ Snapshot Handling }
{ ============================================= }

procedure TfrmActivityMonitor.RefreshSnapshot;
begin
  if trRead.Active then
    trRead.CommitRetaining
  else
    trRead.StartTransaction;
end;

{ ============================================= }
{ Own Attachment Detection }
{ ============================================= }

function TfrmActivityMonitor.FetchMyAttachmentID: Int64;
var
  q: TIBQuery;
begin

  Result := -1;

  q := TIBQuery.Create(nil);

  try

    q.Database := IBDatabase;
    q.Transaction := trRead;

    RefreshSnapshot;

    q.SQL.Text :=
      'SELECT CURRENT_CONNECTION FROM RDB$DATABASE';

    q.Open;

    if not q.IsEmpty then
      Result := q.Fields[0].AsLargeInt;

    q.Close;

  finally
    q.Free;
  end;

end;

function TfrmActivityMonitor.IsMyAttachment(
  const ID: Int64): Boolean;
begin
  Result := ID = FMyAttachmentID;
end;

{ ============================================= }
{ Init }
{ ============================================= }

procedure TfrmActivityMonitor.Init(
  ANodeInfos: TPNodeInfos;
  dbIndex: Integer);
begin

  FNodeInfos := ANodeInfos;
  FDBIndex := dbIndex;

  DisconnectDatabase;
  ConnectDatabase;

  qryAttachments.Database := IBDatabase;
  qryTransactions.Database := IBDatabase;
  qryStatements.Database := IBDatabase;

  qryAttachments.Transaction := trRead;
  qryTransactions.Transaction := trRead;
  qryStatements.Transaction := trRead;

  dsAttachments.DataSet := qryAttachments;
  dsTransactions.DataSet := qryTransactions;
  dsStatements.DataSet := qryStatements;

  FMyAttachmentID := FetchMyAttachmentID;

  LoadAttachments;

end;

{ ============================================= }
{ Load Data }
{ ============================================= }

procedure TfrmActivityMonitor.LoadAttachments;
begin
  qryAttachments.Close;

  RefreshSnapshot;

  qryAttachments.SQL.Text :=
    'SELECT '+
    ' MON$TIMESTAMP, '+
    ' MON$REMOTE_PROCESS, '+
    ' MON$USER, '+
    ' MON$REMOTE_ADDRESS, '+
    ' MON$SERVER_PID, '+
    ' MON$REMOTE_PROTOCOL, '+

    ' MON$ATTACHMENT_ID, '+

    ' MON$STATE, '+
    ' MON$ATTACHMENT_NAME, '+

    ' MON$ROLE, '+

    ' MON$CHARACTER_SET_ID, '+

    ' MON$GARBAGE_COLLECTION, '+
    ' MON$AUTH_METHOD '+
    'FROM MON$ATTACHMENTS '+
    'ORDER BY MON$ATTACHMENT_ID';

  qryAttachments.Open;

  LoadTransactions;
  LoadStatements;

end;

procedure TfrmActivityMonitor.LoadTransactions;
var
  ID: Int64;
begin

  qryTransactions.Close;

  ID := SelectedID(qryAttachments,'MON$ATTACHMENT_ID');

  if ID < 0 then Exit;

  qryTransactions.SQL.Text :=
    'SELECT * FROM MON$TRANSACTIONS '+
    'WHERE MON$ATTACHMENT_ID = :ID';

  qryTransactions.ParamByName('ID').AsLargeInt := ID;

  qryTransactions.Open;

end;

procedure TfrmActivityMonitor.LoadStatements;
var
  ID: Int64;
begin

  qryStatements.Close;

  ID := SelectedID(qryAttachments,'MON$ATTACHMENT_ID');

  if ID < 0 then Exit;

  qryStatements.SQL.Text :=
    'SELECT * FROM MON$STATEMENTS '+
    'WHERE MON$ATTACHMENT_ID = :ID';

  qryStatements.ParamByName('ID').AsLargeInt := ID;

  qryStatements.Open;

end;

{ ============================================= }
{ Selected ID }
{ ============================================= }

function TfrmActivityMonitor.SelectedID(
  DataSet: TDataSet;
  const FieldName: String): Int64;
begin

  Result := -1;

  if not Assigned(DataSet) then Exit;
  if not DataSet.Active then Exit;
  if DataSet.IsEmpty then Exit;

  Result :=
    DataSet.FieldByName(FieldName).
    AsLargeInt;

end;

{ ============================================= }
{ Exec Admin SQL }
{ ============================================= }

procedure TfrmActivityMonitor.ExecSQL(
  const SQL: String;
  const ID: Int64);
var
  q: TIBQuery;
begin

  if IsMyAttachment(ID) then
  begin
    ShowMessage(
      'Cannot kill own attachment.');
    Exit;
  end;

  q := TIBQuery.Create(nil);

  try

    q.Database := IBDatabase;
    q.Transaction := trExec;

    q.SQL.Text := SQL;
    q.ParamByName('ID').AsLargeInt := ID;

    q.ExecSQL;

    trExec.Commit;
    trExec.StartTransaction;

  finally
    q.Free;
  end;

end;

{ ============================================= }
{ UI }
{ ============================================= }

procedure TfrmActivityMonitor.btnRefreshClick(Sender: TObject);
begin
  DisconnectDatabase;
  ConnectDatabase;

  FMyAttachmentID := FetchMyAttachmentID;
  LoadAttachments;
end;

procedure TfrmActivityMonitor.btnKillAttachmentClick(Sender: TObject);
begin
  ExecSQL(
    'DELETE FROM MON$ATTACHMENTS WHERE MON$ATTACHMENT_ID=:ID',
    SelectedID(qryAttachments,'MON$ATTACHMENT_ID')
  );

  DisconnectDatabase;
  ConnectDatabase;

  FMyAttachmentID := FetchMyAttachmentID;
  LoadAttachments;
end;


procedure TfrmActivityMonitor.btnKillStatementClick(Sender: TObject);
begin
  ExecSQL(
    'DELETE FROM MON$STATEMENTS WHERE MON$STATEMENT_ID=:ID',
    SelectedID(qryStatements,'MON$STATEMENT_ID')
  );
  LoadStatements;
end;

procedure TfrmActivityMonitor.grdAttachmentsCellClick(
  Column: TColumn);
begin
  LoadTransactions;
  LoadStatements;
end;

{ ============================================= }
{ Drawing }
{ ============================================= }

procedure TfrmActivityMonitor.grdAttachmentsDrawColumnCell(
  Sender: TObject;
  const Rect: TRect;
  DataCol: Integer;
  Column: TColumn;
  State: TGridDrawState);
var
  grid: TDBGrid;
  ID: Int64;
  txt: String;
begin

  grid := Sender as TDBGrid;

  ID :=
    SelectedID(qryAttachments,'MON$ATTACHMENT_ID');

  if IsMyAttachment(ID) then
  begin
    grid.Canvas.Brush.Color := $00D8FFD8;
    grid.Canvas.Font.Style := [fsBold];
  end
  else
  begin
    grid.Canvas.Brush.Color := clWhite;
    grid.Canvas.Font.Style := [];
  end;

  grid.Canvas.FillRect(Rect);

  txt := Column.Field.DisplayText;

  grid.Canvas.TextRect(
    Rect,
    Rect.Left+4,
    Rect.Top+2,
    txt);

end;

procedure TfrmActivityMonitor.Splitter2CanOffset(Sender: TObject;
  var NewOffset: Integer; var Accept: Boolean);
begin

end;

{ ============================================= }
{ Lifecycle }
{ ============================================= }

procedure TfrmActivityMonitor.FormCreate(Sender: TObject);
begin
  grdAttachments.OnDrawColumnCell :=
    @grdAttachmentsDrawColumnCell;
end;

procedure TfrmActivityMonitor.FormDestroy(Sender: TObject);
begin

  {qryStatements.Close;
  qryTransactions.Close;
  qryAttachments.Close;

  if trRead.Active then trRead.Commit;
  if trExec.Active then trExec.Commit;

  IBDatabase.Connected := False;
  }
end;

procedure TfrmActivityMonitor.FormClose(
  Sender: TObject;
  var CloseAction: TCloseAction);
begin
  try
    DisconnectDatabase;
  finally
    if Assigned(FNodeInfos) then
      FNodeInfos^.ViewForm := nil;
  end;
end;

end.


{unit fActivityMonitor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBGrids,
  ExtCtrls, StdCtrls, DB,
  IBDatabase, IBQuery,
  turbocommon;

type

  { TfrmActivityMonitor }

  TfrmActivityMonitor = class(TForm)
    btnRefresh: TButton;
    btnKillAttachment: TButton;
    btnKillTransaction: TButton;
    btnKillStatement: TButton;

    pnlTop: TPanel;
    pnlMain: TPanel;
    pnlBottom: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;

    grdAttachments: TDBGrid;
    grdTransactions: TDBGrid;
    grdStatements: TDBGrid;

    IBDatabase: TIBDatabase;
    trRead: TIBTransaction;
    trExec: TIBTransaction;

    qryAttachments: TIBQuery;
    qryTransactions: TIBQuery;
    qryStatements: TIBQuery;

    dsAttachments: TDataSource;
    dsTransactions: TDataSource;
    dsStatements: TDataSource;

    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure btnRefreshClick(Sender: TObject);
    procedure btnKillAttachmentClick(Sender: TObject);
    procedure btnKillTransactionClick(Sender: TObject);
    procedure btnKillStatementClick(Sender: TObject);

    procedure grdAttachmentsCellClick(Column: TColumn);
    procedure grdTransactionsCellClick(Column: TColumn);

  private
    FNodeInfos: TPNodeInfos;
    FDBIndex: integer;
    FMyAttachmentID: Integer;
    procedure LoadAttachments;
    procedure LoadTransactions;
    procedure LoadStatements;
    function SelectedIntField(ADataSet: TDataSet; const AField: string): Integer;
    procedure ExecAdminSQL(const ASQL: string; const AParam: Integer);

  public
    procedure Init(ANodeInfos: TPNodeInfos; dbIndex: integer);
  end;

implementation

{$R *.lfm}

{ TfrmActivityMonitor }

procedure TfrmActivityMonitor.Init(ANodeInfos: TPNodeInfos; dbIndex: integer);
begin
  FNodeInfos := ANodeInfos;
  FDBIndex   := dbIndex;

  if not IBDatabase.Connected then
  begin
    IBDatabase.DatabaseName := RegisteredDatabases[dbIndex].IBDatabase.DatabaseName;
    IBDatabase.LoginPrompt := False;
    IBDatabase.Params.Clear;
    IBDatabase.Params.Add('user_name=' + RegisteredDatabases[dbIndex].RegRec.UserName);
    IBDatabase.Params.Add('password=' + RegisteredDatabases[dbIndex].RegRec.Password);
    IBDatabase.Connected := True;
  end;

  // Transactions
  if not trRead.InTransaction then
  begin
    trRead.DefaultDatabase := IBDatabase;
    trRead.Params.Clear;
    trRead.Params.Add('read_committed');
    trRead.Params.Add('rec_version');
    trRead.Params.Add('nowait');
    trRead.StartTransaction;
  end;

  trExec.DefaultDatabase := IBDatabase;
  if not trExec.InTransaction then
    trExec.StartTransaction;

  // Queries zuweisen
  qryAttachments.Database := IBDatabase;
  qryAttachments.Transaction := trRead;

  qryTransactions.Database := IBDatabase;
  qryTransactions.Transaction := trRead;

  qryStatements.Database := IBDatabase;
  qryStatements.Transaction := trRead;

  // DataSources zuweisen
  dsAttachments.DataSet := qryAttachments;
  dsTransactions.DataSet := qryTransactions;
  dsStatements.DataSet := qryStatements;

  // Eigene Attachment-ID (FB >= 2.1)
  try
    qryAttachments.SQL.Text := 'SELECT CURRENT_CONNECTION FROM RDB$DATABASE';
    qryAttachments.Open;
    FMyAttachmentID := qryAttachments.Fields[0].AsInteger;
    qryAttachments.Close;
  except
    FMyAttachmentID := 0; // FB 2.0
  end;

  LoadAttachments;

end;

procedure TfrmActivityMonitor.FormCreate(Sender: TObject);
begin
  Caption := 'Activity Monitor';
end;

procedure TfrmActivityMonitor.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if Assigned(FNodeInfos) then
    FNodeInfos^.ViewForm := nil;
end;

procedure TfrmActivityMonitor.FormDestroy(Sender: TObject);
begin
  qryStatements.Close;
  qryTransactions.Close;
  qryAttachments.Close;
  if trRead.Active then trRead.Commit;
  if trExec.Active then trExec.Commit;
  IBDatabase.Connected := False;

  Parent.Free;
end;

procedure TfrmActivityMonitor.btnRefreshClick(Sender: TObject);
begin
  qryAttachments.SQL.Text := 'SELECT CURRENT_CONNECTION FROM RDB$DATABASE';
  qryAttachments.Open;
  FMyAttachmentID := qryAttachments.Fields[0].AsInteger;
  qryAttachments.Close;

  LoadAttachments;
end;

procedure TfrmActivityMonitor.LoadAttachments;
begin
  qryAttachments.Close;
  trRead.Commit;
  qryAttachments.SQL.Text :=
    'SELECT  MON$ATTACHMENT_ID, MON$USER, MON$REMOTE_ADDRESS, MON$REMOTE_PROCESS, ' +
    'MON$TIMESTAMP ' +
    'FROM MON$ATTACHMENTS ' +
    'WHERE (:MY_ID = 0 OR MON$ATTACHMENT_ID <> :MY_ID) ' +
    'ORDER BY MON$ATTACHMENT_ID';
  qryAttachments.ParamByName('MY_ID').AsInteger := FMyAttachmentID;
  qryAttachments.Open;

  LoadTransactions;
  LoadStatements;
end;

procedure TfrmActivityMonitor.LoadTransactions;
begin
  qryTransactions.Close;
  if qryAttachments.IsEmpty then Exit;

  qryTransactions.SQL.Text :=
    'SELECT * FROM MON$TRANSACTIONS ' +
    'WHERE MON$ATTACHMENT_ID = :ATT_ID ' +
    'ORDER BY MON$TRANSACTION_ID';
  qryTransactions.ParamByName('ATT_ID').AsInteger :=
    SelectedIntField(qryAttachments,'MON$ATTACHMENT_ID');
  qryTransactions.Open;
end;

procedure TfrmActivityMonitor.LoadStatements;
begin
  qryStatements.Close;
  if qryAttachments.IsEmpty then Exit;

  qryStatements.SQL.Text :=
    'SELECT * FROM MON$STATEMENTS ' +
    'WHERE MON$ATTACHMENT_ID = :ATT_ID ' +
    'ORDER BY MON$STATEMENT_ID';
  qryStatements.ParamByName('ATT_ID').AsInteger :=
    SelectedIntField(qryAttachments,'MON$ATTACHMENT_ID');
  qryStatements.Open;
end;

procedure TfrmActivityMonitor.grdAttachmentsCellClick(Column: TColumn);
begin
  LoadTransactions;
  LoadStatements;
end;

procedure TfrmActivityMonitor.grdTransactionsCellClick(Column: TColumn);
begin
  // optional Statements nach Transaction filtern
end;

function TfrmActivityMonitor.SelectedIntField(ADataSet: TDataSet;
  const AField: string): Integer;
begin
  Result := ADataSet.FieldByName(AField).AsInteger;
end;

procedure TfrmActivityMonitor.ExecAdminSQL(const ASQL: string;
  const AParam: Integer);
begin
  with TIBQuery.Create(nil) do
  try
    Database := IBDatabase;
    Transaction := trExec;
    SQL.Text := ASQL;
    ParamByName('ID').AsInteger := AParam;
    ExecSQL;
    trExec.Commit;
    trExec.StartTransaction;
  finally
    Free;
  end;
end;

procedure TfrmActivityMonitor.btnKillStatementClick(Sender: TObject);
begin
  if qryStatements.IsEmpty then Exit;
  ExecAdminSQL('DELETE FROM MON$STATEMENTS WHERE MON$STATEMENT_ID = :ID',
    SelectedIntField(qryStatements,'MON$STATEMENT_ID'));
  LoadStatements;
end;

procedure TfrmActivityMonitor.btnKillTransactionClick(Sender: TObject);
begin
  if qryTransactions.IsEmpty then Exit;
  ExecAdminSQL('DELETE FROM MON$TRANSACTIONS WHERE MON$TRANSACTION_ID = :ID',
    SelectedIntField(qryTransactions,'MON$TRANSACTION_ID'));
  LoadTransactions;
  LoadStatements;
end;

procedure TfrmActivityMonitor.btnKillAttachmentClick(Sender: TObject);
begin
  if qryAttachments.IsEmpty then Exit;
  ExecAdminSQL('DELETE FROM MON$ATTACHMENTS WHERE MON$ATTACHMENT_ID = :ID',
    SelectedIntField(qryAttachments,'MON$ATTACHMENT_ID'));
  LoadAttachments;
end;

end. }

