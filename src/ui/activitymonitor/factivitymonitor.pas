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

  {qryAttachments.SQL.Text :=
    'SELECT '+
    ' MON$TIMESTAMP, '+
    ' MON$USER, '+
    ' MON$REMOTE_PROCESS, '+
    ' MON$REMOTE_ADDRESS, '+
    ' MON$SERVER_PID, '+
    ' MON$REMOTE_PROTOCOL, '+
    ' MON$ATTACHMENT_ID, '+
    ' MON$STATE, '+
    ' MON$ATTACHMENT_NAME, '+
    ' MON$ROLE, '+
    ' MON$CHARACTER_SET_ID, '+
    ' MON$GARBAGE_COLLECTION '+
    //' MON$AUTH_METHOD '+
    'FROM MON$ATTACHMENTS '+
    'ORDER BY MON$ATTACHMENT_ID';  }

    qryAttachments.SQL.Text :=
      'SELECT '+
      ' a.MON$TIMESTAMP, '+
      ' a.MON$USER, '+
      ' a.MON$REMOTE_PROCESS, '+
      ' a.MON$REMOTE_ADDRESS, '+
      ' a.MON$SERVER_PID, '+
      ' a.MON$REMOTE_PROTOCOL, '+
      ' a.MON$ATTACHMENT_ID, '+
      ' a.MON$STATE, '+
      ' a.MON$ATTACHMENT_NAME, '+
      ' a.MON$ROLE, '+
      ' a.MON$CHARACTER_SET_ID, '+
      ' a.MON$GARBAGE_COLLECTION, '+
      ' v.MON$VARIABLE_VALUE AS MON$APPLICATION_NAME '+
      'FROM MON$ATTACHMENTS a '+
      'LEFT JOIN MON$CONTEXT_VARIABLES v '+
      '  ON v.MON$ATTACHMENT_ID = a.MON$ATTACHMENT_ID '+
      ' AND v.MON$VARIABLE_NAME = ''ApplicationName'' '+
      'ORDER BY a.MON$ATTACHMENT_ID';


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
