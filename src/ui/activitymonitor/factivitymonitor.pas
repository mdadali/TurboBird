unit fActivityMonitor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBGrids,
  ExtCtrls, StdCtrls, DB,
  IBDatabase, IBQuery;

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

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure btnRefreshClick(Sender: TObject);
    procedure btnKillAttachmentClick(Sender: TObject);
    procedure btnKillTransactionClick(Sender: TObject);
    procedure btnKillStatementClick(Sender: TObject);

    procedure grdAttachmentsCellClick(Column: TColumn);
    procedure grdTransactionsCellClick(Column: TColumn);

  private
    FMyAttachmentID: Integer;
    procedure LoadAttachments;
    procedure LoadTransactions;
    procedure LoadStatements;
    function SelectedIntField(ADataSet: TDataSet; const AField: string): Integer;
    procedure ExecAdminSQL(const ASQL: string; const AParam: Integer);

  public

  end;

implementation

{$R *.lfm}

{ TfrmActivityMonitor }

procedure TfrmActivityMonitor.FormCreate(Sender: TObject);
begin
  Caption := 'Activity Monitor';

  // Beispiel DB-Verbindung konfigurieren (passen!)
  IBDatabase.DatabaseName := 'mxLinux/3030:employee';
  IBDatabase.LoginPrompt := False;
  IBDatabase.Params.Clear;
  IBDatabase.Params.Add('user_name=sysdba');
  IBDatabase.Params.Add('password=masterkey');
  IBDatabase.Connected := True;

  // Transactions
  trRead.DefaultDatabase := IBDatabase;
  trRead.Params.Clear;
  trRead.Params.Add('read_committed');
  trRead.Params.Add('rec_version');
  trRead.Params.Add('nowait');
  trRead.StartTransaction;

  trExec.DefaultDatabase := IBDatabase;
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

procedure TfrmActivityMonitor.FormDestroy(Sender: TObject);
begin
  qryStatements.Close;
  qryTransactions.Close;
  qryAttachments.Close;
  if trRead.Active then trRead.Commit;
  if trExec.Active then trExec.Commit;
  IBDatabase.Connected := False;
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
    'SELECT MON$REMOTE_ADDRESS, MON$REMOTE_PROCESS, MON$ATTACHMENT_ID, MON$USER, ' +
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

end.

