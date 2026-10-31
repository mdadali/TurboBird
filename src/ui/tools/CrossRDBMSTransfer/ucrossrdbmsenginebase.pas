unit uCrossRDBMSEngineBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB,
  uCrossRDBMSTypes,
  uCrossRDBMSTypeMapping;

type
  { ------------------------------------------------------------------ }
  { Basis-Klasse für alle Transfer-Engines                             }
  { ------------------------------------------------------------------ }
  TCrossEngineBase = class
  private
    FOnProgress: TCrossProgressEvent;
    FOnLog     : TCrossLogEvent;
  protected
    FOptions   : TCrossTransferOptions;
    FStats     : TCrossTransferStats;
    FCancelled : Boolean;

    procedure DoLog(const AMsg: string);
    procedure DoProgress(ARowsDone, ARowsTotal: Int64; const AElapsed: string; var ACancel: Boolean);
    procedure AddWarning(const AMsg: string);

    procedure EnsureSourceTrans;  virtual;
    procedure EnsureDestTrans;    virtual;
    procedure CommitDest;         virtual;
    procedure RollbackDest;       virtual;
    procedure CleanupQueries;     virtual;

    function  CountSourceRows: Int64;  virtual;
    function  BuildSelectList: string; virtual;
    function  BuildDestFieldList: string; virtual;
    function  ExpandFormula(const AFormula, AOwnFieldName: string): string; virtual;

    // Von den konkreten Engines zu überschreiben
    procedure InternalExecute; virtual; abstract;

    function  IsComputedStaysComputed(AFieldIndex: Integer): Boolean;

  public
    constructor Create(const AOptions: TCrossTransferOptions);
    destructor Destroy; override;

    procedure Execute;
    procedure Cancel;

    property Stats: TCrossTransferStats read FStats;
    property OnProgress: TCrossProgressEvent read FOnProgress write FOnProgress;
    property OnLog: TCrossLogEvent read FOnLog write FOnLog;
    property Cancelled: Boolean read FCancelled;
  end;

implementation

constructor TCrossEngineBase.Create(const AOptions: TCrossTransferOptions);
begin
  inherited Create;
  FOptions := AOptions;
  FCancelled := False;

  FStats := Default(TCrossTransferStats);
  FStats.Warnings := TStringList.Create;

  { ---- Source-Info ---- }
  if FOptions.SourceIsIBX then
  begin
    FStats.SourceDBMS := cdFirebird;
    FStats.SourceConnectorType := 'IBX';
    if Assigned(FOptions.SourceIBXConn) then
      FStats.SourceDatabase := FOptions.SourceIBXConn.DatabaseName;
  end
  else if Assigned(FOptions.SourceConn) then
  begin
    if FOptions.SourceBackend <> cdUnknown then
      FStats.SourceDBMS := FOptions.SourceBackend
    else
      FStats.SourceDBMS := DBMSFromConnection(FOptions.SourceConn);
    FStats.SourceConnectorType := FOptions.SourceConn.ClassName;
    FStats.SourceDatabase := FOptions.SourceConn.DatabaseName;
  end;
  FStats.SourceTable := FOptions.SourceTable;

  { ---- Dest-Info ---- }
  if FOptions.DestIsIBX then
  begin
    FStats.DestDBMS := cdFirebird;
    FStats.DestConnectorType := 'IBX';
    if Assigned(FOptions.DestIBXConn) then
      FStats.DestDatabase := FOptions.DestIBXConn.DatabaseName;
  end
  else if Assigned(FOptions.DestConn) then
  begin
    if FOptions.DestBackend <> cdUnknown then
      FStats.DestDBMS := FOptions.DestBackend
    else
      FStats.DestDBMS := DBMSFromConnection(FOptions.DestConn);
    FStats.DestConnectorType := FOptions.DestConn.ClassName;
    FStats.DestDatabase := FOptions.DestConn.DatabaseName;
  end;
  FStats.DestTable := FOptions.DestTable;

  FStats.StrategyUsed := FOptions.Strategy;
end;

destructor TCrossEngineBase.Destroy;
begin
  CleanupQueries;

  if Assigned(FStats.Warnings) then
    FStats.Warnings.Free;

  inherited Destroy;
end;

procedure TCrossEngineBase.DoLog(const AMsg: string);
begin
  if Assigned(FOnLog) then
    FOnLog(AMsg);
end;

procedure TCrossEngineBase.DoProgress(ARowsDone, ARowsTotal: Int64;
  const AElapsed: string; var ACancel: Boolean);
begin
  if Assigned(FOnProgress) then
    FOnProgress(ARowsDone, ARowsTotal, AElapsed, ACancel);
  if ACancel then
    FCancelled := True;
end;

procedure TCrossEngineBase.AddWarning(const AMsg: string);
begin
  FStats.Warnings.Add(AMsg);
end;

procedure TCrossEngineBase.Cancel;
begin
  FCancelled := True;
end;

procedure TCrossEngineBase.EnsureSourceTrans;
begin
  if not Assigned(FOptions.SourceTrans) then Exit;
  if not FOptions.SourceTrans.Active then
    FOptions.SourceTrans.StartTransaction;
end;

procedure TCrossEngineBase.EnsureDestTrans;
begin
  if not Assigned(FOptions.DestTrans) then Exit;
  if not FOptions.DestTrans.Active then
    FOptions.DestTrans.StartTransaction;
end;

procedure TCrossEngineBase.CleanupQueries;
begin
  try
    if Assigned(FOptions.SourceQuery) and FOptions.SourceQuery.Active then
      FOptions.SourceQuery.Close;
  except
  end;

  try
    if Assigned(FOptions.DestQuery) then
    begin
      if FOptions.DestQuery.Active then
        FOptions.DestQuery.Close;
      try
        FOptions.DestQuery.UnPrepare;
      except
      end;
    end;
  except
  end;
end;

procedure TCrossEngineBase.CommitDest;
begin
  if not Assigned(FOptions.DestTrans) then Exit;

  { Query vor Commit schließen }
  try
    if Assigned(FOptions.DestQuery) and FOptions.DestQuery.Active then
      FOptions.DestQuery.Close;
  except
  end;

  try
    if FOptions.DestTrans.Active then
      FOptions.DestTrans.Commit;
  except
    on E: Exception do
    begin
      DoLog('WARNING: Commit failed: ' + E.Message);
      AddWarning('Commit failed: ' + E.Message);
    end;
  end;

  EnsureDestTrans;
  Inc(FStats.CommitsDone);
end;

procedure TCrossEngineBase.RollbackDest;
begin
  CleanupQueries;

  if not Assigned(FOptions.DestTrans) then Exit;

  try
    if FOptions.DestTrans.Active then
      FOptions.DestTrans.Rollback;
  except
  end;

  EnsureDestTrans;
end;

function TCrossEngineBase.CountSourceRows: Int64;
begin
  Result := 0;
  if not Assigned(FOptions.SourceQuery) then Exit;

  EnsureSourceTrans;
  FOptions.SourceQuery.Close;
  FOptions.SourceQuery.SQL.Text :=
    'SELECT COUNT(*) FROM ' + QuoteIdentifier(FOptions.SourceTable, FStats.SourceDBMS);
  try
    FOptions.SourceQuery.Open;
    if not FOptions.SourceQuery.EOF then
      Result := FOptions.SourceQuery.Fields[0].AsLargeInt;
  finally
    FOptions.SourceQuery.Close;
  end;
end;

function TCrossEngineBase.ExpandFormula(const AFormula, AOwnFieldName: string): string;
begin
  Result := Trim(AFormula);
  if Result = '' then
    Result := AOwnFieldName
  else
    Result := StringReplace(Result, '$1', AOwnFieldName, [rfReplaceAll]);
end;

function TCrossEngineBase.IsComputedStaysComputed(AFieldIndex: Integer): Boolean;
begin
  { Computed-Feld bleibt nur bei Firebird→Firebird als COMPUTED erhalten
    und wird dann NICHT in SELECT/INSERT aufgenommen. }
  Result := FOptions.Fields[AFieldIndex].IsComputed and
            (FStats.SourceDBMS = cdFirebird) and
            (FStats.DestDBMS = cdFirebird);
end;

function TCrossEngineBase.BuildSelectList: string;
var
  i: Integer;
  Expr, QuotedName: string;
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    for i := 0 to High(FOptions.Fields) do
    begin
      if not FOptions.Fields[i].Selected then Continue;
      if IsComputedStaysComputed(i) then Continue;

      QuotedName := QuoteIdentifier(FOptions.Fields[i].SourceName, FStats.SourceDBMS);

      if FOptions.UseFormulas and (Trim(FOptions.Fields[i].Formula) <> '') then
        Expr := ExpandFormula(FOptions.Fields[i].Formula, QuotedName)
      else
        Expr := QuotedName;

      SL.Add(Expr);
    end;
    Result := String.Join(', ', SL.ToStringArray);
  finally
    SL.Free;
  end;
end;

function TCrossEngineBase.BuildDestFieldList: string;
var
  i: Integer;
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    for i := 0 to High(FOptions.Fields) do
    begin
      if not FOptions.Fields[i].Selected then Continue;
      if IsComputedStaysComputed(i) then Continue;
      SL.Add(QuoteIdentifier(FOptions.Fields[i].DestName, FStats.DestDBMS));
    end;
    Result := String.Join(', ', SL.ToStringArray);
  finally
    SL.Free;
  end;
end;

procedure TCrossEngineBase.Execute;
begin
  FCancelled := False;
  FStats.RowsProcessed := 0;
  FStats.CommitsDone := 0;
  FStats.BytesProcessed := 0;
  FStats.StartTime := Now;

  try
    { ---- Quelle prüfen und öffnen ---- }
    if FOptions.SourceIsIBX then
    begin
      if not Assigned(FOptions.SourceIBXConn) then
        raise Exception.Create('Source IBX connection not assigned.');
      if not FOptions.SourceIBXConn.Connected then
        FOptions.SourceIBXConn.Open;
      if not Assigned(FOptions.SourceIBXQuery) then
        raise Exception.Create('Source IBX query not assigned.');
    end
    else
    begin
      if not Assigned(FOptions.SourceConn) then
        raise Exception.Create('Source connection not assigned.');
      if not FOptions.SourceConn.Connected then
        FOptions.SourceConn.Open;
      if not Assigned(FOptions.SourceQuery) then
        raise Exception.Create('Source query not assigned.');
    end;

    { ---- Ziel prüfen und öffnen ---- }
    if FOptions.DestIsIBX then
    begin
      if not Assigned(FOptions.DestIBXConn) then
        raise Exception.Create('Destination IBX connection not assigned.');
      if not FOptions.DestIBXConn.Connected then
        FOptions.DestIBXConn.Open;
      if not Assigned(FOptions.DestIBXQuery) then
        raise Exception.Create('Destination IBX query not assigned.');
    end
    else
    begin
      if not Assigned(FOptions.DestConn) then
        raise Exception.Create('Destination connection not assigned.');
      if not FOptions.DestConn.Connected then
        FOptions.DestConn.Open;
      if not Assigned(FOptions.DestQuery) then
        raise Exception.Create('Destination query not assigned.');
    end;

    EnsureSourceTrans;
    EnsureDestTrans;

    try
      InternalExecute;
    finally
      CleanupQueries;
    end;

  except
    on E: Exception do
    begin
      DoLog('Error: ' + E.Message);
      CleanupQueries;
      raise;
    end;
  end;

  { Endstatistik }
  FStats.ElapsedSeconds := (Now - FStats.StartTime) * 86400.0;
  if FStats.ElapsedSeconds > 0.001 then
  begin
    FStats.RowsPerSec := FStats.RowsProcessed / FStats.ElapsedSeconds;
    FStats.MBPerSec   := (FStats.BytesProcessed / 1024 / 1024) / FStats.ElapsedSeconds;
  end;
end;

end.
