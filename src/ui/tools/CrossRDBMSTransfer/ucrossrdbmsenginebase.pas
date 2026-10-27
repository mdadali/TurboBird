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
    FCancelled : Boolean;
    FOptions   : TCrossTransferOptions;
    FStats     : TCrossTransferStats;

    procedure DoLog(const AMsg: string);
    procedure DoProgress(ARowsDone, ARowsTotal: Int64; const AElapsed: string; var ACancel: Boolean);
    procedure AddWarning(const AMsg: string);

    procedure EnsureSourceTrans;
    procedure EnsureDestTrans;
    procedure CommitDest;
    procedure RollbackDest;

    function  CountSourceRows: Int64;
    function  BuildSelectList: string;           // Spalten + Formeln
    function  BuildDestFieldList: string;        // Ziel-Spaltennamen
    function  ExpandFormula(const AFormula, AOwnFieldName: string): string;

    // Von den konkreten Engines zu überschreiben
    procedure InternalExecute; virtual; abstract;

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

  // Basis-Infos füllen
  if Assigned(FOptions.SourceConn) then
  begin
    FStats.SourceDBMS          := DBMSFromConnection(FOptions.SourceConn);
    FStats.SourceConnectorType := FOptions.SourceConn.ClassName;
    FStats.SourceDatabase      := FOptions.SourceConn.DatabaseName;
  end;
  FStats.SourceTable := FOptions.SourceTable;

  if Assigned(FOptions.DestConn) then
  begin
    FStats.DestDBMS          := DBMSFromConnection(FOptions.DestConn);
    FStats.DestConnectorType := FOptions.DestConn.ClassName;
    FStats.DestDatabase      := FOptions.DestConn.DatabaseName;
  end;
  FStats.DestTable := FOptions.DestTable;

  FStats.StrategyUsed := FOptions.Strategy;
end;

destructor TCrossEngineBase.Destroy;
begin
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

procedure TCrossEngineBase.CommitDest;
begin
  if not Assigned(FOptions.DestTrans) then Exit;

  try
    if Assigned(FOptions.DestQuery) and FOptions.DestQuery.Active then
      FOptions.DestQuery.Close;
  except
  end;

  try
    if FOptions.DestTrans.Active then
      FOptions.DestTrans.Commit;
  except
  end;

  EnsureDestTrans;
  Inc(FStats.CommitsDone);
end;

procedure TCrossEngineBase.RollbackDest;
begin
  if not Assigned(FOptions.DestTrans) then Exit;

  try
    if Assigned(FOptions.DestQuery) and FOptions.DestQuery.Active then
      FOptions.DestQuery.Close;
  except
  end;

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
    if not Assigned(FOptions.SourceConn) then
      raise Exception.Create('Source connection not assigned.');
    if not Assigned(FOptions.DestConn) then
      raise Exception.Create('Destination connection not assigned.');
    if not Assigned(FOptions.SourceQuery) then
      raise Exception.Create('Source query not assigned.');
    if not Assigned(FOptions.DestQuery) then
      raise Exception.Create('Destination query not assigned.');

    if not FOptions.SourceConn.Connected then
      FOptions.SourceConn.Open;
    if not FOptions.DestConn.Connected then
      FOptions.DestConn.Open;

    EnsureSourceTrans;
    EnsureDestTrans;

    // Die eigentliche Arbeit machen die abgeleiteten Klassen
    InternalExecute;

  except
    on E: Exception do
    begin
      DoLog('Error: ' + E.Message);
      raise;
    end;
  end;

  // Endstatistik
  FStats.ElapsedSeconds := (Now - FStats.StartTime) * 86400.0;
  if FStats.ElapsedSeconds > 0.001 then
  begin
    FStats.RowsPerSec := FStats.RowsProcessed / FStats.ElapsedSeconds;
    FStats.MBPerSec   := (FStats.BytesProcessed / 1024 / 1024) / FStats.ElapsedSeconds;
  end;
end;

end.
