unit uCrossRDBMSEngineInsertSelect;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB,
  uCrossRDBMSTypes,
  uCrossRDBMSTypeMapping,
  uCrossRDBMSEngineBase;

type
  { ------------------------------------------------------------------ }
  { Strategie C – batchweises INSERT … SELECT                          }
  { Wird nur verwendet, wenn Quelle und Ziel dieselbe Connection       }
  { nutzen. Arbeitet mit FIRST/SKIP bzw. LIMIT/OFFSET + Commit.        }
  { ------------------------------------------------------------------ }
  TCrossEngineInsertSelect = class(TCrossEngineBase)
  private
    function BuildBatchSQL(ASkip, ACount: Int64): string;
  protected
    procedure InternalExecute; override;
  end;

implementation

function TCrossEngineInsertSelect.BuildBatchSQL(ASkip, ACount: Int64): string;
var
  SelectPart, InsertPart: string;
begin
  // SELECT-Teil mit Formeln
  SelectPart := 'SELECT ' + BuildSelectList +
                ' FROM ' + QuoteIdentifier(FOptions.SourceTable, FStats.SourceDBMS);

  // RDBMS-spezifische Begrenzung
  case FStats.SourceDBMS of
    cdFirebird:
      SelectPart := 'SELECT FIRST ' + IntToStr(ACount) +
                    ' SKIP ' + IntToStr(ASkip) + ' ' +
                    BuildSelectList +
                    ' FROM ' + QuoteIdentifier(FOptions.SourceTable, FStats.SourceDBMS);

    cdPostgreSQL, cdMySQL, cdSQLite:
      SelectPart := SelectPart +
                    ' LIMIT ' + IntToStr(ACount) +
                    ' OFFSET ' + IntToStr(ASkip);

    cdMSSQL:
      // Ab SQL Server 2012
      SelectPart := SelectPart +
                    ' ORDER BY (SELECT NULL)' +
                    ' OFFSET ' + IntToStr(ASkip) + ' ROWS' +
                    ' FETCH NEXT ' + IntToStr(ACount) + ' ROWS ONLY';

    cdOracle:
      // Einfache Variante mit ROWNUM (älteres Oracle)
      SelectPart := 'SELECT * FROM (' +
                    'SELECT ' + BuildSelectList +
                    ', ROWNUM AS RN FROM ' + QuoteIdentifier(FOptions.SourceTable, FStats.SourceDBMS) +
                    ') WHERE RN > ' + IntToStr(ASkip) +
                    ' AND RN <= ' + IntToStr(ASkip + ACount);

  else
    // Fallback – sollte eigentlich nicht vorkommen, da Strategie C nur bei gleicher Connection
    SelectPart := SelectPart + ' /* batch limit not supported */';
  end;

  InsertPart := 'INSERT INTO ' + QuoteIdentifier(FOptions.DestTable, FStats.DestDBMS) +
                ' (' + BuildDestFieldList + ')';

  Result := InsertPart + sLineBreak + SelectPart;
end;

procedure TCrossEngineInsertSelect.InternalExecute;
var
  BatchSize: Integer;
  Skip: Int64;
  Remaining, CurrentBatch: Int64;
  CancelFlag: Boolean;
  Elapsed: string;
  LastProgressTime: TDateTime;
  RefreshMs: Integer;
  SQL: string;
begin
  // ---------------------------------------------------------------
  // Nur sinnvoll bei gleicher Connection
  // ---------------------------------------------------------------
  if FOptions.SourceConn <> FOptions.DestConn then
    raise Exception.Create('INSERT … SELECT strategy requires the same connection for source and destination.');

  // ---------------------------------------------------------------
  // Parameter
  // ---------------------------------------------------------------
  BatchSize := FOptions.BatchSize;
  if BatchSize <= 0 then BatchSize := 50000;
  if BatchSize > 500000 then BatchSize := 500000;

  RefreshMs := FOptions.RefreshIntervalMs;
  if RefreshMs < 100 then RefreshMs := 100;

  // ---------------------------------------------------------------
  // Gesamtzahl ermitteln
  // ---------------------------------------------------------------
  FStats.RowsTotal := CountSourceRows;

  if FOptions.FromRow > 1 then
  begin
    if FOptions.ToRow > 0 then
      FStats.RowsTotal := FOptions.ToRow - FOptions.FromRow + 1
    else
      FStats.RowsTotal := FStats.RowsTotal - FOptions.FromRow + 1;
  end
  else if FOptions.ToRow > 0 then
    FStats.RowsTotal := FOptions.ToRow;

  if FStats.RowsTotal < 0 then FStats.RowsTotal := 0;

  // ---------------------------------------------------------------
  // Batch-Schleife
  // ---------------------------------------------------------------
  Skip := FOptions.FromRow - 1;
  if Skip < 0 then Skip := 0;

  Remaining := FStats.RowsTotal;
  LastProgressTime := Now;
  CancelFlag := False;

  EnsureDestTrans;

  while (Remaining > 0) and (not FCancelled) do
  begin
    if Remaining > BatchSize then
      CurrentBatch := BatchSize
    else
      CurrentBatch := Remaining;

    SQL := BuildBatchSQL(Skip, CurrentBatch);
    FStats.InsertSQL := SQL; // letztes Statement merken

    FOptions.DestQuery.Close;
    FOptions.DestQuery.SQL.Text := SQL;

    try
      FOptions.DestQuery.ExecSQL;
    except
      on E: Exception do
      begin
        RollbackDest;
        raise Exception.Create('INSERT … SELECT batch failed: ' + E.Message);
      end;
    end;

    // Commit nach jedem Batch
    CommitDest;

    Inc(FStats.RowsProcessed, CurrentBatch);
    Dec(Remaining, CurrentBatch);
    Inc(Skip, CurrentBatch);

    // Progress
    if (Round((Now - LastProgressTime) * 86400 * 1000) >= RefreshMs) or (Remaining <= 0) then
    begin
      LastProgressTime := Now;
      Elapsed := FormatDateTime('hh:nn:ss', Now - FStats.StartTime);
      CancelFlag := FCancelled;
      DoProgress(FStats.RowsProcessed, FStats.RowsTotal, Elapsed, CancelFlag);
      if CancelFlag then FCancelled := True;
    end;
  end;

  if FCancelled then
  begin
    RollbackDest;
    FStats.Cancelled := True;
  end;

  // Finaler Progress
  Elapsed := FormatDateTime('hh:nn:ss', Now - FStats.StartTime);
  CancelFlag := FCancelled;
  DoProgress(FStats.RowsProcessed, FStats.RowsTotal, Elapsed, CancelFlag);
end;

end.
