unit uCrossRDBMSEngineBatched;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB,
  uCrossRDBMSTypes,
  uCrossRDBMSTypeMapping,
  uCrossRDBMSEngineBase;

type
  { ------------------------------------------------------------------ }
  { Strategie B – Batched Parameter Inserts                            }
  { Mehrere Zeilen werden gesammelt und als Multi-Value-INSERT         }
  { oder in einer Transaktion geschrieben.                             }
  { ------------------------------------------------------------------ }
  TCrossEngineBatched = class(TCrossEngineBase)
  private
    FBatchSize: Integer;
  protected
    procedure InternalExecute; override;
  end;

implementation

uses
  Contnrs; // für TObjectList (falls benötigt)

procedure TCrossEngineBatched.InternalExecute;
var
  SelectSQL: string;
  FromRow, ToRow, RowCounter: Int64;
  CancelFlag: Boolean;
  Elapsed: string;
  LastProgressTime: TDateTime;
  RefreshMs: Integer;
  CommitInterval: Integer;
  RowsSinceCommit: Integer;
  CurrentBatchCount: Integer;
  i, ParamIndex: Integer;
  Field: TField;
  InsertSQL: string;
  DestFields, ValuePlaceholders: string;
  FirstField: Boolean;
begin
  // ---------------------------------------------------------------
  // Batch-Größe festlegen
  // ---------------------------------------------------------------
  FBatchSize := FOptions.BatchSize;
  if FBatchSize <= 0 then FBatchSize := 500;          // sinnvoller Default
  if FBatchSize > 5000 then FBatchSize := 5000;       // Sicherheitslimit

  CommitInterval := FOptions.CommitInterval;
  if CommitInterval <= 0 then CommitInterval := 10000;

  RefreshMs := FOptions.RefreshIntervalMs;
  if RefreshMs < 100 then RefreshMs := 100;

  // ---------------------------------------------------------------
  // Anzahl Zeilen
  // ---------------------------------------------------------------
  FStats.RowsTotal := CountSourceRows;

  FromRow := FOptions.FromRow;
  if FromRow < 1 then FromRow := 1;
  ToRow := FOptions.ToRow;

  if (ToRow > 0) and (ToRow < FStats.RowsTotal) then
    FStats.RowsTotal := ToRow - FromRow + 1
  else if FromRow > 1 then
    FStats.RowsTotal := FStats.RowsTotal - FromRow + 1;
  if FStats.RowsTotal < 0 then FStats.RowsTotal := 0;

  // ---------------------------------------------------------------
  // SELECT aufbauen
  // ---------------------------------------------------------------
  SelectSQL := 'SELECT ' + BuildSelectList +
               ' FROM ' + QuoteIdentifier(FOptions.SourceTable, FStats.SourceDBMS);

  EnsureSourceTrans;
  FOptions.SourceQuery.Close;
  FOptions.SourceQuery.SQL.Text := SelectSQL;
  FOptions.SourceQuery.Open;

  // ---------------------------------------------------------------
  // INSERT-Statement vorbereiten (parametrisiert, eine Zeile)
  // Wir bleiben bei klassischem parametrisiertem INSERT und
  // machen das Batching über mehrere ExecSQL + Commit.
  // Das ist mit SQLDB am robustesten.
  // ---------------------------------------------------------------
  DestFields := '';
  ValuePlaceholders := '';
  FirstField := True;
  ParamIndex := 0;

  for i := 0 to High(FOptions.Fields) do
  begin
    if not FOptions.Fields[i].Selected then Continue;

    if not FirstField then
    begin
      DestFields := DestFields + ', ';
      ValuePlaceholders := ValuePlaceholders + ', ';
    end;
    FirstField := False;

    DestFields := DestFields + QuoteIdentifier(FOptions.Fields[i].DestName, FStats.DestDBMS);
    ValuePlaceholders := ValuePlaceholders + ':P' + IntToStr(ParamIndex);
    Inc(ParamIndex);
  end;

  if DestFields = '' then
    raise Exception.Create('No fields selected for transfer.');

  InsertSQL := 'INSERT INTO ' + QuoteIdentifier(FOptions.DestTable, FStats.DestDBMS) +
               ' (' + DestFields + ') VALUES (' + ValuePlaceholders + ')';

  EnsureDestTrans;
  FOptions.DestQuery.Close;
  FOptions.DestQuery.SQL.Text := InsertSQL;
  FOptions.DestQuery.Prepare;
  FStats.InsertSQL := InsertSQL;

  // ---------------------------------------------------------------
  // Hauptschleife
  // ---------------------------------------------------------------
  RowsSinceCommit := 0;
  CurrentBatchCount := 0;
  RowCounter := 0;
  LastProgressTime := Now;
  CancelFlag := False;

  // Vorspulen
  FOptions.SourceQuery.First;
  while (not FOptions.SourceQuery.EOF) and (RowCounter < FromRow - 1) do
  begin
    Inc(RowCounter);
    FOptions.SourceQuery.Next;
  end;

  while not FOptions.SourceQuery.EOF do
  begin
    if FCancelled then Break;

    // Parameter binden
    ParamIndex := 0;
    for i := 0 to High(FOptions.Fields) do
    begin
      if not FOptions.Fields[i].Selected then Continue;

      Field := FOptions.SourceQuery.Fields[ParamIndex];

      if Field.IsNull then
        FOptions.DestQuery.Params[ParamIndex].Clear
      else
        FOptions.DestQuery.Params[ParamIndex].Assign(Field);

      if not Field.IsNull then
      begin
        if Field is TStringField then
          Inc(FStats.BytesProcessed, Length(Field.AsString))
        else
          Inc(FStats.BytesProcessed, Field.DataSize);
      end;

      Inc(ParamIndex);
    end;

    FOptions.DestQuery.ExecSQL;

    Inc(FStats.RowsProcessed);
    Inc(RowsSinceCommit);
    Inc(CurrentBatchCount);
    Inc(RowCounter);

    // Bereichsende?
    if (ToRow > 0) and (FStats.RowsProcessed >= (ToRow - FromRow + 1)) then
      Break;

    // Commit-Intervall
    if RowsSinceCommit >= CommitInterval then
    begin
      CommitDest;
      try
        FOptions.DestQuery.SQL.Text := InsertSQL;
        FOptions.DestQuery.Prepare;
      except
      end;
      RowsSinceCommit := 0;
      CurrentBatchCount := 0;
    end;

    // Progress
    if (Round((Now - LastProgressTime) * 86400 * 1000) >= RefreshMs) or
       FOptions.SourceQuery.EOF then
    begin
      LastProgressTime := Now;
      Elapsed := FormatDateTime('hh:nn:ss', Now - FStats.StartTime);
      CancelFlag := FCancelled;
      DoProgress(FStats.RowsProcessed, FStats.RowsTotal, Elapsed, CancelFlag);
      if CancelFlag then FCancelled := True;
    end;

    if FCancelled then Break;

    FOptions.SourceQuery.Next;
  end;

  // Abschluss
  if not FCancelled then
    CommitDest
  else
  begin
    RollbackDest;
    FStats.Cancelled := True;
  end;

  Elapsed := FormatDateTime('hh:nn:ss', Now - FStats.StartTime);
  CancelFlag := FCancelled;
  DoProgress(FStats.RowsProcessed, FStats.RowsTotal, Elapsed, CancelFlag);

  try
    if FOptions.SourceQuery.Active then FOptions.SourceQuery.Close;
  except
  end;
  try
    if FOptions.DestQuery.Active then FOptions.DestQuery.Close;
  except
  end;
end;

end.
