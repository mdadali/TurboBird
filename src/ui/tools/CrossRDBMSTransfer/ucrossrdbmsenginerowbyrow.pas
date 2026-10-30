unit uCrossRDBMSEngineRowByRow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB,
  uCrossRDBMSTypes,
  uCrossRDBMSTypeMapping,
  uCrossRDBMSEngineBase;

type
  { ------------------------------------------------------------------ }
  { Strategie A – Parametrisiertes Row-by-Row                          }
  { ------------------------------------------------------------------ }
  TCrossEngineRowByRow = class(TCrossEngineBase)
  private
    procedure PrepareInsertStatement;
    procedure BindParamsFromSource;
  protected
    procedure InternalExecute; override;
  end;

implementation

procedure TCrossEngineRowByRow.PrepareInsertStatement;
var
  i: Integer;
  DestFields, Params: string;
  First: Boolean;
begin
  DestFields := '';
  Params := '';
  First := True;

  for i := 0 to High(FOptions.Fields) do
  begin
    if not FOptions.Fields[i].Selected then Continue;

    if not First then
    begin
      DestFields := DestFields + ', ';
      Params := Params + ', ';
    end;
    First := False;

    DestFields := DestFields + QuoteIdentifier(FOptions.Fields[i].DestName, FStats.DestDBMS);
    Params := Params + ':P' + IntToStr(i);
  end;

  if DestFields = '' then
    raise Exception.Create('No fields selected for transfer.');

  FOptions.DestQuery.Close;
  FOptions.DestQuery.SQL.Text :=
    'INSERT INTO ' + QuoteIdentifier(FOptions.DestTable, FStats.DestDBMS) +
    ' (' + DestFields + ') VALUES (' + Params + ')';
  FOptions.DestQuery.Prepare;

  FStats.InsertSQL := FOptions.DestQuery.SQL.Text;
end;

procedure TCrossEngineRowByRow.BindParamsFromSource;
var
  i, ParamIndex: Integer;
  Field: TField;
begin
  ParamIndex := 0;
  for i := 0 to High(FOptions.Fields) do
  begin
    if not FOptions.Fields[i].Selected then Continue;

    Field := FOptions.SourceQuery.Fields[ParamIndex];

    if Field.IsNull then
      FOptions.DestQuery.Params[ParamIndex].Clear
    else
      FOptions.DestQuery.Params[ParamIndex].Assign(Field);

    { Bytes zählen — erweitert für TMemoField/TWideStringField.
      PostgreSQL/SQLite liefern TEXT-Felder als TMemoField; dort ist
      Field.DataSize = 0, daher müssen wir Length(AsString) verwenden. }
    if not Field.IsNull then
    begin
      case Field.DataType of
        ftString, ftFixedChar, ftWideString, ftMemo, ftWideMemo:
          Inc(FStats.BytesProcessed, Length(Field.AsString));
      else
        Inc(FStats.BytesProcessed, Field.DataSize);
      end;
    end;

    Inc(ParamIndex);
  end;
end;

procedure TCrossEngineRowByRow.InternalExecute;
var
  SelectSQL: string;
  RowsSinceCommit: Integer;
  CommitInterval: Integer;
  FromRow, ToRow, RowCounter: Int64;
  CancelFlag: Boolean;
  Elapsed: string;
  LastProgressTime: TDateTime;
  RefreshMs: Integer;
begin
  // ---------------------------------------------------------------
  // 1. Anzahl Zeilen ermitteln
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
  // 2. SELECT aufbauen (mit Formeln)
  // ---------------------------------------------------------------
  SelectSQL := 'SELECT ' + BuildSelectList +
               ' FROM ' + QuoteIdentifier(FOptions.SourceTable, FStats.SourceDBMS);

  EnsureSourceTrans;
  FOptions.SourceQuery.Close;
  FOptions.SourceQuery.SQL.Text := SelectSQL;
  FOptions.SourceQuery.Open;

  // ---------------------------------------------------------------
  // 3. INSERT vorbereiten
  // ---------------------------------------------------------------
  EnsureDestTrans;
  PrepareInsertStatement;

  // ---------------------------------------------------------------
  // 4. Parameter
  // ---------------------------------------------------------------
  CommitInterval := FOptions.CommitInterval;
  if CommitInterval <= 0 then CommitInterval := 10000;

  RefreshMs := FOptions.RefreshIntervalMs;
  if RefreshMs < 100 then RefreshMs := 100;

  RowsSinceCommit := 0;
  RowCounter := 0;
  LastProgressTime := Now;
  CancelFlag := False;

  // ---------------------------------------------------------------
  // 5. Zum Startpunkt vorspulen
  // ---------------------------------------------------------------
  FOptions.SourceQuery.First;
  while (not FOptions.SourceQuery.EOF) and (RowCounter < FromRow - 1) do
  begin
    Inc(RowCounter);
    FOptions.SourceQuery.Next;
  end;

  // ---------------------------------------------------------------
  // 6. Hauptschleife
  // ---------------------------------------------------------------
  while not FOptions.SourceQuery.EOF do
  begin
    if FCancelled then Break;

    BindParamsFromSource;
    FOptions.DestQuery.ExecSQL;

    Inc(FStats.RowsProcessed);
    Inc(RowsSinceCommit);
    Inc(RowCounter);

    // Bereichsende?
    if (ToRow > 0) and (FStats.RowsProcessed >= (ToRow - FromRow + 1)) then
      Break;

    // Commit-Intervall
    if RowsSinceCommit >= CommitInterval then
    begin
      CommitDest;
      // Statement nach Commit neu vorbereiten (manche Treiber brauchen das)
      try
        FOptions.DestQuery.SQL.Text := FStats.InsertSQL;
        FOptions.DestQuery.Prepare;
      except
      end;
      RowsSinceCommit := 0;
    end;

    // Progress (zeitgesteuert)
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

  // ---------------------------------------------------------------
  // 7. Abschluss
  // ---------------------------------------------------------------
  if not FCancelled then
    CommitDest
  else
  begin
    RollbackDest;
    FStats.Cancelled := True;
  end;

  // Finaler Progress
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
