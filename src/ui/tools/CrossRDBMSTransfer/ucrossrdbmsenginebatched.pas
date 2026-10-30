unit uCrossRDBMSEngineBatched;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, Dialogs, Controls,
  uCrossRDBMSTypes,
  uCrossRDBMSTypeMapping,
  uCrossRDBMSEngineBase;

type
  { ------------------------------------------------------------------ }
  { Strategie B – Batched Inserts                                      }
  { - PostgreSQL / MySQL / SQLite / MSSQL → Multi-Value INSERT         }
  { - Firebird → begrenzte Batch-Größe + Hinweis                       }
  { - Sonst Fallback auf parametrisiertes Einfügen                     }
  { ------------------------------------------------------------------ }
  TCrossEngineBatched = class(TCrossEngineBase)
  private
    FBatchSize: Integer;
    function  SupportsMultiValueInsert: Boolean;
    procedure ExecuteMultiValue;
    procedure ExecuteParametrized;
  protected
    procedure InternalExecute; override;
  end;

implementation

function TCrossEngineBatched.SupportsMultiValueInsert: Boolean;
begin
  // Firebird kann kein VALUES (...), (...), (...)
  // Oracle ist mit SQLDB unzuverlässig dafür
  case FStats.DestDBMS of
    cdPostgreSQL, cdMySQL, cdSQLite, cdMSSQL:
      Result := True;
  else
    Result := False;
  end;
end;

procedure TCrossEngineBatched.InternalExecute;
begin
  // BatchSize direkt aus den übergebenen Options (kommt aus der GUI)
  FBatchSize := FOptions.BatchSize;
  if FBatchSize <= 0 then
    FBatchSize := 500;

  // Firebird-Sonderbehandlung
  if FStats.DestDBMS = cdFirebird then
  begin
    if FBatchSize > 256 then
    begin
      if MessageDlg(
        'Firebird does not support multi-value INSERT.' + sLineBreak +
        'Maximum useful batch size for Firebird is 256.' + sLineBreak + sLineBreak +
        'Set Batch Size to 256 and continue?',
        mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      begin
        FBatchSize := 256;
      end
      else
      begin
        // User will nicht → wir begrenzen trotzdem intern auf 256
        FBatchSize := 256;
      end;
    end;
  end;

  if SupportsMultiValueInsert then
    ExecuteMultiValue
  else
    ExecuteParametrized;
end;

{ ------------------------------------------------------------------ }
{ Multi-Value INSERT (PostgreSQL, MySQL, SQLite, MSSQL)              }
{ ------------------------------------------------------------------ }
procedure TCrossEngineBatched.ExecuteMultiValue;
var
  SelectSQL: string;
  FromRow, ToRow, RowCounter: Int64;
  CancelFlag: Boolean;
  Elapsed: string;
  LastProgressTime: TDateTime;
  RefreshMs, CommitInterval: Integer;
  RowsSinceCommit: Integer;
  BatchRows: TStringList;
  ValueLine: string;
  i, ParamIndex: Integer;
  Field: TField;
  DestFields: string;
  FirstField: Boolean;
  SQL: string;
begin
  CommitInterval := FOptions.CommitInterval;
  if CommitInterval <= 0 then CommitInterval := 10000;

  RefreshMs := FOptions.RefreshIntervalMs;
  if RefreshMs < 100 then RefreshMs := 100;

  FStats.RowsTotal := CountSourceRows;

  FromRow := FOptions.FromRow;
  if FromRow < 1 then FromRow := 1;
  ToRow := FOptions.ToRow;

  if (ToRow > 0) and (ToRow < FStats.RowsTotal) then
    FStats.RowsTotal := ToRow - FromRow + 1
  else if FromRow > 1 then
    FStats.RowsTotal := FStats.RowsTotal - FromRow + 1;
  if FStats.RowsTotal < 0 then FStats.RowsTotal := 0;

  { SELECT }
  SelectSQL := 'SELECT ' + BuildSelectList +
               ' FROM ' + QuoteIdentifier(FOptions.SourceTable, FStats.SourceDBMS);

  EnsureSourceTrans;
  FOptions.SourceQuery.Close;
  FOptions.SourceQuery.SQL.Text := SelectSQL;
  FOptions.SourceQuery.Open;

  { Zielfeldliste }
  DestFields := '';
  FirstField := True;
  for i := 0 to High(FOptions.Fields) do
  begin
    if not FOptions.Fields[i].Selected then Continue;
    if not FirstField then DestFields := DestFields + ', ';
    FirstField := False;
    DestFields := DestFields + QuoteIdentifier(FOptions.Fields[i].DestName, FStats.DestDBMS);
  end;

  if DestFields = '' then
    raise Exception.Create('No fields selected for transfer.');

  EnsureDestTrans;

  BatchRows := TStringList.Create;
  try
    RowsSinceCommit := 0;
    RowCounter := 0;
    LastProgressTime := Now;
    CancelFlag := False;

    { Vorspulen }
    FOptions.SourceQuery.First;
    while (not FOptions.SourceQuery.EOF) and (RowCounter < FromRow - 1) do
    begin
      Inc(RowCounter);
      FOptions.SourceQuery.Next;
    end;

    while not FOptions.SourceQuery.EOF do
    begin
      if FCancelled then Break;

      { Eine Werte-Zeile bauen: (val1, val2, ...) }
      ValueLine := '(';
      ParamIndex := 0;
      FirstField := True;

      for i := 0 to High(FOptions.Fields) do
      begin
        if not FOptions.Fields[i].Selected then Continue;

        Field := FOptions.SourceQuery.Fields[ParamIndex];

        if not FirstField then ValueLine := ValueLine + ', ';
        FirstField := False;

        if Field.IsNull then
          ValueLine := ValueLine + 'NULL'
        else
        begin
          case Field.DataType of
            ftSmallint, ftInteger, ftWord, ftLargeint, ftAutoInc,
            ftFloat, ftCurrency, ftBCD, ftFMTBcd:
              ValueLine := ValueLine + Field.AsString;
            ftBoolean:
              if Field.AsBoolean then ValueLine := ValueLine + 'TRUE'
              else ValueLine := ValueLine + 'FALSE';
          else
            { String / Datum etc. → quoten }
            ValueLine := ValueLine + QuotedStr(Field.AsString);
          end;

          { Bytes zählen — erweitert für TMemoField/TWideStringField }
          case Field.DataType of
            ftString, ftFixedChar, ftWideString, ftMemo, ftWideMemo:
              Inc(FStats.BytesProcessed, Length(Field.AsString));
          else
            Inc(FStats.BytesProcessed, Field.DataSize);
          end;
        end;

        Inc(ParamIndex);
      end;
      ValueLine := ValueLine + ')';
      BatchRows.Add(ValueLine);

      Inc(FStats.RowsProcessed);
      Inc(RowsSinceCommit);
      Inc(RowCounter);

      { Bereichsende? }
      if (ToRow > 0) and (FStats.RowsProcessed >= (ToRow - FromRow + 1)) then
        Break;

      { Batch voll → ausführen }
      if BatchRows.Count >= FBatchSize then
      begin
        SQL := 'INSERT INTO ' + QuoteIdentifier(FOptions.DestTable, FStats.DestDBMS) +
               ' (' + DestFields + ') VALUES ' + sLineBreak +
               String.Join(',' + sLineBreak, BatchRows.ToStringArray);

        FOptions.DestQuery.Close;
        FOptions.DestQuery.SQL.Text := SQL;
        FOptions.DestQuery.ExecSQL;

        { Template-Beschreibung — NICHT die tatsächliche SQL mit Daten }
        FStats.InsertSQL := 'INSERT INTO ' +
                            QuoteIdentifier(FOptions.DestTable, FStats.DestDBMS) +
                            ' (' + DestFields + ') VALUES (... batch of up to ' +
                            IntToStr(FBatchSize) + ' rows ...)';

        BatchRows.Clear;

        if RowsSinceCommit >= CommitInterval then
        begin
          CommitDest;
          RowsSinceCommit := 0;
        end;
      end;

      { Progress }
      if (Round((Now - LastProgressTime) * 86400 * 1000) >= RefreshMs) then
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

    { Restlichen Batch schreiben }
    if (BatchRows.Count > 0) and (not FCancelled) then
    begin
      SQL := 'INSERT INTO ' + QuoteIdentifier(FOptions.DestTable, FStats.DestDBMS) +
             ' (' + DestFields + ') VALUES ' + sLineBreak +
             String.Join(',' + sLineBreak, BatchRows.ToStringArray);

      FOptions.DestQuery.Close;
      FOptions.DestQuery.SQL.Text := SQL;
      FOptions.DestQuery.ExecSQL;

      { Template-Beschreibung — NICHT die tatsächliche SQL mit Daten }
      FStats.InsertSQL := 'INSERT INTO ' +
                          QuoteIdentifier(FOptions.DestTable, FStats.DestDBMS) +
                          ' (' + DestFields + ') VALUES (... batch of up to ' +
                          IntToStr(FBatchSize) + ' rows ...)';
    end;

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

  finally
    BatchRows.Free;
    try
      if FOptions.SourceQuery.Active then FOptions.SourceQuery.Close;
    except
    end;
    try
      if FOptions.DestQuery.Active then FOptions.DestQuery.Close;
    except
    end;
  end;
end;

{ ------------------------------------------------------------------ }
{ Fallback: parametrisiertes Einfügen (Firebird, Oracle, ODBC ...)   }
{ ------------------------------------------------------------------ }
procedure TCrossEngineBatched.ExecuteParametrized;
var
  SelectSQL: string;
  FromRow, ToRow, RowCounter: Int64;
  CancelFlag: Boolean;
  Elapsed: string;
  LastProgressTime: TDateTime;
  RefreshMs, CommitInterval: Integer;
  RowsSinceCommit: Integer;
  i, ParamIndex: Integer;
  Field: TField;
  InsertSQL, DestFields, Placeholders: string;
  FirstField: Boolean;
begin
  CommitInterval := FOptions.CommitInterval;
  if CommitInterval <= 0 then CommitInterval := 10000;

  RefreshMs := FOptions.RefreshIntervalMs;
  if RefreshMs < 100 then RefreshMs := 100;

  FStats.RowsTotal := CountSourceRows;

  FromRow := FOptions.FromRow;
  if FromRow < 1 then FromRow := 1;
  ToRow := FOptions.ToRow;

  if (ToRow > 0) and (ToRow < FStats.RowsTotal) then
    FStats.RowsTotal := ToRow - FromRow + 1
  else if FromRow > 1 then
    FStats.RowsTotal := FStats.RowsTotal - FromRow + 1;
  if FStats.RowsTotal < 0 then FStats.RowsTotal := 0;

  SelectSQL := 'SELECT ' + BuildSelectList +
               ' FROM ' + QuoteIdentifier(FOptions.SourceTable, FStats.SourceDBMS);

  EnsureSourceTrans;
  FOptions.SourceQuery.Close;
  FOptions.SourceQuery.SQL.Text := SelectSQL;
  FOptions.SourceQuery.Open;

  { INSERT vorbereiten }
  DestFields := '';
  Placeholders := '';
  FirstField := True;
  ParamIndex := 0;

  for i := 0 to High(FOptions.Fields) do
  begin
    if not FOptions.Fields[i].Selected then Continue;
    if not FirstField then
    begin
      DestFields := DestFields + ', ';
      Placeholders := Placeholders + ', ';
    end;
    FirstField := False;
    DestFields := DestFields + QuoteIdentifier(FOptions.Fields[i].DestName, FStats.DestDBMS);
    Placeholders := Placeholders + ':P' + IntToStr(ParamIndex);
    Inc(ParamIndex);
  end;

  if DestFields = '' then
    raise Exception.Create('No fields selected for transfer.');

  InsertSQL := 'INSERT INTO ' + QuoteIdentifier(FOptions.DestTable, FStats.DestDBMS) +
               ' (' + DestFields + ') VALUES (' + Placeholders + ')';

  EnsureDestTrans;
  FOptions.DestQuery.Close;
  FOptions.DestQuery.SQL.Text := InsertSQL;
  FOptions.DestQuery.Prepare;
  FStats.InsertSQL := InsertSQL;

  RowsSinceCommit := 0;
  RowCounter := 0;
  LastProgressTime := Now;
  CancelFlag := False;

  FOptions.SourceQuery.First;
  while (not FOptions.SourceQuery.EOF) and (RowCounter < FromRow - 1) do
  begin
    Inc(RowCounter);
    FOptions.SourceQuery.Next;
  end;

  while not FOptions.SourceQuery.EOF do
  begin
    if FCancelled then Break;

    ParamIndex := 0;
    for i := 0 to High(FOptions.Fields) do
    begin
      if not FOptions.Fields[i].Selected then Continue;
      Field := FOptions.SourceQuery.Fields[ParamIndex];

      if Field.IsNull then
        FOptions.DestQuery.Params[ParamIndex].Clear
      else
        FOptions.DestQuery.Params[ParamIndex].Assign(Field);

      { Bytes zählen — erweitert für TMemoField/TWideStringField }
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

    FOptions.DestQuery.ExecSQL;

    Inc(FStats.RowsProcessed);
    Inc(RowsSinceCommit);
    Inc(RowCounter);

    if (ToRow > 0) and (FStats.RowsProcessed >= (ToRow - FromRow + 1)) then
      Break;

    if RowsSinceCommit >= CommitInterval then
    begin
      CommitDest;
      try
        FOptions.DestQuery.SQL.Text := InsertSQL;
        FOptions.DestQuery.Prepare;
      except
      end;
      RowsSinceCommit := 0;
    end;

    if (Round((Now - LastProgressTime) * 86400 * 1000) >= RefreshMs) then
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
