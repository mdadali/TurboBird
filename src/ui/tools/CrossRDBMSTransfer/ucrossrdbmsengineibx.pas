unit uCrossRDBMSEngineIBX;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, IB, IBQuery,  Dialogs,
  uCrossRDBMSTypes,
  uCrossRDBMSTypeMapping,
  uCrossRDBMSEngineBase;

type
  { ------------------------------------------------------------------ }
  { IBX-Engine — Row-by-Row für alle Kombinationen mit IBX-Beteiligung   }
  {                                                                     }
  { Deckt ab:                                                           }
  {   - Firebird (IBX)  → Firebird (IBX)                                }
  {   - Firebird (IBX)  → SQLDB (PostgreSQL, MySQL, ...)                }
  {   - SQLDB           → Firebird (IBX)                                }
  { ------------------------------------------------------------------ }
  TCrossEngineIBX = class(TCrossEngineBase)
  protected
    procedure InternalExecute; override;

    procedure EnsureSourceTrans; override;
    procedure EnsureDestTrans; override;
    procedure CommitDest; override;
    procedure RollbackDest; override;
    procedure CleanupQueries; override;

    function  CountSourceRows: Int64; override;
  end;

implementation

// ============================================================================
// Transaktionen
// ============================================================================

procedure TCrossEngineIBX.EnsureSourceTrans;
begin
  if FOptions.SourceIsIBX then
  begin
    if Assigned(FOptions.SourceIBXTrans) and
       (not FOptions.SourceIBXTrans.Active) then
      FOptions.SourceIBXTrans.StartTransaction;
  end
  else
    inherited EnsureSourceTrans;
end;

procedure TCrossEngineIBX.EnsureDestTrans;
begin
  if FOptions.DestIsIBX then
  begin
    if Assigned(FOptions.DestIBXTrans) and
       (not FOptions.DestIBXTrans.Active) then
      FOptions.DestIBXTrans.StartTransaction;
  end
  else
    inherited EnsureDestTrans;
end;

procedure TCrossEngineIBX.CommitDest;
begin
  if FOptions.DestIsIBX then
  begin
    if Assigned(FOptions.DestIBXTrans) then
    begin
      try
        if Assigned(FOptions.DestIBXQuery) and
           FOptions.DestIBXQuery.Active then
          FOptions.DestIBXQuery.Close;
      except
      end;

      try
        if FOptions.DestIBXTrans.Active then
          FOptions.DestIBXTrans.Commit;
      except
      end;

      if not FOptions.DestIBXTrans.Active then
        FOptions.DestIBXTrans.StartTransaction;
    end;
    Inc(FStats.CommitsDone);
  end
  else
    inherited CommitDest;
end;

procedure TCrossEngineIBX.RollbackDest;
begin
  if FOptions.DestIsIBX then
  begin
    try
      if Assigned(FOptions.DestIBXQuery) and
         FOptions.DestIBXQuery.Active then
        FOptions.DestIBXQuery.Close;
    except
    end;

    if Assigned(FOptions.DestIBXTrans) then
    begin
      try
        if FOptions.DestIBXTrans.Active then
          FOptions.DestIBXTrans.Rollback;
      except
      end;

      if not FOptions.DestIBXTrans.Active then
        FOptions.DestIBXTrans.StartTransaction;
    end;
  end
  else
    inherited RollbackDest;
end;

procedure TCrossEngineIBX.CleanupQueries;
begin
  try
    if FOptions.SourceIsIBX then
    begin
      if Assigned(FOptions.SourceIBXQuery) and
         FOptions.SourceIBXQuery.Active then
        FOptions.SourceIBXQuery.Close;
    end
    else
    begin
      if Assigned(FOptions.SourceQuery) and
         FOptions.SourceQuery.Active then
        FOptions.SourceQuery.Close;
    end;
  except
  end;

  try
    if FOptions.DestIsIBX then
    begin
      if Assigned(FOptions.DestIBXQuery) and
         FOptions.DestIBXQuery.Active then
        FOptions.DestIBXQuery.Close;
    end
    else
    begin
      if Assigned(FOptions.DestQuery) and
         FOptions.DestQuery.Active then
        FOptions.DestQuery.Close;
    end;
  except
  end;
end;

// ============================================================================
// COUNT(*) auf Source
// ============================================================================

function TCrossEngineIBX.CountSourceRows: Int64;
var
  SQL: string;
begin
  Result := 0;
  SQL := 'SELECT COUNT(*) FROM ' +
    QuoteIdentifier(FOptions.SourceTable, FStats.SourceDBMS);

  EnsureSourceTrans;

  if FOptions.SourceIsIBX then
  begin
    if not Assigned(FOptions.SourceIBXQuery) then Exit;
    FOptions.SourceIBXQuery.Close;
    FOptions.SourceIBXQuery.SQL.Text := SQL;
    try
      FOptions.SourceIBXQuery.Open;
      if not FOptions.SourceIBXQuery.EOF then
        Result := FOptions.SourceIBXQuery.Fields[0].AsLargeInt;
    finally
      FOptions.SourceIBXQuery.Close;
    end;
  end
  else
  begin
    if not Assigned(FOptions.SourceQuery) then Exit;
    FOptions.SourceQuery.Close;
    FOptions.SourceQuery.SQL.Text := SQL;
    try
      FOptions.SourceQuery.Open;
      if not FOptions.SourceQuery.EOF then
        Result := FOptions.SourceQuery.Fields[0].AsLargeInt;
    finally
      FOptions.SourceQuery.Close;
    end;
  end;
end;

// ============================================================================
// Hauptausführung (Row-by-Row)
// ============================================================================
procedure TCrossEngineIBX.InternalExecute;
var
  SelectSQL, InsertSQL: string;
  FromRow, ToRow, RowCounter: Int64;
  RowsSinceCommit, CommitInterval: Integer;
  RefreshMs: Integer;
  LastProgressTime: TDateTime;
  CancelFlag: Boolean;
  Elapsed: string;
  i, SelectedCount, ParamIndex: Integer;
  Field: TField;
  DestFields, Placeholders: string;
  FirstField: Boolean;
  SourceIBX: TIBQuery;
  SourceSQLDB: TSQLQuery;
  DestIBX: TIBQuery;
  DestSQLDB: TSQLQuery;

  procedure SourceNext;
  begin
    if FOptions.SourceIsIBX then
      SourceIBX.Next
    else
      SourceSQLDB.Next;
  end;

  function SourceEOF: Boolean;
  begin
    if FOptions.SourceIsIBX then
      Result := SourceIBX.EOF
    else
      Result := SourceSQLDB.EOF;
  end;

  procedure SourceFirst;
  begin
    if FOptions.SourceIsIBX then
      SourceIBX.First
    else
      SourceSQLDB.First;
  end;

  function SourceFieldByIndex(AIndex: Integer): TField;
  begin
    if FOptions.SourceIsIBX then
      Result := SourceIBX.Fields[AIndex]
    else
      Result := SourceSQLDB.Fields[AIndex];
  end;

begin
  { ---- Vorbereitung ---- }
  FStats.RowsTotal := CountSourceRows;

  FromRow := FOptions.FromRow;
  if FromRow < 1 then FromRow := 1;
  ToRow := FOptions.ToRow;

  if (ToRow > 0) and (ToRow < FStats.RowsTotal) then
    FStats.RowsTotal := ToRow - FromRow + 1
  else if FromRow > 1 then
    FStats.RowsTotal := FStats.RowsTotal - FromRow + 1;
  if FStats.RowsTotal < 0 then FStats.RowsTotal := 0;

  { ---- SELECT aufbauen ---- }
  SelectSQL := 'SELECT ' + BuildSelectList +
               ' FROM ' + QuoteIdentifier(FOptions.SourceTable, FStats.SourceDBMS);

  SourceIBX := nil;
  SourceSQLDB := nil;

  if FOptions.SourceIsIBX then
  begin
    SourceIBX := FOptions.SourceIBXQuery;
    EnsureSourceTrans;
    SourceIBX.Close;
    SourceIBX.SQL.Text := SelectSQL;
    SourceIBX.Open;
  end
  else
  begin
    SourceSQLDB := FOptions.SourceQuery;
    EnsureSourceTrans;
    SourceSQLDB.Close;
    SourceSQLDB.SQL.Text := SelectSQL;
    SourceSQLDB.Open;
  end;

  { ---- INSERT vorbereiten ---- }
  SelectedCount := 0;
  DestFields := '';
  Placeholders := '';
  FirstField := True;
  for i := 0 to High(FOptions.Fields) do
  begin
    if not FOptions.Fields[i].Selected then Continue;

    if not FirstField then
    begin
      DestFields := DestFields + ', ';
      Placeholders := Placeholders + ', ';
    end;
    FirstField := False;

    DestFields := DestFields +
      QuoteIdentifier(FOptions.Fields[i].DestName, FStats.DestDBMS);
    Placeholders := Placeholders + ':P' + IntToStr(SelectedCount);
    Inc(SelectedCount);
  end;

  if SelectedCount = 0 then
  begin
    AddWarning('No fields selected - nothing to copy.');
    Exit;
  end;

  InsertSQL := 'INSERT INTO ' +
    QuoteIdentifier(FOptions.DestTable, FStats.DestDBMS) +
    ' (' + DestFields + ') VALUES (' + Placeholders + ')';
  FStats.InsertSQL := InsertSQL;

  DestIBX := nil;
  DestSQLDB := nil;

  if FOptions.DestIsIBX then
  begin
    DestIBX := FOptions.DestIBXQuery;
    EnsureDestTrans;
    DestIBX.Close;
    DestIBX.SQL.Text := InsertSQL;
    DestIBX.Prepare;
  end
  else
  begin
    DestSQLDB := FOptions.DestQuery;
    EnsureDestTrans;
    DestSQLDB.Close;
    DestSQLDB.SQL.Text := InsertSQL;
    DestSQLDB.Prepare;
  end;

  { ---- Parameter ---- }
  CommitInterval := FOptions.CommitInterval;
  if CommitInterval <= 0 then CommitInterval := 10000;

  RefreshMs := FOptions.RefreshIntervalMs;
  if RefreshMs < 100 then RefreshMs := 100;
  if RefreshMs > 60000 then RefreshMs := 60000;

  RowsSinceCommit := 0;
  RowCounter := 0;
  LastProgressTime := Now;
  CancelFlag := False;

  { ---- Vorspulen ---- }
  SourceFirst;
  while (not SourceEOF) and (RowCounter < FromRow - 1) do
  begin
    Inc(RowCounter);
    SourceNext;
  end;

  { ---- Hauptschleife ---- }
  while not SourceEOF do
  begin
    if FCancelled then Break;

    ParamIndex := 0;
    for i := 0 to High(FOptions.Fields) do
    begin
      if not FOptions.Fields[i].Selected then Continue;

      Field := SourceFieldByIndex(ParamIndex);

      if FOptions.DestIsIBX then
      begin
        if Field.IsNull then
          DestIBX.Params[ParamIndex].Clear
        else
          DestIBX.Params[ParamIndex].Assign(Field);
      end
      else
      begin
        if Field.IsNull then
          DestSQLDB.Params[ParamIndex].Clear
        else
          DestSQLDB.Params[ParamIndex].Assign(Field);
      end;

      { Bytes zählen — erweitert für TMemoField/TWideStringField.
        SQLite/PostgreSQL liefern TEXT-Felder als TMemoField,
        dort ist Field.DataSize = 0. }
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

    if FOptions.DestIsIBX then
      DestIBX.ExecSQL
    else
      DestSQLDB.ExecSQL;

    Inc(FStats.RowsProcessed);
    Inc(RowsSinceCommit);
    Inc(RowCounter);

    if (ToRow > 0) and (FStats.RowsProcessed >= (ToRow - FromRow + 1)) then
      Break;

    if RowsSinceCommit >= CommitInterval then
    begin
      CommitDest;

      try
        if FOptions.DestIsIBX then
        begin
          DestIBX.SQL.Text := InsertSQL;
          DestIBX.Prepare;
        end
        else
        begin
          DestSQLDB.SQL.Text := InsertSQL;
          DestSQLDB.Prepare;
        end;
      except
      end;

      RowsSinceCommit := 0;
    end;

    if (Round((Now - LastProgressTime) * 86400 * 1000) >= RefreshMs) or
       SourceEOF then
    begin
      LastProgressTime := Now;
      Elapsed := FormatDateTime('hh:nn:ss', Now - FStats.StartTime);
      CancelFlag := FCancelled;
      DoProgress(FStats.RowsProcessed, FStats.RowsTotal, Elapsed, CancelFlag);
      if CancelFlag then FCancelled := True;
    end;

    if FCancelled then Break;

    SourceNext;
  end;

  { ---- Abschluss ---- }
  if not FCancelled then
    CommitDest
  else
  begin
    RollbackDest;
    FStats.Cancelled := True;
  end;

  ShowMessage(FOptions.SourceIBXConn.DatabaseName);
  ShowMessage(FOptions.DestIBXConn.DatabaseName);

  Elapsed := FormatDateTime('hh:nn:ss', Now - FStats.StartTime);
  CancelFlag := FCancelled;
  DoProgress(FStats.RowsProcessed, FStats.RowsTotal, Elapsed, CancelFlag);
end;

end.
