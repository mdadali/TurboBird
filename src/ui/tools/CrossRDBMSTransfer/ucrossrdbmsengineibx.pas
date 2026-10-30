unit uCrossRDBMSEngineIBX;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, IB, IBQuery,  Dialogs, TypInfo,
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
    procedure DebugDumpEverything(AList: TStringList);
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

procedure TCrossEngineIBX.DebugDumpEverything(AList: TStringList);
var
  i, SelCnt: Integer;
  Field: TField;
  Param: TParam;
  MetaIBX: TIBQuery;
  s: string;

  procedure L(const Msg: string);
  begin
    AList.Add(Msg);
  end;

begin
  if not Assigned(AList) then Exit;
  AList.Clear;

  L('========== CROSS ENGINE DEBUG DUMP (IBX) ==========');
  L(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now));
  L('');

  // ------------------------------------------------------------------
  // 1. Basis-Infos
  // ------------------------------------------------------------------
  L('--- OPTIONS ---');
  L(Format('SourceTable      : %s', [FOptions.SourceTable]));
  L(Format('DestTable        : %s', [FOptions.DestTable]));
  L(Format('FromRow / ToRow  : %d / %d', [FOptions.FromRow, FOptions.ToRow]));
  L(Format('CommitInterval   : %d', [FOptions.CommitInterval]));
  L(Format('Fields.Count     : %d', [Length(FOptions.Fields)]));
  L('');

  // ------------------------------------------------------------------
  // 2. Source
  // ------------------------------------------------------------------
  L('--- SOURCE (IBX) ---');
  L(Format('Active            : %s', [BoolToStr(FOptions.SourceIBXQuery.Active, True)]));
  L(Format('SQL.Text          : %s', [FOptions.SourceIBXQuery.SQL.Text]));
  if Assigned(FOptions.SourceIBXQuery.Transaction) then
  begin
    L(Format('Transaction.Active: %s', [BoolToStr(FOptions.SourceIBXQuery.Transaction.Active, True)]));
    L(Format('Transaction.Name  : %s', [FOptions.SourceIBXQuery.Transaction.Name]));
  end
  else
    L('Transaction       : NIL !!!');

  if FOptions.SourceIBXQuery.Active then
  begin
    L(Format('FieldCount        : %d', [FOptions.SourceIBXQuery.FieldCount]));
    L('Source Fields:');
    for i := 0 to FOptions.SourceIBXQuery.FieldCount - 1 do
    begin
      Field := FOptions.SourceIBXQuery.Fields[i];
      L(Format('  [%2d] %-30s  DataType=%-15s  Size=%5d  IsNull=%s',
        [i, Field.FieldName,
         GetEnumName(TypeInfo(TFieldType), Ord(Field.DataType)),
         Field.DataSize, BoolToStr(Field.IsNull, True)]));
    end;
  end
  else
    L('!!! SOURCE QUERY NICHT AKTIV !!!');
  L('');

  // ------------------------------------------------------------------
  // 3. Destination
  // ------------------------------------------------------------------
  L('--- DESTINATION (IBX) ---');
  L(Format('Active            : %s', [BoolToStr(FOptions.DestIBXQuery.Active, True)]));
  L(Format('Prepared          : %s', [BoolToStr(FOptions.DestIBXQuery.Prepared, True)]));
  L(Format('SQL.Text          : %s', [FOptions.DestIBXQuery.SQL.Text]));
  L(Format('InsertSQL (Stats) : %s', [FStats.InsertSQL]));

  if Assigned(FOptions.DestIBXQuery.Transaction) then
  begin
    L(Format('Transaction.Active: %s', [BoolToStr(FOptions.DestIBXQuery.Transaction.Active, True)]));
    L(Format('Transaction.Name  : %s', [FOptions.DestIBXQuery.Transaction.Name]));
  end
  else
    L('Transaction       : NIL !!!');
  L('');

  // ------------------------------------------------------------------
  // 4. Parameter
  // ------------------------------------------------------------------
  L('--- PARAMETER ---');
  SelCnt := 0;
  for i := 0 to High(FOptions.Fields) do
    if FOptions.Fields[i].Selected then Inc(SelCnt);

  L(Format('Selected fields   : %d', [SelCnt]));
  L(Format('Params.Count      : %d', [FOptions.DestIBXQuery.Params.Count]));

  if FOptions.DestIBXQuery.Params.Count <> SelCnt then
    L(Format('!!! PARAM COUNT MISMATCH: Params=%d  vs Selected=%d !!!',
      [FOptions.DestIBXQuery.Params.Count, SelCnt]));

  for i := 0 to FOptions.DestIBXQuery.Params.Count - 1 do
  begin
    Param := FOptions.DestIBXQuery.Params[i];
    L(Format('  Param[%d] Name="%-12s"  DataType=%-15s  IsNull=%s',
      [i, Param.Name,
       GetEnumName(TypeInfo(TFieldType), Ord(Param.DataType)),
       BoolToStr(Param.IsNull, True)]));
  end;
  L('');

  // ------------------------------------------------------------------
  // 5. Mapping aus FOptions.Fields
  // ------------------------------------------------------------------
  L('--- FIELD MAPPING ---');
  for i := 0 to High(FOptions.Fields) do
  begin
    with FOptions.Fields[i] do
      L(Format('  [%2d] Sel=%-5s  Source="%-25s"  Dest="%-25s"',
        [i, BoolToStr(Selected, True), SourceName, DestName]));
  end;
  L('');

  // ------------------------------------------------------------------
  // 6. Echte Spalten der Zieltabelle (das ist der wichtigste Teil!)
  // ------------------------------------------------------------------
  L('--- REAL DEST TABLE COLUMNS (RDB$RELATION_FIELDS) ---');
  try
    MetaIBX := TIBQuery.Create(nil);
    try
      MetaIBX.Database := FOptions.DestIBXQuery.Database;
      MetaIBX.Transaction := FOptions.DestIBXQuery.Transaction;
      MetaIBX.SQL.Text :=
        'SELECT TRIM(RDB$FIELD_NAME) AS FNAME, RDB$FIELD_POSITION AS FPOS ' +
        'FROM RDB$RELATION_FIELDS ' +
        'WHERE RDB$RELATION_NAME = ' + QuotedStr(UpperCase(FOptions.DestTable)) +
        ' ORDER BY RDB$FIELD_POSITION';
      MetaIBX.Open;

      if MetaIBX.IsEmpty then
        L('!!! Tabelle existiert nicht oder hat keine Spalten !!!')
      else
      begin
        while not MetaIBX.Eof do
        begin
          L(Format('  Pos %2d : "%s"',
            [MetaIBX.FieldByName('FPOS').AsInteger,
             MetaIBX.FieldByName('FNAME').AsString]));
          MetaIBX.Next;
        end;
      end;
    finally
      MetaIBX.Free;
    end;
  except
    on E: Exception do
      L('!!! FEHLER beim Meta-Abruf: ' + E.ClassName + ' - ' + E.Message);
  end;
  L('');

  // ------------------------------------------------------------------
  // 7. Extra-Check: Existieren die DestNames wirklich?
  // ------------------------------------------------------------------
  L('--- DESTNAME vs REAL COLUMNS CHECK ---');
  try
    MetaIBX := TIBQuery.Create(nil);
    try
      MetaIBX.Database := FOptions.DestIBXQuery.Database;
      MetaIBX.Transaction := FOptions.DestIBXQuery.Transaction;
      MetaIBX.SQL.Text :=
        'SELECT TRIM(RDB$FIELD_NAME) AS FNAME ' +
        'FROM RDB$RELATION_FIELDS ' +
        'WHERE RDB$RELATION_NAME = ' + QuotedStr(UpperCase(FOptions.DestTable));
      MetaIBX.Open;

      for i := 0 to High(FOptions.Fields) do
      begin
        if not FOptions.Fields[i].Selected then Continue;

        s := UpperCase(FOptions.Fields[i].DestName);
        // Quotes entfernen falls vorhanden
        if (Length(s) >= 2) and (s[1] = '"') and (s[Length(s)] = '"') then
          s := Copy(s, 2, Length(s)-2);

        if not MetaIBX.Locate('FNAME', s, []) then
          L(Format('!!! FEHLT IN ZIELTABELLE: DestName="%s" (gesucht als "%s")',
            [FOptions.Fields[i].DestName, s]))
        else
          L(Format('OK: "%s"', [FOptions.Fields[i].DestName]));
      end;
    finally
      MetaIBX.Free;
    end;
  except
    on E: Exception do
      L('!!! FEHLER beim Name-Check: ' + E.Message);
  end;

  L('');
  L('========== ENDE DEBUG DUMP ==========');
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

  // ---------- Source-Hilfsroutinen ----------
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

  function SourceFieldByName(const AName: string): TField;
  begin
    if FOptions.SourceIsIBX then
      Result := SourceIBX.FieldByName(AName)
    else
      Result := SourceSQLDB.FieldByName(AName);
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

    DestFields := DestFields + QuoteIdentifier(FOptions.Fields[i].DestName, FStats.DestDBMS);
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

  { ---- Parameter / Intervalle ---- }
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

    // ---------------------------------------------------------------
    // Parameter sicher setzen
    // ---------------------------------------------------------------
    ParamIndex := 0;
    for i := 0 to High(FOptions.Fields) do
    begin
      if not FOptions.Fields[i].Selected then Continue;

      Field := SourceFieldByName(FOptions.Fields[i].SourceName);

      if FOptions.DestIsIBX then
      begin
        // ---------- IBX Ziel (explizit & sicher) ----------
        if Field.IsNull then
          DestIBX.Params[ParamIndex].Clear
        else
        begin
          case Field.DataType of
            ftString, ftFixedChar, ftWideString:
              DestIBX.Params[ParamIndex].AsString := Field.AsString;

            ftMemo, ftWideMemo:
              DestIBX.Params[ParamIndex].AsString := Field.AsString;

            ftSmallint, ftInteger, ftWord, ftAutoInc:
              DestIBX.Params[ParamIndex].AsInteger := Field.AsInteger;

            ftLargeint:
              DestIBX.Params[ParamIndex].AsLargeInt := Field.AsLargeInt;

            ftFloat, ftCurrency, ftBCD, ftFMTBcd:
              DestIBX.Params[ParamIndex].AsFloat := Field.AsFloat;

            ftDate:
              DestIBX.Params[ParamIndex].AsDate := Field.AsDateTime;

            ftTime:
              DestIBX.Params[ParamIndex].AsTime := Field.AsDateTime;

            ftDateTime, ftTimeStamp:
              DestIBX.Params[ParamIndex].AsDateTime := Field.AsDateTime;

            ftBoolean:
              DestIBX.Params[ParamIndex].AsBoolean := Field.AsBoolean;

            ftBlob, ftGraphic:
              DestIBX.Params[ParamIndex].Assign(Field);

          else
            DestIBX.Params[ParamIndex].Value := Field.Value;
          end;
        end;
      end
      else
      begin
        // ---------- SQLDB Ziel (klassisch) ----------
        if Field.IsNull then
          DestSQLDB.Params[ParamIndex].Clear
        else
          DestSQLDB.Params[ParamIndex].Assign(Field);
      end;

      // Bytes zählen
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

    // ---------------------------------------------------------------
    // INSERT ausführen
    // ---------------------------------------------------------------
    if FOptions.DestIsIBX then
      DestIBX.ExecSQL
    else
      DestSQLDB.ExecSQL;

    Inc(FStats.RowsProcessed);
    Inc(RowsSinceCommit);
    Inc(RowCounter);

    if (ToRow > 0) and (FStats.RowsProcessed >= (ToRow - FromRow + 1)) then
      Break;

    // Commit-Interval
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
        // absichtlich leer
      end;
      RowsSinceCommit := 0;
    end;

    // Fortschritt
    if (Round((Now - LastProgressTime) * 86400 * 1000) >= RefreshMs) or SourceEOF then
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

  Elapsed := FormatDateTime('hh:nn:ss', Now - FStats.StartTime);
  CancelFlag := FCancelled;
  DoProgress(FStats.RowsProcessed, FStats.RowsTotal, Elapsed, CancelFlag);
end;

end.
