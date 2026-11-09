unit uCrossRDBMSEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, IB,
  uCrossRDBMSTypes,
  uCrossRDBMSTypeMapping,
  uCrossRDBMSStrategies,
  uCrossRDBMSEngineBase,
  uCrossRDBMSEngineRowByRow,
  uCrossRDBMSEngineBatched,
  uCrossRDBMSEngineInsertSelect,
  uCrossRDBMSEngineIBX;

type
  { ------------------------------------------------------------------ }
  { Orchestrierungs-Engine                                             }
  { Wählt die passende Strategie und startet die konkrete Engine.      }
  { ------------------------------------------------------------------ }
  TCrossTransferEngine = class
  private
    FOptions   : TCrossTransferOptions;
    FStats     : TCrossTransferStats;
    FEngine    : TCrossEngineBase;
    FOnProgress: TCrossProgressEvent;
    FOnLog     : TCrossLogEvent;
    FCancelled : Boolean;

    function  CreateEngine: TCrossEngineBase;
    procedure Log(const AMsg: string);

    procedure ExecuteCreatePhase;
    procedure DropTable(const ATableName: string);
    procedure CreateTable;

    procedure InternalProgress(ARowsDone, ARowsTotal: Int64;
      const AElapsed: string; var ACancel: Boolean);
    procedure SyncStatsFromWorker;

    function  IsAnyIBX: Boolean;
    function  HasSameConnection: Boolean;

    function  IsSamePhysicalDatabase: Boolean;
  public
    constructor Create(const AOptions: TCrossTransferOptions);
    destructor Destroy; override;

    function PreviewSQL: string;
    function BuildCreateSQL: string;

    function DestTableExists: Boolean;
    function IsSameTableInSameDatabase: Boolean;

    procedure SetSystemInfo(const AValue: string);

    procedure Execute;
    procedure Cancel;

    property Stats: TCrossTransferStats read FStats;
    property OnProgress: TCrossProgressEvent read FOnProgress write FOnProgress;
    property OnLog: TCrossLogEvent read FOnLog write FOnLog;
  end;

implementation

constructor TCrossTransferEngine.Create(const AOptions: TCrossTransferOptions);
begin
  inherited Create;
  FOptions := AOptions;
  FEngine := nil;
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
end;

destructor TCrossTransferEngine.Destroy;
begin
  if Assigned(FEngine) then
    FEngine.Free;
  if Assigned(FStats.Warnings) then
    FStats.Warnings.Free;
  inherited Destroy;
end;

procedure TCrossTransferEngine.Log(const AMsg: string);
begin
  if Assigned(FOnLog) then
    FOnLog(AMsg);
end;

procedure TCrossTransferEngine.InternalProgress(ARowsDone, ARowsTotal: Int64;
  const AElapsed: string; var ACancel: Boolean);
begin
  SyncStatsFromWorker;
  if Assigned(FOnProgress) then
    FOnProgress(ARowsDone, ARowsTotal, AElapsed, ACancel);
end;

// ============================================================================
// Hilfsfunktionen
// ============================================================================

function TCrossTransferEngine.IsAnyIBX: Boolean;
begin
  Result := FOptions.SourceIsIBX or FOptions.DestIsIBX;
end;

function TCrossTransferEngine.HasSameConnection: Boolean;
begin
  { Beide SQLDB und dieselbe Connection? }
  if (not FOptions.SourceIsIBX) and (not FOptions.DestIsIBX) then
    Exit((FOptions.SourceConn <> nil) and
         (FOptions.SourceConn = FOptions.DestConn));

  { Beide IBX und dieselbe Connection? }
  if FOptions.SourceIsIBX and FOptions.DestIsIBX then
    Exit((FOptions.SourceIBXConn <> nil) and
         (FOptions.SourceIBXConn = FOptions.DestIBXConn));

  { Gemischt — nie "same connection" }
  Result := False;
end;

// ============================================================================
// Engine-Auswahl
// ============================================================================
function TCrossTransferEngine.CreateEngine: TCrossEngineBase;
var
  Strategy: TCrossStrategy;
  SameConn: Boolean;
begin
  SameConn := HasSameConnection;

  if FOptions.Strategy = csAuto then
    Strategy := RecommendStrategy(FStats.SourceDBMS, FStats.DestDBMS, SameConn)
  else
    Strategy := FOptions.Strategy;

  if not IsStrategyAvailable(Strategy, FStats.SourceDBMS, FStats.DestDBMS, SameConn) then
  begin
    Log('Requested strategy not available – falling back to Row-by-Row.');
    Strategy := csRowByRow;
  end;

  { ---- IBX-Beteiligung ---- }
  if IsAnyIBX then
  begin
    { IBX-Source → SQLDB-Dest: Batched (Multi-Value) erlaubt }
    if FOptions.SourceIsIBX and (not FOptions.DestIsIBX) and
       (Strategy = csBatched) then
    begin
      FStats.StrategyUsed := csBatched;
      FOptions.Strategy := csBatched;
      Log('Using engine: IBX (Batched → SQLDB)');
    end
    { SQLDB-Source → IBX-Dest: Batched (EXECUTE BLOCK) erlaubt }
    else if (not FOptions.SourceIsIBX) and FOptions.DestIsIBX and
            (Strategy = csBatched) then
    begin
      FStats.StrategyUsed := csBatched;
      FOptions.Strategy := csBatched;
      Log('Using engine: IBX (Batched ← SQLDB via EXECUTE BLOCK)');
    end
    else
    begin
      FStats.StrategyUsed := csRowByRow;
      FOptions.Strategy := csRowByRow;
      Log('Using engine: IBX (Row-by-Row)');
    end;

    Result := TCrossEngineIBX.Create(FOptions);
    Result.OnProgress := @InternalProgress;
    Result.OnLog := FOnLog;
    Exit;
  end;

  { ---- Reiner SQLDB-Pfad ---- }
  FStats.StrategyUsed := Strategy;
  FOptions.Strategy := Strategy;

  case Strategy of
    csInsertSelect:
      begin
        Log('Using strategy: INSERT … SELECT (batched)');
        Result := TCrossEngineInsertSelect.Create(FOptions);
      end;
    csBatched:
      begin
        Log('Using strategy: Batched Inserts');
        Result := TCrossEngineBatched.Create(FOptions);
      end;
  else
    begin
      Log('Using strategy: Row-by-Row');
      Result := TCrossEngineRowByRow.Create(FOptions);
    end;
  end;

  Result.OnProgress := @InternalProgress;
  Result.OnLog := FOnLog;
end;

// ============================================================================
// CREATE TABLE (Vorschau)
// ============================================================================

function TCrossTransferEngine.BuildCreateSQL: string;
var
  i: Integer;
  SL: TStringList;
  FieldLine: string;
  IsComputedFB: Boolean;
begin
  Result := '';
  SL := TStringList.Create;
  try
    for i := 0 to High(FOptions.Fields) do
    begin
      if not FOptions.Fields[i].Selected then Continue;

      IsComputedFB := FOptions.Fields[i].IsComputed and
                      (FStats.SourceDBMS = cdFirebird) and
                      (FStats.DestDBMS = cdFirebird) and
                      (Trim(FOptions.Fields[i].ComputedExpr) <> '');

      if IsComputedFB then
        FieldLine := QuoteIdentifier(FOptions.Fields[i].DestName, FStats.DestDBMS) +
                     ' COMPUTED BY (' + FOptions.Fields[i].ComputedExpr + ')'
      else
        FieldLine := QuoteIdentifier(FOptions.Fields[i].DestName, FStats.DestDBMS) +
                     ' ' + Trim(FOptions.Fields[i].DestType);

      SL.Add('  ' + FieldLine);
    end;

    if SL.Count = 0 then Exit;

    Result := 'CREATE TABLE ' + QuoteIdentifier(FOptions.DestTable, FStats.DestDBMS) + ' (' + sLineBreak;
    for i := 0 to SL.Count - 1 do
    begin
      if i < SL.Count - 1 then
        Result := Result + SL[i] + ',' + sLineBreak
      else
        Result := Result + SL[i] + sLineBreak;
    end;
    Result := Result + ')';
  finally
    SL.Free;
  end;
end;

function TCrossTransferEngine.PreviewSQL: string;
var
  CreateSQL, InsertSQL: string;
  i: Integer;
  SelectList, DestList: string;
  First: Boolean;
  IsComputedFB: Boolean;
begin
  CreateSQL := BuildCreateSQL;

  SelectList := '';
  DestList := '';
  First := True;

  for i := 0 to High(FOptions.Fields) do
  begin
    if not FOptions.Fields[i].Selected then Continue;

    { FB→FB computed: erscheint in CREATE, aber nicht in INSERT/SELECT }
    IsComputedFB := FOptions.Fields[i].IsComputed and
                    (FStats.SourceDBMS = cdFirebird) and
                    (FStats.DestDBMS = cdFirebird) and
                    (Trim(FOptions.Fields[i].ComputedExpr) <> '');
    if IsComputedFB then Continue;

    if not First then
    begin
      SelectList := SelectList + ', ';
      DestList := DestList + ', ';
    end;
    First := False;

    DestList := DestList + QuoteIdentifier(FOptions.Fields[i].DestName, FStats.DestDBMS);

    if FOptions.UseFormulas and (Trim(FOptions.Fields[i].Formula) <> '') then
      SelectList := SelectList + StringReplace(FOptions.Fields[i].Formula, '$1',
                      QuoteIdentifier(FOptions.Fields[i].SourceName, FStats.SourceDBMS),
                      [rfReplaceAll])
    else
      SelectList := SelectList + QuoteIdentifier(FOptions.Fields[i].SourceName, FStats.SourceDBMS);
  end;

  if (DestList <> '') and (SelectList <> '') then
    InsertSQL := 'INSERT INTO ' + QuoteIdentifier(FOptions.DestTable, FStats.DestDBMS) +
                 ' (' + DestList + ')' + sLineBreak +
                 'SELECT ' + SelectList + sLineBreak +
                 'FROM ' + QuoteIdentifier(FOptions.SourceTable, FStats.SourceDBMS)
  else
    InsertSQL := '';

  FStats.CreateSQL := CreateSQL;
  FStats.InsertSQL := InsertSQL;

  Result := '';
  if CreateSQL <> '' then
    Result := '-- CREATE TABLE' + sLineBreak + CreateSQL;
  if InsertSQL <> '' then
  begin
    if Result <> '' then Result := Result + sLineBreak + sLineBreak;
    Result := Result + '-- INSERT (logical preview)' + sLineBreak + InsertSQL;
  end;
end;

// ============================================================================
// DestTableExists (IBX-fähig)
// ============================================================================

function TCrossTransferEngine.DestTableExists: Boolean;
var
  SQL: string;
  CleanName: string;

  function LocalStripIdentifierQuotes(const AName: string): string;
  var
    S: string;
  begin
    S := Trim(AName);
    if Length(S) < 2 then
      Exit(S);

    if ((S[1] = '"') and (S[Length(S)] = '"')) or
       ((S[1] = '`') and (S[Length(S)] = '`')) or
       ((S[1] = '[') and (S[Length(S)] = ']')) then
      Result := Copy(S, 2, Length(S) - 2)
    else
      Result := S;
  end;

begin
  Result := False;

  CleanName := LocalStripIdentifierQuotes(FOptions.DestTable);
  if CleanName = '' then
    Exit;

  case FStats.DestDBMS of
    cdFirebird:
      SQL := 'SELECT 1 FROM RDB$RELATIONS WHERE RDB$RELATION_NAME = ' +
             QuotedStr(CleanName) + ' AND RDB$VIEW_BLR IS NULL';
    cdPostgreSQL:
      SQL := 'SELECT 1 FROM information_schema.tables WHERE table_name = ' +
             QuotedStr(CleanName) + ' AND table_schema = current_schema()';
    cdMySQL:
      SQL := 'SELECT 1 FROM information_schema.tables WHERE table_name = ' +
             QuotedStr(CleanName) + ' AND table_schema = DATABASE()';
    cdSQLite:
      SQL := 'SELECT 1 FROM sqlite_master WHERE type = ''table'' ' +
             'AND name = ' + QuotedStr(CleanName) + ' COLLATE NOCASE';
    cdMSSQL:
      SQL := 'SELECT 1 FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_NAME = ' +
             QuotedStr(CleanName);
    cdOracle:
      SQL := 'SELECT 1 FROM USER_TABLES WHERE TABLE_NAME = ' + QuotedStr(CleanName);
    cdSybase:
      SQL := 'SELECT 1 FROM sysobjects WHERE name = ' +
             QuotedStr(CleanName) + ' AND type = ''U''';
  else
    SQL := 'SELECT 1 FROM ' + QuoteIdentifier(CleanName, FStats.DestDBMS) +
           ' WHERE 1=0';
  end;

  if FOptions.DestIsIBX then
  begin
    if not Assigned(FOptions.DestIBXQuery) then Exit;

    if Assigned(FOptions.DestIBXTrans) then
    begin
      try
        if FOptions.DestIBXTrans.Active then
          FOptions.DestIBXTrans.Active := False;
      except
      end;
      FOptions.DestIBXTrans.StartTransaction;
    end;

    try
      FOptions.DestIBXQuery.Close;
      FOptions.DestIBXQuery.SQL.Text := SQL;
      FOptions.DestIBXQuery.Open;
      Result := not FOptions.DestIBXQuery.EOF;
      FOptions.DestIBXQuery.Close;
    except
      on E: Exception do
      begin
        Log('DestTableExists (IBX) failed: ' + E.Message);
        FStats.Warnings.Add('DestTableExists failed: ' + E.Message);
        try FOptions.DestIBXQuery.Close; except end;
      end;
    end;
  end
  else
  begin
    if not Assigned(FOptions.DestQuery) then Exit;

    if Assigned(FOptions.DestTrans) then
    begin
      try
        if FOptions.DestTrans.Active then
          FOptions.DestTrans.Commit;
      except
      end;
      if not FOptions.DestTrans.Active then
        FOptions.DestTrans.StartTransaction;
    end;

    try
      FOptions.DestQuery.Close;
      FOptions.DestQuery.SQL.Text := SQL;
      FOptions.DestQuery.Open;
      Result := not FOptions.DestQuery.EOF;
      FOptions.DestQuery.Close;
    except
      on E: Exception do
      begin
        Log('DestTableExists (SQLDB) failed: ' + E.Message);
        FStats.Warnings.Add('DestTableExists failed: ' + E.Message);
        try FOptions.DestQuery.Close; except end;
      end;
    end;
  end;
end;

// ============================================================================
// DropTable (IBX-fähig)
// ============================================================================

procedure TCrossTransferEngine.DropTable(const ATableName: string);
var
  SQL: string;
begin
  SQL := 'DROP TABLE ' + QuoteIdentifier(ATableName, FStats.DestDBMS);

  if FOptions.DestIsIBX then
  begin
    if not Assigned(FOptions.DestIBXQuery) then
      raise Exception.Create('DestIBXQuery not assigned.');

    if not FOptions.DestIBXTrans.Active then
      FOptions.DestIBXTrans.StartTransaction;

    FOptions.DestIBXQuery.Close;
    FOptions.DestIBXQuery.SQL.Text := SQL;
    FOptions.DestIBXQuery.ExecSQL;

    try FOptions.DestIBXTrans.Commit; except end;
    if not FOptions.DestIBXTrans.Active then
      FOptions.DestIBXTrans.StartTransaction;
  end
  else
  begin
    if not Assigned(FOptions.DestQuery) then
      raise Exception.Create('DestQuery not assigned.');

    if not FOptions.DestTrans.Active then
      FOptions.DestTrans.StartTransaction;

    FOptions.DestQuery.Close;
    FOptions.DestQuery.SQL.Text := SQL;
    FOptions.DestQuery.ExecSQL;

    try FOptions.DestTrans.Commit; except end;
    if not FOptions.DestTrans.Active then
      FOptions.DestTrans.StartTransaction;
  end;
end;

// ============================================================================
// CreateTable (IBX-fähig)
// ============================================================================

procedure TCrossTransferEngine.CreateTable;
var
  SQL: string;
begin
  SQL := BuildCreateSQL;
  if Trim(SQL) = '' then Exit;

  if FOptions.DestIsIBX then
  begin
    if not Assigned(FOptions.DestIBXQuery) then
      raise Exception.Create('DestIBXQuery not assigned.');

    if not FOptions.DestIBXTrans.Active then
      FOptions.DestIBXTrans.StartTransaction;

    FOptions.DestIBXQuery.Close;
    FOptions.DestIBXQuery.SQL.Text := SQL;
    FOptions.DestIBXQuery.ExecSQL;

    try FOptions.DestIBXTrans.Commit; except end;
    if not FOptions.DestIBXTrans.Active then
      FOptions.DestIBXTrans.StartTransaction;
  end
  else
  begin
    if not Assigned(FOptions.DestQuery) then
      raise Exception.Create('DestQuery not assigned.');

    if not FOptions.DestTrans.Active then
      FOptions.DestTrans.StartTransaction;

    FOptions.DestQuery.Close;
    FOptions.DestQuery.SQL.Text := SQL;
    FOptions.DestQuery.ExecSQL;

    try FOptions.DestTrans.Commit; except end;
    if not FOptions.DestTrans.Active then
      FOptions.DestTrans.StartTransaction;
  end;

  FStats.CreateSQL := SQL;
end;

// ============================================================================
// CREATE-Phase
// ============================================================================

procedure TCrossTransferEngine.ExecuteCreatePhase;
var
  Exists: Boolean;
begin
  Exists := DestTableExists;
  FStats.TableExisted := Exists;

  if not Exists then
  begin
    Log('Destination table does not exist – creating.');
    CreateTable;
    FStats.TableCreated := True;
    Exit;
  end;

  case FOptions.IfExists of
    ieaRecreate:
      begin
        Log('Recreating destination table.');
        DropTable(FOptions.DestTable);
        CreateTable;
        FStats.TableCreated := True;
        FStats.TableRecreated := True;
      end;

    ieaAppend:
      begin
        Log('Appending to existing table.');
        FStats.TableCreated := False;
      end;

    ieaAbort:
      raise Exception.Create('Destination table "' + FOptions.DestTable +
        '" already exists. Aborted.');

    ieaAsk:
      raise Exception.Create('IfExists = Ask must be resolved before Execute.');
  end;
end;

// ============================================================================
// Sicherheitschecks — Source = Dest verhindern
// ============================================================================

function TCrossTransferEngine.IsSamePhysicalDatabase: Boolean;
begin
  Result := False;

  { ---- Beide SQLDB ---- }
  if (not FOptions.SourceIsIBX) and (not FOptions.DestIsIBX) then
  begin
    if (FOptions.SourceConn = nil) or (FOptions.DestConn = nil) then
      Exit;

    { 1. Gleiche Connection-Instanz }
    if FOptions.SourceConn = FOptions.DestConn then
      Exit(True);

    { 2. Gleicher HostName + DatabaseName → gleiche physische DB }
    if SameText(Trim(FOptions.SourceConn.HostName),
                Trim(FOptions.DestConn.HostName)) and
       SameText(Trim(FOptions.SourceConn.DatabaseName),
                Trim(FOptions.DestConn.DatabaseName)) then
      Exit(True);

    Exit;
  end;

  { ---- Beide IBX ---- }
  if FOptions.SourceIsIBX and FOptions.DestIsIBX then
  begin
    if (FOptions.SourceIBXConn = nil) or (FOptions.DestIBXConn = nil) then
      Exit;

    { 1. Gleiche Connection-Instanz }
    if FOptions.SourceIBXConn = FOptions.DestIBXConn then
      Exit(True);

    { 2. Gleicher DatabaseName → gleiche physische DB }
    if SameText(Trim(FOptions.SourceIBXConn.DatabaseName),
                Trim(FOptions.DestIBXConn.DatabaseName)) then
      Exit(True);

    Exit;
  end;

  { ---- Gemischt (IBX ↔ SQLDB) — physisch nie dieselbe DB ---- }
  Result := False;
end;

function TCrossTransferEngine.IsSameTableInSameDatabase: Boolean;
begin
  Result := IsSamePhysicalDatabase and
            SameText(Trim(FOptions.SourceTable), Trim(FOptions.DestTable));
end;

procedure TCrossTransferEngine.SetSystemInfo(const AValue: string);
begin
  FStats.SystemInfo := AValue;
end;

// ============================================================================
// Execute
// ============================================================================

procedure TCrossTransferEngine.Execute;
var
  OldWarnings: TStringList;
  OldTableCreated, OldTableRecreated, OldTableExisted: Boolean;
begin
  FCancelled := False;

  if Assigned(FEngine) then
    FreeAndNil(FEngine);

  { ---- Sicherheitscheck: Source = Dest (gleiche Tabelle, gleiche DB) ---- }
  if FOptions.CopyData and IsSameTableInSameDatabase then
    raise Exception.Create(
      'Source and Destination are the same table in the same database:' +
      sLineBreak + sLineBreak +
      '  Table:    ' + FOptions.SourceTable + sLineBreak +
      '  Database: ' + FStats.SourceDatabase + sLineBreak + sLineBreak +
      'This would cause data duplication or a database lock.' + sLineBreak +
      'Please choose a different destination table name.');

  ExecuteCreatePhase;

  if FOptions.CopyData then
  begin
    FEngine := CreateEngine;
    try
      FEngine.Execute;

      { ---- Tabellen-Status-Flags retten — der Worker kennt sie nicht ---- }
      OldWarnings       := FStats.Warnings;
      OldTableCreated   := FStats.TableCreated;
      OldTableRecreated := FStats.TableRecreated;
      OldTableExisted   := FStats.TableExisted;

      FStats := FEngine.Stats;

      FStats.Warnings       := OldWarnings;
      FStats.TableCreated   := OldTableCreated;
      FStats.TableRecreated := OldTableRecreated;
      FStats.TableExisted   := OldTableExisted;

      if Assigned(FEngine.Stats.Warnings) then
        FStats.Warnings.Assign(FEngine.Stats.Warnings);
    finally
      { FEngine wird im Destructor freigegeben }
    end;
  end
  else
    Log('Copy Data unchecked – only table structure processed.');
end;

procedure TCrossTransferEngine.Cancel;
begin
  FCancelled := True;
  if Assigned(FEngine) then
    FEngine.Cancel;
end;

procedure TCrossTransferEngine.SyncStatsFromWorker;
begin
  if not Assigned(FEngine) then Exit;

  FStats.RowsProcessed  := FEngine.Stats.RowsProcessed;
  FStats.RowsTotal      := FEngine.Stats.RowsTotal;
  FStats.BytesProcessed := FEngine.Stats.BytesProcessed;
  FStats.CommitsDone    := FEngine.Stats.CommitsDone;
  FStats.StartTime      := FEngine.Stats.StartTime;
  FStats.StrategyUsed   := FEngine.Stats.StrategyUsed;
  FStats.Cancelled      := FEngine.Stats.Cancelled;
end;

end.
