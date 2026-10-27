unit uCrossRDBMSEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB,
  uCrossRDBMSTypes,
  uCrossRDBMSTypeMapping,
  uCrossRDBMSStrategies,
  uCrossRDBMSEngineBase,
  uCrossRDBMSEngineRowByRow,
  uCrossRDBMSEngineBatched,
  uCrossRDBMSEngineInsertSelect;

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
  public
    constructor Create(const AOptions: TCrossTransferOptions);
    destructor Destroy; override;

    // SQL-Vorschau (CREATE + logisches INSERT)
    function PreviewSQL: string;
    function BuildCreateSQL: string;

    // Tabellen-Existenz prüfen
    function DestTableExists: Boolean;

    // Hauptausführung
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

  // Basis-Infos
  if Assigned(FOptions.SourceConn) then
  begin
    FStats.SourceDBMS := DBMSFromConnection(FOptions.SourceConn);
    FStats.SourceDatabase := FOptions.SourceConn.DatabaseName;
  end;
  FStats.SourceTable := FOptions.SourceTable;

  if Assigned(FOptions.DestConn) then
  begin
    FStats.DestDBMS := DBMSFromConnection(FOptions.DestConn);
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

function TCrossTransferEngine.CreateEngine: TCrossEngineBase;
var
  Strategy: TCrossStrategy;
  SameConn: Boolean;
begin
  SameConn := (FOptions.SourceConn = FOptions.DestConn);

  // Strategie bestimmen
  if FOptions.Strategy <> csRowByRow then  // manuell gewählt?
    Strategy := FOptions.Strategy
  else
    Strategy := RecommendStrategy(FStats.SourceDBMS, FStats.DestDBMS, SameConn);

  // Verfügbarkeit prüfen und ggf. Fallback
  if not IsStrategyAvailable(Strategy, FStats.SourceDBMS, FStats.DestDBMS, SameConn) then
  begin
    Log('Requested strategy not available – falling back to Row-by-Row.');
    Strategy := csRowByRow;
  end;

  FStats.StrategyUsed := Strategy;

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

  // Events durchreichen
  Result.OnProgress := FOnProgress;
  Result.OnLog := FOnLog;
end;

function TCrossTransferEngine.BuildCreateSQL: string;
var
  i: Integer;
  SL: TStringList;
  FieldLine: string;
begin
  Result := '';
  SL := TStringList.Create;
  try
    for i := 0 to High(FOptions.Fields) do
    begin
      if not FOptions.Fields[i].Selected then Continue;

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
begin
  CreateSQL := BuildCreateSQL;

  // Logisches INSERT … SELECT als Vorschau
  SelectList := '';
  DestList := '';
  First := True;

  for i := 0 to High(FOptions.Fields) do
  begin
    if not FOptions.Fields[i].Selected then Continue;

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

  InsertSQL := 'INSERT INTO ' + QuoteIdentifier(FOptions.DestTable, FStats.DestDBMS) +
               ' (' + DestList + ')' + sLineBreak +
               'SELECT ' + SelectList + sLineBreak +
               'FROM ' + QuoteIdentifier(FOptions.SourceTable, FStats.SourceDBMS);

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

function TCrossTransferEngine.DestTableExists: Boolean;
var
  Q: TSQLQuery;
begin
  Result := False;
  if not Assigned(FOptions.DestQuery) then Exit;

  Q := FOptions.DestQuery;
  Q.Close;

  case FStats.DestDBMS of
    cdFirebird:
      Q.SQL.Text := 'SELECT 1 FROM RDB$RELATIONS WHERE RDB$RELATION_NAME = ' +
                    QuotedStr(UpperCase(FOptions.DestTable)) + ' AND RDB$VIEW_BLR IS NULL';
    cdPostgreSQL:
      Q.SQL.Text := 'SELECT 1 FROM information_schema.tables WHERE table_name = ' +
                    QuotedStr(LowerCase(FOptions.DestTable)) + ' AND table_schema = current_schema()';
    cdMySQL:
      Q.SQL.Text := 'SELECT 1 FROM information_schema.tables WHERE table_name = ' +
                    QuotedStr(FOptions.DestTable) + ' AND table_schema = DATABASE()';
    cdSQLite:
      Q.SQL.Text := 'SELECT 1 FROM sqlite_master WHERE type = ''table'' AND name = ' +
                    QuotedStr(FOptions.DestTable);
    cdMSSQL:
      Q.SQL.Text := 'SELECT 1 FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_NAME = ' +
                    QuotedStr(FOptions.DestTable);
    cdOracle:
      Q.SQL.Text := 'SELECT 1 FROM USER_TABLES WHERE TABLE_NAME = ' +
                    QuotedStr(UpperCase(FOptions.DestTable));
  else
    // Generischer Fallback
    Q.SQL.Text := 'SELECT 1 FROM ' + QuoteIdentifier(FOptions.DestTable, FStats.DestDBMS) + ' WHERE 1=0';
    try
      if not FOptions.DestTrans.Active then FOptions.DestTrans.StartTransaction;
      Q.Open;
      Result := True;
      Q.Close;
    except
      Result := False;
    end;
    Exit;
  end;

  try
    if not FOptions.DestTrans.Active then FOptions.DestTrans.StartTransaction;
    Q.Open;
    Result := not Q.EOF;
    Q.Close;
  except
    Result := False;
  end;
end;

procedure TCrossTransferEngine.Execute;
var
  OldWarnings: TStringList;
begin
  FCancelled := False;

  if Assigned(FEngine) then
    FreeAndNil(FEngine);

  // Zuerst Tabellen-Struktur behandeln
  ExecuteCreatePhase;

  if FOptions.CopyData then
  begin
    FEngine := CreateEngine;
    try
      FEngine.Execute;

      // Statistik übernehmen – Warnings-Liste korrekt behandeln
      OldWarnings := FStats.Warnings;          // unsere bisherige Liste merken
      FStats := FEngine.Stats;                 // Record kopieren
      FStats.Warnings := OldWarnings;          // unsere Liste wieder einsetzen

      // Inhalt von der Engine-Liste übernehmen
      if Assigned(FEngine.Stats.Warnings) then
        FStats.Warnings.Assign(FEngine.Stats.Warnings);

    finally
      // FEngine wird im Destructor freigegeben
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

procedure TCrossTransferEngine.DropTable(const ATableName: string);
begin
  if not Assigned(FOptions.DestQuery) then
    raise Exception.Create('DestQuery not assigned.');

  if not FOptions.DestTrans.Active then
    FOptions.DestTrans.StartTransaction;

  FOptions.DestQuery.Close;
  FOptions.DestQuery.SQL.Text := 'DROP TABLE ' + QuoteIdentifier(ATableName, FStats.DestDBMS);
  FOptions.DestQuery.ExecSQL;

  // Commit nach DDL
  try
    FOptions.DestTrans.Commit;
  except
  end;
  if not FOptions.DestTrans.Active then
    FOptions.DestTrans.StartTransaction;
end;

procedure TCrossTransferEngine.CreateTable;
var
  SQL: string;
begin
  if not Assigned(FOptions.DestQuery) then
    raise Exception.Create('DestQuery not assigned.');

  SQL := BuildCreateSQL;
  if Trim(SQL) = '' then Exit;

  if not FOptions.DestTrans.Active then
    FOptions.DestTrans.StartTransaction;

  FOptions.DestQuery.Close;
  FOptions.DestQuery.SQL.Text := SQL;
  FOptions.DestQuery.ExecSQL;

  // Commit nach DDL
  try
    FOptions.DestTrans.Commit;
  except
  end;
  if not FOptions.DestTrans.Active then
    FOptions.DestTrans.StartTransaction;

  FStats.CreateSQL := SQL;
end;

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
      raise Exception.Create('Destination table "' + FOptions.DestTable + '" already exists. Aborted.');

    ieaAsk:
      raise Exception.Create('IfExists = Ask must be resolved before Execute.');
  end;
end;

end.
