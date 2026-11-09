unit uCrossTransferEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB,
  turbocommon,
  uCrossRDBMSTypes;

type
  TIfExistsAction = (ieaAsk, ieaRecreate, ieaAppend, ieaAbort);

  TCrossField = record
    SourceName: string;
    SourceType: string;
    DestType: string;
    Formula: string;
    Selected: Boolean;
  end;

  TCrossFieldArray = array of TCrossField;

  TCrossTransferOptions = record
    SourceConn: TSQLConnection;
    DestConn: TSQLConnection;
    SourceTrans: TSQLTransaction;
    DestTrans: TSQLTransaction;
    SourceQuery: TSQLQuery;
    DestQuery: TSQLQuery;
    SourceTable: string;
    DestTable: string;
    Fields: TCrossFieldArray;
    UseFormulas: Boolean;
    CopyData: Boolean;
    IfExists: TIfExistsAction;
    FromRow: Int64;
    ToRow: Int64;
    CommitInterval: Integer;
    RefreshIntervalMs: Integer;
  end;

  TCrossTransferStats = record
    { Source-Info }
    SourceConnectorType: string;
    SourceDatabase: string;
    SourceTable: string;
    SourceDBMS: TCrossDBMS;

    { Dest-Info }
    DestConnectorType: string;
    DestDatabase: string;
    DestTable: string;
    DestDBMS: TCrossDBMS;

    { Transfer-Statistik }
    RowsProcessed: Int64;
    RowsTotal: Int64;
    ElapsedSeconds: Double;
    RowsPerSec: Double;
    BytesProcessed: Int64;
    MBPerSec: Double;
    CommitsDone: Integer;

    { Zeitstempel für Live-Berechnung }
    StartTime: TDateTime;

    { Tabellen-Status }
    TableCreated: Boolean;
    TableRecreated: Boolean;
    TableExisted: Boolean;

    { SQL }
    CreateSQL: string;
    InsertSQL: string;

    { Status }
    Cancelled: Boolean;
    Warnings: TStringList;
  end;

  TProgressEvent = procedure(ARowsDone, ARowsTotal: Int64;
    const AElapsed: string; var ACancel: Boolean) of object;
  TLogEvent = procedure(const AMsg: string) of object;

  TCrossTransferEngine = class
  private
    FOptions: TCrossTransferOptions;
    FStats: TCrossTransferStats;
    FOnProgress: TProgressEvent;
    FOnLog: TLogEvent;
    FCancelled: Boolean;

    function  BuildSelectExpr(AFieldIndex: Integer): string;
    function  ExpandFormula(const AFormula, AOwnFieldName: string): string;
    function  TableExistsViaDestQuery(const ATableName: string): Boolean;
    procedure DropTable(const ATableName: string);
    procedure CreateTable;
    procedure ExecuteCreatePhase;
    procedure ExecuteCopyPhase;
    procedure EnsureSourceTrans;
    procedure EnsureDestTrans;
    procedure CommitDest;
    procedure RollbackDest;
    procedure DoLog(const AMsg: string);
    procedure AddWarning(const AMsg: string);
    function  CountSourceRows: Int64;
  public
    constructor Create(const AOptions: TCrossTransferOptions);
    destructor Destroy; override;

    function BuildCreateSQL: string;
    function BuildInsertSQL: string;
    function PreviewSQL: string;
    function DestTableExists: Boolean;

    procedure Execute;
    procedure Cancel;

    procedure SyncStatsFromWorker;

    property Stats: TCrossTransferStats read FStats;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property OnLog: TLogEvent read FOnLog write FOnLog;
  end;


  { Formatiert die vollständige Transfer-Statistik als Text. }
  function FormatTransferReport(const AStats: TCrossTransferStats): string;


implementation

// ============================================================================
// Hilfsfunktionen
// ============================================================================

function JoinStrings(const AList: TStringList; const ASep: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to AList.Count - 1 do
  begin
    if i > 0 then Result := Result + ASep;
    Result := Result + AList[i];
  end;
end;

function GetConnectorTypeName(AConn: TSQLConnection): string;
begin
  if AConn = nil then
    Exit('(none)');

  if AConn is TSQLConnector then
    Result := TSQLConnector(AConn).ConnectorType
  else
  begin
    Result := AConn.ClassName;
    if (Length(Result) > 1) and (Result[1] = 'T') then
      Delete(Result, 1, 1);
  end;

  if Trim(Result) = '' then
    Result := AConn.ClassName;
end;

// ============================================================================
// FormatTransferReport
// ============================================================================

function FormatTransferReport(const AStats: TCrossTransferStats): string;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Add('===============================================================');
    SL.Add('  CROSSTRANSFER - TRANSFER REPORT');
    SL.Add('===============================================================');
    SL.Add('');

    SL.Add('--- SOURCE ---------------------------------------------------');
    SL.Add('  Connector  : ' + AStats.SourceConnectorType);
    SL.Add('  DBMS       : ' + DBMSName(AStats.SourceDBMS));
    SL.Add('  Database   : ' + AStats.SourceDatabase);
    SL.Add('  Table      : ' + AStats.SourceTable);
    SL.Add('');

    SL.Add('--- DESTINATION ----------------------------------------------');
    SL.Add('  Connector  : ' + AStats.DestConnectorType);
    SL.Add('  DBMS       : ' + DBMSName(AStats.DestDBMS));
    SL.Add('  Database   : ' + AStats.DestDatabase);
    SL.Add('  Table      : ' + AStats.DestTable);
    SL.Add('');

    SL.Add('--- STATUS ---------------------------------------------------');
    if AStats.Cancelled then
      SL.Add('  Result     : CANCELLED')
    else
      SL.Add('  Result     : COMPLETED');
    SL.Add('');

    SL.Add('--- TRANSFER STATISTICS --------------------------------------');
    SL.Add(Format('  Rows processed  : %s',
      [FormatFloat('#,##0', AStats.RowsProcessed)]));
    SL.Add(Format('  Rows total      : %s',
      [FormatFloat('#,##0', AStats.RowsTotal)]));
    SL.Add(Format('  Elapsed         : %.2f sec', [AStats.ElapsedSeconds]));
    SL.Add(Format('  Rows/sec        : %s',
      [FormatFloat('#,##0', AStats.RowsPerSec)]));
    SL.Add(Format('  Bytes processed : %s',
      [FormatFloat('#,##0', AStats.BytesProcessed)]));
    SL.Add(Format('  MB/sec          : %.2f', [AStats.MBPerSec]));
    SL.Add(Format('  Commits         : %d', [AStats.CommitsDone]));
    SL.Add('');

    SL.Add('--- TABLE STATUS ---------------------------------------------');
    if AStats.TableCreated then
      SL.Add('  Created         : YES')
    else
      SL.Add('  Created         : NO');
    if AStats.TableRecreated then
      SL.Add('  Recreated       : YES')
    else
      SL.Add('  Recreated       : NO');
    if AStats.TableExisted then
      SL.Add('  Existed before  : YES')
    else
      SL.Add('  Existed before  : NO');
    SL.Add('');

    if AStats.Warnings.Count > 0 then
    begin
      SL.Add('--- WARNINGS -------------------------------------------------');
      SL.Add(AStats.Warnings.Text);
      SL.Add('');
    end;

    if Trim(AStats.CreateSQL) <> '' then
    begin
      SL.Add('--- CREATE TABLE SQL -----------------------------------------');
      SL.Add(AStats.CreateSQL);
      SL.Add('');
    end;

    if Trim(AStats.InsertSQL) <> '' then
    begin
      SL.Add('--- INSERT SQL -----------------------------------------------');
      SL.Add(AStats.InsertSQL);
      SL.Add('');
    end;

    SL.Add('===============================================================');

    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

// ============================================================================
// Konstruktor / Destruktor
// ============================================================================

constructor TCrossTransferEngine.Create(const AOptions: TCrossTransferOptions);
begin
  inherited Create;
  FOptions := AOptions;

  FStats := Default(TCrossTransferStats);
  FStats.Warnings := TStringList.Create;

  FStats.SourceConnectorType := GetConnectorTypeName(FOptions.SourceConn);
  FStats.SourceDatabase      := FOptions.SourceConn.DatabaseName;
  FStats.SourceTable         := FOptions.SourceTable;
  FStats.SourceDBMS          := DBMSFromConnection(FOptions.SourceConn);

  FStats.DestConnectorType   := GetConnectorTypeName(FOptions.DestConn);
  FStats.DestDatabase        := FOptions.DestConn.DatabaseName;
  FStats.DestTable           := FOptions.DestTable;
  FStats.DestDBMS            := DBMSFromConnection(FOptions.DestConn);

  FCancelled := False;
end;

destructor TCrossTransferEngine.Destroy;
begin
  FStats.Warnings.Free;
  inherited;
end;

// ============================================================================
// Hilfsfunktionen
// ============================================================================

procedure TCrossTransferEngine.DoLog(const AMsg: string);
begin
  if Assigned(FOnLog) then
    FOnLog(AMsg);
end;

procedure TCrossTransferEngine.AddWarning(const AMsg: string);
begin
  FStats.Warnings.Add(AMsg);
end;

procedure TCrossTransferEngine.Cancel;
begin
  FCancelled := True;
end;

// ============================================================================
// Transaktionen
// ============================================================================

procedure TCrossTransferEngine.EnsureSourceTrans;
begin
  if not Assigned(FOptions.SourceTrans) then Exit;
  if not FOptions.SourceTrans.Active then
    FOptions.SourceTrans.StartTransaction;
end;

procedure TCrossTransferEngine.EnsureDestTrans;
begin
  if not Assigned(FOptions.DestTrans) then Exit;
  if not FOptions.DestTrans.Active then
    FOptions.DestTrans.StartTransaction;
end;

procedure TCrossTransferEngine.CommitDest;
begin
  if not Assigned(FOptions.DestTrans) then Exit;

  { Query vor Commit schließen — gibt das serverseitige
    Prepared Statement frei. Verhindert SIGSEGV nach Commit. }
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
end;

procedure TCrossTransferEngine.RollbackDest;
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

// ============================================================================
// Formel-Expansion
// ============================================================================

function TCrossTransferEngine.ExpandFormula(
  const AFormula, AOwnFieldName: string): string;
begin
  Result := Trim(AFormula);
  if Result = '' then
    Result := AOwnFieldName
  else
    Result := StringReplace(Result, '$1', AOwnFieldName, [rfReplaceAll]);
end;

// ============================================================================
// SQL-Bau
// ============================================================================
function TCrossTransferEngine.BuildSelectExpr(AFieldIndex: Integer): string;
var
  F: TCrossField;
  SrcQuoted: string;
begin
  F := FOptions.Fields[AFieldIndex];
  SrcQuoted := QuoteIdentifier(F.SourceName, FStats.SourceDBMS);

  if FOptions.UseFormulas and (Trim(F.Formula) <> '') then
    Result := ExpandFormula(F.Formula, SrcQuoted) + ' AS ' + SrcQuoted
  else
    Result := SrcQuoted;
end;

function TCrossTransferEngine.BuildCreateSQL: string;
var
  SL, FieldList: TStringList;
  i: Integer;
  FieldLine: string;
begin
  Result := '';

  FieldList := TStringList.Create;
  try
    for i := 0 to High(FOptions.Fields) do
    begin
      if not FOptions.Fields[i].Selected then Continue;
      FieldLine := QuoteIdentifier(FOptions.Fields[i].SourceName, FStats.DestDBMS)
        + ' ' + Trim(FOptions.Fields[i].DestType);
      FieldList.Add('  ' + FieldLine);
    end;

    if FieldList.Count = 0 then Exit;

    SL := TStringList.Create;
    try
      SL.Add('CREATE TABLE ' +
        QuoteIdentifier(FOptions.DestTable, FStats.DestDBMS) + ' (');
      for i := 0 to FieldList.Count - 1 do
      begin
        if i < FieldList.Count - 1 then
          SL.Add(FieldList[i] + ',')
        else
          SL.Add(FieldList[i]);
      end;
      SL.Add(')');
      Result := SL.Text;
    finally
      SL.Free;
    end;
  finally
    FieldList.Free;
  end;
end;

function TCrossTransferEngine.BuildInsertSQL: string;
var
  SL, DestFields, SrcFields: TStringList;
  i: Integer;
begin
  Result := '';
  DestFields := TStringList.Create;
  SrcFields := TStringList.Create;
  SL := TStringList.Create;
  try
    for i := 0 to High(FOptions.Fields) do
    begin
      if not FOptions.Fields[i].Selected then Continue;
      DestFields.Add(QuoteIdentifier(FOptions.Fields[i].SourceName, FStats.DestDBMS));
      SrcFields.Add(BuildSelectExpr(i));
    end;

    if DestFields.Count = 0 then Exit;

    SL.Add('INSERT INTO ' +
      QuoteIdentifier(FOptions.DestTable, FStats.DestDBMS) +
      ' (' + JoinStrings(DestFields, ', ') + ')');
    SL.Add('SELECT');
    for i := 0 to SrcFields.Count - 1 do
    begin
      if i < SrcFields.Count - 1 then
        SL.Add('  ' + SrcFields[i] + ',')
      else
        SL.Add('  ' + SrcFields[i]);
    end;
    SL.Add('FROM ' + QuoteIdentifier(FOptions.SourceTable, FStats.SourceDBMS));

    Result := SL.Text;
  finally
    SL.Free;
    DestFields.Free;
    SrcFields.Free;
  end;
end;

function TCrossTransferEngine.PreviewSQL: string;
var
  CreateSQL, InsertSQL: string;
begin
  CreateSQL := BuildCreateSQL;
  InsertSQL := BuildInsertSQL;
  FStats.CreateSQL := CreateSQL;
  FStats.InsertSQL := InsertSQL;

  Result := '';
  if CreateSQL <> '' then
    Result := '-- CREATE TABLE' + sLineBreak + CreateSQL;
  if InsertSQL <> '' then
  begin
    if Result <> '' then
      Result := Result + sLineBreak + sLineBreak;
    Result := Result + '-- INSERT' + sLineBreak + InsertSQL;
  end;
end;

// ============================================================================
// COUNT(*) auf Source
// ============================================================================

function TCrossTransferEngine.CountSourceRows: Int64;
begin
  Result := 0;
  if not Assigned(FOptions.SourceQuery) then Exit;

  EnsureSourceTrans;
  FOptions.SourceQuery.Close;
  FOptions.SourceQuery.SQL.Text :=
    'SELECT COUNT(*) FROM ' +
    QuoteIdentifier(FOptions.SourceTable, FStats.SourceDBMS);
  FOptions.SourceQuery.Open;
  try
    if not FOptions.SourceQuery.EOF then
      Result := FOptions.SourceQuery.Fields[0].AsLargeInt;
  finally
    FOptions.SourceQuery.Close;
  end;
end;

// ============================================================================
// Tabellen-Existenz
// ============================================================================

function TCrossTransferEngine.TableExistsViaDestQuery(
  const ATableName: string): Boolean;
begin
  Result := False;
  if not Assigned(FOptions.DestQuery) then Exit;

  FOptions.DestQuery.Close;

  case FStats.DestDBMS of
    cdFirebird:
      FOptions.DestQuery.SQL.Text :=
        'SELECT 1 FROM RDB$RELATIONS WHERE RDB$RELATION_NAME = ' +
        QuotedStr(UpperCase(ATableName)) + ' AND RDB$VIEW_BLR IS NULL';

    cdPostgreSQL:
      FOptions.DestQuery.SQL.Text :=
        'SELECT 1 FROM information_schema.tables ' +
        'WHERE table_name = ' + QuotedStr(LowerCase(ATableName)) +
        ' AND table_schema = current_schema()';

    cdMySQL:
      FOptions.DestQuery.SQL.Text :=
        'SELECT 1 FROM information_schema.tables ' +
        'WHERE table_name = ' + QuotedStr(ATableName) +
        ' AND table_schema = DATABASE()';

    cdSQLite:
      FOptions.DestQuery.SQL.Text :=
        'SELECT 1 FROM sqlite_master WHERE type = ''table'' AND name = ' +
        QuotedStr(ATableName);

    cdMSSQL:
      FOptions.DestQuery.SQL.Text :=
        'SELECT 1 FROM INFORMATION_SCHEMA.TABLES ' +
        'WHERE TABLE_NAME = ' + QuotedStr(ATableName);

    cdOracle:
      FOptions.DestQuery.SQL.Text :=
        'SELECT 1 FROM USER_TABLES WHERE TABLE_NAME = ' +
        QuotedStr(UpperCase(ATableName));

    cdSybase:
      FOptions.DestQuery.SQL.Text :=
        'SELECT 1 FROM sysobjects ' +
        'WHERE name = ' + QuotedStr(ATableName) + ' AND type = ''U''';

    cdODBC, cdUnknown:
      begin
        { Generischer Fallback: SELECT 1 FROM "Tabelle" WHERE 1=0.
          Funktioniert für ODBC und alle RDBMS, die wir nicht kennen.
          Wenn es fehlschlägt, existiert die Tabelle nicht. }
        FOptions.DestQuery.SQL.Text :=
          'SELECT 1 FROM ' + QuoteIdentifier(ATableName, FStats.DestDBMS) +
          ' WHERE 1=0';
        try
          EnsureDestTrans;
          FOptions.DestQuery.Open;
          Result := True;
          FOptions.DestQuery.Close;
        except
          Result := False;
          try
            FOptions.DestQuery.Close;
          except
          end;
        end;
        Exit;
      end;
  else
    Exit;
  end;

  EnsureDestTrans;
  FOptions.DestQuery.Open;
  Result := not FOptions.DestQuery.EOF;
  FOptions.DestQuery.Close;
end;

function TCrossTransferEngine.DestTableExists: Boolean;
var
  SQL: string;
  CleanName: string;
begin
  Result := False;

  { Identifier unverändert lassen — nur äußere Quotes entfernen,
    falls der User den Namen selbst gequotet hat. }
  CleanName := StripIdentifierQuotes(Trim(FOptions.DestTable));
  if CleanName = '' then
    Exit;

  case FStats.DestDBMS of
    cdFirebird:
      SQL := 'SELECT 1 FROM RDB$RELATIONS WHERE RDB$RELATION_NAME = ' +
             QuotedStr(CleanName) +
             ' AND RDB$VIEW_BLR IS NULL';

    cdPostgreSQL:
      SQL := 'SELECT 1 FROM information_schema.tables WHERE table_name = ' +
             QuotedStr(CleanName) +
             ' AND table_schema = current_schema()';

    cdMySQL:
      SQL := 'SELECT 1 FROM information_schema.tables WHERE table_name = ' +
             QuotedStr(CleanName) + ' AND table_schema = DATABASE()';

    cdSQLite:
      SQL := 'SELECT 1 FROM sqlite_master WHERE type = ''table'' AND name = ' +
             QuotedStr(CleanName);

    cdMSSQL:
      SQL := 'SELECT 1 FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_NAME = ' +
             QuotedStr(CleanName);

    cdOracle:
      SQL := 'SELECT 1 FROM USER_TABLES WHERE TABLE_NAME = ' +
             QuotedStr(CleanName);

    cdSybase:
      SQL := 'SELECT 1 FROM sysobjects WHERE name = ' +
             QuotedStr(CleanName) + ' AND type = ''U''';
  else
    { ODBC / Unknown — generischer Fallback }
    SQL := 'SELECT 1 FROM ' + QuoteIdentifier(CleanName, FStats.DestDBMS) +
           ' WHERE 1=0';
  end;

  { ---- Ausführen — IBX oder SQLDB ---- }
  if FOptions.DestIsIBX then
  begin
    if not Assigned(FOptions.DestIBXQuery) then Exit;

    if Assigned(FOptions.DestIBXTrans) and not FOptions.DestIBXTrans.Active then
      FOptions.DestIBXTrans.StartTransaction;

    try
      FOptions.DestIBXQuery.Close;
      FOptions.DestIBXQuery.SQL.Text := SQL;
      FOptions.DestIBXQuery.Open;
      Result := not FOptions.DestIBXQuery.EOF;
      FOptions.DestIBXQuery.Close;
    except
      Result := False;
      try FOptions.DestIBXQuery.Close; except end;
    end;
  end
  else
  begin
    if not Assigned(FOptions.DestQuery) then Exit;

    if Assigned(FOptions.DestTrans) and not FOptions.DestTrans.Active then
      FOptions.DestTrans.StartTransaction;

    try
      FOptions.DestQuery.Close;
      FOptions.DestQuery.SQL.Text := SQL;
      FOptions.DestQuery.Open;
      Result := not FOptions.DestQuery.EOF;
      FOptions.DestQuery.Close;
    except
      Result := False;
      try FOptions.DestQuery.Close; except end;
    end;
  end;
end;

// ============================================================================
// Drop / Create
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

    try
      if FOptions.DestIBXTrans.Active then
        FOptions.DestIBXTrans.Active := False;
    except
    end;
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

    try
      if FOptions.DestIBXTrans.Active then
        FOptions.DestIBXTrans.Active := False;
    except
    end;
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
  Exists := TableExistsViaDestQuery(FOptions.DestTable);
  FStats.TableExisted := Exists;

  if not Exists then
  begin
    DoLog('Destination table does not exist - creating.');
    CreateTable;
    CommitDest;
    FStats.TableCreated := True;
    Exit;
  end;

  case FOptions.IfExists of
    ieaRecreate:
      begin
        DoLog('Recreating destination table.');
        DropTable(FOptions.DestTable);
        CommitDest;
        CreateTable;
        CommitDest;
        FStats.TableCreated := True;
        FStats.TableRecreated := True;
      end;

    ieaAppend:
      begin
        DoLog('Appending to existing table.');
        if not FOptions.CopyData then
          AddWarning('Destination table exists - Copy Data unchecked.');
        FStats.TableCreated := False;
      end;

    ieaAbort:
      raise Exception.Create(
        'Destination table "' + FOptions.DestTable +
        '" already exists. Aborted.');

    ieaAsk:
      raise Exception.Create(
        'IfExists = Ask must be resolved before Execute.');
  end;
end;

// ============================================================================
// COPY-Phase
// ============================================================================

procedure TCrossTransferEngine.ExecuteCopyPhase;
var
  i, RowsSinceCommit, CommitInterval: Integer;
  FromRow, ToRow: Int64;
  Elapsed: string;
  FieldIdx: array of Integer;
  SelectedCount: Integer;
  DestFieldNames, SrcFieldNames, ParamMarkers: TStringList;
  InsertSQL: string;
  CancelFlag: Boolean;
  RowCounter: Int64;
  Field: TField;
  RefreshMs: Integer;
  LastProgressTime: TDateTime;
begin
  if not FOptions.CopyData then
  begin
    DoLog('Copy Data unchecked - skipping data copy.');
    Exit;
  end;

  if not Assigned(FOptions.SourceQuery) then
    raise Exception.Create('SourceQuery not assigned.');
  if not Assigned(FOptions.DestQuery) then
    raise Exception.Create('DestQuery not assigned.');

  SelectedCount := 0;
  SetLength(FieldIdx, 0);
  for i := 0 to High(FOptions.Fields) do
    if FOptions.Fields[i].Selected then
    begin
      SetLength(FieldIdx, SelectedCount + 1);
      FieldIdx[SelectedCount] := i;
      Inc(SelectedCount);
    end;

  if SelectedCount = 0 then
  begin
    AddWarning('No fields selected - nothing to copy.');
    Exit;
  end;

  DestFieldNames := TStringList.Create;
  SrcFieldNames  := TStringList.Create;
  ParamMarkers   := TStringList.Create;
  try
    for i := 0 to SelectedCount - 1 do
    begin
      DestFieldNames.Add(
        QuoteIdentifier(FOptions.Fields[FieldIdx[i]].SourceName, FStats.DestDBMS));

      { Quell-Ausdruck: Formel berücksichtigen! }
      SrcFieldNames.Add(BuildSelectExpr(FieldIdx[i]));

      ParamMarkers.Add(':P' + IntToStr(i));
    end;

    InsertSQL :=
      'INSERT INTO ' + QuoteIdentifier(FOptions.DestTable, FStats.DestDBMS) +
      ' (' + JoinStrings(DestFieldNames, ', ') + ') VALUES (' +
      JoinStrings(ParamMarkers, ', ') + ')';
    FStats.InsertSQL := InsertSQL;

    { --- Gesamtzahl der Zeilen via COUNT(*) --- }
    FStats.RowsTotal := CountSourceRows;

    FromRow := FOptions.FromRow;
    if FromRow < 1 then FromRow := 1;
    ToRow := FOptions.ToRow;

    if (ToRow > 0) and (ToRow < FStats.RowsTotal) then
      FStats.RowsTotal := ToRow - FromRow + 1
    else if (FromRow > 1) then
      FStats.RowsTotal := FStats.RowsTotal - FromRow + 1;
    if FStats.RowsTotal < 0 then FStats.RowsTotal := 0;

    { --- SELECT auf SourceQuery: SrcFieldNames verwenden! --- }
    EnsureSourceTrans;
    FOptions.SourceQuery.Close;
    FOptions.SourceQuery.SQL.Text :=
      'SELECT ' + JoinStrings(SrcFieldNames, ', ') +
      ' FROM ' + QuoteIdentifier(FOptions.SourceTable, FStats.SourceDBMS);
    FOptions.SourceQuery.Open;

    { --- INSERT auf DestQuery --- }
    EnsureDestTrans;
    FOptions.DestQuery.Close;
    FOptions.DestQuery.SQL.Text := InsertSQL;
    FOptions.DestQuery.Prepare;

    CommitInterval := FOptions.CommitInterval;
    if CommitInterval <= 0 then CommitInterval := 10000;

    RefreshMs := FOptions.RefreshIntervalMs;
    if RefreshMs < 100 then RefreshMs := 100;
    if RefreshMs > 60000 then RefreshMs := 60000;

    FStats.StartTime := Now;
    RowsSinceCommit := 0;
    FStats.CommitsDone := 0;
    FStats.RowsProcessed := 0;
    FStats.BytesProcessed := 0;
    RowCounter := 0;
    LastProgressTime := Now;

    FOptions.SourceQuery.First;
    while (not FOptions.SourceQuery.EOF) and (RowCounter < FromRow - 1) do
    begin
      Inc(RowCounter);
      FOptions.SourceQuery.Next;
    end;

    CancelFlag := False;
    while not FOptions.SourceQuery.EOF do
    begin
      if FCancelled then Break;

      { --- Parameter binden + Bytes zählen --- }
      for i := 0 to SelectedCount - 1 do
      begin
        Field := FOptions.SourceQuery.Fields[i];
        FOptions.DestQuery.Params[i].Assign(Field);

        if not Field.IsNull then
        begin
          if Field is TStringField then
            Inc(FStats.BytesProcessed, Length(Field.AsString))
          else
            Inc(FStats.BytesProcessed, Field.DataSize);
        end;
      end;

      FOptions.DestQuery.ExecSQL;
      Inc(FStats.RowsProcessed);
      Inc(RowsSinceCommit);
      Inc(RowCounter);

      if (ToRow > 0) and (FStats.RowsProcessed >= (ToRow - FromRow + 1)) then
        Break;

      { --- Commit-Intervall: Query schließen + Commit + Query neu aufbauen --- }
      if RowsSinceCommit >= CommitInterval then
      begin
        CommitDest;

        try
          FOptions.DestQuery.SQL.Text := InsertSQL;
          FOptions.DestQuery.Prepare;
        except
        end;

        RowsSinceCommit := 0;
        Inc(FStats.CommitsDone);
      end;

      { --- Zeitbasierter Progress-Refresh --- }
      if (Round((Now - LastProgressTime) * 86400 * 1000) >= RefreshMs) or
         FOptions.SourceQuery.EOF then
      begin
        LastProgressTime := Now;
        Elapsed := FormatDateTime('hh:nn:ss', Now - FStats.StartTime);
        if Assigned(FOnProgress) then
        begin
          CancelFlag := FCancelled;
          FOnProgress(FStats.RowsProcessed, FStats.RowsTotal, Elapsed, CancelFlag);
          if CancelFlag then
            FCancelled := True;
        end;
        if FCancelled then Break;
      end;

      FOptions.SourceQuery.Next;
    end;

    { --- Finaler Commit / Rollback --- }
    if not FCancelled then
    begin
      CommitDest;
      Inc(FStats.CommitsDone);
    end
    else
    begin
      RollbackDest;
      FStats.Cancelled := True;
    end;

    { --- Endstatistik --- }
    FStats.ElapsedSeconds := (Now - FStats.StartTime) * 86400.0;
    if FStats.ElapsedSeconds > 0.001 then
    begin
      FStats.RowsPerSec := FStats.RowsProcessed / FStats.ElapsedSeconds;
      FStats.MBPerSec   := (FStats.BytesProcessed / 1024 / 1024) /
                            FStats.ElapsedSeconds;
    end
    else
    begin
      FStats.RowsPerSec := 0;
      FStats.MBPerSec   := 0;
    end;

    if Assigned(FOnProgress) then
    begin
      Elapsed := FormatDateTime('hh:nn:ss', Now - FStats.StartTime);
      CancelFlag := FCancelled;
      FOnProgress(FStats.RowsProcessed, FStats.RowsTotal, Elapsed, CancelFlag);
    end;

  finally
    try
      if FOptions.SourceQuery.Active then
        FOptions.SourceQuery.Close;
    except
    end;
    try
      if FOptions.DestQuery.Active then
        FOptions.DestQuery.Close;
    except
    end;
    DestFieldNames.Free;
    SrcFieldNames.Free;
    ParamMarkers.Free;
  end;
end;

// ============================================================================
// Execute
// ============================================================================

procedure TCrossTransferEngine.Execute;
begin
  FCancelled := False;
  FStats.RowsProcessed := 0;
  FStats.CommitsDone := 0;
  FStats.BytesProcessed := 0;

  try
    if not Assigned(FOptions.SourceConn) then
      raise Exception.Create('Source connection not assigned.');
    if not Assigned(FOptions.DestConn) then
      raise Exception.Create('Destination connection not assigned.');
    if not Assigned(FOptions.SourceTrans) then
      raise Exception.Create('Source transaction not assigned.');
    if not Assigned(FOptions.DestTrans) then
      raise Exception.Create('Destination transaction not assigned.');
    if not Assigned(FOptions.SourceQuery) then
      raise Exception.Create('Source query not assigned.');
    if not Assigned(FOptions.DestQuery) then
      raise Exception.Create('Destination query not assigned.');

    if (FOptions.SourceConn = FOptions.DestConn) and
       SameText(Trim(FOptions.SourceTable), Trim(FOptions.DestTable)) then
      raise Exception.Create(
        'Source and Destination are the same table (' +
        FOptions.SourceTable + ').' + sLineBreak +
        'Please choose a different destination table name.');

    if not FOptions.SourceConn.Connected then
      FOptions.SourceConn.Open;
    if not FOptions.DestConn.Connected then
      FOptions.DestConn.Open;

    EnsureSourceTrans;
    EnsureDestTrans;

    ExecuteCreatePhase;
    ExecuteCopyPhase;

  except
    on E: Exception do
    begin
      DoLog('Error: ' + E.Message);
      raise;
    end;
  end;
end;

procedure TCrossTransferEngine.SyncStatsFromWorker;
begin
  if Assigned(FEngine) then
  begin
    FStats.RowsProcessed  := FEngine.Stats.RowsProcessed;
    FStats.RowsTotal      := FEngine.Stats.RowsTotal;
    FStats.BytesProcessed := FEngine.Stats.BytesProcessed;
    FStats.CommitsDone    := FEngine.Stats.CommitsDone;
    FStats.StartTime      := FEngine.Stats.StartTime;
    FStats.StrategyUsed   := FEngine.Stats.StrategyUsed;
    FStats.Cancelled      := FEngine.Stats.Cancelled;
  end;
end;

end.
