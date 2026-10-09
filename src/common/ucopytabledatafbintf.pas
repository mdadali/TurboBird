unit uCopyTableDataFBIntf;

{ ============================================================================
  FBIntf-based Copy Engine — Cross-Database with Arrays and BLOBs

  v3.1 — Klare Trennung: Batch Memory vs. Row-by-Row Commit Interval

  Strategie (automatisch, transparent):
    * BLOB/ARRAY in Auswahl      → Row-by-Row
    * Force Row-by-Row gesetzt   → Row-by-Row
    * Nur Skalare + FB 4.0+      → Batch
    * Nur Skalare + FB < 4.0     → Row-by-Row

  Batch-Mode:
    * Batch Memory (MB) → BatchRowLimit (Speicher-Budget, adaptiv)
    * ExecuteBatch alle BatchRowLimit Zeilen
    * CommitRetaining nach jedem Flush

  Row-by-Row-Mode:
    * Commit-Intervall kommt DIREKT aus der GUI
    * CommitRetaining alle N Zeilen

  SICHERHEITSREGELN (unantastbar):
    * Binary BLOBs → IMMER Stream-basiert (SaveToStream/LoadFromStream)
    * Text BLOBs   → IMMER Stream-basiert (byte-transparent)
    * Arrays       → IMMER Element-für-Element (rekursiv, typ-erhaltend)
    * Batch NUR bei rein skalaren Feldern (Firebird-Batch-API-Limit)
  ============================================================================ }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, Forms, Controls, StdCtrls, ComCtrls,
  Graphics, Dialogs, IB, IBExternals, turbocommon;

type
  TParamKind = (pkScalar, pkBlob, pkArray);

  TCopyTableDataFBIntf = class
  private
    FSourceDBIndex: Integer;
    FDestDBIndex: Integer;
    FSourceTable: string;
    FDestTable: string;
    FFieldTransforms: TFieldTransformArray;

    FBatchMemoryMB: Integer;            // Speicher-Budget (Batch-Mode)
    FRowByRowCommitInterval: Integer;   // Commit-Intervall (Row-by-Row-Mode)
    FForceRowByRow: Boolean;

    FFromRow: Integer;
    FToRow: Integer;
    FCancelled: Boolean;
    FStatistics: TTransferStatistic;

    FSrcAttach: IAttachment;
    FSrcTrans:  ITransaction;
    FDstAttach: IAttachment;
    FDstTrans:  ITransaction;

    FParamKinds:      array of TParamKind;
    FUnquotedNames:   array of string;

    FBtnCancelRef: TButton;
    procedure CancelClick(Sender: TObject);

    function  CountRows: Int64;
    function  OpenConnection(ADBIndex: Integer): IAttachment;
    function  EstimateRowSize: Int64;
    function  HasBlobOrArray: Boolean;

    procedure CopyRow(ASrcCursor: IResultSet; ADstStmt: IStatement);
    procedure CopyScalarParam(ASrcData: ISQLData; ADstParam: ISQLParam);
    procedure CopyBlobParam(ASrcData: ISQLData; ADstParam: ISQLParam;
                            const AColumnName: string);
    procedure CopyArrayParam(ASrcData: ISQLData; ADstParam: ISQLParam;
                             const AColumnName: string);
  public
    constructor Create(
      ASourceDBIndex, ADestDBIndex: Integer;
      const ASourceTable, ADestTable: string;
      const AFieldTransforms: TFieldTransformArray;
      ABatchMemoryMB: Integer;
      AFromRow: Integer;
      AToRow: Integer;
      AForceRowByRow: Boolean;
      ARowByRowCommitInterval: Integer);

    destructor Destroy; override;

    function Execute: Boolean;

    property Statistics: TTransferStatistic read FStatistics;
    property Cancelled: Boolean read FCancelled write FCancelled;
  end;

implementation

const
  // Batch-Mode: FBIntf-Cap (MWA Guide, Issue 1.12)
  MAX_BATCH_BUFFER_MB = 256;
  MIN_BATCH_BUFFER_MB = 16;
  // Batch-Zeilen-Sicherheitsnetz
  MAX_BATCH_ROWS      = 5000000;
  // FBIntf-Overhead-Faktor (FBIntf rechnet intern mit mehr als wir)
  ROW_SIZE_SAFETY     = 2;
  // Retry-Versuche bei SetBatchRowLimit (Buffer-Overflow)
  MAX_BATCH_RETRIES   = 10;

// ============================================================
// Zeilengrößen-Schätzung aus Firebird-Typ-String
// (nur für Batch-Mode: BatchRowLimit-Berechnung)
// ============================================================
function EstimateSizeFromType(const ATypeStr: string): Int64;
var
  S: string;
  P1, P2: Integer;
  N: Integer;
begin
  S := UpperCase(Trim(ATypeStr));

  // Arrays und BLOBs: konservativ (werden ohnehin Row-by-Row)
  if Pos('[', S) > 0 then Exit(1024 * 1024);
  if Pos('BLOB', S) > 0 then Exit(1024 * 1024);

  if Pos('VARCHAR', S) > 0 then
  begin
    P1 := Pos('(', S); P2 := Pos(')', S);
    if (P1 > 0) and (P2 > P1) then
    begin
      N := StrToIntDef(Trim(Copy(S, P1 + 1, P2 - P1 - 1)), 255);
      Exit(N + 4);
    end;
    Exit(259);
  end;

  if Pos('CHAR', S) > 0 then
  begin
    P1 := Pos('(', S); P2 := Pos(')', S);
    if (P1 > 0) and (P2 > P1) then
    begin
      N := StrToIntDef(Trim(Copy(S, P1 + 1, P2 - P1 - 1)), 1);
      Exit(N + 4);
    end;
    Exit(5);
  end;

  if (Pos('NUMERIC', S) > 0) or (Pos('DECIMAL', S) > 0) then
  begin
    P1 := Pos('(', S);
    if P1 > 0 then
    begin
      P2 := Pos(',', S);
      if P2 > P1 then
        N := StrToIntDef(Trim(Copy(S, P1 + 1, P2 - P1 - 1)), 18)
      else
      begin
        P2 := Pos(')', S);
        N := StrToIntDef(Trim(Copy(S, P1 + 1, P2 - P1 - 1)), 18);
      end;
      if N <= 4  then Exit(2);
      if N <= 9  then Exit(4);
      if N <= 18 then Exit(8);
      Exit(16);
    end;
    Exit(8);
  end;

  if Pos('SMALLINT', S) > 0  then Exit(2);
  if Pos('BIGINT', S) > 0    then Exit(8);
  if Pos('INT128', S) > 0    then Exit(16);
  if Pos('DECFLOAT', S) > 0  then Exit(8);
  if Pos('INTEGER', S) > 0   then Exit(4);
  if Pos('DOUBLE', S) > 0    then Exit(8);
  if Pos('FLOAT', S) > 0     then Exit(4);
  if Pos('TIMESTAMP', S) > 0 then Exit(12);
  if Pos('DATE', S) > 0      then Exit(4);
  if Pos('TIME', S) > 0      then Exit(8);
  if Pos('BOOLEAN', S) > 0   then Exit(1);

  Result := 128;
end;

procedure TCopyTableDataFBIntf.CancelClick(Sender: TObject);
begin
  FCancelled := True;
  if Assigned(FBtnCancelRef) then
  begin
    FBtnCancelRef.Enabled := False;
    FBtnCancelRef.Caption := 'Cancelling...';
  end;
  Application.ProcessMessages;
end;

function TCopyTableDataFBIntf.CountRows: Int64;
var
  Q: IResultSet;
  ActualCount: Int64;
begin
  Result := 0;

  try
    Q := FSrcAttach.OpenCursor(FSrcTrans,
      AnsiString('SELECT COUNT(*) FROM ' + FSourceTable), 3);
    if Q.FetchNext then
      ActualCount := Q.Data[0].AsInt64
    else
      ActualCount := 0;
    Q.Close;
  except
    ActualCount := 0;
  end;

  if FToRow > 0 then
  begin
    if FToRow > ActualCount then FToRow := ActualCount;
    if FFromRow > FToRow then Exit(0);
    Result := FToRow - FFromRow + 1;
  end
  else
  begin
    if FFromRow > ActualCount then Exit(0);
    FToRow := ActualCount;
    Result := FToRow - FFromRow + 1;
  end;
end;

function TCopyTableDataFBIntf.OpenConnection(ADBIndex: Integer): IAttachment;
var
  Rec: TRegisteredDatabase;
  DPB: IDPB;
  Pwd: string;
begin
  Rec := RegisteredDatabases[ADBIndex].RegRec;

  Pwd := Rec.Password;
  if Pwd = '' then
    Pwd := GetDBSessionPassword(Rec.ServerName, Rec.DatabaseName);
  if (Pwd = '') and Rec.IsEmbedded then
    Pwd := 'embedded_local';

  DPB := FirebirdAPI.AllocateDPB;
  DPB.Add(isc_dpb_user_name).AsString := AnsiString(Rec.UserName);
  if Pwd <> '' then
    DPB.Add(isc_dpb_password).AsString := AnsiString(Pwd);
  if Rec.Role <> '' then
    DPB.Add(isc_dpb_sql_role_name).AsString := AnsiString(Rec.Role);
  if Rec.Charset <> '' then
    DPB.Add(isc_dpb_lc_ctype).AsString := AnsiString(Rec.Charset);

  Result := FirebirdAPI.OpenDatabase(AnsiString(Rec.DatabaseName), DPB);
end;

function TCopyTableDataFBIntf.HasBlobOrArray: Boolean;
var
  i: Integer;
  S: string;
begin
  for i := 0 to High(FFieldTransforms) do
  begin
    if not FFieldTransforms[i].CopyField then Continue;
    S := UpperCase(FFieldTransforms[i].DestFieldType);
    if (Pos('[', S) > 0) or (Pos('BLOB', S) > 0) then
      Exit(True);
  end;
  Result := False;
end;

function TCopyTableDataFBIntf.EstimateRowSize: Int64;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(FFieldTransforms) do
  begin
    if not FFieldTransforms[i].CopyField then Continue;
    Result := Result + EstimateSizeFromType(FFieldTransforms[i].DestFieldType);
  end;
  if Result <= 0 then Result := 128;
end;

constructor TCopyTableDataFBIntf.Create(
  ASourceDBIndex, ADestDBIndex: Integer;
  const ASourceTable, ADestTable: string;
  const AFieldTransforms: TFieldTransformArray;
  ABatchMemoryMB: Integer;
  AFromRow: Integer;
  AToRow: Integer;
  AForceRowByRow: Boolean;
  ARowByRowCommitInterval: Integer);
var
  i, k: Integer;
  S: string;
begin
  inherited Create;

  FSourceDBIndex := ASourceDBIndex;
  FDestDBIndex := ADestDBIndex;
  FSourceTable := MakeCaseSensitiveAuto(ASourceTable);
  FDestTable := MakeCaseSensitiveAuto(ADestTable);

  // Batch-Memory clampen (FBIntf-Cap)
  if ABatchMemoryMB < MIN_BATCH_BUFFER_MB then ABatchMemoryMB := MIN_BATCH_BUFFER_MB;
  if ABatchMemoryMB > MAX_BATCH_BUFFER_MB then ABatchMemoryMB := MAX_BATCH_BUFFER_MB;
  FBatchMemoryMB := ABatchMemoryMB;

  // Row-by-Row Commit-Intervall clampen
  FRowByRowCommitInterval := ARowByRowCommitInterval;

  FForceRowByRow := AForceRowByRow;
  FFromRow := AFromRow;
  FToRow := AToRow;
  FCancelled := False;

  SetLength(FFieldTransforms, Length(AFieldTransforms));
  for i := 0 to High(AFieldTransforms) do
    FFieldTransforms[i] := AFieldTransforms[i];

  // Precomputed Mapping: nur tatsächlich zu kopierende Felder
  SetLength(FParamKinds, 0);
  SetLength(FUnquotedNames, 0);
  k := 0;
  for i := 0 to High(FFieldTransforms) do
  begin
    if not FFieldTransforms[i].CopyField then Continue;

    SetLength(FParamKinds, k + 1);
    SetLength(FUnquotedNames, k + 1);

    S := UpperCase(FFieldTransforms[i].DestFieldType);
    if Pos('[', S) > 0 then
      FParamKinds[k] := pkArray
    else if Pos('BLOB', S) > 0 then
      FParamKinds[k] := pkBlob
    else
      FParamKinds[k] := pkScalar;

    FUnquotedNames[k] := StripIdentifierQuotes(FFieldTransforms[i].DestField);
    Inc(k);
  end;

  FSrcAttach := OpenConnection(FSourceDBIndex);
  FSrcTrans  := FSrcAttach.StartTransaction([]);
  FDstAttach := OpenConnection(FDestDBIndex);
  FDstTrans  := FDstAttach.StartTransaction([]);
end;

destructor TCopyTableDataFBIntf.Destroy;
begin
  try
    if Assigned(FSrcTrans) and FSrcTrans.InTransaction then
      FSrcTrans.Rollback;
    if Assigned(FDstTrans) and FDstTrans.InTransaction then
      FDstTrans.Rollback;
  except
  end;

  if Assigned(FSrcAttach) then
  begin
    try FSrcAttach.Disconnect; except end;
  end;
  if Assigned(FDstAttach) then
  begin
    try FDstAttach.Disconnect; except end;
  end;

  FSrcAttach := nil; FSrcTrans := nil;
  FDstAttach := nil; FDstTrans := nil;

  inherited Destroy;
end;

// ============================================================
// SCALAR — funktioniert für alle skalaren Typen
// ============================================================
procedure TCopyTableDataFBIntf.CopyScalarParam(ASrcData: ISQLData;
  ADstParam: ISQLParam);
begin
  if ASrcData.IsNull then
    ADstParam.SetIsNull(True)
  else
    ADstParam.SetAsString(ASrcData.AsString);
end;

// ============================================================
// BLOB — KRITISCH: Stream-basiert, byte-transparent
//
// Gilt für BEIDE Subtypen (BINARY + TEXT):
//   * SaveToStream → TMemoryStream → LoadFromStream
//   * CreateBlob mit RelationName + ColumnName
//     → Subtyp wird aus der Ziel-Spalte übernommen
// ============================================================
procedure TCopyTableDataFBIntf.CopyBlobParam(ASrcData: ISQLData;
  ADstParam: ISQLParam; const AColumnName: string);
var
  SrcBlob, DstBlob: IBlob;
  Stream: TMemoryStream;
begin
  if ASrcData.IsNull then
  begin
    ADstParam.SetIsNull(True);
    Exit;
  end;

  SrcBlob := ASrcData.AsBlob;
  if SrcBlob = nil then
  begin
    ADstParam.SetIsNull(True);
    Exit;
  end;

  Stream := TMemoryStream.Create;
  try
    // 1. Quell-BLOB byte-transparent in den Stream
    SrcBlob.SaveToStream(Stream);
    Stream.Position := 0;

    // 2. Ziel-BLOB anlegen — Subtyp aus Ziel-Spalte
    DstBlob := FDstAttach.CreateBlob(FDstTrans,
      AnsiString(StripIdentifierQuotes(FDestTable)),
      AnsiString(AColumnName));

    if DstBlob = nil then
      raise Exception.Create(
        'FBIntf: Could not create destination blob for column "' +
        AColumnName + '"');

    // 3. Stream in den Ziel-BLOB laden (byte-transparent)
    DstBlob.LoadFromStream(Stream);

    // 4. BLOB-Referenz an den Parameter binden
    ADstParam.SetAsBlob(DstBlob);

  finally
    Stream.Free;
  end;
end;

// ============================================================
// ARRAY — KRITISCH: Element-für-Element, rekursiv
//
// Rekursive Iteration über alle Dimensionen.
// Jedes Element typ-erhaltend via GetAsString/SetAsString.
// ============================================================
procedure TCopyTableDataFBIntf.CopyArrayParam(ASrcData: ISQLData;
  ADstParam: ISQLParam; const AColumnName: string);
var
  SrcArr, DstArr: IArray;
  Bounds: TArrayBounds;
  Coords: array of Integer;
  i, DimCount: Integer;

  procedure CopyDim(dim: Integer);
  var
    Idx: Integer;
  begin
    for Idx := Bounds[dim].LowerBound to Bounds[dim].UpperBound do
    begin
      Coords[dim] := Idx;
      if dim = DimCount - 1 then
        DstArr.SetAsString(Coords, SrcArr.GetAsString(Coords))
      else
        CopyDim(dim + 1);
    end;
  end;

begin
  if ASrcData.IsNull then
  begin
    ADstParam.SetIsNull(True);
    Exit;
  end;

  SrcArr := ASrcData.AsArray;
  if SrcArr = nil then
  begin
    ADstParam.SetIsNull(True);
    Exit;
  end;

  // Ziel-Array anlegen — Firebird setzt Bounds automatisch
  // anhand der Ziel-Spalte. KEIN SetBounds-Aufruf!
  DstArr := FDstAttach.CreateArray(FDstTrans,
    StripIdentifierQuotes(FDestTable), AColumnName);
  if DstArr = nil then
  begin
    ADstParam.SetIsNull(True);
    Exit;
  end;

  Bounds := SrcArr.GetBounds;
  DimCount := Length(Bounds);
  SetLength(Coords, DimCount);

  // Koordinaten für CopyDim vorbereiten
  for i := 0 to DimCount - 1 do
    Coords[i] := Bounds[i].LowerBound;

  // Alle Elemente rekursiv kopieren — mit den QUELLE-Koordinaten.
  // Wenn die Ziel-Spalte kleinere Bounds hat, wird das SetAsString
  // mit einer klaren Exception fehlschlagen — das ist korrekt,
  // dann war die Ziel-Definition unpassend.
  CopyDim(0);

  DstArr.SaveChanges;
  ADstParam.SetAsArray(DstArr);
end;

// ============================================================
// CopyRow — precomputed, kein FFieldTransforms-Scan pro Zeile
// ============================================================
procedure TCopyTableDataFBIntf.CopyRow(ASrcCursor: IResultSet;
  ADstStmt: IStatement);
var
  k: Integer;
  Data: ISQLData;
  Param: ISQLParam;
begin
  for k := 0 to High(FParamKinds) do
  begin
    Data  := ASrcCursor.Data[k];
    Param := ADstStmt.SQLParams[k];

    case FParamKinds[k] of
      pkScalar: CopyScalarParam(Data, Param);
      pkBlob:   CopyBlobParam(Data, Param, FUnquotedNames[k]);
      pkArray:  CopyArrayParam(Data, Param, FUnquotedNames[k]);
    end;
  end;
end;

// ============================================================
// Execute — die eigentliche Kopierlogik
// ============================================================
function TCopyTableDataFBIntf.Execute: Boolean;
var
  SrcCursor: IResultSet;
  DstStmt: IStatement;
  BatchCompletion: IBatchCompletion;
  SelectSQL, InsertSQL: string;
  ColList, InsCols, InsPlaceholders: string;
  i, k: Integer;
  Rows, TotalRows, RowCount: Int64;
  RowsInBatch: Integer;
  UseRange, UseBatch: Boolean;
  StartTime, EndTime, LastUpdateTime: TDateTime;
  FieldName, OptionsTxt: string;
  BatchRowLimit: Integer;
  EstimatedSize: Int64;
  HasProblem: Boolean;
  HasBatchAPI: Boolean;
  FailedRow: Integer;
  FailedStatus: IStatus;
  BatchCount: Int64;
  RetryCount: Integer;
  BatchLimitSet: Boolean;
  StrategyNote: string;
  DestRec: TRegisteredDatabase;

  ProgressForm: TForm;
  LblPhase, LblNote, LblRows, LblElapsed: TLabel;
  ProgressBar: TProgressBar;
  BtnCancel: TButton;

  procedure UpdateProgress;
  var
    ElapsedSec, RowsPerSec: Double;
  begin
    if TotalRows > 0 then
      ProgressBar.Position := Rows;

    ElapsedSec := (Now - StartTime) * 86400;
    if ElapsedSec > 0 then
      RowsPerSec := Rows / ElapsedSec
    else
      RowsPerSec := 0;

    if TotalRows > 0 then
      LblRows.Caption := Format('Copied %s of %s rows',
        [FormatFloat('#,##0', Rows), FormatFloat('#,##0', TotalRows)])
    else
      LblRows.Caption := Format('Copied %s rows', [FormatFloat('#,##0', Rows)]);

    LblElapsed.Caption := 'Elapsed: ' + FormatDateTime('hh:nn:ss', Now - StartTime) +
      '   |   ' + FormatFloat('#,##0', Round(RowsPerSec)) + ' rows/sec';
  end;

  procedure CheckBatchCompletion(const AContext: string);
  begin
    if not Assigned(BatchCompletion) then Exit;
    FailedRow := -1;
    if BatchCompletion.GetErrorStatus(FailedRow, FailedStatus) then
    begin
      if Assigned(FailedStatus) then
        raise Exception.Create(AContext + ' failed at row ' +
          IntToStr(FailedRow) + ': ' + FailedStatus.GetMessage(CP_UTF8));
    end;
  end;

begin
  Result := False;
  FillChar(FStatistics, SizeOf(FStatistics), 0);

  FStatistics.Kind := tkCopy;
  FStatistics.Method := cmFBIntf;
  FStatistics.SourceKind := 'Firebird Table';
  FStatistics.SourceServer := RegisteredDatabases[FSourceDBIndex].RegRec.ServerName;
  FStatistics.SourceDatabase := RegisteredDatabases[FSourceDBIndex].RegRec.Title;
  FStatistics.SourceTable := FSourceTable;
  FStatistics.SourceServerVersion := RegisteredDatabases[FSourceDBIndex].RegRec.ServerVersionString;
  FStatistics.DestKind := 'Firebird Table';
  FStatistics.DestServer := RegisteredDatabases[FDestDBIndex].RegRec.ServerName;
  FStatistics.DestDatabase := RegisteredDatabases[FDestDBIndex].RegRec.Title;
  FStatistics.DestTable := FDestTable;
  FStatistics.DestServerVersion := RegisteredDatabases[FDestDBIndex].RegRec.ServerVersionString;
  FStatistics.FromRow := FFromRow;
  FStatistics.ToRow := FToRow;
  FStatistics.UseRowRange := (FFromRow > 1) or (FToRow > 0);
  FStatistics.FormulaUsed := False;

  // ============================================================
  // Feldlisten aufbauen
  // ============================================================
  ColList := ''; InsCols := ''; InsPlaceholders := '';
  k := 0;
  for i := 0 to High(FFieldTransforms) do
  begin
    if not FFieldTransforms[i].CopyField then Continue;

    FieldName := MakeCaseSensitiveAuto(FFieldTransforms[i].SourceField);
    if ColList <> '' then ColList := ColList + ', ';
    ColList := ColList + FieldName;

    if InsCols <> '' then InsCols := InsCols + ', ';
    InsCols := InsCols + MakeCaseSensitiveAuto(FFieldTransforms[i].DestField);

    if InsPlaceholders <> '' then InsPlaceholders := InsPlaceholders + ', ';
    InsPlaceholders := InsPlaceholders + '?';
    Inc(k);
  end;

  if ColList = '' then Exit;
  FStatistics.FieldsCount := k;

  // ============================================================
  // SELECT- und INSERT-SQL
  // ============================================================
  UseRange := (FFromRow > 1) or (FToRow > 0);
  RowCount := 0;
  if UseRange and (FToRow > 0) then
    RowCount := FToRow - FFromRow + 1;

  if UseRange and (RowCount > 0) then
    SelectSQL := 'SELECT FIRST ' + IntToStr(RowCount) +
                 ' SKIP ' + IntToStr(FFromRow - 1) + ' ' +
                 ColList + ' FROM ' + FSourceTable
  else if FFromRow > 1 then
    SelectSQL := 'SELECT SKIP ' + IntToStr(FFromRow - 1) + ' ' +
                 ColList + ' FROM ' + FSourceTable
  else
    SelectSQL := 'SELECT ' + ColList + ' FROM ' + FSourceTable;

  InsertSQL := 'INSERT INTO ' + FDestTable +
               ' (' + InsCols + ') VALUES (' + InsPlaceholders + ')';

  // ============================================================
  // Strategie entscheiden
  // ============================================================
  HasProblem := HasBlobOrArray;

  HasBatchAPI := False;
  try
    HasBatchAPI := FDstAttach.HasBatchMode;
  except
    HasBatchAPI := False;
  end;

  UseBatch := HasBatchAPI and (not HasProblem) and (not FForceRowByRow);

  // ============================================================
  // Strategie-Notiz (für Progress-Fenster)
  // ============================================================
  StrategyNote := '';

  if not UseBatch then
  begin
    DestRec := RegisteredDatabases[FDestDBIndex].RegRec;

    if FForceRowByRow then
      StrategyNote := 'Row-by-Row mode forced by user setting'

    else if HasProblem then
      StrategyNote := 'BLOB/ARRAY detected in selection — auto-switched to FBIntf Row-by-Row mode'

    else if not HasBatchAPI then
    begin
      if (DestRec.ServerVersionMajor > 0) then
        StrategyNote := Format('No Batch API on %s (Firebird %d.%d) — auto-switched to FBIntf Row-by-Row mode',
          [DestRec.ServerName, DestRec.ServerVersionMajor, DestRec.ServerVersionMinor])
      else
        StrategyNote := Format('No Batch API on %s — auto-switched to FBIntf Row-by-Row mode',
          [DestRec.ServerName]);
    end

    else
      StrategyNote := 'Batch buffer rejected by server — auto-switched to FBIntf Row-by-Row mode';
  end;

  // ============================================================
  // Zeilengröße für Batch-Berechnung (nur im Batch-Mode relevant)
  // ============================================================
  EstimatedSize := EstimateRowSize;

  // ============================================================
  // Progress-Fenster
  // ============================================================
  ProgressForm := TForm.Create(nil);
  try
    ProgressForm.FormStyle := fsNormal;
    ProgressForm.Caption := 'Copying data (FBIntf)...';
    ProgressForm.Width := 520;
    ProgressForm.Height := 280;
    ProgressForm.Position := poScreenCenter;
    ProgressForm.BorderStyle := bsDialog;

    LblPhase := TLabel.Create(ProgressForm);
    LblPhase.Parent := ProgressForm;
    LblPhase.Left := 16; LblPhase.Top := 16;
    LblPhase.Caption := 'Counting records...';
    LblPhase.Font.Style := [fsBold]; LblPhase.Font.Size := 10;
    LblPhase.Width := 480;

    LblNote := TLabel.Create(ProgressForm);
    LblNote.Parent := ProgressForm;
    LblNote.Left := 16; LblNote.Top := 40;
    LblNote.Caption := StrategyNote;
    LblNote.Font.Color := clGray;
    LblNote.Width := 480;
    LblNote.AutoSize := False;
    LblNote.Height := 18;
    LblNote.Visible := StrategyNote <> '';

    LblRows := TLabel.Create(ProgressForm);
    LblRows.Parent := ProgressForm;
    LblRows.Left := 16;
    if StrategyNote <> '' then
      LblRows.Top := 64
    else
      LblRows.Top := 42;
    LblRows.Caption := 'Please wait...';
    LblRows.Width := 480;

    ProgressBar := TProgressBar.Create(ProgressForm);
    ProgressBar.Parent := ProgressForm;
    ProgressBar.Left := 16;
    if StrategyNote <> '' then
      ProgressBar.Top := 90
    else
      ProgressBar.Top := 70;
    ProgressBar.Width := 480; ProgressBar.Height := 20;
    ProgressBar.Min := 0; ProgressBar.Max := 100;
    ProgressBar.Style := pbstMarquee;

    LblElapsed := TLabel.Create(ProgressForm);
    LblElapsed.Parent := ProgressForm;
    LblElapsed.Left := 16;
    if StrategyNote <> '' then
      LblElapsed.Top := 118
    else
      LblElapsed.Top := 100;
    LblElapsed.Caption := 'Elapsed: 00:00:00';
    LblElapsed.Width := 480;

    BtnCancel := TButton.Create(ProgressForm);
    BtnCancel.Parent := ProgressForm;
    BtnCancel.Caption := 'Cancel';
    BtnCancel.Left := 200;
    if StrategyNote <> '' then
      BtnCancel.Top := 170
    else
      BtnCancel.Top := 150;
    BtnCancel.Width := 100;
    FBtnCancelRef := BtnCancel;
    BtnCancel.OnClick := @CancelClick;

    ProgressForm.Show;
    ProgressForm.BringToFront;
    Application.ProcessMessages;
    Sleep(50);
    Application.ProcessMessages;

    // ============================================================
    // Zählen
    // ============================================================
    TotalRows := CountRows;
    FStatistics.ToRow := FFromRow + TotalRows - 1;

    if UseBatch then
    begin
      BatchRowLimit := (Int64(FBatchMemoryMB) * 1024 * 1024)
                       div (EstimatedSize * ROW_SIZE_SAFETY);
      if BatchRowLimit < 1 then BatchRowLimit := 1;
      if BatchRowLimit > MAX_BATCH_ROWS then BatchRowLimit := MAX_BATCH_ROWS;
      LblPhase.Caption := Format('Preparing batch copy (%d MB Buffer Memory)...',
        [FBatchMemoryMB]);
    end
    else
    begin
      BatchRowLimit := 0;
      LblPhase.Caption := 'Preparing row-by-row copy...';
    end;

    LblRows.Caption := Format('Total: %s rows', [FormatFloat('#,##0', TotalRows)]);
    ProgressBar.Style := pbstNormal;
    if TotalRows > 0 then ProgressBar.Max := TotalRows
    else ProgressBar.Max := 100;
    ProgressBar.Position := 0;
    Application.ProcessMessages;

    // ============================================================
    // Cursor + Statement
    // ============================================================
    SrcCursor := FSrcAttach.OpenCursor(FSrcTrans, AnsiString(SelectSQL), 3);
    try
      if not SrcCursor.FetchNext then
      begin
        Result := True;
        Exit;
      end;

      DstStmt := FDstAttach.Prepare(FDstTrans, AnsiString(InsertSQL));
      try
        DstStmt.Prepare;

        // ============================================================
        // SetBatchRowLimit mit Retry-Loop
        // ============================================================
        RetryCount := 0;
        BatchLimitSet := False;

        if UseBatch then
        begin
          while RetryCount <= MAX_BATCH_RETRIES do
          begin
            try
              DstStmt.SetBatchRowLimit(BatchRowLimit);
              BatchLimitSet := True;
              Break;
            except
              on E: Exception do
              begin
                Inc(RetryCount);
                if (BatchRowLimit <= 1) or (RetryCount > MAX_BATCH_RETRIES) then
                  Break;
                BatchRowLimit := BatchRowLimit div 2;
                if BatchRowLimit < 1 then BatchRowLimit := 1;
              end;
            end;
          end;

          if not BatchLimitSet then
          begin
            UseBatch := False;
            LblNote.Caption := 'Batch buffer rejected by server — auto-switched to FBIntf Row-by-Row mode';
            LblNote.Visible := True;
          end;
        end;

        // ---- Finale Strategie-Anzeige ----
        if UseBatch then
        begin
          BatchCount := (TotalRows + BatchRowLimit - 1) div BatchRowLimit;
          LblPhase.Caption := Format('Copying data (batch, %d MB, %s rows/batch, %d batches)...',
            [FBatchMemoryMB, FormatFloat('#,##0', BatchRowLimit), BatchCount]);
        end
        else
          LblPhase.Caption := 'Copying data (row-by-row)...';
        Application.ProcessMessages;

        Rows := 0;
        RowsInBatch := 0;
        StartTime := Now;
        LastUpdateTime := StartTime;
        BatchCompletion := nil;

        try
          if UseBatch then
          begin
            // ============================================================
            // BATCH MODE
            // ============================================================
            repeat
              if FCancelled then Break;

              CopyRow(SrcCursor, DstStmt);
              DstStmt.AddToBatch;
              Inc(Rows);
              Inc(RowsInBatch);

              if RowsInBatch >= BatchRowLimit then
              begin
                LblPhase.Caption := Format('Executing batch (%s/%s rows)...',
                  [FormatFloat('#,##0', Rows), FormatFloat('#,##0', TotalRows)]);
                Application.ProcessMessages;

                BatchCompletion := DstStmt.ExecuteBatch;
                CheckBatchCompletion('Batch execution');

                if FDstTrans.InTransaction then
                  FDstTrans.CommitRetaining;

                RowsInBatch := 0;

                LblPhase.Caption := Format('Copying data (batch, %d MB, %s rows/batch)...',
                  [FBatchMemoryMB, FormatFloat('#,##0', BatchRowLimit)]);
                Application.ProcessMessages;
              end;

              if (Now - LastUpdateTime) * 86400 > 0.15 then
              begin
                UpdateProgress;
                LastUpdateTime := Now;
                Application.ProcessMessages;
              end;
            until not SrcCursor.FetchNext;

            if (RowsInBatch > 0) and (not FCancelled) then
            begin
              LblPhase.Caption := 'Executing final batch...';
              Application.ProcessMessages;

              BatchCompletion := DstStmt.ExecuteBatch;
              CheckBatchCompletion('Final batch execution');
            end;

            if not FCancelled then
              FDstTrans.Commit
            else if FDstTrans.InTransaction then
              FDstTrans.Rollback;

            Result := True;
          end
          else
          begin
            // ============================================================
            // ROW-BY-ROW MODE
            // Commit-Intervall kommt direkt aus der GUI
            // ============================================================
            repeat
              if FCancelled then Break;
              CopyRow(SrcCursor, DstStmt);
              DstStmt.Execute;
              Inc(Rows);

              if (Rows mod FRowByRowCommitInterval = 0) and FDstTrans.InTransaction then
                FDstTrans.CommitRetaining;

              if (Now - LastUpdateTime) * 86400 > 0.15 then
              begin
                UpdateProgress;
                LastUpdateTime := Now;
                Application.ProcessMessages;
              end;
            until not SrcCursor.FetchNext;

            if not FCancelled then
              FDstTrans.Commit
            else if FDstTrans.InTransaction then
              FDstTrans.Rollback;

            Result := True;
          end;
        except
          on E: Exception do
          begin
            if FDstTrans.InTransaction then
              FDstTrans.Rollback;
            raise;
          end;
        end;

        UpdateProgress;
        LblPhase.Caption := 'Finalizing...';
        Application.ProcessMessages;

        FStatistics.RowsProcessed := Rows;
        FStatistics.BatchSize := FBatchMemoryMB;

        // ============================================================
        // Options-Extra für den Report
        // ============================================================
        if UseBatch then
        begin
          OptionsTxt := Format('Batch mode, %d MB Buffer Memory, %s rows/batch',
            [FBatchMemoryMB, FormatFloat('#,##0', BatchRowLimit)]);
          if RetryCount > 0 then
            OptionsTxt := OptionsTxt + Format(' (%d retr%s)',
              [RetryCount, BoolToStr(RetryCount = 1, 'y', 'ies')]);
        end
        else
        begin
          if FForceRowByRow then
            OptionsTxt := Format('Row-by-Row (forced), commit every %s',
              [FormatFloat('#,##0', FRowByRowCommitInterval)])
          else if HasProblem then
            OptionsTxt := Format('Row-by-Row (BLOB/ARRAY), commit every %s',
              [FormatFloat('#,##0', FRowByRowCommitInterval)])
          else
            OptionsTxt := Format('Row-by-Row (no Batch API), commit every %s',
              [FormatFloat('#,##0', FRowByRowCommitInterval)]);
        end;

        if FCancelled then
          OptionsTxt := OptionsTxt + ' (CANCELLED)';

        FStatistics.OptionsExtra := OptionsTxt;

      finally
        DstStmt := nil;
      end;
    finally
      if Assigned(SrcCursor) then
        SrcCursor.Close;
    end;

    EndTime := Now;
    FStatistics.ElapsedSeconds := (EndTime - StartTime) * SecsPerDay;

    Sleep(80);
    Application.ProcessMessages;

  finally
    FBtnCancelRef := nil;
    ProgressForm.Free;
  end;
end;

end.
