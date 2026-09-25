unit uCopyTableDataLocal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls, Dialogs,
  Graphics,
  IBDatabase, IBQuery, DateUtils,
  turbocommon,
  uCopyStatistics;

type

  { TCopyThreadLocal }

  TCopyThreadLocal = class(TThread)
  private
    FSourceDB      : TIBDatabase;
    FSourceTrans   : TIBTransaction;
    FDestDB        : TIBDatabase;
    FDestTrans     : TIBTransaction;
    FSourceTable   : string;
    FDestTable     : string;
    FFieldTransforms : TFieldTransformArray;
    FBatchSize     : Integer;
    FTotalRows     : Integer;
    FFromRow       : Integer;
    FCopiedRows    : Integer;
    FStartTime     : TDateTime;
    FErrorMessage  : string;

    FProgressLabel : TLabel;
    FProgressBar   : TProgressBar;
    FLblElapsed    : TLabel;
    FBtnCancel     : TButton;

    procedure BuildInsertSQL(FromRow, BatchRows: Integer; out SQL: string);
    procedure UpdateProgressGUI;
  protected
    procedure Execute; override;
  public
    Cancelled : Boolean;

    constructor Create(
      ASourceDB, ADestDB : TIBDatabase;
      ASourceTrans, ADestTrans : TIBTransaction;
      const ASourceTable, ADestTable : string;
      const AFieldTransforms : TFieldTransformArray;
      ABatchSize, ATotalRows, AFromRow : Integer);

    destructor Destroy; override;

    procedure SetProgressControls(
      AProgressLabel : TLabel;
      AProgressBar   : TProgressBar;
      ALblElapsed    : TLabel;
      ABtnCancel     : TButton);

    property CopiedRows : Integer read FCopiedRows;
    property StartTime  : TDateTime read FStartTime;
    property ErrorMessage : string read FErrorMessage;
  end;

  { TCopyTableDataLocal }

  TCopyTableDataLocal = class
  private
    FSourceDBIndex : Integer;
    FDestDBIndex   : Integer;
    FSourceTable   : string;
    FDestTable     : string;
    FFieldTransforms : array of TFieldTransform;
    FBatchSize     : Integer;
    FFromRow       : Integer;
    FToRow         : Integer;
    FTotalRows     : Integer;
    FCopiedRows    : Integer;
    FStartTime     : TDateTime;
    FCancelled     : Boolean;
    FThread        : TCopyThreadLocal;

    // Form-eigene Verbindungen (optional – wenn nil, Fallback auf RegisteredDatabases)
    FSourceDB      : TIBDatabase;
    FSourceTrans   : TIBTransaction;
    FDestDB        : TIBDatabase;
    FDestTrans     : TIBTransaction;

    FStatistics: TCopyStatistics;

    function  GetSourceDB : TIBDatabase;
    function  GetSourceTrans : TIBTransaction;
    function  GetDestDB : TIBDatabase;
    function  GetDestTrans : TIBTransaction;
    procedure CancelButtonClick(Sender: TObject);
  public
    constructor Create(
      ASourceDBIndex, ADestDBIndex : Integer;
      const ASourceTable, ADestTable : string;
      const AFieldTransforms : TFieldTransformArray;
      ABatchSize : Integer = 500000;
      AFromRow   : Integer = 1;
      AToRow     : Integer = 0;
      ASourceDB  : TIBDatabase = nil;
      ASourceTrans : TIBTransaction = nil;
      ADestDB    : TIBDatabase = nil;
      ADestTrans : TIBTransaction = nil);

    destructor Destroy; override;

    function Execute : Boolean;

    property TotalRows : Integer read FTotalRows;
    property CopiedRows : Integer read FCopiedRows;
    property Statistics: TCopyStatistics read FStatistics;
  end;

implementation


{ TCopyThreadLocal }

constructor TCopyThreadLocal.Create(
  ASourceDB, ADestDB : TIBDatabase;
  ASourceTrans, ADestTrans : TIBTransaction;
  const ASourceTable, ADestTable : string;
  const AFieldTransforms : TFieldTransformArray;
  ABatchSize, ATotalRows, AFromRow : Integer);
var
  i : Integer;
begin
  inherited Create(True);
  FreeOnTerminate := False;

  FSourceDB := ASourceDB;
  FSourceTrans := ASourceTrans;
  FDestDB := ADestDB;
  FDestTrans := ADestTrans;
  FSourceTable := MakeCaseSensitiveAuto(ASourceTable);
  FDestTable := MakeCaseSensitiveAuto(ADestTable);
  FBatchSize := ABatchSize;
  FTotalRows := ATotalRows;
  FFromRow := AFromRow;
  FCopiedRows := 0;
  Cancelled := False;

  SetLength(FFieldTransforms, Length(AFieldTransforms));
  for i := 0 to High(AFieldTransforms) do
    FFieldTransforms[i] := AFieldTransforms[i];

  FProgressLabel := nil;
  FProgressBar := nil;
  FLblElapsed := nil;
  FBtnCancel := nil;
end;

destructor TCopyThreadLocal.Destroy;
begin
  inherited Destroy;
end;

procedure TCopyThreadLocal.SetProgressControls(
  AProgressLabel : TLabel;
  AProgressBar   : TProgressBar;
  ALblElapsed    : TLabel;
  ABtnCancel     : TButton);
begin
  FProgressLabel := AProgressLabel;
  FProgressBar   := AProgressBar;
  FLblElapsed    := ALblElapsed;
  FBtnCancel     := ABtnCancel;
end;

procedure TCopyThreadLocal.BuildInsertSQL(FromRow, BatchRows: Integer; out SQL: string);
var
  DestFields, SourceFields : string;
  i : Integer;
  FormulaExpr : string;
begin
  DestFields := '';
  SourceFields := '';

  for i := 0 to High(FFieldTransforms) do
  begin
    if not FFieldTransforms[i].CopyField then
      Continue;

    // Zielfeld
    if DestFields <> '' then
      DestFields := DestFields + ', ';
    DestFields := DestFields + FFieldTransforms[i].DestField;

    // Quellfeld (mit oder ohne Formel)
    if SourceFields <> '' then
      SourceFields := SourceFields + ', ';

    if FFieldTransforms[i].Formula = '' then
      SourceFields := SourceFields + FFieldTransforms[i].SourceField
    else
    begin
      // $1 durch den Quell-Spaltennamen ersetzen
      FormulaExpr := StringReplace(FFieldTransforms[i].Formula, '$1',
                                   FFieldTransforms[i].SourceField, [rfReplaceAll]);
      SourceFields := SourceFields + FormulaExpr;
    end;
  end;

  SQL := 'INSERT INTO ' + FDestTable + ' (' + DestFields + ')' + sLineBreak +
         'SELECT FIRST ' + IntToStr(BatchRows) +
         ' SKIP ' + IntToStr(FromRow - 1) + ' ' +
         SourceFields + sLineBreak +
         'FROM ' + FSourceTable;
end;

procedure TCopyThreadLocal.UpdateProgressGUI;
var
  Elapsed: TDateTime;
  ElapsedSeconds: Double;
  RowsPerSec: Double;
begin
  if Assigned(FProgressLabel) then
    FProgressLabel.Caption := Format('Copied %d of %d rows...', [FCopiedRows, FTotalRows]);
  if Assigned(FProgressBar) then
    FProgressBar.Position := FCopiedRows;

  if Assigned(FLblElapsed) then
  begin
    Elapsed := Now - FStartTime;
    ElapsedSeconds := Elapsed * 24 * 60 * 60;

    if ElapsedSeconds > 0 then
      RowsPerSec := FCopiedRows / ElapsedSeconds
    else
      RowsPerSec := 0;

    FLblElapsed.Caption := Format('Elapsed: %s   |   %.0f rows/sec',
      [FormatDateTime('hh:nn:ss', Elapsed), RowsPerSec]);
  end;

  Application.ProcessMessages;
end;

procedure TCopyThreadLocal.Execute;
var
  DestQuery : TIBQuery;
  HasFormula : Boolean;
  BatchCount, BatchIndex : Integer;
  FromRow, ToRow, BatchRows : Integer;
  SQL : string;
  i : Integer;
begin
  try
    // Prüfen, ob überhaupt Formeln vorhanden sind
    HasFormula := False;
    for i := 0 to High(FFieldTransforms) do
    begin
      if (FFieldTransforms[i].CopyField) and (FFieldTransforms[i].Formula <> '') then
      begin
        HasFormula := True;
        Break;
      end;
    end;

    DestQuery := TIBQuery.Create(nil);

    try
      DestQuery.Database := FDestDB;
      DestQuery.Transaction := FDestTrans;
      DestQuery.AllowAutoActivateTransaction := true;

      BatchCount := (FTotalRows + FBatchSize - 1) div FBatchSize;
      FStartTime := Now;

      for BatchIndex := 0 to BatchCount - 1 do
      begin
        if Cancelled then Break;

        FromRow := FFromRow + (BatchIndex * FBatchSize);
        ToRow := FromRow + FBatchSize - 1;
        if ToRow > (FFromRow + FTotalRows - 1) then
          ToRow := FFromRow + FTotalRows - 1;
        BatchRows := ToRow - FromRow + 1;

        // SQL generieren (mit oder ohne Formeln)
        BuildInsertSQL(FromRow, BatchRows, SQL);

        DestQuery.Close;
        DestQuery.SQL.Text := SQL;

        if not FDestTrans.InTransaction then
          FDestTrans.StartTransaction;

        DestQuery.ExecSQL;
        FDestTrans.CommitRetaining;

        Inc(FCopiedRows, BatchRows);
        Synchronize(@UpdateProgressGUI);
      end;

      if not Cancelled then
        FDestTrans.Commit;

    finally
      DestQuery.Free;
    end;

  except
    on E: Exception do
    begin
      FErrorMessage := E.Message;
    end;
  end;
end;

{ TCopyTableDataLocal }

constructor TCopyTableDataLocal.Create(
  ASourceDBIndex, ADestDBIndex : Integer;
  const ASourceTable, ADestTable : string;
  const AFieldTransforms : TFieldTransformArray;
  ABatchSize : Integer;
  AFromRow : Integer;
  AToRow : Integer;
  ASourceDB : TIBDatabase;
  ASourceTrans : TIBTransaction;
  ADestDB : TIBDatabase;
  ADestTrans : TIBTransaction);
var
  i : Integer;
begin
  inherited Create;

  FSourceDBIndex := ASourceDBIndex;
  FDestDBIndex   := ADestDBIndex;
  FSourceTable   := ASourceTable;
  FDestTable     := ADestTable;
  FBatchSize     := ABatchSize;
  FFromRow       := AFromRow;
  FToRow         := AToRow;
  FThread        := nil;
  FCancelled     := False;
  FCopiedRows    := 0;
  FTotalRows     := 0;

  // Form-eigene Verbindungen speichern (können nil sein → Fallback auf RegisteredDatabases)
  FSourceDB      := ASourceDB;
  FSourceTrans   := ASourceTrans;
  FDestDB        := ADestDB;
  FDestTrans     := ADestTrans;

  SetLength(FFieldTransforms, Length(AFieldTransforms));
  for i := 0 to High(AFieldTransforms) do
    FFieldTransforms[i] := AFieldTransforms[i];
end;

destructor TCopyTableDataLocal.Destroy;
begin
  if Assigned(FThread) then
  begin
    FThread.Cancelled := True;
    FThread.WaitFor;
    FThread.Free;
  end;
  inherited Destroy;
end;

function TCopyTableDataLocal.GetSourceDB : TIBDatabase;
begin
  if Assigned(FSourceDB) then
    Result := FSourceDB
  else
    Result := RegisteredDatabases[FSourceDBIndex].IBDatabase;
end;

function TCopyTableDataLocal.GetSourceTrans : TIBTransaction;
begin
  if Assigned(FSourceTrans) then
    Result := FSourceTrans
  else
    Result := RegisteredDatabases[FSourceDBIndex].IBTransaction;
end;

function TCopyTableDataLocal.GetDestDB : TIBDatabase;
begin
  if Assigned(FDestDB) then
    Result := FDestDB
  else
    Result := RegisteredDatabases[FDestDBIndex].IBDatabase;
end;

function TCopyTableDataLocal.GetDestTrans : TIBTransaction;
begin
  if Assigned(FDestTrans) then
    Result := FDestTrans
  else
    Result := RegisteredDatabases[FDestDBIndex].IBTransaction;
end;

procedure TCopyTableDataLocal.CancelButtonClick(Sender: TObject);
begin
  FCancelled := True;
  if Assigned(FThread) then
    FThread.Cancelled := True;
  if Sender is TButton then
  begin
    TButton(Sender).Enabled := False;
    TButton(Sender).Caption := 'Cancelling...';
  end;
end;

function TCopyTableDataLocal.Execute : Boolean;
var
  CountQuery : TIBQuery;
  TotalInSource : Integer;
  ProgressForm : TForm;
  LblPhase : TLabel;          // ← NEU: Phase-Label
  ProgressLabel : TLabel;
  ProgressBar : TProgressBar;
  LblElapsed : TLabel;
  BtnCancel : TButton;
  EndTime : TDateTime;
  RowsPerSec : Double;
  StatusStr : string;
  Msg : string;
  ErrorMsg : string;
begin
  Result := False;
  FCopiedRows := 0;
  FCancelled := False;

  // ------------------------------------------------------------------
  // Progress-Fenster
  // ------------------------------------------------------------------
  ProgressForm := TForm.Create(nil);
  try
    ProgressForm.FormStyle := fsNormal;
    ProgressForm.Caption := 'Copying data...';
    ProgressForm.Width := 520;
    ProgressForm.Height := 260;
    ProgressForm.Position := poScreenCenter;
    ProgressForm.BorderStyle := bsDialog;

    // === Phase-Label (oberste Zeile, fett) ===
    LblPhase := TLabel.Create(ProgressForm);
    LblPhase.Parent := ProgressForm;
    LblPhase.Left := 16;
    LblPhase.Top := 16;
    LblPhase.Caption := 'Preparing...';
    LblPhase.Font.Style := [fsBold];
    LblPhase.Font.Size := 10;
    LblPhase.Width := 480;

    // === Zeilen-Info (zweite Zeile) ===
    ProgressLabel := TLabel.Create(ProgressForm);
    ProgressLabel.Parent := ProgressForm;
    ProgressLabel.Left := 16;
    ProgressLabel.Top := 42;
    ProgressLabel.Caption := 'Please wait...';
    ProgressLabel.Width := 480;

    // === Progressbar (dritte Zeile) ===
    ProgressBar := TProgressBar.Create(ProgressForm);
    ProgressBar.Parent := ProgressForm;
    ProgressBar.Left := 16;
    ProgressBar.Top := 70;
    ProgressBar.Width := 480;
    ProgressBar.Height := 20;
    ProgressBar.Min := 0;
    ProgressBar.Max := 100;
    ProgressBar.Style := pbstMarquee;

    // === Elapsed-Label (vierte Zeile) ===
    LblElapsed := TLabel.Create(ProgressForm);
    LblElapsed.Parent := ProgressForm;
    LblElapsed.Left := 16;
    LblElapsed.Top := 100;
    LblElapsed.Caption := 'Elapsed: 00:00:00';
    LblElapsed.Width := 480;

    // === Cancel-Button ===
    BtnCancel := TButton.Create(ProgressForm);
    BtnCancel.Parent := ProgressForm;
    BtnCancel.Caption := 'Cancel';
    BtnCancel.Left := 200;
    BtnCancel.Top := 140;
    BtnCancel.Width := 100;
    BtnCancel.Enabled := False;
    BtnCancel.OnClick := @CancelButtonClick;

    // === SOFORT SICHTBAR ===
    ProgressForm.Show;
    ProgressForm.BringToFront;
    Application.ProcessMessages;
    Sleep(100);
    Application.ProcessMessages;

    // ------------------------------------------------------------------
    // Phase 1: Datensätze zählen
    // ------------------------------------------------------------------
    LblPhase.Caption := 'Counting records...';
    ProgressLabel.Caption := 'Please wait...';
    Application.ProcessMessages;

    CountQuery := TIBQuery.Create(nil);
    try
      CountQuery.Database := GetSourceDB;
      CountQuery.Transaction := GetSourceTrans;
      CountQuery.AllowAutoActivateTransaction := True;
      CountQuery.SQL.Text := 'SELECT COUNT(*) FROM ' + FSourceTable;
      CountQuery.Open;
      TotalInSource := CountQuery.Fields[0].AsInteger;
      CountQuery.Close;
    finally
      CountQuery.Free;
    end;

    if TotalInSource = 0 then
    begin
      ProgressForm.Close;
      ShowMessage('Source table is empty. Nothing to copy.');
      Exit;
    end;

    if FToRow = 0 then
      FToRow := TotalInSource;
    if FFromRow < 1 then
      FFromRow := 1;
    if FToRow > TotalInSource then
      FToRow := TotalInSource;

    FTotalRows := FToRow - FFromRow + 1;

    // ------------------------------------------------------------------
    // Phase 2: Vorbereitung
    // ------------------------------------------------------------------
    LblPhase.Caption := 'Preparing copy...';
    ProgressLabel.Caption := Format('Total Records: %s', [FormatFloat('#,##0', FTotalRows)]);
    ProgressBar.Style := pbstNormal;
    ProgressBar.Max := FTotalRows;
    ProgressBar.Position := 0;
    BtnCancel.Enabled := True;
    Application.ProcessMessages;

    // ------------------------------------------------------------------
    // Phase 3: Kopieren
    // ------------------------------------------------------------------
    LblPhase.Caption := 'Copying data...';
    Application.ProcessMessages;

    // ------------------------------------------------------------------
    // Thread starten
    // ------------------------------------------------------------------
    FThread := TCopyThreadLocal.Create(
      GetSourceDB, GetDestDB,
      GetSourceTrans, GetDestTrans,
      FSourceTable, FDestTable,
      FFieldTransforms,
      FBatchSize, FTotalRows, FFromRow
    );

    FThread.SetProgressControls(ProgressLabel, ProgressBar, LblElapsed, BtnCancel);
    FThread.Start;

    while (not FThread.Finished) and (ProgressForm.Visible) do
    begin
      Application.ProcessMessages;
      Sleep(50);
    end;

    if not FThread.Finished then
    begin
      FThread.Cancelled := True;
      FThread.WaitFor;
    end;

    FCopiedRows := FThread.CopiedRows;
    FStartTime := FThread.StartTime;
    ErrorMsg := FThread.ErrorMessage;
    FThread.Free;
    FThread := nil;

    // ------------------------------------------------------------------
    // Phase 4: Abschluss
    // ------------------------------------------------------------------
    LblPhase.Caption := 'Finalizing...';
    ProgressLabel.Caption := Format('Copied %s of %s rows',
      [FormatFloat('#,##0', FCopiedRows), FormatFloat('#,##0', FTotalRows)]);
    Application.ProcessMessages;

  finally
    ProgressForm.Free;
  end;

  // Fehler anzeigen, falls aufgetreten
  if ErrorMsg <> '' then
  begin
    ShowMessage('Copy error: ' + ErrorMsg);
    Result := False;
    Exit;
  end;

  // ------------------------------------------------------------------
  // Statistik
  // ------------------------------------------------------------------
  EndTime := Now;
  if EndTime > FStartTime then
    RowsPerSec := FCopiedRows / ((EndTime - FStartTime) * 24 * 60 * 60)
  else
    RowsPerSec := 0;

  // ------------------------------------------------------------------
  // Statistik-Record füllen
  // ------------------------------------------------------------------
  FStatistics.Method          := cmLocal;
  FStatistics.SourceServer    := RegisteredDatabases[FSourceDBIndex].RegRec.ServerName;
  FStatistics.SourceDatabase  := RegisteredDatabases[FSourceDBIndex].RegRec.Title;
  FStatistics.SourceTable     := FSourceTable;
  FStatistics.SourceIsExternal := False;
  FStatistics.DestServer      := RegisteredDatabases[FDestDBIndex].RegRec.ServerName;
  FStatistics.DestDatabase    := RegisteredDatabases[FDestDBIndex].RegRec.Title;
  FStatistics.DestTable       := FDestTable;
  FStatistics.DestIsExternal  := False;
  FStatistics.RowsCopied      := FCopiedRows;
  FStatistics.BatchSize       := FBatchSize;
  FStatistics.FromRow         := FFromRow;
  FStatistics.ToRow           := FToRow;
  FStatistics.UseRowRange     := (FFromRow > 1) or (FToRow > 0);
  FStatistics.ElapsedSeconds  := (EndTime - FStartTime) * SecsPerDay;

  if FCancelled then
    StatusStr := 'cancelled'
  else
    StatusStr := 'completed';

  Result := True;
end;

end.
