unit uCopyTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls, Dialogs,
  IBDatabase, IBQuery, DateUtils;

type
  TFieldTransform = record
    SourceField : string;      // Quell-Spaltenname
    DestField   : string;      // Ziel-Spaltenname
    Formula     : string;      // SQL-Ausdruck mit $1 als Platzhalter für SourceField
    CopyField   : Boolean;     // True = kopieren/transformieren, False = überspringen
  end;

  { TCopyThread }

  TCopyThread = class(TThread)
  private
    FSourceDB      : TIBDatabase;
    FSourceTrans   : TIBTransaction;
    FDestDB        : TIBDatabase;
    FDestTrans     : TIBTransaction;
    FSourceTable   : string;
    FDestTable     : string;
    FFieldTransforms : array of TFieldTransform;
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
      const AFieldTransforms : array of TFieldTransform;
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

  { TCopyTable }

  TCopyTable = class
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
    FThread        : TCopyThread;

    function  GetSourceDB : TIBDatabase;
    function  GetSourceTrans : TIBTransaction;
    function  GetDestDB : TIBDatabase;
    function  GetDestTrans : TIBTransaction;
    procedure CancelButtonClick(Sender: TObject);
  public
    constructor Create(
      ASourceDBIndex, ADestDBIndex : Integer;
      const ASourceTable, ADestTable : string;
      const AFieldTransforms : array of TFieldTransform;
      ABatchSize : Integer = 500000;
      AFromRow   : Integer = 1;
      AToRow     : Integer = 0);

    destructor Destroy; override;

    function Execute : Boolean;

    property TotalRows : Integer read FTotalRows;
    property CopiedRows : Integer read FCopiedRows;
  end;

implementation

uses
  turbocommon;

{ TCopyThread }

constructor TCopyThread.Create(
  ASourceDB, ADestDB : TIBDatabase;
  ASourceTrans, ADestTrans : TIBTransaction;
  const ASourceTable, ADestTable : string;
  const AFieldTransforms : array of TFieldTransform;
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
  FSourceTable := ASourceTable;
  FDestTable := ADestTable;
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

destructor TCopyThread.Destroy;
begin
  inherited Destroy;
end;

procedure TCopyThread.SetProgressControls(
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

procedure TCopyThread.BuildInsertSQL(FromRow, BatchRows: Integer; out SQL: string);
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

procedure TCopyThread.UpdateProgressGUI;
begin
  if Assigned(FProgressLabel) then
    FProgressLabel.Caption := Format('Copied %d of %d rows...', [FCopiedRows, FTotalRows]);
  if Assigned(FProgressBar) then
    FProgressBar.Position := FCopiedRows;
  if Assigned(FLblElapsed) then
    FLblElapsed.Caption := 'Elapsed: ' + FormatDateTime('hh:nn:ss', Now - FStartTime);
  Application.ProcessMessages;
end;

procedure TCopyThread.Execute;
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

{ TCopyTable }

constructor TCopyTable.Create(
  ASourceDBIndex, ADestDBIndex : Integer;
  const ASourceTable, ADestTable : string;
  const AFieldTransforms : array of TFieldTransform;
  ABatchSize : Integer;
  AFromRow : Integer;
  AToRow : Integer);
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

  SetLength(FFieldTransforms, Length(AFieldTransforms));
  for i := 0 to High(AFieldTransforms) do
    FFieldTransforms[i] := AFieldTransforms[i];
end;

destructor TCopyTable.Destroy;
begin
  if Assigned(FThread) then
  begin
    FThread.Cancelled := True;
    FThread.WaitFor;
    FThread.Free;
  end;
  inherited Destroy;
end;

function TCopyTable.GetSourceDB : TIBDatabase;
begin
  Result := RegisteredDatabases[FSourceDBIndex].IBDatabase;
end;

function TCopyTable.GetSourceTrans : TIBTransaction;
begin
  Result := RegisteredDatabases[FSourceDBIndex].IBTransaction;
end;

function TCopyTable.GetDestDB : TIBDatabase;
begin
  Result := RegisteredDatabases[FDestDBIndex].IBDatabase;
end;

function TCopyTable.GetDestTrans : TIBTransaction;
begin
  Result := RegisteredDatabases[FDestDBIndex].IBTransaction;
end;

procedure TCopyTable.CancelButtonClick(Sender: TObject);
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

function TCopyTable.Execute : Boolean;
var
  CountQuery : TIBQuery;
  TotalInSource : Integer;
  ProgressForm : TForm;
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
    ProgressForm.Height := 230;
    ProgressForm.Position := poScreenCenter;
    ProgressForm.BorderStyle := bsDialog;

    ProgressLabel := TLabel.Create(ProgressForm);
    ProgressLabel.Parent := ProgressForm;
    ProgressLabel.Left := 16;
    ProgressLabel.Top := 16;
    ProgressLabel.Caption := 'Please wait, counting records...';
    ProgressLabel.Width := 460;

    ProgressBar := TProgressBar.Create(ProgressForm);
    ProgressBar.Parent := ProgressForm;
    ProgressBar.Left := 16;
    ProgressBar.Top := 45;
    ProgressBar.Width := 470;
    ProgressBar.Height := 20;
    ProgressBar.Min := 0;
    ProgressBar.Max := 100;
    ProgressBar.Style := pbstMarquee;

    LblElapsed := TLabel.Create(ProgressForm);
    LblElapsed.Parent := ProgressForm;
    LblElapsed.Left := 16;
    LblElapsed.Top := 80;

    BtnCancel := TButton.Create(ProgressForm);
    BtnCancel.Parent := ProgressForm;
    BtnCancel.Caption := 'Cancel';
    BtnCancel.Left := 200;
    BtnCancel.Top := 120;
    BtnCancel.Width := 100;
    BtnCancel.Enabled := False;
    BtnCancel.OnClick := @CancelButtonClick;

    ProgressForm.Show;
    Application.ProcessMessages;

    // ------------------------------------------------------------------
    // Datensätze zählen
    // ------------------------------------------------------------------
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

    ProgressBar.Style := pbstNormal;
    ProgressBar.Max := FTotalRows;
    ProgressBar.Position := 0;
    ProgressLabel.Caption := Format('Total Records: %d', [FTotalRows]);
    BtnCancel.Enabled := True;
    Application.ProcessMessages;

    // ------------------------------------------------------------------
    // Thread starten
    // ------------------------------------------------------------------
    FThread := TCopyThread.Create(
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

  if FCancelled then
    StatusStr := 'cancelled'
  else
    StatusStr := 'completed';

  Msg := Format('Data copy %s!' + sLineBreak + sLineBreak +
                'Rows copied: %d' + sLineBreak +
                'Time: %s' + sLineBreak +
                'Speed: %.0f rows/sec',
                [StatusStr, FCopiedRows,
                 FormatDateTime('hh:nn:ss', EndTime - FStartTime),
                 RowsPerSec]);

  ShowMessage(Msg);
  Result := True;
end;

end.
