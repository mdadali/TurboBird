unit uCopyTableDataCrossRowByRow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls, Dialogs,
  IBDatabase, IBQuery, ibxscript, DateUtils,
  DB,
  turbocommon,
  uCopyStatistics;


type

  { TCopyThreadRowByRow }

  TCopyThreadRowByRow = class(TThread)
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

    procedure BuildBatchInsert(SourceQuery: TIBQuery; BatchRows: Integer; out SQL: string);
  protected
    procedure UpdateProgressGUI;
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

  { TCopyTableDataCrossRowByRow }

  TCopyTableDataCrossRowByRow = class
  private
    FSourceDBIndex : Integer;
    FDestDBIndex   : Integer;
    FSourceTable   : string;
    FDestTable     : string;
    FFieldTransforms : TFieldTransformArray;
    FBatchSize     : Integer;
    FFromRow       : Integer;
    FToRow         : Integer;
    FTotalRows     : Integer;
    FCopiedRows    : Integer;
    FStartTime     : TDateTime;
    FCancelled     : Boolean;
    FThread        : TCopyThreadRowByRow;

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
      ABatchSize : Integer = 10000;
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


{ TCopyThreadRowByRow }

constructor TCopyThreadRowByRow.Create(
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

destructor TCopyThreadRowByRow.Destroy;
begin
  inherited Destroy;
end;

procedure TCopyThreadRowByRow.SetProgressControls(
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

procedure TCopyThreadRowByRow.BuildBatchInsert(SourceQuery: TIBQuery; BatchRows: Integer; out SQL: string);
var
  DestFields, RowValues: string;
  i: Integer;
  FieldValue: string;
begin
  DestFields := '';
  for i := 0 to High(FFieldTransforms) do
  begin
    if FFieldTransforms[i].CopyField then
    begin
      if DestFields <> '' then
        DestFields := DestFields + ', ';
      DestFields := DestFields + FFieldTransforms[i].DestField;
    end;
  end;

  // NUR EINE Zeile!
  RowValues := '';
  for i := 0 to High(FFieldTransforms) do
  begin
    if not FFieldTransforms[i].CopyField then
      Continue;

    if RowValues <> '' then
      RowValues := RowValues + ', ';

    if SourceQuery.FieldByName(FFieldTransforms[i].SourceField).IsNull then
      RowValues := RowValues + 'NULL'
    else
    begin
      if FFieldTransforms[i].Formula <> '' then
      begin
        // Formel: $1 durch Feldnamen ersetzen, nicht durch Wert!
        RowValues := RowValues + StringReplace(FFieldTransforms[i].Formula, '$1',
                                               FFieldTransforms[i].SourceField, [rfReplaceAll]);
      end
      else
      begin
        FieldValue := SourceQuery.FieldByName(FFieldTransforms[i].SourceField).AsString;
        RowValues := RowValues + QuotedStr(FieldValue);
      end;
    end;
  end;

  SQL := 'INSERT INTO ' + FDestTable + ' (' + DestFields + ') VALUES (' + RowValues + ')';
end;

procedure TCopyThreadRowByRow.UpdateProgressGUI;
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

procedure TCopyThreadRowByRow.Execute;
var
  SourceQuery, DestQuery: TIBQuery;
  BatchCount, BatchIndex: Integer;
  FromRow, ToRow, BatchRows: Integer;
  i: Integer;
  DestFields, SelectFields, ParamNames: string;
  FieldValue: string;
  OldDecimalSep: Char;
  RowsInBatch: Integer;
begin
  OldDecimalSep := DefaultFormatSettings.DecimalSeparator;
  DefaultFormatSettings.DecimalSeparator := '.';
  try
    try
      // Zielfelder + Parameter-Namen einmalig sammeln
      DestFields := '';
      ParamNames := '';
      for i := 0 to High(FFieldTransforms) do
      begin
        if FFieldTransforms[i].CopyField then
        begin
          if DestFields <> '' then
          begin
            DestFields := DestFields + ', ';
            ParamNames := ParamNames + ', ';
          end;
          DestFields := DestFields + FFieldTransforms[i].DestField;
          ParamNames := ParamNames + ':' + FFieldTransforms[i].DestField;
        end;
      end;

      // SELECT-Felder mit Formeln
      SelectFields := '';
      for i := 0 to High(FFieldTransforms) do
      begin
        if not FFieldTransforms[i].CopyField then
          Continue;
        if SelectFields <> '' then
          SelectFields := SelectFields + ', ';
        if FFieldTransforms[i].Formula <> '' then
          SelectFields := SelectFields + '(' +
            StringReplace(FFieldTransforms[i].Formula, '$1',
                          FFieldTransforms[i].SourceField, [rfReplaceAll]) + ')'
        else
          SelectFields := SelectFields + FFieldTransforms[i].SourceField;
      end;

      SourceQuery := TIBQuery.Create(nil);
      DestQuery := TIBQuery.Create(nil);
      try
        SourceQuery.Database := FSourceDB;
        SourceQuery.AllowAutoActivateTransaction := true;
        SourceQuery.Transaction := FSourceTrans;

        DestQuery.Database := FDestDB;
        DestQuery.AllowAutoActivateTransaction := true;
        DestQuery.Transaction := FDestTrans;

        // INSERT EINMAL vorbereiten!
        DestQuery.SQL.Text := 'INSERT INTO ' + FDestTable + ' (' + DestFields + ') VALUES (' + ParamNames + ')';
        DestQuery.Prepare;

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

          // SELECT auf Quell-DB
          SourceQuery.Close;
          SourceQuery.SQL.Text :=
            'SELECT FIRST ' + IntToStr(BatchRows) +
            ' SKIP ' + IntToStr(FromRow - 1) + ' ' +
            SelectFields + ' FROM ' + FSourceTable;

          if not FSourceTrans.InTransaction then
            FSourceTrans.StartTransaction;
          SourceQuery.Open;

          RowsInBatch := 0;

          while not SourceQuery.EOF do
          begin
            if Cancelled then Break;

            // Params aus SourceQuery-Feldern setzen
            for i := 0 to High(FFieldTransforms) do
            begin
              if not FFieldTransforms[i].CopyField then Continue;

              if SourceQuery.Fields[i].IsNull then
                DestQuery.ParamByName(FFieldTransforms[i].DestField).Clear
              else
              begin
                FieldValue := SourceQuery.Fields[i].AsString;
                if SourceQuery.Fields[i].DataType in [ftSmallint, ftInteger, ftLargeint,
                                                       ftFloat, ftCurrency, ftBCD, ftFMTBcd] then
                  DestQuery.ParamByName(FFieldTransforms[i].DestField).AsFloat := StrToFloat(FieldValue)
                else if SourceQuery.Fields[i].DataType = ftBoolean then
                  DestQuery.ParamByName(FFieldTransforms[i].DestField).AsBoolean := (FieldValue = 'True')
                else
                  DestQuery.ParamByName(FFieldTransforms[i].DestField).AsString := FieldValue;
              end;
            end;

            DestQuery.ExecSQL;

            Inc(FCopiedRows);
            Inc(RowsInBatch);
            SourceQuery.Next;

            // Batch-Commit
            if (RowsInBatch >= FBatchSize) then
            begin
              FDestTrans.CommitRetaining;
              RowsInBatch := 0;
              Synchronize(@UpdateProgressGUI);
            end;
          end;

          SourceQuery.Close;

          // Rest committen
          if RowsInBatch > 0 then
          begin
            FDestTrans.CommitRetaining;
            Synchronize(@UpdateProgressGUI);
          end;
        end;

        if FDestTrans.InTransaction then
          FDestTrans.Commit;

        if FSourceTrans.InTransaction then
          FSourceTrans.Rollback;

        DestQuery.UnPrepare;

      finally
        SourceQuery.Free;
        DestQuery.Free;
      end;

    except
      on E: Exception do
      begin
        FErrorMessage := E.Message;
        if FDestTrans.InTransaction then
          FDestTrans.Rollback;
      end;
    end;
  finally
    DefaultFormatSettings.DecimalSeparator := OldDecimalSep;
  end;
end;


{ TCopyTableDataCrossRowByRow }

constructor TCopyTableDataCrossRowByRow.Create(
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

  // Form-eigene Verbindungen speichern
  FSourceDB      := ASourceDB;
  FSourceTrans   := ASourceTrans;
  FDestDB        := ADestDB;
  FDestTrans     := ADestTrans;

  SetLength(FFieldTransforms, Length(AFieldTransforms));
  for i := 0 to High(AFieldTransforms) do
    FFieldTransforms[i] := AFieldTransforms[i];
end;

destructor TCopyTableDataCrossRowByRow.Destroy;
begin
  if Assigned(FThread) then
  begin
    FThread.Cancelled := True;
    FThread.WaitFor;
    FThread.Free;
  end;
  inherited Destroy;
end;

function TCopyTableDataCrossRowByRow.GetSourceDB : TIBDatabase;
begin
  if Assigned(FSourceDB) then
    Result := FSourceDB
  else
    Result := RegisteredDatabases[FSourceDBIndex].IBDatabase;
end;

function TCopyTableDataCrossRowByRow.GetSourceTrans : TIBTransaction;
begin
  if Assigned(FSourceTrans) then
    Result := FSourceTrans
  else
    Result := RegisteredDatabases[FSourceDBIndex].IBTransaction;
end;

function TCopyTableDataCrossRowByRow.GetDestDB : TIBDatabase;
begin
  if Assigned(FDestDB) then
    Result := FDestDB
  else
    Result := RegisteredDatabases[FDestDBIndex].IBDatabase;
end;

function TCopyTableDataCrossRowByRow.GetDestTrans : TIBTransaction;
begin
  if Assigned(FDestTrans) then
    Result := FDestTrans
  else
    Result := RegisteredDatabases[FDestDBIndex].IBTransaction;
end;

procedure TCopyTableDataCrossRowByRow.CancelButtonClick(Sender: TObject);
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

function TCopyTableDataCrossRowByRow.Execute : Boolean;
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

  // Progress-Fenster
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

    // Record Count
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

    // Thread starten
    FThread := TCopyThreadRowByRow.Create(
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
      CheckSynchronize;  // <-- Verarbeitet Synchronize aus allen Threads
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

    if FDestDBIndex >= 0 then
    begin
      if GetDestTrans.InTransaction then
        GetDestTrans.Commit;
    end;

  finally
    ProgressForm.Free;
  end;

  // ============================================================
  // FEHLER ZUERST ANZEIGEN!
  // ============================================================
  if ErrorMsg <> '' then
  begin
    ShowMessage('Copy error: ' + ErrorMsg);
    Result := False;
    Exit;
  end;

  // ============================================================
  // Statistik (nur wenn kein Fehler)
  // ============================================================
  EndTime := Now;
  if EndTime > FStartTime then
    RowsPerSec := FCopiedRows / ((EndTime - FStartTime) * 24 * 60 * 60)
  else
    RowsPerSec := 0;

  // ------------------------------------------------------------------
  // Statistik-Record füllen
  // ------------------------------------------------------------------
  FStatistics.Method          := cmCrossRowByRow;
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
