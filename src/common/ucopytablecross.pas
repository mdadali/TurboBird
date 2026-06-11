unit uCopyTableCross;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls, Dialogs,
  IBDatabase, IBQuery, ibxscript, DateUtils,
  DB,
  turbocommon;


type

  { TCopyThreadCross }

  TCopyThreadCross = class(TThread)
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

  { TCopyTableCross }

  TCopyTableCross = class
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
    FThread        : TCopyThreadCross;

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
      AToRow     : Integer = 0);

    destructor Destroy; override;

    function Execute : Boolean;

    property TotalRows : Integer read FTotalRows;
    property CopiedRows : Integer read FCopiedRows;
  end;

implementation


{ TCopyThreadCross }

constructor TCopyThreadCross.Create(
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

destructor TCopyThreadCross.Destroy;
begin
  inherited Destroy;
end;

procedure TCopyThreadCross.SetProgressControls(
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

procedure TCopyThreadCross.BuildBatchInsert(SourceQuery: TIBQuery; BatchRows: Integer; out SQL: string);
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

procedure TCopyThreadCross.UpdateProgressGUI;
begin
  if Assigned(FProgressLabel) then
    FProgressLabel.Caption := Format('Copied %d of %d rows...', [FCopiedRows, FTotalRows]);
  if Assigned(FProgressBar) then
    FProgressBar.Position := FCopiedRows;
  if Assigned(FLblElapsed) then
    FLblElapsed.Caption := 'Elapsed: ' + FormatDateTime('hh:nn:ss', Now - FStartTime);
  Application.ProcessMessages;
end;

procedure TCopyThreadCross.Execute;
var
  SourceQuery: TIBQuery;
  DestQuery: TIBQuery;
  Script: TIBXScript;
  BatchCount, BatchIndex: Integer;
  FromRow, ToRow, BatchRows: Integer;
  SQL: string;
  i: Integer;
  DestFields, DestValues: string;
  FieldValue: string;
  SelectFields: string;
  OldDecimalSep: Char;
  ScriptText: string;
  RowCount: Integer;
begin
  OldDecimalSep := DefaultFormatSettings.DecimalSeparator;
  DefaultFormatSettings.DecimalSeparator := '.';
  try
    try
      // Zielfelder einmalig sammeln
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

      // SELECT-Felder einmalig zusammenstellen (mit Formeln!)
      SelectFields := '';
      for i := 0 to High(FFieldTransforms) do
      begin
        if not FFieldTransforms[i].CopyField then
          Continue;

        if SelectFields <> '' then
          SelectFields := SelectFields + ', ';

        if FFieldTransforms[i].Formula <> '' then
        begin
          SelectFields := SelectFields + '(' +
            StringReplace(FFieldTransforms[i].Formula, '$1',
                          FFieldTransforms[i].SourceField, [rfReplaceAll]) + ')';
        end
        else
        begin
          SelectFields := SelectFields + FFieldTransforms[i].SourceField;
        end;
      end;

      SourceQuery := TIBQuery.Create(nil);
      DestQuery := TIBQuery.Create(nil);
      Script := TIBXScript.Create(nil);
      try
        SourceQuery.Database := FSourceDB;
        SourceQuery.Transaction := FSourceTrans;
        DestQuery.Database := FDestDB;
        DestQuery.Transaction := FDestTrans;
        Script.Database := FDestDB;
        Script.Transaction := FDestTrans;

        BatchCount := (FTotalRows + FBatchSize - 1) div FBatchSize;
        FStartTime := Now;

        for BatchIndex := 0 to BatchCount - 1 do
        begin
          if Cancelled then
            Break;

          FromRow := FFromRow + (BatchIndex * FBatchSize);
          ToRow := FromRow + FBatchSize - 1;
          if ToRow > (FFromRow + FTotalRows - 1) then
            ToRow := FFromRow + FTotalRows - 1;
          BatchRows := ToRow - FromRow + 1;

          // ============================================================
          // 1) SELECT mit Formeln auf der QUELL-DB ausführen
          // ============================================================
          SourceQuery.Close;
          SourceQuery.SQL.Text :=
            'SELECT FIRST ' + IntToStr(BatchRows) +
            ' SKIP ' + IntToStr(FromRow - 1) + ' ' +
            SelectFields +
            ' FROM ' + FSourceTable;

          if not FSourceTrans.InTransaction then
            FSourceTrans.StartTransaction;

          SourceQuery.Open;

          // ============================================================
          // 2) INSERT-Statements sammeln
          // ============================================================
          ScriptText := '';
          RowCount := 0;
          while not SourceQuery.EOF do
          begin
            if Cancelled then
              Break;

            DestValues := '';
            for i := 0 to SourceQuery.FieldCount - 1 do
            begin
              if DestValues <> '' then
                DestValues := DestValues + ', ';

              if SourceQuery.Fields[i].IsNull then
                DestValues := DestValues + 'NULL'
              else
              begin
                FieldValue := SourceQuery.Fields[i].AsString;

                if SourceQuery.Fields[i].DataType in [ftSmallint, ftInteger, ftLargeint,
                                                       ftFloat, ftCurrency, ftBCD, ftFMTBcd] then
                  DestValues := DestValues + FieldValue
                else if SourceQuery.Fields[i].DataType = ftBoolean then
                  DestValues := DestValues + FieldValue
                else
                  DestValues := DestValues + QuotedStr(FieldValue);
              end;
            end;

            ScriptText := ScriptText + 'INSERT INTO ' + FDestTable + ' (' + DestFields + ') VALUES (' + DestValues + ');' + sLineBreak;

            Inc(RowCount);
            SourceQuery.Next;
          end;

          SourceQuery.Close;

          // ============================================================
          // 3) Alle INSERTs mit TIBXScript auf einmal ausführen!
          // ============================================================
          if ScriptText <> '' then
          begin
            if not FDestTrans.InTransaction then
              FDestTrans.StartTransaction;

            Script.ExecSQLScript(ScriptText);
            FDestTrans.CommitRetaining;

            FCopiedRows := FCopiedRows + RowCount;
          end;

          Synchronize(@UpdateProgressGUI);
        end;

        if not Cancelled then
          FDestTrans.Commit;

      finally
        if Assigned(SourceQuery) then
        begin
          if SourceQuery.Active then
            SourceQuery.Close;
          SourceQuery.Free;
        end;

        if Assigned(SourceQuery.Transaction) then
        begin
          if SourceQuery.Transaction.InTransaction then
            SourceQuery.Transaction.Rollback;
        end;

        if Assigned(SourceQuery.Database) then
          if SourceQuery.Database.Connected then
            SourceQuery.Database.Connected := false;

        if Assigned(DestQuery) then
        begin
          if DestQuery.Active then
            DestQuery.Close;
          DestQuery.Free;
        end;

        if Assigned(DestQuery.Transaction) then
        begin
          if DestQuery.Transaction.InTransaction then
            DestQuery.Transaction.Commit;
        end;

        if Assigned(DestQuery.Database) then
          if DestQuery.Database.Connected then
            DestQuery.Database.Connected := false;

        if Assigned(Script) then
        begin
          Script.Free;
          Script := nil;
        end;
      end;

    except
      on E: Exception do
      begin
        FErrorMessage := E.Message;
      end;
    end;
  finally
    DefaultFormatSettings.DecimalSeparator := OldDecimalSep;
  end;

end;

{ TCopyTableCross }

constructor TCopyTableCross.Create(
  ASourceDBIndex, ADestDBIndex : Integer;
  const ASourceTable, ADestTable : string;
  const AFieldTransforms : TFieldTransformArray;
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

destructor TCopyTableCross.Destroy;
begin
  if Assigned(FThread) then
  begin
    FThread.Cancelled := True;
    FThread.WaitFor;
    FThread.Free;
  end;
  inherited Destroy;
end;

function TCopyTableCross.GetSourceDB : TIBDatabase;
begin
  Result := RegisteredDatabases[FSourceDBIndex].IBDatabase;
end;

function TCopyTableCross.GetSourceTrans : TIBTransaction;
begin
  Result := RegisteredDatabases[FSourceDBIndex].IBTransaction;
end;

function TCopyTableCross.GetDestDB : TIBDatabase;
begin
  Result := RegisteredDatabases[FDestDBIndex].IBDatabase;
end;

function TCopyTableCross.GetDestTrans : TIBTransaction;
begin
  Result := RegisteredDatabases[FDestDBIndex].IBTransaction;
end;

procedure TCopyTableCross.CancelButtonClick(Sender: TObject);
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

function TCopyTableCross.Execute : Boolean;
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
    FThread := TCopyThreadCross.Create(
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
