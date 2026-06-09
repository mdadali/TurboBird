unit uCopyTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls, Dialogs,
  IBDatabase, IBQuery, DateUtils, Variants, DB, fpexprpars;

type
  TFieldTransform = record
    SourceField : string;
    DestField   : string;
    Formula     : string;      // '' = 1:1 Copy, sonst Formel mit $1-Platzhalter
    CopyField   : Boolean;
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
    FDestValues    : array of string;

    FProgressLabel : TLabel;
    FProgressBar   : TProgressBar;
    FLblElapsed    : TLabel;
    FBtnCancel     : TButton;

    function  TransformRow(SourceQuery: TIBQuery): Boolean;
    procedure BuildDirectInsertSQL(FromRow, BatchRows: Integer; out SQL: string);
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

function TCopyThread.TransformRow(SourceQuery: TIBQuery): Boolean;
var
  i          : Integer;
  FieldValue : string;
  FormulaStr : string;
  Parser     : TFPExpressionParser;
  ExprValue  : TFPExpressionResult;
  IsString   : Boolean;
begin
  Result := True;
  SetLength(FDestValues, Length(FFieldTransforms));

  for i := 0 to High(FFieldTransforms) do
  begin
    if not FFieldTransforms[i].CopyField then Continue;

    if FFieldTransforms[i].Formula = '' then
    begin
      if SourceQuery.FieldByName(FFieldTransforms[i].SourceField).IsNull then
        FDestValues[i] := ''
      else
        FDestValues[i] := SourceQuery.FieldByName(FFieldTransforms[i].SourceField).AsString;
    end
    else begin
      if SourceQuery.FieldByName(FFieldTransforms[i].SourceField).IsNull then
        FieldValue := ''
      else
      begin
        IsString := SourceQuery.FieldByName(FFieldTransforms[i].SourceField).DataType in
                     [ftString, ftWideString, ftFixedChar, ftMemo, ftFmtMemo];
        if IsString then
          FieldValue := QuotedStr(SourceQuery.FieldByName(FFieldTransforms[i].SourceField).AsString)
        else
          FieldValue := SourceQuery.FieldByName(FFieldTransforms[i].SourceField).AsString;
      end;

      FormulaStr := StringReplace(FFieldTransforms[i].Formula, '$1', FieldValue, [rfReplaceAll]);

      Parser := TFPExpressionParser.Create(nil);
      try
        Parser.BuiltIns := [bcStrings, bcMath];
        Parser.Expression := FormulaStr;
        ExprValue := Parser.Evaluate;
        case ExprValue.ResultType of
          rtBoolean : FDestValues[i] := BoolToStr(ExprValue.ResBoolean, True);
          rtInteger : FDestValues[i] := IntToStr(ExprValue.ResInteger);
          rtFloat   : FDestValues[i] := FloatToStr(ExprValue.ResFloat);
          rtString  : FDestValues[i] := ExprValue.ResString;
          else FDestValues[i] := '';
        end;
      except
        on E: Exception do
        begin
          FErrorMessage := 'Expression error in field ' + FFieldTransforms[i].DestField + ': ' + E.Message;
          Result := False;
          Parser.Free;
          Exit;
        end;
      end;
      Parser.Free;
    end;
  end;
end;

procedure TCopyThread.BuildDirectInsertSQL(FromRow, BatchRows: Integer; out SQL: string);
var
  DestFields, SourceFields : string;
  i : Integer;
begin
  DestFields := '';
  SourceFields := '';
  for i := 0 to High(FFieldTransforms) do
  begin
    if FFieldTransforms[i].CopyField then
    begin
      if DestFields <> '' then
        DestFields := DestFields + ', ';
      DestFields := DestFields + FFieldTransforms[i].DestField;

      if SourceFields <> '' then
        SourceFields := SourceFields + ', ';
      if FFieldTransforms[i].Formula = '' then
        SourceFields := SourceFields + FFieldTransforms[i].SourceField
      else
        SourceFields := SourceFields + FFieldTransforms[i].Formula;
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
  SourceQuery, DestQuery : TIBQuery;
  HasFormula : Boolean;
  BatchCount, BatchIndex : Integer;
  FromRow, ToRow, BatchRows : Integer;
  SQL : string;
  i : Integer;
  DestFields, DestValues : string;
  OldDecimalSep : Char;                 // <-- NEU
begin
  // === EINMALIGE UMSCHALTUNG DES DEZIMALTRENNZEICHENS ===
  OldDecimalSep := DefaultFormatSettings.DecimalSeparator;
  DefaultFormatSettings.DecimalSeparator := '.';
  try
    HasFormula := False;
    for i := 0 to High(FFieldTransforms) do
    begin
      if (FFieldTransforms[i].CopyField) and (FFieldTransforms[i].Formula <> '') then
      begin
        HasFormula := True;
        Break;
      end;
    end;

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

    SourceQuery := TIBQuery.Create(nil);
    DestQuery := TIBQuery.Create(nil);
    try
      SourceQuery.Database := FSourceDB;
      SourceQuery.Transaction := FSourceTrans;
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

        if HasFormula then
        begin
          SourceQuery.Close;
          SourceQuery.SQL.Text :=
            'SELECT FIRST ' + IntToStr(BatchRows) +
            ' SKIP ' + IntToStr(FromRow - 1) + ' *' +
            ' FROM ' + FSourceTable;

          if not FSourceTrans.InTransaction then
            FSourceTrans.StartTransaction;
          SourceQuery.Open;

          while not SourceQuery.EOF do
          begin
            if Cancelled then Break;
            if not TransformRow(SourceQuery) then Break;

            DestValues := '';
            for i := 0 to High(FFieldTransforms) do
            begin
              if FFieldTransforms[i].CopyField then
              begin
                if DestValues <> '' then
                  DestValues := DestValues + ', ';
                if FDestValues[i] = '' then
                  DestValues := DestValues + 'NULL'
                else
                  DestValues := DestValues + QuotedStr(FDestValues[i]);
              end;
            end;

            SQL := 'INSERT INTO ' + FDestTable + ' (' + DestFields + ') VALUES (' + DestValues + ')';
            DestQuery.Close;
            DestQuery.SQL.Text := SQL;

            if not FDestTrans.InTransaction then
              FDestTrans.StartTransaction;
            DestQuery.ExecSQL;
            FDestTrans.CommitRetaining;

            Inc(FCopiedRows);
            SourceQuery.Next;
          end;
          SourceQuery.Close;
        end
        else begin
          BuildDirectInsertSQL(FromRow, BatchRows, SQL);
          DestQuery.Close;
          DestQuery.SQL.Text := SQL;
          if not FDestTrans.InTransaction then
            FDestTrans.StartTransaction;
          DestQuery.ExecSQL;
          FDestTrans.CommitRetaining;
          Inc(FCopiedRows, BatchRows);
        end;

        Synchronize(@UpdateProgressGUI);
      end;

      if not Cancelled then FDestTrans.Commit;
    finally
      SourceQuery.Free;
      DestQuery.Free;
    end;
  finally
    // === ZURÜCKSETZEN ===
    DefaultFormatSettings.DecimalSeparator := OldDecimalSep;
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

function TCopyTable.Execute: Boolean;
var
  CountQuery: TIBQuery;
  TotalInSource: Integer;
  ProgressForm: TForm;
  ProgressLabel: TLabel;
  ProgressBar: TProgressBar;
  LblElapsed: TLabel;
  BtnCancel: TButton;
  EndTime: TDateTime;
  RowsPerSec: Double;
  StatusStr: string;
  Msg: string;
  ErrorMsg: string;                // <-- NEU
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

    // From/To berechnen
    if FToRow = 0 then
      FToRow := TotalInSource;
    if FFromRow < 1 then
      FFromRow := 1;
    if FToRow > TotalInSource then
      FToRow := TotalInSource;

    FTotalRows := FToRow - FFromRow + 1;

    // ProgressBar auf "echten" Modus
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

    // Warten bis Thread fertig ist (oder Fenster geschlossen)
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
    ErrorMsg := FThread.ErrorMessage;          // <-- Fehlertext sichern
    FThread.Free;
    FThread := nil;

  finally
    ProgressForm.Free;
  end;

  // ------------------------------------------------------------------
  // Fehlerbehandlung (nachdem Fortschrittsfenster geschlossen ist)
  // ------------------------------------------------------------------
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
