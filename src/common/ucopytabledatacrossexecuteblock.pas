unit uCopyTableDataCrossExecuteBlock;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls, Dialogs,
  IBDatabase, IBQuery, DateUtils,

  turbocommon,
  uCopyTableDataCrossRowByRow;


type

  { TCopyThreadCrossExecuteBlock }

  TCopyThreadCrossExecuteBlock = class(TThread)
  private
    FSourceDB      : TIBDatabase;
    FSourceTrans   : TIBTransaction;
    FDestDB        : TIBDatabase;
    FDestTrans     : TIBTransaction;
    FSourceTable   : string;
    FDestTable     : string;
    FSourceConnStr : string;
    FSourceUser    : string;
    FSourcePwd     : string;
    FFieldTransforms : TFieldTransformArray;
    FProblemFieldTransforms : TFieldTransformArray;
    FBatchSize     : Integer;
    FTotalRows     : Integer;
    FFromRow       : Integer;
    FFirstRow      : Integer;
    FLastRow       : Integer;
    FCopiedRows    : Integer;
    FStartTime     : TDateTime;
    FErrorMessage  : string;
    FHasProblemFields : Boolean;

    FProgressLabel : TLabel;
    FProgressBar   : TProgressBar;
    FLblElapsed    : TLabel;
    FBtnCancel     : TButton;

    function HasProblemFields: Boolean;
    procedure SplitFieldTransforms;
    procedure BuildInsertExecuteBlockSQL(BatchRows: Integer; out SQL: string);
    procedure BuildUpdateRowByRowSQL(out SQL: string);
    procedure ExecuteNormal;
    procedure ExecuteWithProblemFields;
    procedure UpdateProgressGUI;
  protected
    procedure Execute; override;
  public
    Cancelled : Boolean;

    constructor Create(
      ASourceDB, ADestDB : TIBDatabase;
      ASourceTrans, ADestTrans : TIBTransaction;
      const ASourceConnStr, ASourceUser, ASourcePwd, ASourceTable, ADestTable : string;
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

  { TCopyTableDataCrossExecuteBlock }

  TCopyTableDataCrossExecuteBlock = class
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
    FThread        : TCopyThreadCrossExecuteBlock;

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

{ TCopyThreadCrossExecuteBlock }

constructor TCopyThreadCrossExecuteBlock.Create(
  ASourceDB, ADestDB : TIBDatabase;
  ASourceTrans, ADestTrans : TIBTransaction;
  const ASourceConnStr, ASourceUser, ASourcePwd, ASourceTable, ADestTable : string;
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
  FSourceConnStr := ASourceConnStr;
  FSourceUser := ASourceUser;
  FSourcePwd := ASourcePwd;
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

  // Problemfelder erkennen und aufteilen
  FHasProblemFields := HasProblemFields;
  if FHasProblemFields then
    SplitFieldTransforms;

  FProgressLabel := nil;
  FProgressBar := nil;
  FLblElapsed := nil;
  FBtnCancel := nil;
end;

destructor TCopyThreadCrossExecuteBlock.Destroy;
begin
  inherited Destroy;
end;

procedure TCopyThreadCrossExecuteBlock.SetProgressControls(
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

function TCopyThreadCrossExecuteBlock.HasProblemFields: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to High(FFieldTransforms) do
  begin
    if not FFieldTransforms[i].CopyField then
      Continue;
    if Pos('[', FFieldTransforms[i].DestFieldType) > 0 then
      Exit(True);
    if Pos('BLOB', UpperCase(FFieldTransforms[i].DestFieldType)) > 0 then
      Exit(True);
  end;
end;

procedure TCopyThreadCrossExecuteBlock.SplitFieldTransforms;
var
  i, ProblemIdx: Integer;
begin
  SetLength(FProblemFieldTransforms, 0);

  // Problemfelder in separates Array kopieren
  ProblemIdx := 0;
  for i := 0 to High(FFieldTransforms) do
  begin
    if not FFieldTransforms[i].CopyField then
      Continue;
    if (Pos('[', FFieldTransforms[i].DestFieldType) > 0) or
       (Pos('BLOB', UpperCase(FFieldTransforms[i].DestFieldType)) > 0) then
    begin
      SetLength(FProblemFieldTransforms, ProblemIdx + 1);
      FProblemFieldTransforms[ProblemIdx] := FFieldTransforms[i];
      Inc(ProblemIdx);
    end;
  end;
end;

procedure TCopyThreadCrossExecuteBlock.BuildInsertExecuteBlockSQL(BatchRows: Integer; out SQL: string);
var
  DestFields, SelectFields, IntoVars, ValuesVars, VarType: string;
  i: Integer;
begin
  SQL := 'EXECUTE BLOCK' + sLineBreak + 'AS' + sLineBreak;

  for i := 0 to High(FFieldTransforms) do
  begin
    if not FFieldTransforms[i].CopyField then Continue;
    if Pos('[', FFieldTransforms[i].DestFieldType) > 0 then Continue;
    if Pos('BLOB', UpperCase(FFieldTransforms[i].DestFieldType)) > 0 then Continue;

    VarType := FFieldTransforms[i].DestFieldType;
    if VarType = '' then VarType := 'VARCHAR(32765)';
    SQL := SQL + '  DECLARE v_' + FFieldTransforms[i].DestField + ' ' + VarType + ';' + sLineBreak;
  end;

  SQL := SQL + 'BEGIN' + sLineBreak;

  SelectFields := '';
  for i := 0 to High(FFieldTransforms) do
  begin
    if not FFieldTransforms[i].CopyField then Continue;
    if Pos('[', FFieldTransforms[i].DestFieldType) > 0 then Continue;
    if Pos('BLOB', UpperCase(FFieldTransforms[i].DestFieldType)) > 0 then Continue;

    if SelectFields <> '' then SelectFields := SelectFields + ', ';
    if FFieldTransforms[i].Formula <> '' then
    begin
      VarType := StringReplace(FFieldTransforms[i].Formula, '''', '''''', [rfReplaceAll]);
      SelectFields := SelectFields + '(' + StringReplace(VarType, '$1', FFieldTransforms[i].SourceField, [rfReplaceAll]) + ')';
    end
    else
      SelectFields := SelectFields + FFieldTransforms[i].SourceField;
  end;

  DestFields := '';
  for i := 0 to High(FFieldTransforms) do
  begin
    if not FFieldTransforms[i].CopyField then Continue;
    if Pos('[', FFieldTransforms[i].DestFieldType) > 0 then Continue;
    if Pos('BLOB', UpperCase(FFieldTransforms[i].DestFieldType)) > 0 then Continue;
    if DestFields <> '' then DestFields := DestFields + ', ';
    DestFields := DestFields + FFieldTransforms[i].DestField;
  end;

  IntoVars := '';
  for i := 0 to High(FFieldTransforms) do
  begin
    if not FFieldTransforms[i].CopyField then Continue;
    if Pos('[', FFieldTransforms[i].DestFieldType) > 0 then Continue;
    if Pos('BLOB', UpperCase(FFieldTransforms[i].DestFieldType)) > 0 then Continue;
    if IntoVars <> '' then IntoVars := IntoVars + ', ';
    IntoVars := IntoVars + ':v_' + FFieldTransforms[i].DestField;
  end;

  ValuesVars := '';
  for i := 0 to High(FFieldTransforms) do
  begin
    if not FFieldTransforms[i].CopyField then Continue;
    if Pos('[', FFieldTransforms[i].DestFieldType) > 0 then Continue;
    if Pos('BLOB', UpperCase(FFieldTransforms[i].DestFieldType)) > 0 then Continue;
    if ValuesVars <> '' then ValuesVars := ValuesVars + ', ';
    ValuesVars := ValuesVars + ':v_' + FFieldTransforms[i].DestField;
  end;

  SQL := SQL +
    '  FOR EXECUTE STATEMENT' + sLineBreak +
    '    ''SELECT FIRST ' + IntToStr(BatchRows) +
    ' SKIP ' + IntToStr(FFromRow - 1) + ' ' + SelectFields +
    ' FROM ' + FSourceTable + '''' + sLineBreak +
    '    ON EXTERNAL DATA SOURCE ''' + FSourceConnStr + '''' + sLineBreak +
    '    AS USER ''' + FSourceUser + ''' PASSWORD ''' + FSourcePwd + '''' + sLineBreak +
    '    INTO ' + IntoVars + sLineBreak +
    '  DO' + sLineBreak +
    '  BEGIN' + sLineBreak +
    '    INSERT INTO ' + FDestTable + ' (' + DestFields + ')' + sLineBreak +
    '    VALUES (' + ValuesVars + ');' + sLineBreak +
    '  END' + sLineBreak +
    'END';
end;

procedure TCopyThreadCrossExecuteBlock.BuildUpdateRowByRowSQL(out SQL: string);
var
  SetFields: string;
  i: Integer;
begin
  SetFields := '';
  for i := 0 to High(FProblemFieldTransforms) do
  begin
    if not FProblemFieldTransforms[i].CopyField then Continue;
    if SetFields <> '' then SetFields := SetFields + ', ';
    SetFields := SetFields + FProblemFieldTransforms[i].DestField + ' = :v_' + FProblemFieldTransforms[i].DestField;
  end;

  SQL := 'UPDATE ' + FDestTable + ' SET ' + SetFields +
         ' WHERE RDB$DB_KEY IN ' +
         '(SELECT RDB$DB_KEY FROM ' + FDestTable + ' ' +
         'ROWS ' + IntToStr(FFirstRow) + ' TO ' + IntToStr(FLastRow) + ')';
end;

procedure TCopyThreadCrossExecuteBlock.ExecuteNormal;
var
  DestQuery: TIBQuery;
  BatchCount, BatchIndex: Integer;
  FromRow, ToRow, BatchRows: Integer;
  SQL: string;
  OldDecimalSep: Char;
begin
  OldDecimalSep := DefaultFormatSettings.DecimalSeparator;
  DefaultFormatSettings.DecimalSeparator := '.';
  try
    try
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
          if ToRow > (FFromRow + FTotalRows - 1) then ToRow := FFromRow + FTotalRows - 1;
          BatchRows := ToRow - FromRow + 1;
          FFromRow := FromRow;

          BuildInsertExecuteBlockSQL(BatchRows, SQL);
          DestQuery.Close;
          DestQuery.SQL.Text := SQL;

          if not FDestTrans.InTransaction then FDestTrans.StartTransaction;
          DestQuery.ExecSQL;
          FDestTrans.Commit;

          FCopiedRows := FCopiedRows + BatchRows;
          Synchronize(@UpdateProgressGUI);
        end;
      finally
        DestQuery.Free;
      end;
    except
      on E: Exception do
      begin
        FErrorMessage := E.Message;
        if FDestTrans.InTransaction then FDestTrans.Rollback;
      end;
    end;
  finally
    DefaultFormatSettings.DecimalSeparator := OldDecimalSep;
  end;
end;

procedure TCopyThreadCrossExecuteBlock.ExecuteWithProblemFields;
var
  DestQuery: TIBQuery;
  BatchCount, BatchIndex: Integer;
  FromRow, ToRow, BatchRows: Integer;
  SQL: string;
  RowByRowThread: TCopyThreadRowByRow;  // ← Der THREAD, nicht der Wrapper!
  OldDecimalSep: Char;
begin
  OldDecimalSep := DefaultFormatSettings.DecimalSeparator;
  DefaultFormatSettings.DecimalSeparator := '.';
  try
    try
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
          if ToRow > (FFromRow + FTotalRows - 1) then ToRow := FFromRow + FTotalRows - 1;
          BatchRows := ToRow - FromRow + 1;
          FFromRow := FromRow;

          // Schritt 1: INSERT per EXECUTE BLOCK für normale Felder
          BuildInsertExecuteBlockSQL(BatchRows, SQL);
          DestQuery.Close;
          DestQuery.SQL.Text := SQL;
          if not FDestTrans.InTransaction then FDestTrans.StartTransaction;
          DestQuery.ExecSQL;
          FDestTrans.Commit;

          // Schritt 2: Problemfelder per RowByRow-Thread kopieren
          {if Length(FProblemFieldTransforms) > 0 then
          begin
            RowByRowThread := TCopyThreadRowByRow.Create(
              FSourceDB, FDestDB,
              FSourceTrans, FDestTrans,
              FSourceTable, FDestTable,
              FProblemFieldTransforms,
              BatchRows,     // BatchSize = BatchRows (alle Zeilen dieses Batches)
              BatchRows,     // TotalRows = BatchRows
              FromRow        // FromRow = Startposition
            );
            try
              RowByRowThread.Start;
              RowByRowThread.WaitFor;
            finally
              RowByRowThread.Free;
            end;
          end;}

          FCopiedRows := FCopiedRows + BatchRows;
          Synchronize(@UpdateProgressGUI);
        end;
      finally
        DestQuery.Free;
      end;
    except
      on E: Exception do
      begin
        FErrorMessage := E.Message;
        if FDestTrans.InTransaction then FDestTrans.Rollback;
      end;
    end;
  finally
    DefaultFormatSettings.DecimalSeparator := OldDecimalSep;
  end;
end;

procedure TCopyThreadCrossExecuteBlock.UpdateProgressGUI;
begin
  if Assigned(FProgressLabel) then
    FProgressLabel.Caption := Format('Copied %d of %d rows...', [FCopiedRows, FTotalRows]);
  if Assigned(FProgressBar) then
    FProgressBar.Position := FCopiedRows;
  if Assigned(FLblElapsed) then
    FLblElapsed.Caption := 'Elapsed: ' + FormatDateTime('hh:nn:ss', Now - FStartTime);
  Application.ProcessMessages;
end;

procedure TCopyThreadCrossExecuteBlock.Execute;
begin
  if FHasProblemFields then
    ExecuteWithProblemFields
  else
    ExecuteNormal;
end;

{ TCopyTableDataCrossExecuteBlock }

constructor TCopyTableDataCrossExecuteBlock.Create(
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

destructor TCopyTableDataCrossExecuteBlock.Destroy;
begin
  if Assigned(FThread) then
  begin
    FThread.Cancelled := True;
    FThread.WaitFor;
    FThread.Free;
  end;
  inherited Destroy;
end;

function TCopyTableDataCrossExecuteBlock.GetSourceDB : TIBDatabase;
begin
  Result := RegisteredDatabases[FSourceDBIndex].IBDatabase;
end;

function TCopyTableDataCrossExecuteBlock.GetSourceTrans : TIBTransaction;
begin
  Result := RegisteredDatabases[FSourceDBIndex].IBTransaction;
end;

function TCopyTableDataCrossExecuteBlock.GetDestDB : TIBDatabase;
begin
  Result := RegisteredDatabases[FDestDBIndex].IBDatabase;
end;

function TCopyTableDataCrossExecuteBlock.GetDestTrans : TIBTransaction;
begin
  Result := RegisteredDatabases[FDestDBIndex].IBTransaction;
end;

procedure TCopyTableDataCrossExecuteBlock.CancelButtonClick(Sender: TObject);
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

function TCopyTableDataCrossExecuteBlock.Execute : Boolean;
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
  SourceConnStr, SourceUser, SourcePwd: string;
begin
  Result := False;
  FCopiedRows := 0;
  FCancelled := False;

  SourceConnStr := RegisteredDatabases[FSourceDBIndex].RegRec.DatabaseName;
  SourceUser := RegisteredDatabases[FSourceDBIndex].RegRec.UserName;
  SourcePwd := RegisteredDatabases[FSourceDBIndex].RegRec.Password;

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

    if FToRow = 0 then FToRow := TotalInSource;
    if FFromRow < 1 then FFromRow := 1;
    if FToRow > TotalInSource then FToRow := TotalInSource;
    FTotalRows := FToRow - FFromRow + 1;

    ProgressBar.Style := pbstNormal;
    ProgressBar.Max := FTotalRows;
    ProgressBar.Position := 0;
    ProgressLabel.Caption := Format('Total Records: %d', [FTotalRows]);
    BtnCancel.Enabled := True;
    Application.ProcessMessages;

    FThread := TCopyThreadCrossExecuteBlock.Create(
      GetSourceDB, GetDestDB,
      GetSourceTrans, GetDestTrans,
      SourceConnStr, SourceUser, SourcePwd,
      FSourceTable, FDestTable,
      FFieldTransforms,
      FBatchSize, FTotalRows, FFromRow
    );

    FThread.SetProgressControls(ProgressLabel, ProgressBar, LblElapsed, BtnCancel);
    FThread.Start;

    while (not FThread.Finished) and (ProgressForm.Visible) do
    begin
      Application.ProcessMessages;
      CheckSynchronize;
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

  if ErrorMsg <> '' then
  begin
    ShowMessage('Copy error: ' + ErrorMsg);
    Result := False;
    Exit;
  end;

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

{unit uCopyTableDataCrossExecuteBlock;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls, Dialogs,
  IBDatabase, IBQuery, DateUtils, turbocommon;

type

  { TCopyThreadCrossExecuteBlock }

  TCopyThreadCrossExecuteBlock = class(TThread)
  private
    FSourceDB      : TIBDatabase;
    FSourceTrans   : TIBTransaction;
    FDestDB        : TIBDatabase;
    FDestTrans     : TIBTransaction;
    FSourceTable   : string;
    FDestTable     : string;
    FSourceConnStr : string;
    FSourceUser    : string;
    FSourcePwd     : string;
    FFieldTransforms : TFieldTransformArray;
    FProblemFieldTransforms : TFieldTransformArray;
    FBatchSize     : Integer;
    FTotalRows     : Integer;
    FFromRow       : Integer;
    FFirstRow      : Integer;
    FLastRow       : Integer;
    FCopiedRows    : Integer;
    FStartTime     : TDateTime;
    FErrorMessage  : string;
    FHasProblemFields : Boolean;

    FProgressLabel : TLabel;
    FProgressBar   : TProgressBar;
    FLblElapsed    : TLabel;
    FBtnCancel     : TButton;

    function HasProblemFields: Boolean;
    procedure SplitFieldTransforms;
    procedure BuildInsertExecuteBlockSQL(BatchRows: Integer; out SQL: string);
    procedure BuildUpdateRowByRowSQL(out SQL: string);
    procedure ExecuteNormal;
    procedure ExecuteWithProblemFields;
    procedure UpdateProgressGUI;
  protected
    procedure Execute; override;
  public
    Cancelled : Boolean;

    constructor Create(
      ASourceDB, ADestDB : TIBDatabase;
      ASourceTrans, ADestTrans : TIBTransaction;
      const ASourceConnStr, ASourceUser, ASourcePwd, ASourceTable, ADestTable : string;
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

  { TCopyTableDataCrossExecuteBlock }

  TCopyTableDataCrossExecuteBlock = class
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
    FThread        : TCopyThreadCrossExecuteBlock;

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

{ TCopyThreadCrossExecuteBlock }

constructor TCopyThreadCrossExecuteBlock.Create(
  ASourceDB, ADestDB : TIBDatabase;
  ASourceTrans, ADestTrans : TIBTransaction;
  const ASourceConnStr, ASourceUser, ASourcePwd, ASourceTable, ADestTable : string;
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
  FSourceConnStr := ASourceConnStr;
  FSourceUser := ASourceUser;
  FSourcePwd := ASourcePwd;
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

  // Problemfelder erkennen und aufteilen
  FHasProblemFields := HasProblemFields;
  if FHasProblemFields then
    SplitFieldTransforms;

  FProgressLabel := nil;
  FProgressBar := nil;
  FLblElapsed := nil;
  FBtnCancel := nil;
end;

destructor TCopyThreadCrossExecuteBlock.Destroy;
begin
  inherited Destroy;
end;

procedure TCopyThreadCrossExecuteBlock.SetProgressControls(
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

function TCopyThreadCrossExecuteBlock.HasProblemFields: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to High(FFieldTransforms) do
  begin
    if not FFieldTransforms[i].CopyField then
      Continue;
    if Pos('[', FFieldTransforms[i].DestFieldType) > 0 then
      Exit(True);
    if Pos('BLOB', UpperCase(FFieldTransforms[i].DestFieldType)) > 0 then
      Exit(True);
  end;
end;

procedure TCopyThreadCrossExecuteBlock.SplitFieldTransforms;
var
  i, CleanIdx, ProblemIdx: Integer;
begin
  SetLength(FProblemFieldTransforms, 0);

  // Problemfelder in separates Array kopieren
  ProblemIdx := 0;
  for i := 0 to High(FFieldTransforms) do
  begin
    if not FFieldTransforms[i].CopyField then
      Continue;
    if (Pos('[', FFieldTransforms[i].DestFieldType) > 0) or
       (Pos('BLOB', UpperCase(FFieldTransforms[i].DestFieldType)) > 0) then
    begin
      SetLength(FProblemFieldTransforms, ProblemIdx + 1);
      FProblemFieldTransforms[ProblemIdx] := FFieldTransforms[i];
      Inc(ProblemIdx);
    end;
  end;
end;

procedure TCopyThreadCrossExecuteBlock.BuildInsertExecuteBlockSQL(BatchRows: Integer; out SQL: string);
var
  DestFields, SelectFields, IntoVars, ValuesVars, VarType: string;
  i: Integer;
begin
  SQL := 'EXECUTE BLOCK' + sLineBreak + 'AS' + sLineBreak;

  for i := 0 to High(FFieldTransforms) do
  begin
    if not FFieldTransforms[i].CopyField then Continue;
    if Pos('[', FFieldTransforms[i].DestFieldType) > 0 then Continue;
    if Pos('BLOB', UpperCase(FFieldTransforms[i].DestFieldType)) > 0 then Continue;

    VarType := FFieldTransforms[i].DestFieldType;
    if VarType = '' then VarType := 'VARCHAR(32765)';
    SQL := SQL + '  DECLARE v_' + FFieldTransforms[i].DestField + ' ' + VarType + ';' + sLineBreak;
  end;

  SQL := SQL + 'BEGIN' + sLineBreak;

  SelectFields := '';
  for i := 0 to High(FFieldTransforms) do
  begin
    if not FFieldTransforms[i].CopyField then Continue;
    if Pos('[', FFieldTransforms[i].DestFieldType) > 0 then Continue;
    if Pos('BLOB', UpperCase(FFieldTransforms[i].DestFieldType)) > 0 then Continue;

    if SelectFields <> '' then SelectFields := SelectFields + ', ';
    if FFieldTransforms[i].Formula <> '' then
    begin
      VarType := StringReplace(FFieldTransforms[i].Formula, '''', '''''', [rfReplaceAll]);
      SelectFields := SelectFields + '(' + StringReplace(VarType, '$1', FFieldTransforms[i].SourceField, [rfReplaceAll]) + ')';
    end
    else
      SelectFields := SelectFields + FFieldTransforms[i].SourceField;
  end;

  DestFields := '';
  for i := 0 to High(FFieldTransforms) do
  begin
    if not FFieldTransforms[i].CopyField then Continue;
    if Pos('[', FFieldTransforms[i].DestFieldType) > 0 then Continue;
    if Pos('BLOB', UpperCase(FFieldTransforms[i].DestFieldType)) > 0 then Continue;
    if DestFields <> '' then DestFields := DestFields + ', ';
    DestFields := DestFields + FFieldTransforms[i].DestField;
  end;

  IntoVars := '';
  for i := 0 to High(FFieldTransforms) do
  begin
    if not FFieldTransforms[i].CopyField then Continue;
    if Pos('[', FFieldTransforms[i].DestFieldType) > 0 then Continue;
    if Pos('BLOB', UpperCase(FFieldTransforms[i].DestFieldType)) > 0 then Continue;
    if IntoVars <> '' then IntoVars := IntoVars + ', ';
    IntoVars := IntoVars + ':v_' + FFieldTransforms[i].DestField;
  end;

  ValuesVars := '';
  for i := 0 to High(FFieldTransforms) do
  begin
    if not FFieldTransforms[i].CopyField then Continue;
    if Pos('[', FFieldTransforms[i].DestFieldType) > 0 then Continue;
    if Pos('BLOB', UpperCase(FFieldTransforms[i].DestFieldType)) > 0 then Continue;
    if ValuesVars <> '' then ValuesVars := ValuesVars + ', ';
    ValuesVars := ValuesVars + ':v_' + FFieldTransforms[i].DestField;
  end;

  SQL := SQL +
    '  FOR EXECUTE STATEMENT' + sLineBreak +
    '    ''SELECT FIRST ' + IntToStr(BatchRows) +
    ' SKIP ' + IntToStr(FFromRow - 1) + ' ' + SelectFields +
    ' FROM ' + FSourceTable + '''' + sLineBreak +
    '    ON EXTERNAL DATA SOURCE ''' + FSourceConnStr + '''' + sLineBreak +
    '    AS USER ''' + FSourceUser + ''' PASSWORD ''' + FSourcePwd + '''' + sLineBreak +
    '    INTO ' + IntoVars + sLineBreak +
    '  DO' + sLineBreak +
    '  BEGIN' + sLineBreak +
    '    INSERT INTO ' + FDestTable + ' (' + DestFields + ')' + sLineBreak +
    '    VALUES (' + ValuesVars + ');' + sLineBreak +
    '  END' + sLineBreak +
    'END';
end;

procedure TCopyThreadCrossExecuteBlock.BuildUpdateRowByRowSQL(out SQL: string);
var
  SetFields: string;
  i: Integer;
begin
  SetFields := '';
  for i := 0 to High(FProblemFieldTransforms) do
  begin
    if not FProblemFieldTransforms[i].CopyField then Continue;
    if SetFields <> '' then SetFields := SetFields + ', ';
    SetFields := SetFields + FProblemFieldTransforms[i].DestField + ' = :v_' + FProblemFieldTransforms[i].DestField;
  end;

  SQL := 'UPDATE ' + FDestTable + ' SET ' + SetFields +
         ' WHERE RDB$DB_KEY IN ' +
         '(SELECT RDB$DB_KEY FROM ' + FDestTable + ' ' +
         'ROWS ' + IntToStr(FFirstRow) + ' TO ' + IntToStr(FLastRow) + ')';
end;

procedure TCopyThreadCrossExecuteBlock.ExecuteNormal;
var
  DestQuery: TIBQuery;
  BatchCount, BatchIndex: Integer;
  FromRow, ToRow, BatchRows: Integer;
  SQL: string;
  OldDecimalSep: Char;
begin
  OldDecimalSep := DefaultFormatSettings.DecimalSeparator;
  DefaultFormatSettings.DecimalSeparator := '.';
  try
    try
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
          if ToRow > (FFromRow + FTotalRows - 1) then ToRow := FFromRow + FTotalRows - 1;
          BatchRows := ToRow - FromRow + 1;
          FFromRow := FromRow;

          BuildInsertExecuteBlockSQL(BatchRows, SQL);
          DestQuery.Close;
          DestQuery.SQL.Text := SQL;

          if not FDestTrans.InTransaction then FDestTrans.StartTransaction;
          DestQuery.ExecSQL;
          FDestTrans.Commit;

          FCopiedRows := FCopiedRows + BatchRows;
          Synchronize(@UpdateProgressGUI);
        end;
      finally
        DestQuery.Free;
      end;
    except
      on E: Exception do
      begin
        FErrorMessage := E.Message;
        if FDestTrans.InTransaction then FDestTrans.Rollback;
      end;
    end;
  finally
    DefaultFormatSettings.DecimalSeparator := OldDecimalSep;
  end;
end;

procedure TCopyThreadCrossExecuteBlock.ExecuteWithProblemFields;
var
  DestQuery, SourceQuery: TIBQuery;
  BatchCount, BatchIndex: Integer;
  FromRow, ToRow, BatchRows: Integer;
  SQL: string;
  i: Integer;
  OldDecimalSep: Char;
begin
  OldDecimalSep := DefaultFormatSettings.DecimalSeparator;
  DefaultFormatSettings.DecimalSeparator := '.';
  try
    try
      DestQuery := TIBQuery.Create(nil);
      SourceQuery := TIBQuery.Create(nil);
      try
        DestQuery.Database := FDestDB;
        DestQuery.Transaction := FDestTrans;
        SourceQuery.Database := FSourceDB;
        SourceQuery.Transaction := FSourceTrans;

        BatchCount := (FTotalRows + FBatchSize - 1) div FBatchSize;
        FStartTime := Now;

        for BatchIndex := 0 to BatchCount - 1 do
        begin
          if Cancelled then Break;
          FromRow := FFromRow + (BatchIndex * FBatchSize);
          ToRow := FromRow + FBatchSize - 1;
          if ToRow > (FFromRow + FTotalRows - 1) then ToRow := FFromRow + FTotalRows - 1;
          BatchRows := ToRow - FromRow + 1;

          FFirstRow := FromRow;
          FLastRow := ToRow;

          // Schritt 1: INSERT per EXECUTE BLOCK
          BuildInsertExecuteBlockSQL(BatchRows, SQL);
          DestQuery.Close;
          DestQuery.SQL.Text := SQL;
          if not FDestTrans.InTransaction then FDestTrans.StartTransaction;
          DestQuery.ExecSQL;
          FDestTrans.Commit;
          FCopiedRows := FCopiedRows + BatchRows;

          // Schritt 2: UPDATE per Row-by-Row
          if Length(FProblemFieldTransforms) > 0 then
          begin
            // Transaction für UPDATE neu starten!
            if not FDestTrans.InTransaction then
              FDestTrans.StartTransaction;

            SourceQuery.Close;
            SourceQuery.SQL.Text := 'SELECT * FROM ' + FSourceTable +
              ' ROWS ' + IntToStr(FFirstRow) + ' TO ' + IntToStr(FLastRow);
            if not FSourceTrans.InTransaction then FSourceTrans.StartTransaction;
            SourceQuery.Open;

            BuildUpdateRowByRowSQL(SQL);
            DestQuery.Close;
            DestQuery.SQL.Text := SQL;
            DestQuery.Prepare;

            while not SourceQuery.EOF do
            begin
              if Cancelled then Break;
              for i := 0 to High(FProblemFieldTransforms) do
              begin
                if SourceQuery.FieldByName(FProblemFieldTransforms[i].SourceField).IsNull then
                  DestQuery.ParamByName('v_' + FProblemFieldTransforms[i].DestField).Clear
                else
                  DestQuery.ParamByName('v_' + FProblemFieldTransforms[i].DestField).AsString :=
                    SourceQuery.FieldByName(FProblemFieldTransforms[i].SourceField).AsString;
              end;
              DestQuery.ExecSQL;
              SourceQuery.Next;
            end;

            SourceQuery.Close;
            FDestTrans.Commit;
            DestQuery.UnPrepare;
          end;

          Synchronize(@UpdateProgressGUI);
        end;
      finally
        SourceQuery.Free;
        DestQuery.Free;
      end;
    except
      on E: Exception do
      begin
        FErrorMessage := E.Message;
        if FDestTrans.InTransaction then FDestTrans.Rollback;
      end;
    end;
  finally
    DefaultFormatSettings.DecimalSeparator := OldDecimalSep;
  end;
end;

procedure TCopyThreadCrossExecuteBlock.UpdateProgressGUI;
begin
  if Assigned(FProgressLabel) then
    FProgressLabel.Caption := Format('Copied %d of %d rows...', [FCopiedRows, FTotalRows]);
  if Assigned(FProgressBar) then
    FProgressBar.Position := FCopiedRows;
  if Assigned(FLblElapsed) then
    FLblElapsed.Caption := 'Elapsed: ' + FormatDateTime('hh:nn:ss', Now - FStartTime);
  Application.ProcessMessages;
end;

procedure TCopyThreadCrossExecuteBlock.Execute;
begin
  if FHasProblemFields then
    ExecuteWithProblemFields
  else
    ExecuteNormal;
end;

{ TCopyTableDataCrossExecuteBlock }

constructor TCopyTableDataCrossExecuteBlock.Create(
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

destructor TCopyTableDataCrossExecuteBlock.Destroy;
begin
  if Assigned(FThread) then
  begin
    FThread.Cancelled := True;
    FThread.WaitFor;
    FThread.Free;
  end;
  inherited Destroy;
end;

function TCopyTableDataCrossExecuteBlock.GetSourceDB : TIBDatabase;
begin
  Result := RegisteredDatabases[FSourceDBIndex].IBDatabase;
end;

function TCopyTableDataCrossExecuteBlock.GetSourceTrans : TIBTransaction;
begin
  Result := RegisteredDatabases[FSourceDBIndex].IBTransaction;
end;

function TCopyTableDataCrossExecuteBlock.GetDestDB : TIBDatabase;
begin
  Result := RegisteredDatabases[FDestDBIndex].IBDatabase;
end;

function TCopyTableDataCrossExecuteBlock.GetDestTrans : TIBTransaction;
begin
  Result := RegisteredDatabases[FDestDBIndex].IBTransaction;
end;

procedure TCopyTableDataCrossExecuteBlock.CancelButtonClick(Sender: TObject);
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

function TCopyTableDataCrossExecuteBlock.Execute : Boolean;
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
  SourceConnStr, SourceUser, SourcePwd: string;
begin
  Result := False;
  FCopiedRows := 0;
  FCancelled := False;

  SourceConnStr := RegisteredDatabases[FSourceDBIndex].RegRec.DatabaseName;
  SourceUser := RegisteredDatabases[FSourceDBIndex].RegRec.UserName;
  SourcePwd := RegisteredDatabases[FSourceDBIndex].RegRec.Password;

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

    if FToRow = 0 then FToRow := TotalInSource;
    if FFromRow < 1 then FFromRow := 1;
    if FToRow > TotalInSource then FToRow := TotalInSource;
    FTotalRows := FToRow - FFromRow + 1;

    ProgressBar.Style := pbstNormal;
    ProgressBar.Max := FTotalRows;
    ProgressBar.Position := 0;
    ProgressLabel.Caption := Format('Total Records: %d', [FTotalRows]);
    BtnCancel.Enabled := True;
    Application.ProcessMessages;

    FThread := TCopyThreadCrossExecuteBlock.Create(
      GetSourceDB, GetDestDB,
      GetSourceTrans, GetDestTrans,
      SourceConnStr, SourceUser, SourcePwd,
      FSourceTable, FDestTable,
      FFieldTransforms,
      FBatchSize, FTotalRows, FFromRow
    );

    FThread.SetProgressControls(ProgressLabel, ProgressBar, LblElapsed, BtnCancel);
    FThread.Start;

    while (not FThread.Finished) and (ProgressForm.Visible) do
    begin
      Application.ProcessMessages;
      CheckSynchronize;
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

  if ErrorMsg <> '' then
  begin
    ShowMessage('Copy error: ' + ErrorMsg);
    Result := False;
    Exit;
  end;

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

end. }
