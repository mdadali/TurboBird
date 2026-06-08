unit uCopyTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls,
  IBDatabase, IBQuery, DateUtils, Dialogs;

type
  TFieldTransform = record
    SourceField: string;
    DestField: string;
    Formula: string;
    CopyField: Boolean;
  end;

  { TCopyThread }

  TCopyThread = class(TThread)
  private
    FSourceDB: TIBDatabase;
    FSourceTrans: TIBTransaction;
    FDestDB: TIBDatabase;
    FDestTrans: TIBTransaction;
    FSourceTable: string;
    FDestTable: string;
    FFieldTransforms: array of TFieldTransform;
    FFieldList: string;
    FBatchSize: Integer;
    FTotalRows: Integer;
    FFromRow: Integer;
    FCopiedRows: Integer;
    FStartTime: TDateTime;
    FErrorMessage: string;

    // GUI-Update Felder
    FProgressLabel: TLabel;
    FProgressBar: TProgressBar;
    FLblElapsed: TLabel;
    FBtnCancel: TButton;

    procedure BuildInsertSQL(FromRow, BatchRows: Integer; out SQL: string);
    procedure UpdateProgressGUI;
    procedure CopyFinished;
  protected
    procedure Execute; override;
  public
    Cancelled: Boolean;

    constructor Create(
      ASourceDB, ADestDB: TIBDatabase;
      ASourceTrans, ADestTrans: TIBTransaction;
      const ASourceTable, ADestTable: string;
      const AFieldTransforms: array of TFieldTransform;
      ABatchSize, ATotalRows, AFromRow: Integer);

    property CopiedRows: Integer read FCopiedRows;
    property StartTime: TDateTime read FStartTime;
    property ErrorMessage: string read FErrorMessage;

    // GUI-Controls von außen setzen
    procedure SetProgressControls(
      AProgressLabel: TLabel;
      AProgressBar: TProgressBar;
      ALblElapsed: TLabel;
      ABtnCancel: TButton);
  end;

  { TCopyTable }

  TCopyTable = class
  private
    FSourceDBIndex: Integer;
    FDestDBIndex: Integer;
    FSourceTable: string;
    FDestTable: string;
    FFieldTransforms: array of TFieldTransform;
    FFieldList: string;
    FBatchSize: Integer;
    FFromRow: Integer;
    FToRow: Integer;
    FTotalRows: Integer;
    FCopiedRows: Integer;
    FStartTime: TDateTime;
    FCancelled: Boolean;

    FThread: TCopyThread;

    function GetSourceDB: TIBDatabase;
    function GetSourceTrans: TIBTransaction;
    function GetDestDB: TIBDatabase;
    function GetDestTrans: TIBTransaction;
    procedure BuildFieldList;
    procedure CancelButtonClick(Sender: TObject);
  public
    constructor Create(
      ASourceDBIndex, ADestDBIndex: Integer;
      const ASourceTable, ADestTable: string;
      const AFieldTransforms: array of TFieldTransform;
      ABatchSize: Integer = 500000;
      AFromRow: Integer = 1;
      AToRow: Integer = 0);

    destructor Destroy; override;

    function Execute: Boolean;

    property TotalRows: Integer read FTotalRows;
    property CopiedRows: Integer read FCopiedRows;
  end;

implementation

uses
  turbocommon;

{ TCopyThread }

constructor TCopyThread.Create(
  ASourceDB, ADestDB: TIBDatabase;
  ASourceTrans, ADestTrans: TIBTransaction;
  const ASourceTable, ADestTable: string;
  const AFieldTransforms: array of TFieldTransform;
  ABatchSize, ATotalRows, AFromRow: Integer);
var
  i: Integer;
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

procedure TCopyThread.SetProgressControls(
  AProgressLabel: TLabel;
  AProgressBar: TProgressBar;
  ALblElapsed: TLabel;
  ABtnCancel: TButton);
begin
  FProgressLabel := AProgressLabel;
  FProgressBar := AProgressBar;
  FLblElapsed := ALblElapsed;
  FBtnCancel := ABtnCancel;
end;

procedure TCopyThread.BuildInsertSQL(FromRow, BatchRows: Integer; out SQL: string);
var
  DestFields, SourceFields: string;
  i: Integer;
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
      SourceFields := SourceFields + FFieldTransforms[i].SourceField;
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

procedure TCopyThread.CopyFinished;
begin
  // Platzhalter – wird von TCopyTable.Execute behandelt
end;

procedure TCopyThread.Execute;
var
  Query: TIBQuery;
  BatchCount, BatchIndex: Integer;
  FromRow, ToRow, BatchRows: Integer;
  SQL: string;
begin
  try
    Query := TIBQuery.Create(nil);
    try
      Query.Database := FDestDB;
      Query.Transaction := FDestTrans;

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

        BuildInsertSQL(FromRow, BatchRows, SQL);

        Query.Close;
        Query.SQL.Text := SQL;

        if not FDestTrans.InTransaction then
          FDestTrans.StartTransaction;

        Query.ExecSQL;
        FDestTrans.CommitRetaining;

        FCopiedRows := FCopiedRows + BatchRows;

        Synchronize(@UpdateProgressGUI);
      end;

      if not Cancelled then
        FDestTrans.Commit;

    finally
      Query.Free;
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
  ASourceDBIndex, ADestDBIndex: Integer;
  const ASourceTable, ADestTable: string;
  const AFieldTransforms: array of TFieldTransform;
  ABatchSize: Integer;
  AFromRow: Integer;
  AToRow: Integer);
var
  i: Integer;
begin
  inherited Create;

  FSourceDBIndex := ASourceDBIndex;
  FDestDBIndex := ADestDBIndex;
  FSourceTable := ASourceTable;
  FDestTable := ADestTable;
  FBatchSize := ABatchSize;
  FFromRow := AFromRow;
  FToRow := AToRow;
  FThread := nil;
  FCancelled := False;
  FCopiedRows := 0;
  FTotalRows := 0;

  SetLength(FFieldTransforms, Length(AFieldTransforms));
  for i := 0 to High(AFieldTransforms) do
    FFieldTransforms[i] := AFieldTransforms[i];

  BuildFieldList;
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

procedure TCopyTable.CancelButtonClick(Sender: TObject);
begin
  FCancelled := True;
  if Assigned(FThread) then
    FThread.Cancelled := True;

  // Optik: Button deaktivieren + Text ändern
  if Sender is TButton then
  begin
    TButton(Sender).Enabled := False;
    TButton(Sender).Caption := 'Cancelling...';
  end;
end;

function TCopyTable.GetSourceDB: TIBDatabase;
begin
  Result := RegisteredDatabases[FSourceDBIndex].IBDatabase;
end;

function TCopyTable.GetSourceTrans: TIBTransaction;
begin
  Result := RegisteredDatabases[FSourceDBIndex].IBTransaction;
end;

function TCopyTable.GetDestDB: TIBDatabase;
begin
  Result := RegisteredDatabases[FDestDBIndex].IBDatabase;
end;

function TCopyTable.GetDestTrans: TIBTransaction;
begin
  Result := RegisteredDatabases[FDestDBIndex].IBTransaction;
end;

procedure TCopyTable.BuildFieldList;
var
  i: Integer;
begin
  FFieldList := '';
  for i := 0 to High(FFieldTransforms) do
  begin
    if FFieldTransforms[i].CopyField then
    begin
      if FFieldList <> '' then
        FFieldList := FFieldList + ', ';
      FFieldList := FFieldList + FFieldTransforms[i].DestField;
    end;
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
begin
  Result := False;
  FCopiedRows := 0;
  FCancelled := False;

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
    FThread := TCopyThread.Create(
      GetSourceDB, GetDestDB,
      GetSourceTrans, GetDestTrans,
      FSourceTable, FDestTable,
      FFieldTransforms,
      FBatchSize, FTotalRows, FFromRow
    );

    FThread.SetProgressControls(ProgressLabel, ProgressBar, LblElapsed, BtnCancel);
    FThread.Start;

    // Warten bis Thread fertig oder Fenster geschlossen
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

    FThread.Free;
    FThread := nil;

  finally
    ProgressForm.Free;
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

{unit uCopyTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls,
  IBDatabase, IBQuery, DateUtils, Dialogs;

type
  TFieldTransform = record
    SourceField: string;
    DestField: string;
    Formula: string;
    CopyField: Boolean;
  end;

  TCopyTable = class
  private
    FSourceDBIndex: Integer;
    FDestDBIndex: Integer;
    FSourceTable: string;
    FDestTable: string;
    FFieldTransforms: array of TFieldTransform;
    FFieldList: string;
    FBatchSize: Integer;
    FFromRow: Integer;
    FToRow: Integer;
    FTotalRows: Integer;
    FCancelled: Boolean;
    FCopiedRows: Integer;
    FStartTime: TDateTime;

    function GetSourceDB: TIBDatabase;
    function GetSourceTrans: TIBTransaction;
    function GetDestDB: TIBDatabase;
    function GetDestTrans: TIBTransaction;
    procedure BuildFieldList;
    procedure BuildInsertSQL(FromRow, BatchRows: Integer; out SQL: string);
  public
    constructor Create(
      ASourceDBIndex, ADestDBIndex: Integer;
      const ASourceTable, ADestTable: string;
      const AFieldTransforms: array of TFieldTransform;
      ABatchSize: Integer = 500000;
      AFromRow: Integer = 1;
      AToRow: Integer = 0);

    function Execute: Boolean;
    procedure Cancel(Sender: TObject);
    property TotalRows: Integer read FTotalRows;
    property CopiedRows: Integer read FCopiedRows;
  end;

implementation

uses
  turbocommon;

constructor TCopyTable.Create(
  ASourceDBIndex, ADestDBIndex: Integer;
  const ASourceTable, ADestTable: string;
  const AFieldTransforms: array of TFieldTransform;
  ABatchSize: Integer;
  AFromRow: Integer;
  AToRow: Integer);
var
  i: Integer;
begin
  inherited Create;

  FSourceDBIndex := ASourceDBIndex;
  FDestDBIndex := ADestDBIndex;
  FSourceTable := ASourceTable;
  FDestTable := ADestTable;
  FBatchSize := ABatchSize;
  FFromRow := AFromRow;
  FToRow := AToRow;
  FCancelled := False;
  FCopiedRows := 0;
  FTotalRows := 0;

  SetLength(FFieldTransforms, Length(AFieldTransforms));
  for i := 0 to High(AFieldTransforms) do
    FFieldTransforms[i] := AFieldTransforms[i];

  BuildFieldList;
end;

function TCopyTable.GetSourceDB: TIBDatabase;
begin
  Result := RegisteredDatabases[FSourceDBIndex].IBDatabase;
end;

function TCopyTable.GetSourceTrans: TIBTransaction;
begin
  Result := RegisteredDatabases[FSourceDBIndex].IBTransaction;
end;

function TCopyTable.GetDestDB: TIBDatabase;
begin
  Result := RegisteredDatabases[FDestDBIndex].IBDatabase;
end;

function TCopyTable.GetDestTrans: TIBTransaction;
begin
  Result := RegisteredDatabases[FDestDBIndex].IBTransaction;
end;

procedure TCopyTable.BuildFieldList;
var
  i: Integer;
begin
  FFieldList := '';
  for i := 0 to High(FFieldTransforms) do
  begin
    if FFieldTransforms[i].CopyField then
    begin
      if FFieldList <> '' then
        FFieldList := FFieldList + ', ';
      FFieldList := FFieldList + FFieldTransforms[i].DestField;
    end;
  end;
end;

procedure TCopyTable.BuildInsertSQL(FromRow, BatchRows: Integer; out SQL: string);
var
  DestFields, SourceFields: string;
  i: Integer;
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
      SourceFields := SourceFields + FFieldTransforms[i].SourceField;
    end;
  end;

  SQL := 'INSERT INTO ' + FDestTable + ' (' + DestFields + ')' + sLineBreak +
         'SELECT FIRST ' + IntToStr(BatchRows) +
         ' SKIP ' + IntToStr(FromRow - 1) + ' ' +
         SourceFields + sLineBreak +
         'FROM ' + FSourceTable;
end;

function TCopyTable.Execute: Boolean;
var
  CountQuery: TIBQuery;
  Query: TIBQuery;
  TotalInSource: Integer;
  BatchCount, BatchIndex: Integer;
  FromRow, ToRow, BatchRows: Integer;
  DestDB: TIBDatabase;
  DestTrans: TIBTransaction;
  ProgressForm: TForm;
  ProgressLabel: TLabel;
  ProgressBar: TProgressBar;
  LblElapsed: TLabel;
  BtnCancel: TButton;
  SQL: string;
  EndTime: TDateTime;
  RowsPerSec: Double;
  StatusStr: string;
  Msg: string;
begin
  Result := False;
  FCopiedRows := 0;
  FCancelled := False;

  DestDB := GetDestDB;
  DestTrans := GetDestTrans;

  // ========================================================================
  // Progress-Fenster ZUERST anzeigen (mit Warte-Meldung)
  // ========================================================================
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
    BtnCancel.OnClick := @Cancel;

    ProgressForm.Show;
    Application.ProcessMessages;

    // ========================================================================
    // Record Count
    // ========================================================================
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
    BatchCount := (FTotalRows + FBatchSize - 1) div FBatchSize;

    // ========================================================================
    // ProgressBar für echten Fortschritt konfigurieren
    // ========================================================================
    ProgressBar.Style := pbstNormal;
    ProgressBar.Max := FTotalRows;
    ProgressBar.Position := 0;
    ProgressLabel.Caption := Format('Total Records: %d', [FTotalRows]);
    BtnCancel.Enabled := True;
    Application.ProcessMessages;

    // ========================================================================
    // Kopieren
    // ========================================================================
    Query := TIBQuery.Create(nil);
    try
      Query.Database := DestDB;
      Query.Transaction := DestTrans;

      FStartTime := Now;

      for BatchIndex := 0 to BatchCount - 1 do
      begin
        Application.ProcessMessages;
        if FCancelled then
          Break;

        FromRow := FFromRow + (BatchIndex * FBatchSize);
        ToRow := FromRow + FBatchSize - 1;
        if ToRow > (FFromRow + FTotalRows - 1) then
          ToRow := FFromRow + FTotalRows - 1;

        BatchRows := ToRow - FromRow + 1;

        BuildInsertSQL(FromRow, BatchRows, SQL);

        Query.Close;
        Query.SQL.Text := SQL;

        if not DestTrans.InTransaction then
          DestTrans.StartTransaction;

        Query.ExecSQL;
        DestTrans.CommitRetaining;

        FCopiedRows := FCopiedRows + BatchRows;

        ProgressBar.Position := FCopiedRows;
        ProgressLabel.Caption := Format('Copied %d of %d rows...', [FCopiedRows, FTotalRows]);
        LblElapsed.Caption := 'Elapsed: ' + FormatDateTime('hh:nn:ss', Now - FStartTime);
        Application.ProcessMessages;
      end;

      if not FCancelled then
        DestTrans.Commit;

    finally
      Query.Free;
    end;

  finally
    ProgressForm.Free;
  end;

  // ========================================================================
  // Statistik
  // ========================================================================
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

procedure TCopyTable.Cancel(Sender: TObject);
begin
  FCancelled := True;
end;

end. }
