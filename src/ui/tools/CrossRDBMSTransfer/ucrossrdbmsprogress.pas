unit uCrossRDBMSProgress;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls,
  uCrossRDBMSTypes,
  uCrossRDBMSEngine;

type
  { ------------------------------------------------------------------ }
  { Progress-Dialog für den Transfer                                   }
  { ------------------------------------------------------------------ }
  TCrossProgressDialog = class
  private
    FForm          : TForm;
    FProgressBar   : TProgressBar;
    FLblInfo       : TLabel;
    FLblElapsed    : TLabel;
    FLblCommits    : TLabel;
    FLblRowsPerSec : TLabel;
    FLblMbPerSec   : TLabel;
    FLblStrategy   : TLabel;
    FLogMemo       : TMemo;
    FBtnCancel     : TButton;
    FEngine        : TCrossTransferEngine;
    FCancelled     : Boolean;

    procedure HandleCancelClick(Sender: TObject);
    procedure HandleProgress(ARowsDone, ARowsTotal: Int64;
      const AElapsed: string; var ACancel: Boolean);
    procedure HandleLog(const AMsg: string);
  public
    constructor Create(AEngine: TCrossTransferEngine);
    destructor Destroy; override;

    function Execute: Boolean;   // True = erfolgreich durchgelaufen
  end;

implementation

constructor TCrossProgressDialog.Create(AEngine: TCrossTransferEngine);
begin
  inherited Create;
  FEngine := AEngine;
  FCancelled := False;

  FForm := TForm.Create(nil);
  FForm.Caption := 'CrossRDBMS Transfer';
  FForm.Width := 580;
  FForm.Height := 420;
  FForm.Position := poScreenCenter;
  FForm.BorderStyle := bsDialog;

  // ProgressBar
  FProgressBar := TProgressBar.Create(FForm);
  FProgressBar.Parent := FForm;
  FProgressBar.Left := 16;
  FProgressBar.Top := 16;
  FProgressBar.Width := 540;
  FProgressBar.Height := 24;
  FProgressBar.Min := 0;
  FProgressBar.Max := 100;

  // Info-Zeile
  FLblInfo := TLabel.Create(FForm);
  FLblInfo.Parent := FForm;
  FLblInfo.Left := 16;
  FLblInfo.Top := 50;
  FLblInfo.Caption := 'Rows: 0 of 0';

  // Elapsed
  FLblElapsed := TLabel.Create(FForm);
  FLblElapsed.Parent := FForm;
  FLblElapsed.Left := 16;
  FLblElapsed.Top := 72;
  FLblElapsed.Caption := 'Elapsed: 00:00:00';

  // Commits
  FLblCommits := TLabel.Create(FForm);
  FLblCommits.Parent := FForm;
  FLblCommits.Left := 16;
  FLblCommits.Top := 94;
  FLblCommits.Caption := 'Commits: 0';

  // Rows/sec
  FLblRowsPerSec := TLabel.Create(FForm);
  FLblRowsPerSec.Parent := FForm;
  FLblRowsPerSec.Left := 16;
  FLblRowsPerSec.Top := 116;
  FLblRowsPerSec.Caption := 'Rows/sec: 0';

  // MB/sec
  FLblMbPerSec := TLabel.Create(FForm);
  FLblMbPerSec.Parent := FForm;
  FLblMbPerSec.Left := 16;
  FLblMbPerSec.Top := 138;
  FLblMbPerSec.Caption := 'MB/sec: 0.00';

  // Strategy
  FLblStrategy := TLabel.Create(FForm);
  FLblStrategy.Parent := FForm;
  FLblStrategy.Left := 16;
  FLblStrategy.Top := 160;
  FLblStrategy.Caption := 'Strategy: –';
  FLblStrategy.Font.Style := [fsBold];

  // Log
  FLogMemo := TMemo.Create(FForm);
  FLogMemo.Parent := FForm;
  FLogMemo.Left := 16;
  FLogMemo.Top := 190;
  FLogMemo.Width := 540;
  FLogMemo.Height := 150;
  FLogMemo.ReadOnly := True;
  FLogMemo.ScrollBars := ssVertical;

  // Cancel-Button
  FBtnCancel := TButton.Create(FForm);
  FBtnCancel.Parent := FForm;
  FBtnCancel.Caption := 'Cancel';
  FBtnCancel.Left := 230;
  FBtnCancel.Top := 350;
  FBtnCancel.Width := 110;
  FBtnCancel.OnClick := @HandleCancelClick;
end;

destructor TCrossProgressDialog.Destroy;
begin
  FForm.Free;
  inherited Destroy;
end;

procedure TCrossProgressDialog.HandleCancelClick(Sender: TObject);
begin
  FCancelled := True;
  FEngine.Cancel;
  FBtnCancel.Enabled := False;
  FBtnCancel.Caption := 'Cancelling...';
end;

procedure TCrossProgressDialog.HandleProgress(ARowsDone, ARowsTotal: Int64;
  const AElapsed: string; var ACancel: Boolean);
var
  ElapsedSec, RowsPerSec, MbPerSec: Double;
begin
  if ARowsTotal > 0 then
    FProgressBar.Position := Round(ARowsDone * 100 / ARowsTotal)
  else
    FProgressBar.Position := 0;

  FLblInfo.Caption := Format('Rows: %s of %s',
    [FormatFloat('#,##0', ARowsDone), FormatFloat('#,##0', ARowsTotal)]);
  FLblElapsed.Caption := 'Elapsed: ' + AElapsed;
  FLblCommits.Caption := Format('Commits: %d', [FEngine.Stats.CommitsDone]);

  if FEngine.Stats.StartTime > 0 then
    ElapsedSec := (Now - FEngine.Stats.StartTime) * 86400.0
  else
    ElapsedSec := 0;

  if ElapsedSec > 0.001 then
  begin
    RowsPerSec := ARowsDone / ElapsedSec;
    MbPerSec := (FEngine.Stats.BytesProcessed / 1024 / 1024) / ElapsedSec;
  end
  else
  begin
    RowsPerSec := 0;
    MbPerSec := 0;
  end;

  FLblRowsPerSec.Caption := Format('Rows/sec: %s', [FormatFloat('#,##0', RowsPerSec)]);
  FLblMbPerSec.Caption := Format('MB/sec: %.2f', [MbPerSec]);

  // Strategy anzeigen (falls schon bekannt)
  case FEngine.Stats.StrategyUsed of
    csRowByRow:     FLblStrategy.Caption := 'Strategy: Row-by-Row';
    csBatched:      FLblStrategy.Caption := 'Strategy: Batched Inserts';
    csInsertSelect: FLblStrategy.Caption := 'Strategy: INSERT … SELECT (batched)';
  end;

  Application.ProcessMessages;
  ACancel := FCancelled;
end;

procedure TCrossProgressDialog.HandleLog(const AMsg: string);
begin
  FLogMemo.Lines.Add(AMsg);
  Application.ProcessMessages;
end;

function TCrossProgressDialog.Execute: Boolean;
begin
  Result := False;

  FEngine.OnProgress := @HandleProgress;
  FEngine.OnLog := @HandleLog;

  FForm.Show;
  Application.ProcessMessages;

  try
    FEngine.Execute;
    Result := not FEngine.Stats.Cancelled;
  except
    on E: Exception do
    begin
      MessageDlg('Transfer failed:' + sLineBreak + E.Message, mtError, [mbOK], 0);
      Result := False;
    end;
  end;

  FForm.Hide;
end;

end.
