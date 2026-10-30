unit uCrossRDBMSReport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  uCrossRDBMSTypes,
  uCrossRDBMSTypeMapping,
  uCrossRDBMSStrategies;

type
  { ------------------------------------------------------------------ }
  { Report-Dialog mit Copy-Button                                      }
  { ------------------------------------------------------------------ }
  TCrossReportDialog = class
  private
    FForm     : TForm;
    FMemo     : TMemo;
    FBtnCopy  : TButton;
    FBtnClose : TButton;

    procedure HandleCopyClick(Sender: TObject);
    procedure HandleCloseClick(Sender: TObject);
  public
    constructor Create(const ATitle, AReportText: string);
    destructor Destroy; override;
    procedure ShowDialog;
  end;

{ Formatiert die Transfer-Statistik als lesbaren Report-Text }
function FormatCrossTransferReport(const AStats: TCrossTransferStats): string;

implementation

function FormatCrossTransferReport(const AStats: TCrossTransferStats): string;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Add('===============================================================');
    SL.Add('  CROSSRDBMS TRANSFER – REPORT');
    SL.Add('===============================================================');
    SL.Add('');

    SL.Add('--- SOURCE ---------------------------------------------------');
    SL.Add('  DBMS       : ' + DBMSName(AStats.SourceDBMS));
    SL.Add('  Connector  : ' + AStats.SourceConnectorType);
    SL.Add('  Database   : ' + AStats.SourceDatabase);
    SL.Add('  Table      : ' + AStats.SourceTable);
    SL.Add('');

    SL.Add('--- DESTINATION ----------------------------------------------');
    SL.Add('  DBMS       : ' + DBMSName(AStats.DestDBMS));
    SL.Add('  Connector  : ' + AStats.DestConnectorType);
    SL.Add('  Database   : ' + AStats.DestDatabase);
    SL.Add('  Table      : ' + AStats.DestTable);
    SL.Add('');

    SL.Add('--- STATUS ---------------------------------------------------');
    if AStats.Cancelled then
      SL.Add('  Result     : CANCELLED')
    else
      SL.Add('  Result     : COMPLETED');
    SL.Add('  Strategy   : ' + StrategyName(AStats.StrategyUsed));
    SL.Add('');

    SL.Add('--- TRANSFER STATISTICS --------------------------------------');
    SL.Add(Format('  Rows processed  : %s', [FormatFloat('#,##0', AStats.RowsProcessed)]));
    SL.Add(Format('  Rows total      : %s', [FormatFloat('#,##0', AStats.RowsTotal)]));
    SL.Add(Format('  Elapsed         : %.2f sec', [AStats.ElapsedSeconds]));
    SL.Add(Format('  Rows/sec        : %s', [FormatFloat('#,##0', AStats.RowsPerSec)]));
    SL.Add(Format('  Bytes processed : %s', [FormatFloat('#,##0', AStats.BytesProcessed)]));
    SL.Add(Format('  MB/sec          : %.2f', [AStats.MBPerSec]));
    SL.Add(Format('  Commits         : %d', [AStats.CommitsDone]));
    SL.Add('');

    SL.Add('--- TABLE ACTION ---------------------------------------------');
    if AStats.TableRecreated then
      SL.Add('  Recreated       : destination table was dropped and re-created')
    else if AStats.TableCreated then
      SL.Add('  Created         : new destination table was created')
    else if AStats.TableExisted then
      SL.Add('  Appended        : data was added to an existing table')
    else
      SL.Add('  No action       : destination table was not touched');
    SL.Add('');

    if Assigned(AStats.Warnings) and (AStats.Warnings.Count > 0) then
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
      SL.Add('--- INSERT SQL / LAST STATEMENT ------------------------------');
      SL.Add(AStats.InsertSQL);
      SL.Add('');
    end;

    SL.Add('===============================================================');

    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

{ TCrossReportDialog }

constructor TCrossReportDialog.Create(const ATitle, AReportText: string);
begin
  inherited Create;

  FForm := TForm.Create(nil);
  FForm.Caption := ATitle;
  FForm.Width := 920;
  FForm.Height := 640;
  FForm.Position := poScreenCenter;
  FForm.BorderStyle := bsSizeable;

  FMemo := TMemo.Create(FForm);
  FMemo.Parent := FForm;
  FMemo.Left := 8;
  FMemo.Top := 8;
  FMemo.Width := 890;
  FMemo.Height := 550;
  FMemo.Anchors := [akLeft, akTop, akRight, akBottom];
  FMemo.ReadOnly := True;
  FMemo.ScrollBars := ssBoth;
  FMemo.WordWrap := False;
  FMemo.Font.Name := 'DejaVu Sans Mono';
  FMemo.Font.Size := 10;
  FMemo.Lines.Text := AReportText;

  FBtnCopy := TButton.Create(FForm);
  FBtnCopy.Parent := FForm;
  FBtnCopy.Left := 680;
  FBtnCopy.Top := 570;
  FBtnCopy.Width := 100;
  FBtnCopy.Caption := 'Copy';
  FBtnCopy.Anchors := [akRight, akBottom];
  FBtnCopy.OnClick := @HandleCopyClick;

  FBtnClose := TButton.Create(FForm);
  FBtnClose.Parent := FForm;
  FBtnClose.Left := 790;
  FBtnClose.Top := 570;
  FBtnClose.Width := 100;
  FBtnClose.Caption := 'Close';
  FBtnClose.Anchors := [akRight, akBottom];
  FBtnClose.ModalResult := mrOK;
  FBtnClose.OnClick := @HandleCloseClick;
end;

destructor TCrossReportDialog.Destroy;
begin
  FForm.Free;
  inherited Destroy;
end;

procedure TCrossReportDialog.HandleCopyClick(Sender: TObject);
begin
  FMemo.SelectAll;
  FMemo.CopyToClipboard;
  FMemo.SelStart := 0;
  FMemo.SelLength := 0;
  FBtnCopy.Caption := 'Copied!';
  Application.ProcessMessages;
  Sleep(30);
  FBtnCopy.Caption := 'Copy';
end;

procedure TCrossReportDialog.HandleCloseClick(Sender: TObject);
begin
  FForm.ModalResult := mrOK;
end;

procedure TCrossReportDialog.ShowDialog;
begin
  FForm.ShowModal;
end;

end.
