unit triggers;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, Buttons,

  turbocommon,
  fbcommon,

  fsimpleobjextractor,

  uthemeselector;

type

  { TfmTriggers }

  TfmTriggers = class(TForm)
    bbRefreshTriggers: TBitBtn;
    bbNewTrigger: TBitBtn;
    bbEditTrigger: TBitBtn;
    bbDropTrigger: TBitBtn;
    sgTriggers: TStringGrid;

    procedure bbRefreshClick(Sender: TObject);
    procedure bbNewClick(Sender: TObject);
    procedure bbEditClick(Sender: TObject);
    procedure bbDropClick(Sender: TObject);
    procedure sgTriggersDblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private
    FDBIndex: Integer;
    FTableName: string;
    FNodeInfos: TPNodeInfos;
    FExtractor: TSimpleObjExtractor;

  public
    procedure Init(ADBIndex: Integer; const ATableName: string;
                   ANodeInfos: TPNodeInfos; AExtractor: TSimpleObjExtractor);
    procedure FillTriggers;
  end;


implementation

{$R triggers.lfm}

uses Main, QueryWindow;

{ TfmTriggers }

procedure TfmTriggers.Init(ADBIndex: Integer; const ATableName: string;
  ANodeInfos: TPNodeInfos; AExtractor: TSimpleObjExtractor);
begin
  FDBIndex := ADBIndex;
  FTableName := ATableName;
  FNodeInfos := ANodeInfos;
  FExtractor := AExtractor;

  Caption := 'Triggers: ' + FTableName;
end;

procedure TfmTriggers.FillTriggers;
var
  Triggers: TFBTriggerRawArray;
  i, Row: Integer;
begin
  sgTriggers.RowCount := 1;

  Triggers := FExtractor.GetTableTriggersRaw(FTableName);

  for i := 0 to High(Triggers) do
  begin
    sgTriggers.RowCount := sgTriggers.RowCount + 1;
    Row := sgTriggers.RowCount - 1;

    sgTriggers.Cells[0, Row] := Triggers[i].TriggerName;
    if Triggers[i].IsActive then
      sgTriggers.Cells[1, Row] := '1'
    else
      sgTriggers.Cells[1, Row] := '0';
  end;

  if sgTriggers.RowCount > 1 then
    sgTriggers.Row := 1;
end;

procedure TfmTriggers.bbNewClick(Sender: TObject);
begin
  fmMain.CreateNewTrigger(FDBIndex, FTableName, @bbRefreshClick);
end;

procedure TfmTriggers.bbEditClick(Sender: TObject);
var
  ATriggerName: string;
  List: TStringList;
begin
  if sgTriggers.RowCount > 1 then
  begin
    List := TStringList.Create;
    try
      ATriggerName := sgTriggers.Cells[0, sgTriggers.Row];
      FExtractor.GetTriggerScript(ATriggerName, List);
      fmMain.ShowCompleteQueryWindow(FDBIndex, 'Edit Trigger', List.Text, @bbRefreshClick);
    finally
      List.Free;
    end;
  end;
end;

procedure TfmTriggers.bbDropClick(Sender: TObject);
var
  ATriggerName: string;
begin
  if (sgTriggers.RowCount > 1) and
    (MessageDlg('Are You sure to drop this trigger', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
    ATriggerName := sgTriggers.Cells[0, sgTriggers.Row];
    fmMain.ShowCompleteQueryWindow(FDBIndex, 'Drop Trigger : ' + ATriggerName,
      'drop trigger ' + ATriggerName, @bbRefreshClick);
  end;
end;

procedure TfmTriggers.bbRefreshClick(Sender: TObject);
begin
  FillTriggers;
end;

procedure TfmTriggers.sgTriggersDblClick(Sender: TObject);
begin
  bbEditClick(Sender);
end;

procedure TfmTriggers.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(Self);
end;

end.
