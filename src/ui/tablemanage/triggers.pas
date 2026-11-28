unit triggers;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, Buttons,

  IBDatabase,
  IBXScript,

  turbocommon,
  fbcommon,

  confirm_query,
  umessagedlg,
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

    function ExecuteSQL(const ASQL: string): Boolean;
  public
    procedure Init(ADBIndex: Integer; const ATableName: string;
                   ANodeInfos: TPNodeInfos; AExtractor: TSimpleObjExtractor);
    procedure FillTriggers;
  end;


implementation

{$R triggers.lfm}

uses Main;

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
  Script: string;
  DlgCaption: string;
begin
  if sgTriggers.RowCount <= 1 then
    Exit;

  ATriggerName := Trim(sgTriggers.Cells[0, sgTriggers.Row]);
  if ATriggerName = '' then
    Exit;

  List := TStringList.Create;
  try
    FExtractor.GetTriggerScript(ATriggerName, List);
    if List.Count = 0 then
    begin
      MsgWarning('Could not read trigger "' + ATriggerName + '".');
      Exit;
    end;

    Script := List.Text;
    DlgCaption := 'Edit Trigger: ' + ATriggerName;

    if not TfrmConfirmQuery.Execute(DlgCaption, Script) then
      Exit;

    if ExecuteSQL(Script) then
    begin
      turbocommon.MetaDataChanged := True;
      FillTriggers;
    end;
  finally
    List.Free;
  end;
end;

procedure TfmTriggers.bbDropClick(Sender: TObject);
var
  ATriggerName: string;
  Script: string;
begin
  if sgTriggers.RowCount <= 1 then
    Exit;

  ATriggerName := Trim(sgTriggers.Cells[0, sgTriggers.Row]);
  if ATriggerName = '' then
    Exit;

  if not MsgConfirm('Are you sure you want to drop trigger "' + ATriggerName + '"?',
                    mkWarning) then
    Exit;

  Script := 'DROP TRIGGER ' + MakeCaseSensitiveAuto(ATriggerName) + ';';

  if ExecuteSQL(Script) then
  begin
    turbocommon.MetaDataChanged := True;
    FillTriggers;
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
  FillTriggers;
  frmThemeSelector.btnApplyClick(Self);
end;

function TfmTriggers.ExecuteSQL(const ASQL: string): Boolean;
var
  Tr: TIBTransaction;
  ScriptEngine: TIBXScript;
  Lines: TStringList;
begin
  Result := False;

  if not RegisteredDatabases[FDBIndex].IBDatabase.Connected then
  begin
    MsgError('Database is not connected.');
    Exit;
  end;

  Tr := TIBTransaction.Create(nil);
  ScriptEngine := TIBXScript.Create(nil);
  Lines := TStringList.Create;
  try
    Tr.DefaultDatabase := RegisteredDatabases[FDBIndex].IBDatabase;
    Tr.Params.Clear;
    Tr.Params.Assign(RegisteredDatabases[FDBIndex].IBTransaction.Params);

    ScriptEngine.Transaction := Tr;
    ScriptEngine.Database    := RegisteredDatabases[FDBIndex].IBDatabase;
    Lines.Text := ASQL;

    try
      if not Tr.InTransaction then
        Tr.StartTransaction;

      ScriptEngine.ExecSQLScript(Lines.Text);

      if Tr.InTransaction then
        Tr.Commit;

      Result := True;
    except
      on E: Exception do
      begin
        if Tr.InTransaction then
          Tr.Rollback;
        MsgError('Error while executing SQL:' + sLineBreak + sLineBreak + E.Message);
        Result := False;
      end;
    end;
  finally
    Lines.Free;
    ScriptEngine.Free;
    Tr.Free;
  end;
end;

end.
