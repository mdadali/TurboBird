unit uCrossRDBMSFieldAnalysis;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Dialogs,  CheckLst,
  uCrossRDBMSTypes;

{ Zeigt einen Dialog mit allen Feldern als Checkboxen.
  Der User entscheidet pro Feld, ob es übertragen wird.
  Gibt True zurück bei OK, False bei Cancel.
  Bei OK wird AFields[i].Selected entsprechend gesetzt. }
function ShowFieldAnalysisDialog(var AFields: TCrossFieldArray): Boolean;

implementation

type
  TFrmFieldAnalysis = class
  private
    FForm     : TForm;
    FListBox  : TCheckListBox;
    FInfoLbl  : TLabel;
    FBtnOK    : TButton;
    FBtnCancel: TButton;
    FFields   : ^TCrossFieldArray;
    FOk       : Boolean;

    procedure BuildUI;
    procedure FillList;
    function  DescribeField(const AField: TCrossField): string;
    procedure HandleOK(Sender: TObject);
    procedure HandleCancel(Sender: TObject);
  public
    constructor Create(var AFields: TCrossFieldArray);
    destructor Destroy; override;
    function Execute: Boolean;
  end;

function DescribeFieldStatic(const AField: TCrossField): string;
begin
  if AField.IsProblemField then
  begin
    if Pos('[ARRAY]', AField.SourceType) > 0 then
      Result := ' [ARRAY — cannot be transferred]'
    else if Pos('BLOB', UpperCase(AField.SourceType)) > 0 then
      Result := ' [BINARY BLOB — may contain non-text data]'
    else
      Result := ' [not transferable]';
  end
  else if AField.IsComputed then
    Result := ' [COMPUTED — no values inserted]'
  else
    Result := '';
end;

constructor TFrmFieldAnalysis.Create(var AFields: TCrossFieldArray);
begin
  inherited Create;
  FFields := @AFields;
  FOk := False;
  BuildUI;
end;

destructor TFrmFieldAnalysis.Destroy;
begin
  FForm.Free;
  inherited;
end;

procedure TFrmFieldAnalysis.BuildUI;
begin
  FForm := TForm.CreateNew(nil);
  FForm.Caption := 'Field Analysis';
  FForm.Width := 560;
  FForm.Height := 420;
  FForm.Position := poScreenCenter;
  FForm.BorderStyle := bsDialog;

  FInfoLbl := TLabel.Create(FForm);
  FInfoLbl.Parent := FForm;
  FInfoLbl.Left := 12;
  FInfoLbl.Top := 8;
  FInfoLbl.Width := 530;
  FInfoLbl.AutoSize := False;
  FInfoLbl.WordWrap := True;
  FInfoLbl.Caption :=
    'Select the fields to transfer. Computed fields are created as ' +
    'COMPUTED BY (Firebird target) or as normal column (SQLDB target). ' +
    'Arrays and binary BLOBs are unchecked — check to try anyway.';
  FInfoLbl.Height := 48;

  FListBox := TCheckListBox.Create(FForm);
  FListBox.Parent := FForm;
  FListBox.Left := 12;
  FListBox.Top := 72;
  FListBox.Width := 530;
  FListBox.Height := 270;
  FListBox.Anchors := [akLeft, akTop, akRight, akBottom];
  FListBox.ItemHeight := 0;

  FBtnOK := TButton.Create(FForm);
  FBtnOK.Parent := FForm;
  FBtnOK.Left := 370;
  FBtnOK.Top := 346;
  FBtnOK.Width := 100;
  FBtnOK.Caption := 'OK';
  FBtnOK.Anchors := [akRight, akBottom];
  FBtnOK.ModalResult := mrNone;
  FBtnOK.OnClick := @HandleOK;

  FBtnCancel := TButton.Create(FForm);
  FBtnCancel.Parent := FForm;
  FBtnCancel.Left := 462;
  FBtnCancel.Top := 346;
  FBtnCancel.Width := 100;
  FBtnCancel.Caption := 'Cancel';
  FBtnCancel.Anchors := [akRight, akBottom];
  FBtnCancel.ModalResult := mrCancel;
  FBtnCancel.OnClick := @HandleCancel;

  FillList;
end;

function TFrmFieldAnalysis.DescribeField(const AField: TCrossField): string;
begin
  Result := AField.SourceName + ' (' + AField.SourceType + ')' +
            DescribeFieldStatic(AField);
end;

procedure TFrmFieldAnalysis.FillList;
var
  i: Integer;
begin
  FListBox.Items.Clear;
  for i := 0 to High(FFields^) do
  begin
    FListBox.Items.Add(DescribeField(FFields^[i]));
    FListBox.Checked[i] := FFields^[i].Selected;
  end;
end;

procedure TFrmFieldAnalysis.HandleOK(Sender: TObject);
var
  i: Integer;
begin
  FOk := True;
  for i := 0 to High(FFields^) do
    FFields^[i].Selected := FListBox.Checked[i];
  FForm.ModalResult := mrOk;
end;

procedure TFrmFieldAnalysis.HandleCancel(Sender: TObject);
begin
  FOk := False;
  FForm.ModalResult := mrCancel;
end;

function TFrmFieldAnalysis.Execute: Boolean;
begin
  FForm.ShowModal;
  Result := FOk;
end;

function ShowFieldAnalysisDialog(var AFields: TCrossFieldArray): Boolean;
var
  Dlg: TFrmFieldAnalysis;
begin
  Dlg := TFrmFieldAnalysis.Create(AFields);
  try
    Result := Dlg.Execute;
  finally
    Dlg.Free;
  end;
end;

end.
