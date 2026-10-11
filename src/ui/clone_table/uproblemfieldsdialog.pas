unit uProblemFieldsDialog;

{ ============================================================================
  Problem-Field-Dialog für CloneTable (ohne LFM — alles dynamisch)

  Kontextsensitiv: Zeigt nur die Sektionen, die Inhalt haben.

  Drei Sektionen:
    - Not supported by this method   (BLOB, Array, Typ auf Ziel nicht verfügbar)
    - May overflow with large values (INT128, NUMERIC>18, DECFLOAT)
    - May fail due to IBX conversion (INT128, NUMERIC>16, DECFLOAT bei Row-by-Row)

  Semantik (identisch zum Hauptformular):
    Häkchen = Feld wird BEHALTEN
    Kein Häkchen = Feld wird ABGEWÄHLT (Default)
  ============================================================================ }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, CheckLst, Graphics;

type
  TProblemCategory = (
    pcNotSupported,
    pcMayOverflow,
    pcIBXConversion
  );

  TProblemFieldInfo = record
    FieldName: string;
    FieldType: string;
    Category: TProblemCategory;
  end;
  TProblemFieldArray = array of TProblemFieldInfo;

  TfrmProblemDialog = class(TForm)
  private
    FFields: TProblemFieldArray;
    FLabels: array[TProblemCategory] of TLabel;
    FLists: array[TProblemCategory] of TCheckListBox;
    FBtnOK: TButton;
    FBtnKeepAll: TButton;
    FBtnCancel: TButton;

    FInfoLabel: TLabel;

    procedure CreateControls;
    procedure LayoutSections;
    procedure BtnKeepAllClick(Sender: TObject);
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
    destructor Destroy; override;

    procedure Init(const AFields: TProblemFieldArray);
    function GetKeptFieldNames: TStringList;
  end;

const
  CategoryTitles: array[TProblemCategory] of string = (
    'Not supported by this method:',
    'May overflow with very large values:',
    'May fail due to IBX client-side conversion:'
  );

implementation

constructor TfrmProblemDialog.CreateNew(AOwner: TComponent; Num: Integer);
begin
  inherited CreateNew(AOwner, Num);
  Caption := 'CloneTable - Field Issues';
  Width := 620;
  Height := 300;
  Position := poScreenCenter;
  BorderStyle := bsDialog;
  CreateControls;
end;

destructor TfrmProblemDialog.Destroy;
var
  C: TProblemCategory;
begin
  for C := Low(TProblemCategory) to High(TProblemCategory) do
  begin
    FLists[C].Free;
    FLabels[C].Free;
  end;
  FInfoLabel.Free;      // ← NEU
  inherited Destroy;
end;

procedure TfrmProblemDialog.CreateControls;
var
  C: TProblemCategory;
begin
  // ============================================================
  // Drei Sektionen — Label + CheckListBox pro Kategorie
  // ============================================================
  for C := Low(TProblemCategory) to High(TProblemCategory) do
  begin
    FLabels[C] := TLabel.Create(Self);
    FLabels[C].Parent := Self;
    FLabels[C].Caption := CategoryTitles[C];
    FLabels[C].Font.Style := [fsBold];
    FLabels[C].Visible := False;

    FLists[C] := TCheckListBox.Create(Self);
    FLists[C].Parent := Self;
    FLists[C].Visible := False;
    FLists[C].Width := 584;
  end;

  // ============================================================
  // Info-Text — erklärt die Button-Aktionen
  // ============================================================
  FInfoLabel := TLabel.Create(Self);
  FInfoLabel.Parent := Self;
  FInfoLabel.AutoSize := False;
  FInfoLabel.WordWrap := True;
  FInfoLabel.Caption :=
    'Checked fields will be kept — unchecked fields will be deselected.' + LineEnding +
    LineEnding +
    '[OK]        - Continue with the selected fields' + LineEnding +
    '[Keep All]  - Keep all fields (copy may fail)' + LineEnding +
    '[Cancel]    - Abort the copy operation';

  // ============================================================
  // Buttons
  // ============================================================
  FBtnOK := TButton.Create(Self);
  FBtnOK.Parent := Self;
  FBtnOK.Caption := 'OK';
  FBtnOK.ModalResult := mrOK;
  FBtnOK.Width := 100;
  FBtnOK.Default := True;

  FBtnKeepAll := TButton.Create(Self);
  FBtnKeepAll.Parent := Self;
  FBtnKeepAll.Caption := 'Keep All';
  FBtnKeepAll.Width := 100;
  FBtnKeepAll.OnClick := @BtnKeepAllClick;

  FBtnCancel := TButton.Create(Self);
  FBtnCancel.Parent := Self;
  FBtnCancel.Caption := 'Cancel';
  FBtnCancel.ModalResult := mrCancel;
  FBtnCancel.Width := 100;
end;

procedure TfrmProblemDialog.Init(const AFields: TProblemFieldArray);
var
  i: Integer;
  C: TProblemCategory;
begin
  SetLength(FFields, Length(AFields));
  for i := 0 to High(AFields) do
    FFields[i] := AFields[i];

  // Alle Sektionen leeren
  for C := Low(TProblemCategory) to High(TProblemCategory) do
    FLists[C].Items.Clear;

  // Felder einsortieren — Default: UNchecked (= abwählen)
  for i := 0 to High(FFields) do
  begin
    FLists[FFields[i].Category].Items.Add(FFields[i].FieldName);
    FLists[FFields[i].Category].Checked[
      FLists[FFields[i].Category].Count - 1] := False;
  end;

  LayoutSections;
end;

procedure TfrmProblemDialog.LayoutSections;
var
  C: TProblemCategory;
  CurTop: Integer;
begin
  CurTop := 16;

  for C := Low(TProblemCategory) to High(TProblemCategory) do
  begin
    if FLists[C].Count = 0 then
    begin
      FLabels[C].Visible := False;
      FLists[C].Visible := False;
      Continue;
    end;

    FLabels[C].Visible := True;
    FLabels[C].Left := 16;
    FLabels[C].Top := CurTop;
    CurTop := CurTop + 22;

    FLists[C].Visible := True;
    FLists[C].Left := 16;
    FLists[C].Top := CurTop;
    if FLists[C].Count <= 6 then
      FLists[C].Height := FLists[C].Count * 20 + 4
    else
      FLists[C].Height := 6 * 20 + 4;
    CurTop := CurTop + FLists[C].Height + 14;
  end;

  // ============================================================
  // Info-Label — zwischen Sektionen und Buttons
  // ============================================================
  FInfoLabel.Left := 16;
  FInfoLabel.Top := CurTop + 8;
  FInfoLabel.Width := ClientWidth - 32;
  FInfoLabel.Height := 90;
  CurTop := CurTop + FInfoLabel.Height + 12;

  // ============================================================
  // Buttons unten rechts
  // ============================================================
  FBtnCancel.Left := ClientWidth - 120;
  FBtnKeepAll.Left := ClientWidth - 230;
  FBtnOK.Left := ClientWidth - 340;
  FBtnCancel.Top := CurTop + 8;
  FBtnKeepAll.Top := CurTop + 8;
  FBtnOK.Top := CurTop + 8;

  ClientHeight := CurTop + 60;
end;

procedure TfrmProblemDialog.BtnKeepAllClick(Sender: TObject);
var
  C: TProblemCategory;
  i: Integer;
begin
  for C := Low(TProblemCategory) to High(TProblemCategory) do
    for i := 0 to FLists[C].Count - 1 do
      FLists[C].Checked[i] := True;
  ModalResult := mrOK;
end;

function TfrmProblemDialog.GetKeptFieldNames: TStringList;
var
  C: TProblemCategory;
  i: Integer;
begin
  Result := TStringList.Create;
  for C := Low(TProblemCategory) to High(TProblemCategory) do
    for i := 0 to FLists[C].Count - 1 do
      if FLists[C].Checked[i] then
        Result.Add(FLists[C].Items[i]);
end;

end.
