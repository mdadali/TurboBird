unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ExtTabCtrl, uStyle, uPosition, uEditor;

type

  { TForm1 }

  TForm1 = class(TForm)
    LogMemo: TMemo;
    ExtTabCtrl1: TExtTabCtrl;
    procedure ExtTabCtrl1AddButtonClick(Sender: TObject);
    procedure ExtTabCtrl1TabChanged(Sender: TObject; NewIndex: Integer);
    procedure ExtTabCtrl1TabChanging(Sender: TObject; OldIndex, NewIndex: Integer; var Allow: Boolean);
    procedure ExtTabCtrl1TabClick(Sender: TObject; Index: Integer);
    procedure ExtTabCtrl1TabCreated(Sender: TObject);
    procedure ExtTabCtrl1TabCreating(Sender: TObject; var ACaption: String; var Data: TObject; var Allow: Boolean);
    procedure ExtTabCtrl1TabDblClick(Sender: TObject; Index: Integer);
    procedure ExtTabCtrl1TabDeleted(Sender: TObject);
    procedure ExtTabCtrl1TabDeleting(Sender: TObject; Index: Integer; var Allow: Boolean);
    procedure ExtTabCtrl1TabReordered(Sender: TObject; OldIndex, NewIndex: Integer);
    procedure ExtTabCtrl1TabReordering(Sender: TObject; OldIndex, NewIndex: Integer; var Allow: Boolean);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);

    procedure StyleRadioGroup1Click(Sender: TObject);
    procedure PositionRadioGroup1Click(Sender: TObject);
  private
    procedure Log(const Msg: string);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Log(const Msg: string);
begin
  if Assigned(LogMemo) then
    LogMemo.Lines.Add(FormatDateTime('hh:nn:ss.zzz', Now) + ' > ' + Msg);
end;

procedure TForm1.ExtTabCtrl1AddButtonClick(Sender: TObject);
var
  f: TfrmEditor;
begin
  f := TfrmEditor.Create(Self);
  f.Parent := Form1;
  f.Align := alClient;
  f.Hide;

  ExtTabCtrl1.AddTab('Editor ' + IntToStr(ExtTabCtrl1.Tabs.Count - 2), f)
end;

procedure TForm1.ExtTabCtrl1TabChanged(Sender: TObject; NewIndex: Integer);
var
  i: Integer;
begin
  for i := 0 to ExtTabCtrl1.Tabs.Count - 1 do
    if Assigned(ExtTabCtrl1.Tabs[i].Data) then
      (ExtTabCtrl1.Tabs[i].Data as TFrame).Hide;

  if Assigned(ExtTabCtrl1.Tabs[NewIndex].Data) then
    (ExtTabCtrl1.Tabs[NewIndex].Data as TFrame).Show;

  if (NewIndex >= 0) and (NewIndex < ExtTabCtrl1.Tabs.Count) then
    Log('Active tab is now: ' + ExtTabCtrl1.Tabs[NewIndex].Caption);
end;

procedure TForm1.ExtTabCtrl1TabChanging(Sender: TObject; OldIndex, NewIndex: Integer; var Allow: Boolean);
begin
  Log(Format('Changing from index %d to %d', [OldIndex, NewIndex]));
  Allow := True;
end;

procedure TForm1.ExtTabCtrl1TabClick(Sender: TObject; Index: Integer);
begin
  Log('Single Click on Tab Index: ' + IntToStr(Index) + ' (' + ExtTabCtrl1.Tabs[Index].Caption + ')');
end;

procedure TForm1.ExtTabCtrl1TabCreated(Sender: TObject);
begin
  Log('Tab created successfully.');
end;

procedure TForm1.ExtTabCtrl1TabCreating(Sender: TObject; var ACaption: String; var Data: TObject; var Allow: Boolean);
var
  NewName: string;
begin
  NewName := InputBox('New Tab', 'Enter tab name:', ACaption);
  if NewName <> '' then
  begin
    ACaption := NewName;
    Allow := True;
    Log('Creating tab: ' + ACaption);
  end
  else
    Allow := False;
end;

procedure TForm1.ExtTabCtrl1TabDblClick(Sender: TObject; Index: Integer);
begin
  Log('Double Click on Tab Index: ' + IntToStr(Index) + ' (' + ExtTabCtrl1.Tabs[Index].Caption + ')');
  // Example action: Rename on double click
  ExtTabCtrl1.Tabs[Index].Caption := InputBox('Rename Tab', 'New name:', ExtTabCtrl1.Tabs[Index].Caption);
end;

procedure TForm1.ExtTabCtrl1TabDeleted(Sender: TObject);
begin
  Log('Tab deleted.');
end;

procedure TForm1.ExtTabCtrl1TabDeleting(Sender: TObject; Index: Integer; var Allow: Boolean);
begin
  if Assigned(ExtTabCtrl1.Tabs[Index].Data) then
    (ExtTabCtrl1.Tabs[Index].Data as TFrame).Free;

  Allow := True;

  Log('Deleting tab "' + ExtTabCtrl1.Tabs[Index].Caption + '".');
end;

procedure TForm1.ExtTabCtrl1TabReordered(Sender: TObject; OldIndex, NewIndex: Integer);
begin
  Log(Format('Tab moved from %d to %d', [OldIndex + 1, NewIndex + 1]));
end;

procedure TForm1.ExtTabCtrl1TabReordering(Sender: TObject; OldIndex, NewIndex: Integer; var Allow: Boolean);
var
  Msg: string;
begin
  Msg := Format('Are you sure you want to move this tab from position %d to %d?',
                [OldIndex + 1, NewIndex + 1]); // +1 makes it human-readable (1-based)

  Allow := MessageDlg('Confirm Reorder', Msg, mtConfirmation, [mbYes, mbNo], 0) = mrYes;
  if Allow then
    Log(Format('User confirmed: Tab moved from %d to %d', [OldIndex, NewIndex]))
  else
    Log(Format('User cancelled: Reorder from %d to %d blocked', [OldIndex, NewIndex]));
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i: Integer;
begin
  for i := 0 to ExtTabCtrl1.Tabs.Count - 1 do
    if Assigned(ExtTabCtrl1.Tabs[i].Data) then
      (ExtTabCtrl1.Tabs[i].Data as TFrame).Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  f1: TfrmStyles;
  f2: TfrmPosition;
  f3: TfrmEditor;
begin
  f1 := TfrmStyles.Create(Self);
  f1.RadioGroup1.OnClick := @StyleRadioGroup1Click;

  f2 := TfrmPosition.Create(Self);
  f2.RadioGroup1.OnClick := @PositionRadioGroup1Click;

  f3 := TfrmEditor.Create(Self);
  LogMemo := f3.Memo1;
  LogMemo.Enabled := False;

  f1.Parent := Form1;
  f2.Parent := Form1;
  f3.Parent := Form1;

  f1.Align := alClient;
  f2.Align := alClient;
  f3.Align := alClient;

  ExtTabCtrl1.Tabs[0].Data := f1;
  ExtTabCtrl1.Tabs[1].Data := f2;
  ExtTabCtrl1.Tabs[2].Data := f3;

  ExtTabCtrl1TabChanged(Self, ExtTabCtrl1.TabIndex);

  Log('Frames created!');
end;

procedure TForm1.StyleRadioGroup1Click(Sender: TObject);
begin
  ExtTabCtrl1.TabStyle := TTabStyle((Sender as TRadioGroup).ItemIndex);
  Log('Style changed!');
end;

procedure TForm1.PositionRadioGroup1Click(Sender: TObject);
begin
  ExtTabCtrl1.TabPosition := TTabPosition((Sender as TRadioGroup).ItemIndex);
  case ExtTabCtrl1.TabPosition of
    tpTop: ExtTabCtrl1.Align := alTop;
    tpBottom: ExtTabCtrl1.Align := alBottom;
    tpLeft: ExtTabCtrl1.Align := alLeft;
    tpRight: ExtTabCtrl1.Align := alRight;
  end;

  Log('Position changed!');
end;

end.

