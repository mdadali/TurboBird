unit CopyTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ComCtrls, ExtCtrls, SynEdit, SynHighlighterSQL,
  IniFiles,

  IB,
  IBDatabase,
  IBQuery,

  fbcommon,
  turbocommon,
  fmetaquerys,
  uthemeselector;

type

  { TfmCopyTable }

  TfmCopyTable = class(TForm)
    bbCopy: TBitBtn;
    btnCancelCopy: TButton;
    btnClose: TButton;
    cbSourceTable: TComboBox;
    cbDestDatabase: TComboBox;
    cbDestTable: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    laDatabase: TLabel;
    laDatabase1: TLabel;
    laSourceDatabase: TLabel;
    SynSQLSyn1: TSynSQLSyn;
    syScript: TSynEdit;
    procedure btnCancelCopyClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure CancelCopy(Sender: TObject);
    procedure bbCopyClick(Sender: TObject);
    procedure cbDestDatabaseChange(Sender: TObject);
    procedure cbSourceTableChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    FNodeInfos: TPNodeInfos;
    FSourceIndex: Integer;

    FCancelled: boolean;
    { private declarations }
  public
    { public declarations }
    procedure Init(SourceIndex: Integer; ATableName: string; ANodeInfos: TPNodeInfos);
  end; 

//var
  //fmCopyTable: TfmCopyTable;

implementation

{ TfmCopyTable }

uses main, SysTables, EnterPass, Reg;

procedure TfmCopyTable.cbDestDatabaseChange(Sender: TObject);
var
  Count: Integer;
begin
  laDatabase.Caption:= RegisteredDatabases[cbDestDatabase.ItemIndex].RegRec.DatabaseName;
  cbDestDatabase.SetFocus;
  dmSysTables.Init(cbDestDatabase.ItemIndex);
  cbDestTable.Items.CommaText:= dmSysTables.GetDBObjectNames(cbDestDatabase.ItemIndex, otTables, count);
  if cbDestTable.Items.IndexOf(cbSourceTable.Text) <> -1 then
    cbDestTable.Text:= cbSourceTable.Text;
end;

procedure TfmCopyTable.cbSourceTableChange(Sender: TObject);
var
  List: TStringList;
  Line: string;
  Iso: TIsolatedQuery;
begin
  List:= TStringList.Create;
  try
    //fmMain.GetFields(FSourceIndex, cbSourceTable.Text, List);
    Iso := GetFieldsIsolated(RegisteredDatabases[FSourceIndex].IBDatabase, cbSourceTable.Text, List);
    Line:= List.CommaText;
  finally
    List.Free;
    Iso.Free;
  end;
  syScript.Lines.Text:= 'select ' + Line;
  syScript.Lines.Add(' from ' + cbSourceTable.Text);
end;

procedure TfmCopyTable.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(FNodeInfos) then
    FNodeInfos^.ViewForm := nil;
  CloseAction := caFree;
end;

procedure TfmCopyTable.FormCreate(Sender: TObject);
begin
  FCancelled := false;
end;

procedure TfmCopyTable.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(self);
end;

procedure TfmCopyTable.SpeedButton1Click(Sender: TObject);
begin
  close;
  Parent.Free;
end;

procedure TfmCopyTable.CancelCopy(Sender: TObject);
begin
  {Cancelled := True;
  if Assigned(ProgressLabel) then
    ProgressLabel.Caption := 'Cancelling...';
  Application.ProcessMessages;}
end;

procedure TfmCopyTable.btnCancelCopyClick(Sender: TObject);
begin
  FCancelled := true;
  bbCopy.Enabled := True;
  btnClose.Enabled := True;
  btnCancelCopy.Enabled := False;

  Application.ProcessMessages;
end;

procedure TfmCopyTable.btnCloseClick(Sender: TObject);
begin
  Close;
  Parent.Free;
end;

procedure TfmCopyTable.bbCopyClick(Sender: TObject);
var
  i, Num: Integer;
  Statement, Values: string;
  SQLTarget: TIBQuery;

  ProgressForm : TForm;
  ProgressBar  : TProgressBar;
  ProgressLabel: TLabel;
  lblStart     : TLabel;
  lblEnd       : TLabel;
  lblElapsed   : TLabel;

  StartTime, EndTime: TDateTime;
begin
  Screen.Cursor := crHourGlass;

  bbCopy.Enabled   := False;
  btnClose.Enabled := False;;
  btnCancelCopy.Enabled := True;

  FCancelled := False;

  ProgressForm := TForm.Create(nil);
  try
    ProgressForm.FormStyle := fsStayOnTop;
    ProgressForm.Caption := 'Copy running...';
    ProgressForm.Width := 500;
    ProgressForm.Height := 200;
    ProgressForm.Position := poScreenCenter;
    ProgressForm.BorderStyle := bsDialog;

    ProgressLabel := TLabel.Create(ProgressForm);
    ProgressLabel.Parent := ProgressForm;
    ProgressLabel.Left := 16;
    ProgressLabel.Top := 16;
    ProgressLabel.Caption := 'Preparing copy...';

    ProgressBar := TProgressBar.Create(ProgressForm);
    ProgressBar.Parent := ProgressForm;
    ProgressBar.Left := 16;
    ProgressBar.Top := 45;
    ProgressBar.Width := 460;
    ProgressBar.Height := 20;
    ProgressBar.Min := 0;
    ProgressBar.Max := 100;

    lblStart := TLabel.Create(ProgressForm);
    lblStart.Parent := ProgressForm;
    lblStart.Left := 16;
    lblStart.Top := 110;

    lblEnd := TLabel.Create(ProgressForm);
    lblEnd.Parent := ProgressForm;
    lblEnd.Left := 16;
    lblEnd.Top := 130;

    lblElapsed := TLabel.Create(ProgressForm);
    lblElapsed.Parent := ProgressForm;
    lblElapsed.Left := 16;
    lblElapsed.Top := 150;

    ProgressForm.Show;
    ProgressForm.Update;
    Application.ProcessMessages;

    StartTime := Now;
    lblStart.Caption := 'Start: ' + FormatDateTime('hh:nn:ss', StartTime);

    dmSysTables.sqQuery.Close;
    dmSysTables.Init(FSourceIndex);
    dmSysTables.sqQuery.SQL.Text := syScript.Lines.Text;
    dmSysTables.sqQuery.Open;

    if dmSysTables.sqQuery.IsEmpty then
    begin
      ShowMessage('No records found');
      Exit;
    end;

    Statement := 'INSERT INTO ' + cbDestTable.Text + ' (';
    Values := '';

    for i := 0 to dmSysTables.sqQuery.Fields.Count - 1 do
    begin
      if Pos('RDB$', UpperCase(dmSysTables.sqQuery.Fields[i].FieldName)) = 1 then
        Continue;

      Statement := Statement + dmSysTables.sqQuery.Fields[i].FieldName + ',';
      Values := Values + ':' + dmSysTables.sqQuery.Fields[i].FieldName + ',';
    end;

    Delete(Statement, Length(Statement), 1);
    Delete(Values, Length(Values), 1);

    Statement := Statement + ') VALUES (' + Values + ')';

    SQLTarget := TIBQuery.Create(nil);

    try
      SQLTarget.Database :=
        RegisteredDatabases[cbDestDatabase.ItemIndex].IBDatabase;

      SQLTarget.Transaction :=
        RegisteredDatabases[cbDestDatabase.ItemIndex].IBTransaction;

      SQLTarget.SQL.Text := Statement;

      if not SQLTarget.Transaction.InTransaction then
        SQLTarget.Transaction.StartTransaction;

      SQLTarget.Prepare;

      Num := 0;
      dmSysTables.sqQuery.First;

      while not dmSysTables.sqQuery.EOF do
      begin
        if FCancelled then
          raise Exception.Create('Copy cancelled by user.');

        for i := 0 to dmSysTables.sqQuery.Fields.Count - 1 do
        begin
          if Pos('RDB$', UpperCase(dmSysTables.sqQuery.Fields[i].FieldName)) = 1 then
            Continue;

          SQLTarget.ParamByName(dmSysTables.sqQuery.Fields[i].FieldName).Value :=
            dmSysTables.sqQuery.Fields[i].Value;
        end;

        SQLTarget.ExecSQL;
        Inc(Num);

        if (Num mod 1000) = 0 then
        begin
          SQLTarget.Transaction.CommitRetaining;

          ProgressBar.Position :=
            (ProgressBar.Position + 1) mod ProgressBar.Max;

          ProgressLabel.Caption :=
            Format('Copied %d records...', [Num]);

          lblElapsed.Caption := 'Elapsed: ' +
            FormatDateTime('nn:ss', Now - StartTime);

          Application.ProcessMessages;
        end;

        dmSysTables.sqQuery.Next;
      end;

      SQLTarget.Transaction.Commit;

      EndTime := Now;

      lblEnd.Caption := 'End: ' + FormatDateTime('hh:nn:ss', EndTime);
      lblElapsed.Caption :=
        'Elapsed: ' + FormatDateTime('hh:nn:ss', EndTime - StartTime);

      ProgressBar.Position := ProgressBar.Max;

      ProgressLabel.Caption :=
        Format('Finished. %d records copied.', [Num]);

      Sleep(500);

      MessageDlg(
        IntToStr(Num) + ' record(s) copied.' + LineEnding +
        'Don''t forget to adjust the generator if necessary.',
        mtInformation,[mbOK],0);

    except
      on E: Exception do
      begin
        if SQLTarget.Transaction.InTransaction then
          SQLTarget.Transaction.Rollback;

        MessageDlg('Error while copy: ' + E.Message, mtError, [mbOK], 0);
      end;
    end;

    SQLTarget.Free;

  finally
    ProgressForm.Free;

    bbCopy.Enabled := True;
    btnClose.Enabled := True;
    btnCancelCopy.Enabled := False;

    Screen.Cursor := crDefault;

    FCancelled := False;

    Application.ProcessMessages;
  end;
end;

procedure TfmCopyTable.Init(SourceIndex: Integer; ATableName: string; ANodeInfos: TPNodeInfos);
var
  i: Integer;
  Count: Integer;
begin
  FNodeInfos := ANodeInfos;
  dmSysTables.sqQuery.Close; //todo: (low priority) is closing sqquery really necessary?
  FSourceIndex:= SourceIndex;
  laSourceDatabase.Caption:= RegisteredDatabases[SourceIndex].RegRec.Title;
  cbDestDatabase.Clear;

  // Display databases in destination combo box
  for i:= 0 to High(RegisteredDatabases) do
    cbDestDatabase.Items.Add(RegisteredDatabases[i].RegRec.Title);
  laDatabase.Caption:= '';

  cbSourceTable.Items.CommaText:= dmSysTables.GetDBObjectNames(SourceIndex, otTables, count);
  cbSourceTable.Text:= ATableName;
  SynSQLSyn1.TableNames.Text:= cbSourceTable.Text;
  cbSourceTableChange(nil);
end;

initialization
  {$I copytable.lrs}

end.

