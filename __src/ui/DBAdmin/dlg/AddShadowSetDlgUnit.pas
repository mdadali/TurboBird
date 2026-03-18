(*
 * AddShadowSetDlgUnit.pas
 * Copyright (C) 2018 Tony Whyman <tony@mwasoftware.co.uk>
 *
 * DBAdmin is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * DBAdmin is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along
 * with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)
unit AddShadowSetDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Controls,
  ExtCtrls, StdCtrls, ActnList, memds, db, IBDatabase, IBSQL, IBDatabaseInfo,
  IBDynamicGrid, Forms, Dialogs, DbCtrls, ComCtrls, IB;

type

  { TAddShadowSetDlg }

  TAddShadowSetDlg = class(TForm)
    Add: TAction;
    Bevel1: TBevel;
    CancelBtn: TButton;
    Creating: TLabel;
    ExecSQL: TIBSQL;
    IBDatabaseInfo: TIBDatabaseInfo;
    OKBtn: TButton;
    ProgressBar: TProgressBar;
    Remove: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    ShadowFileSource: TDataSource;
    IBDynamicGrid1: TIBDynamicGrid;
    Label1: TLabel;
    Label2: TLabel;
    ShadowFileList: TMemDataset;
    RemoveBtn: TButton;
    ShadowMode: TRadioGroup;
    ShadowSet: TEdit;
    WaitTimer: TTimer;
    procedure AddExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure RemoveExecute(Sender: TObject);
    procedure RemoveUpdate(Sender: TObject);
    procedure ShadowFileListBeforeClose(DataSet: TDataSet);
    procedure ShadowSetEditingDone(Sender: TObject);
    procedure WaitTimerTimer(Sender: TObject);
  private
    FShadowSet: integer;
    FShadowThread: TThread;
  public
    function ShowModal( aShadowSet: integer): TModalResult;
  end;

var
  AddShadowSetDlg: TAddShadowSetDlg;

implementation

uses AddShadowFileDlgUnit;

{$R *.lfm}

const
  sCreateShadow      = 'Create Shadow %d %s ''%s''';
  sCreateFirstShadow = 'Create Shadow %d %s ''%s'' LENGTH %d';
  sNextShadow        = 'FILE ''%s'' LENGTH %d';
  sCreateLastShadow  = 'FILE ''%s'' STARTING AT %d';

resourcestring
  sLengthIgnored = 'The Length is ignore for the last or only file in the Shadow Set';
  sNoLength = 'A Length must be specified for all but the last file in a multi-file set';

type

  { TCreateShadowThread }

  TCreateShadowThread = class(TThread)
  private
    FOwner: TAddShadowSetDlg;
    FSQLText: string;
    FErrorMessage: string;
  protected
    procedure Execute; override;
  public
    constructor Create(aOwner: TAddShadowSetDlg; SQLText: string);
    property ErrorMessage: string read FErrorMessage;
  end;

{ TCreateShadowThread }

procedure TCreateShadowThread.Execute;
begin
  FErrorMessage := '';
  try
    with FOwner.ExecSQL do
    begin
      Transaction.Active := true;
      SQL.Text := FSQLText;
      ExecQuery;
      Transaction.Commit;
    end;
  except On E:Exception do
    FErrorMessage := E.Message;
  end;
end;

constructor TCreateShadowThread.Create(aOwner: TAddShadowSetDlg; SQLText: string
  );
begin
  inherited Create(false);
  FOwner := aOwner;
  FErrorMessage := '';
  FSQLText := SQLText;
end;



{ TAddShadowSetDlg }

procedure TAddShadowSetDlg.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
var SQLText: string;
    Mode: string;
    StartAt: integer;
    FileName: string;
    FileLength: integer;
begin
  if ModalResult = mrYes then
  begin
    ShadowFileList.DisableControls;
    ShadowFileList.Last;
    if not ShadowFileList.FieldByName('ShadowFileLength').IsNull then
    begin
      MessageDlg(sLengthIgnored,mtWarning,[mbOK],0);
      CloseAction := caNone;
      Exit;
    end;

    ShadowFileList.First;
    case ShadowMode.ItemIndex of
    0: Mode := 'AUTO';
    1: Mode := 'MANUAL';
    2: Mode := 'CONDITIONAL';
    end;
    if ShadowFileList.RecordCount = 1 then
      SQLText := Format(sCreateShadow,[FShadowSet,Mode,ShadowFileList.FieldByName('ShadowFileName').AsString])
    else
    begin
      if ShadowFileList.FieldByName('ShadowFileLength').AsInteger = 0 then
      begin
        MessageDlg(sNoLength,mtError,[mbOK],0);
        CloseAction := caNone;
        Exit;
      end;
      SQLText := Format(sCreateFirstShadow,[FShadowSet,Mode,
                                            ShadowFileList.FieldByName('ShadowFileName').AsString,
                                            ShadowFileList.FieldByName('ShadowFileLength').AsInteger]);
      StartAt := ShadowFileList.FieldByName('ShadowFileLength').AsInteger;
      ShadowFileList.Next;
      while not ShadowFileList.EOF do
      begin
        FileName := ShadowFileList.FieldByName('ShadowFileName').AsString;
        FileLength := ShadowFileList.FieldByName('ShadowFileLength').AsInteger;
        ShadowFileList.Next;
        if ShadowFileList.EOF then
          SQLText := SQLText + ' ' + Format(sCreateLastShadow,[FileName,StartAt+1])
        else
        begin
          if FileLength = 0 then
          begin
            MessageDlg(sNoLength,mtError,[mbOK],0);
            CloseAction := caNone;
            Exit;
          end;
          Inc(StartAt,FileLength);
          SQLText := SQLText + ' ' + Format(sNextShadow,[FileName,FileLength]);
        end;
      end;
    end;
    Creating.Visible := true;
    ProgressBar.Visible := true;
    Application.ProcessMessages;
//     writeln(SQLText);
    FShadowThread := TCreateShadowThread.Create(self,SQLText);
    WaitTimer.Enabled := true;
    CloseAction := caNone;
  end;
end;

procedure TAddShadowSetDlg.FormShow(Sender: TObject);
begin
  Creating.Visible := false;
  ProgressBar.Visible := false;
  ShadowFileList.Clear(false);
end;

procedure TAddShadowSetDlg.AddExecute(Sender: TObject);
var aFileName: string;
    aFileLength: integer;
    Pages: boolean;
begin
  if AddShadowFileDlg.ShowModal(aFileName,aFileLength,Pages) = mrOK then
  begin
    if not Pages then
    begin
      if aFileLength <> -1 then
        aFileLength := aFileLength*1024*1024 div IBDatabaseInfo.PageSize;
    end;
    with ShadowFileList do
    begin
      Append;
      FieldByName('ShadowFileName').AsString := aFileName;
      if aFileLength <> -1 then
        FieldByName('ShadowFileLength').AsInteger := aFileLength;
      Post;
    end;
  end;
end;

procedure TAddShadowSetDlg.RemoveExecute(Sender: TObject);
begin
  ShadowFileList.Delete;
end;

procedure TAddShadowSetDlg.RemoveUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := ShadowFileList.Active and (ShadowFileList.RecordCount > 0);
end;

procedure TAddShadowSetDlg.ShadowFileListBeforeClose(DataSet: TDataSet);
begin
  ShadowFileList.Clear(false);
end;

procedure TAddShadowSetDlg.ShadowSetEditingDone(Sender: TObject);
begin
  FShadowSet := StrToInt(ShadowSet.Text);
end;

procedure TAddShadowSetDlg.WaitTimerTimer(Sender: TObject);
begin
  with TCreateShadowThread(FShadowThread) do
  begin
    if Finished then
    begin
      if ErrorMessage <> '' then
      begin
        MessageDlg(ErrorMessage,mtError,[mbOK],0);
        ModalResult := mrCancel;
      end;
      ShadowFileList.EnableControls;
      WaitTimer.Enabled := false;
      ModalResult := mrOK;
      Free;
    end;
  end;
end;

function TAddShadowSetDlg.ShowModal(aShadowSet: integer): TModalResult;
begin
  FShadowSet := aShadowSet;
  ShadowSet.Text := IntToStr(aShadowSet);
  Result := inherited ShowModal;
end;


end.

