(*
 *  IBX For Lazarus (Firebird Express)
 *
 *  The contents of this file are subject to the Initial Developer's
 *  Public License Version 1.0 (the "License"); you may not use this
 *  file except in compliance with the License. You may obtain a copy
 *  of the License here:
 *
 *    http://www.firebirdsql.org/index.php?op=doc&id=idpl
 *
 *  Software distributed under the License is distributed on an "AS
 *  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 *  implied. See the License for the specific language governing rights
 *  and limitations under the License.
 *
 *  The Initial Developer of the Original Code is Tony Whyman.
 *
 *  The Original Code is (C) 2015 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*) 
            
unit fScriptEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynCompletion, SynHighlighterSQL, Forms,
  Controls, Graphics, Dialogs, StdCtrls, ComCtrls, ActnList, ExtCtrls,
  ibxscript, IBDatabase, IB,

  turbocommon;

type

  { TfrmScriptEngine }

  TfrmScriptEngine = class(TForm)
    Button1: TButton;
    Button2: TButton;
    btnClose: TButton;
    DBName: TLabel;
    chkBoxEchoInput: TCheckBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    IBScript: TSynEdit;
    Label1: TLabel;
    OpenBlobDialog: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    ProgressBar1: TProgressBar;
    ResultsLog: TMemo;
    RunScript: TAction;
    LoadScript: TAction;
    ActionList1: TActionList;
    IBDatabase1: TIBDatabase;
    IBTransaction1: TIBTransaction;
    IBXScript1: TIBXScript;
    OpenDialog1: TOpenDialog;
    Splitter1: TSplitter;
    chkBoxStopOnError: TCheckBox;
    SynAutoComplete1: TSynAutoComplete;
    SynCompletion1: TSynCompletion;
    SynSQLSyn1: TSynSQLSyn;
    Timer1: TTimer;
    procedure btnCloseClick(Sender: TObject);
    procedure chkBoxEchoInputChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure IBDatabase1BeforeConnect(Sender: TObject);
    procedure IBXScript1GetParamValue(Sender: TObject; ParamName: string;
      var BlobID: TISC_QUAD);
    procedure IBXScript1LogProc(Sender: TObject; Msg: string);
    procedure IBXScript1ProgressEvent(Sender: TObject; Reset: boolean;
      value: integer);
    procedure IBXScript1SelectSQL(Sender: TObject; SQLText: string);
    procedure LoadScriptExecute(Sender: TObject);
    procedure RunScriptExecute(Sender: TObject);
    procedure RunScriptUpdate(Sender: TObject);
    procedure chkBoxStopOnErrorChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FDBIndex: integer;
    procedure DoOpen(Data: PtrInt);
    procedure ReadScripterSettings;
  public
    procedure Init(dbIndex: integer);
  end;

//var
  //frmScriptEngine: TfrmScriptEngine;

implementation

uses IBBlob, DB, selectsqlresultsunit_ext;

{$R *.lfm}

{ TfrmScriptEngine }

procedure TfrmScriptEngine.Init(dbIndex: integer);
var DBRec: TDatabaseRec;
begin
  FDBIndex := dbIndex;

  DBRec := RegisteredDatabases[FDBIndex];

  if IBDatabase1.Connected then
    IBDatabase1.Connected := false;

  AssignIBDatabase(DBRec.IBDatabase, IBDatabase1);
  IBDatabase1.Params.Clear;
  IBDatabase1.Params.Add('user_name=' + DBRec.RegRec.UserName);
  IBDatabase1.Params.Add('password=' + DBRec.RegRec.Password);

  IBDatabase1.Connected := true;

end;

procedure TfrmScriptEngine.ReadScripterSettings;
begin
  turbocommon.ReadIniFile;

  IBXScript1.AutoDDL := AutoDDL;
  IBXScript1.StopOnFirstError := StopOnFirstError;
  IBXScript1.Echo := EchoInput;
  IBXScript1.IgnoreCreateDatabase := IgnoreCreateDatabase;
  IBXScript1.IgnoreGrants := IgnoreGrants;
  IBXScript1.ShowAffectedRows := ShowAffectedRows;
  IBXScript1.ShowPerformanceStats := ShowPerformanceStats;
end;

procedure TfrmScriptEngine.FormShow(Sender: TObject);
var BasePath, SqlPath: string;
begin
  BasePath := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
  SqlPath  := BasePath + 'data' + PathDelim + 'sql_scripts' + PathDelim;
  OpenDialog1.InitialDir := SqlPath;

  ReadScripterSettings; //from inifile

  ResultsLog.Lines.Clear;
  //IBScript.Lines.Clear;
  DBName.Caption := IBDatabase1.DatabaseName;
  chkBoxStopOnError.Checked := IBXScript1.StopOnFirstError;
  chkBoxEchoInput.Checked :=  IBXScript1.Echo;
//  Application.QueueAsyncCall(@DoOpen,0);
end;

procedure TfrmScriptEngine.IBDatabase1BeforeConnect(Sender: TObject);
begin
  with (Sender as TIBDatabase) do
  begin
    LoginPrompt := (Params.IndexOfName('user_name') = -1) or
                   (Params.IndexOfName('password') = -1);
  end;
end;

procedure TfrmScriptEngine.chkBoxEchoInputChange(Sender: TObject);
begin
  IBXScript1.Echo :=  chkBoxEchoInput.Checked;
end;

procedure TfrmScriptEngine.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  //inifile
  EchoInput := chkBoxEchoInput.Checked;
  StopOnFirstError := chkBoxStopOnError.Checked;
  WriteIniFile;
  CloseAction := caFree;
end;

procedure TfrmScriptEngine.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmScriptEngine.IBXScript1GetParamValue(Sender: TObject; ParamName: string;
  var BlobID: TISC_QUAD);
var Blob: TIBBlobStream;
    Source: TStream;
begin
  OpenBlobDialog.Title := 'Resolve Query Parameter: ''' + ParamName + '''';
  if OpenBlobDialog.Execute then
  begin
    ResultsLog.Lines.Add('Loading ' + ParamName + ' from ' + OpenBlobDialog.FileName);
    Blob := TIBBlobStream.Create;
    try
      Blob.Database := (Sender as TIBXScript).Database;
      Blob.Mode := bmWrite;
      Source := TFileStream.Create(OpenBlobDialog.FileName,fmOpenRead or fmShareDenyNone);
      try
        Blob.CopyFrom(Source,0)
      finally
        Source.Free;
      end;
      Blob.Finalize;
      BlobID := Blob.BlobID;
    finally
      Blob.Free;
    end;
  end
  else
    raise Exception.Create('Unable to resolve statement parameter');
end;

procedure TfrmScriptEngine.IBXScript1LogProc(Sender: TObject; Msg: string);
begin
  ResultsLog.Lines.Add(Msg);
end;

procedure TfrmScriptEngine.IBXScript1ProgressEvent(Sender: TObject; Reset: boolean;
  value: integer);
begin
  if Reset then
  begin
    ProgressBar1.Position :=  0;
    ProgressBar1.Max := value;
  end
  else
    ProgressBar1.StepIt;

  Application.ProcessMessages;
end;

procedure TfrmScriptEngine.IBXScript1SelectSQL(Sender: TObject; SQLText: string);
var SelectSQLResults: TSelectSQLResultsExt;
begin
  SelectSQLResults := TSelectSQLResultsExt.Create(self);
  SelectSQLResults.SelectQuery.Database := IBDatabase1;
  SelectSQLResults.IBTransaction1.DefaultDatabase := IBDatabase1;
  SelectSQLResults.Show(IBXScript1, SQLText, true);
end;

procedure TfrmScriptEngine.LoadScriptExecute(Sender: TObject);
begin
  if OpenDialog1.Execute then
    IBScript.Lines.LoadFromFile(OpenDialog1.FileName);
end;

procedure TfrmScriptEngine.RunScriptExecute(Sender: TObject);
begin
  //ReadScripterSettings; //from inifile
  ResultsLog.Lines.Clear;
  IBXScript1.RunScript(IBScript.Lines);
  Timer1.Interval := 1000;
  chkBoxEchoInput.Checked := IBXScript1.Echo;
  chkBoxStopOnError.Checked := IBXScript1.StopOnFirstError;
  DBName.Caption := IBDatabase1.DatabaseName;
end;

procedure TfrmScriptEngine.RunScriptUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := IBScript.Lines.Text <> '';
end;

procedure TfrmScriptEngine.chkBoxStopOnErrorChange(Sender: TObject);
begin
   IBXScript1.StopOnFirstError := chkBoxStopOnError.Checked;
end;

procedure TfrmScriptEngine.Timer1Timer(Sender: TObject);
begin
  Timer1.Interval := 0;
  ProgressBar1.Position := 0;
end;

procedure TfrmScriptEngine.DoOpen(Data: PtrInt);
begin
  try
    IBDatabase1.Connected := true;
  except on E: Exception do
    begin
      MessageDlg(E.Message,mtError,[mbOK],0);
      Application.Terminate;
    end;
  end;
end;

end.

