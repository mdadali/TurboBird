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
            
unit ShutdownDatabaseDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, IBXServices, IB;

type

  { TShutdownDatabaseDlg }

  TShutdownDatabaseDlg = class(TForm)
    Bevel1: TBevel;
    CloseBtn: TButton;
    IBConfigService: TIBXConfigService;
    ProgressBar1: TProgressBar;
    StatusMsg: TLabel;
    procedure CloseBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FShutDownmode: TDBShutdownMode;
    FDelay: integer;
    FAborting: boolean;
    FSecContextError: boolean;
    FShutdownWaitThread: TThread;
    procedure OnWaitCompleted(Sender: TObject);
  public
     procedure Shutdown(aShutDownmode: TDBShutdownMode; aDelay: integer);
     property Aborting: boolean read FAborting;
  end;

var
  ShutdownDatabaseDlg: TShutdownDatabaseDlg;

implementation

{$R *.lfm}

resourcestring
  sWaitStatusMsg = 'Waiting for %s to shutdown';
  sDatabaseShutdown  = 'Database has been successfully shutdown';
  sOnCompleted = 'Shutdown of %s completed with response: %s';

type
    { TShutdownWaitThread }

  TShutdownWaitThread = class(TThread)
  private
    FErrorMessage: string;
    FIBConfigService: TIBXConfigService;
    FOptions: TDBShutdownMode;
    FSecContextError: boolean;
    FSuccess: boolean;
    FWait: integer;
    FOnCompleted: TNotifyEvent;
    procedure DoCallback;
  protected
    procedure Execute; override;
  public
    constructor Create(aService: TIBXConfigService; Options: TDBShutdownMode;
      Wait: Integer; OnCompleted: TNotifyEvent);
    destructor Destroy; override;
    procedure Abort;
    property Success: boolean read FSuccess;
    property SecContextError: boolean read FSecContextError;
    property ErrorMessage: string read FErrorMessage;
  end;



{ TShutdownWaitThread }

procedure TShutdownWaitThread.DoCallback;
begin
  if assigned(FOnCompleted) then
    FOnCompleted(self);
end;

procedure TShutdownWaitThread.Execute;
begin
  FSuccess := false;
  try
    try
      FIBConfigService.ShutDownDatabase(FOptions,FWait);
      FErrorMessage := 'Completed without error';
      FSuccess := true;
    except
      on E: Exception do
        FErrorMessage := E.Message;
    end;
  finally
    if Terminated and FSuccess then
      FIBConfigService.BringDatabaseOnline;
  end;
  Synchronize(@DoCallback);
end;

constructor TShutdownWaitThread.Create(aService: TIBXConfigService;
  Options: TDBShutdownMode; Wait: Integer; OnCompleted: TNotifyEvent);
begin
  inherited Create(false);
  FOptions := Options;
  FWait := Wait;
  FOnCompleted := OnCompleted;
  FreeOnTerminate := true;
  FIBConfigService := TIBXConfigService.Create(nil);
  FIBConfigService.ServicesConnection := aService.ServicesConnection;
  FIBConfigService.DatabaseName := aService.DatabaseNAme;
end;

destructor TShutdownWaitThread.Destroy;
begin
  if FIBConfigService <> nil then FIBConfigService.Free;
  inherited Destroy;
end;

procedure TShutdownWaitThread.Abort;
begin
  Terminate;
end;

{ TShutdownDatabaseDlg }

procedure TShutdownDatabaseDlg.FormShow(Sender: TObject);
begin
  FAborting := false;
  StatusMsg.Caption := Format(sWaitStatusMsg,[IBConfigService.DatabaseName]);
  FShutdownWaitThread := TShutdownWaitThread.Create(IBConfigService,FShutDownMode,FDelay,@OnWaitCompleted);
end;

procedure TShutdownDatabaseDlg.CloseBtnClick(Sender: TObject);
begin
  FAborting := true;
  FShutdownWaitThread.Terminate;
  Close;
end;

procedure TShutdownDatabaseDlg.OnWaitCompleted(Sender: TObject);
begin
  with TShutdownWaitThread(Sender) do
    if not FAborting then
      MessageDlg(Format(sOnCompleted,[IBConfigService.DatabaseName,ErrorMessage]),
               mtInformation,[mbOK],0);
  FAborting := false;
  Close;
end;

procedure TShutdownDatabaseDlg.Shutdown(aShutDownmode: TDBShutdownMode;
  aDelay: integer);
begin
  FShutDownmode := aShutDownmode;
  FDelay := aDelay;
  FSecContextError := false;
  if aDelay <= 0 then
  begin
      IBConfigService.ShutDownDatabase(aShutDownmode,0);
      MessageDlg(sDatabaseShutdown,mtInformation,[mbOK],0);
  end
  else
    ShowModal;
end;

end.

