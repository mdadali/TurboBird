unit fsqlmonitor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, SynEdit,
  SynHighlighterSQL, IBSQLMonitor, uthemeselector;

type

  { TfmSQLMonitor }

  TfmSQLMonitor = class(TForm)
    IBSQLMonitor1: TIBSQLMonitor;
    SynEditMonitor: TSynEdit;
    SynSQLSyn1: TSynSQLSyn;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IBSQLMonitor1SQL(EventText: String; EventTime: TDateTime);
  private

  public

  end;

var
  fmSQLMonitor: TfmSQLMonitor;

implementation

{$R *.lfm}

{ TfmSQLMonitor }

procedure TfmSQLMonitor.IBSQLMonitor1SQL(EventText: String; EventTime: TDateTime);
begin
  SynEditMonitor.Lines.Add(FormatDateTime('dd/mm/yyyy hh:nn:ss.zzzz',EventTime) + ': ' + EventText);
  Application.ProcessMessages;
end;

procedure TfmSQLMonitor.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  if IBSQLMonitor1.Enabled then
    IBSQLMonitor1.Enabled := false;

  DisableMonitoring;
  Application.ProcessMessages;
  CloseAction := caHide;
end;

procedure TfmSQLMonitor.FormCreate(Sender: TObject);
begin
  IBSQLMonitor1.Enabled := true;
end;

procedure TfmSQLMonitor.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(self);
  EnableMonitoring;
  Application.ProcessMessages;
end;

end.

