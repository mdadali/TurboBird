unit fsqlmonitor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, IBSQLMonitor;

type

  { TfmSQLMonitor }

  TfmSQLMonitor = class(TForm)
    IBSQLMonitor1: TIBSQLMonitor;
    Memo1: TMemo;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
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
  Memo1.Lines.Add(DateTimeToStr(EventTime));
  Memo1.Lines.Add(EventText);
end;

procedure TfmSQLMonitor.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  //if IBSQLMonitor1.Enabled then
    IBSQLMonitor1.Enabled := false;
  //CloseAction := caFree;
  CloseAction := caHide;
end;

procedure TfmSQLMonitor.FormCreate(Sender: TObject);
begin
  IBSQLMonitor1.Enabled := true;
end;

end.

