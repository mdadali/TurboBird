program Restore;

{$MODE Delphi}

uses
{$IFDEF LINUX}Forms, Interfaces,{$ELSE}Forms,{$ENDIF}
  main in 'main.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
