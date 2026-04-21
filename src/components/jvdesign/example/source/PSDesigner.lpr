program PSDesigner;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, runtimetypeinfocontrols, printer4lazarus, pascalscript,
  pascalscriptfcl, designer, ide_editor, jvRuntimeDesign;

{$R *.res}

procedure ApplicationOnIdle(Sender: TObject; var Done: Boolean);
begin
  // Hier kann man Code ausführen, wenn die Anwendung inaktiv ist
  Done := True;  // Setze Done auf True, um zu zeigen, dass die Anwendung nichts mehr zu tun hat
end;


begin
  Application.Initialize;
  // Application.OnIdle := @ApplicationOnIdle;  // Setze das OnIdle-Ereignis
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

