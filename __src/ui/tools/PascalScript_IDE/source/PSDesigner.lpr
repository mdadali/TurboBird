program PSDesigner;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, abbrevia, runtimetypeinfocontrols, printer4lazarus,

  jvRuntimeDesign,
  pascalscript,
  uPSI_IBXConnection,
  uPSI_uibconst,

  u_psstudio,
  u_consoleide;

{$R *.res}

procedure ApplicationOnIdle(Sender: TObject; var Done: Boolean);
begin
  // Hier kann man Code ausführen, wenn die Anwendung inaktiv ist
  Done := True;  // Setze Done auf True, um zu zeigen, dass die Anwendung nichts mehr zu tun hat
end;


begin
  Application.Initialize;
  //PSStudio := TfrmPSStudio.Create(nil);
  //PSStudio.Visible := true;
  Application.CreateForm(TfrmPSStudio, PSStudio);
  Application.Run;
end.

