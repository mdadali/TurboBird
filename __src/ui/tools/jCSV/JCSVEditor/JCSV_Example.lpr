program JCSV_Example;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1, LResources, csvBase
  { you can add units after this };

{$IFDEF WINDOWS}{$R JCSV_Example.rc}{$ENDIF}

begin
  {$I JCSV_Example.lrs}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

