program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, zcomponent, ibexpress, Unit1, QBDBFrm, QBDBFrm2, QBAbout, QBuilder
  {$IFDEF QBESQLDB}
  , QBESqlDb
  {$ENDIF}
  {$IFDEF QBEZEOS}
  , QBEZEOS
  {$ENDIF}
  {$IFDEF QBEIBX}
  , QBEIBX
  {$ENDIF}
  , QBDirFrm, QBLnkFrm
  ;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  //Application.CreateForm(TOQBForm, OQBForm);
  Application.Run;
end.

