program rvdemo;

uses
  Interfaces,
  Forms,
  Unit1 in 'unit1.pas' {Form1},
  BackStyl in 'backstyl.pas' {frmBackStyle},
  PrintFrm in 'printfrm.pas' {frmPrint}, lazrichview;

{.$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TfrmBackStyle, frmBackStyle);
  Application.CreateForm(TfrmPrint, frmPrint);
  Application.Run;
end.
