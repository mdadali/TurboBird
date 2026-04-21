program Project1;

{$MODE Delphi}

uses
  Forms, Interfaces,
  ide_editor;  {Form1}

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Teditor, editor);
  Application.Run;
end.
