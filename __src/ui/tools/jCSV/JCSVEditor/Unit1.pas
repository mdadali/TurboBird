unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, jvCSVBase;

type

  { TForm1 }

  TForm1 = class(TForm)
    CSVBase: TjvCSVBase;
    jvCSVCheckBox1: TjvCSVCheckBox;
    jvCSVComboBox1: TjvCSVComboBox;
    jvCSVEdit1: TjvCSVEdit;
    jvCSVEdit2: TjvCSVEdit;
    jvCSVEdit3: TjvCSVEdit;
    jvCSVEdit4: TjvCSVEdit;
    jvCSVEdit5: TjvCSVEdit;
    jvCSVEdit6: TjvCSVEdit;
    jvCSVEdit7: TjvCSVEdit;
    jvCSVEdit8: TjvCSVEdit;
    jvCSVEdit9: TjvCSVEdit;
    jvCSVLabel1: TjvCSVLabel;
    jvCSVNavigator1: TjvCSVNavigator;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
begin
  CSVBase.CSVFileName:=ExtractFilePath(ParamStr(0))+'example.csv';
  CSVbase.DataBaseOpen(ExtractFilePath(ParamStr(0))+'example.csv');
end;

initialization
  {$I Unit1.lrs}

end.

