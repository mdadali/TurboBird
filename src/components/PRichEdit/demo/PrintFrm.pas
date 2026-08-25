{$mode objfpc}{$H+}

unit PrintFrm;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, LResources;

type

  { TfrmPrint }

  TfrmPrint = class(TForm)
    Image1: TImage;
    Panel1: TPanel;
    Label1: TLabel;
    Panel2: TPanel;
    btnOk: TButton;
    btnCancel: TButton;
    Panel3: TPanel;
    ScrollBox1: TScrollBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmPrint: TfrmPrint;

implementation

{.$R *.DFM}

initialization

{$I PrintFrm.lrs}

end.
