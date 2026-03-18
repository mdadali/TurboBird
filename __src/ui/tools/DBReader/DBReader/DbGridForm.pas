unit DbGridForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, Grids, DBGrids, DBReaderBase;

type
  TFormDbGrid = class(TForm)
    dbgr: TDBGrid;
    ds1: TDataSource;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FDataSet: TDbReaderDataSet;
  public
    { Public declarations }
    property DataSet: TDbReaderDataSet read FDataSet;
  end;

var
  FormDbGrid: TFormDbGrid;

implementation

{$R *.dfm}

procedure TFormDbGrid.FormCreate(Sender: TObject);
begin
  FDataSet := TDbReaderDataSet.Create(Self);
  ds1.DataSet := DataSet;
end;

end.
