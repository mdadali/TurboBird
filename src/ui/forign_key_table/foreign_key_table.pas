unit foreign_key_table;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, DBCtrls,
  IBDatabase, IBTable, RxDBGrid, ExtCtrls,

  turbocommon,
  uthemeselector;

type

  { TfrmForeignKeyTable }

  TfrmForeignKeyTable = class(TForm)
    dsForeingKeyTable: TDataSource;
    DBNavigator1: TDBNavigator;
    IBTableForeingKey: TIBTable;
    pnlFKTableName: TPanel;
    RxDBGrid1: TRxDBGrid;
    procedure FormShow(Sender: TObject);
  private
    FDatabase: TIBDatabase;
    FTransaction: TIBTransaction;
    FForeignKeyInfo: TForeignKeyInfo;
  public
    procedure Init(AIBDatabase: TIBDatabase;
                   ATransaction: TIBTransaction;
                   AForeignKeyInfo: TForeignKeyInfo);

    function GetSelectedValue: Variant;
    function GetForeignKeyField: string;

    property ForeignKeyInfo: TForeignKeyInfo read  FForeignKeyInfo;
  end;

implementation

{$R *.lfm}

function TfrmForeignKeyTable.GetSelectedValue: Variant;
begin
  Result := IBTableForeingKey.FieldByName(
              FForeignKeyInfo.MasterField).Value;
end;

function TfrmForeignKeyTable.GetForeignKeyField: string;
begin
  Result := FForeignKeyInfo.ForeignField;
end;

procedure TfrmForeignKeyTable.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(self);
end;

procedure TfrmForeignKeyTable.Init(AIBDatabase: TIBDatabase;
                                   ATransaction: TIBTransaction;
                                   AForeignKeyInfo: TForeignKeyInfo);
begin
  FDatabase := AIBDatabase;
  FTransaction := ATransaction;
  FForeignKeyInfo := AForeignKeyInfo;

  IBTableForeingKey.Database := FDatabase;
  IBTableForeingKey.Transaction := FTransaction;
  IBTableForeingKey.TableName := FForeignKeyInfo.MasterTable;

  pnlFKTableName.Caption := FForeignKeyInfo.MasterTable;

  IBTableForeingKey.Open;
  RxDBGrid1.OptimizeColumnsWidthAll;
end;

end.
