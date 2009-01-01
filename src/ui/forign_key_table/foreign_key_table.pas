unit foreign_key_table;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, DBCtrls,
  IBDatabase, IBTable, RxDBGrid, ExtCtrls,

  Variants,

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

    function GetSelectedValues: Variant;   // Variant-Array
    function GetForeignKeyFields: string;  // Feldliste mit ;

    property ForeignKeyInfo: TForeignKeyInfo read  FForeignKeyInfo;
  end;

implementation

{$R *.lfm}

function TfrmForeignKeyTable.GetSelectedValues: Variant;
var
  FieldList: TStringList;
  i: Integer;
begin
  Result := Null;

  if not IBTableForeingKey.Active then Exit;
  if IBTableForeingKey.IsEmpty then Exit;

  FieldList := TStringList.Create;
  try
    FieldList.Delimiter := ';';
    FieldList.StrictDelimiter := True;
    FieldList.DelimitedText := FForeignKeyInfo.MasterFields;

    Result := VarArrayCreate([0, FieldList.Count - 1], varVariant);

    for i := 0 to FieldList.Count - 1 do
      Result[i] :=
        IBTableForeingKey.FieldByName(
          Trim(FieldList[i])
        ).Value;

  finally
    FieldList.Free;
  end;
end;

function TfrmForeignKeyTable.GetForeignKeyFields: string;
begin
  Result := FForeignKeyInfo.ForeignFields;
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
