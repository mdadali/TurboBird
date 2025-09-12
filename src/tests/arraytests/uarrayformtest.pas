unit uArrayFormTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids,
  Variants,
  IBDatabase, IBCustomDataSet, IBQuery, IB, IBTable, IBSQL,

  IBArrayHelper;

type
  TfrmArrayTest = class(TForm)
    StringGrid1: TStringGrid;
    procedure FormCreate(Sender: TObject);
  private
    procedure LoadArrayData;
  public
  end;

var
  frmArrayTest: TfrmArrayTest;

implementation

{$R *.lfm}

procedure TfrmArrayTest.FormCreate(Sender: TObject);
begin
  // Grid initialisieren
  StringGrid1.ColCount := 7; // Job_Code + 5 Array-Elemente + AsString
  StringGrid1.RowCount := 1; // Header
  StringGrid1.FixedRows := 1;

  StringGrid1.Cells[0,0] := 'JOB_CODE';
  StringGrid1.Cells[1,0] := 'LANG 1';
  StringGrid1.Cells[2,0] := 'LANG 2';
  StringGrid1.Cells[3,0] := 'LANG 3';
  StringGrid1.Cells[4,0] := 'LANG 4';
  StringGrid1.Cells[5,0] := 'LANG 5';
  StringGrid1.Cells[6,0] := 'AsString';

  LoadArrayData;
end;

procedure TfrmArrayTest.LoadArrayData;
var
  DB: TIBDatabase;
  Trans: TIBTransaction;
  Qry: TIBQuery;
  ArrField: TIBArrayField;
  lower, upper, dims, row, i: Integer;
begin
{  DB := TIBDatabase.Create(nil);
  Trans := TIBTransaction.Create(nil);
  Qry := TIBQuery.Create(nil);
  try
    // Firebird employee.fdb anpassen
    DB.DatabaseName := 'localhost:C:\firebird\examples\empbuild\employee.fdb';
    DB.LoginPrompt := False;
    DB.Params.Add('user_name=sysdba');
    DB.Params.Add('password=masterkey');

    Trans.DefaultDatabase := DB;
    DB.Connected := True;

    Qry.Database := DB;
    Qry.Transaction := Trans;
    Trans.StartTransaction;

    Qry.SQL.Text := 'select job_code, language_req from job';
    Qry.Open;

    row := 1;
    while not Qry.EOF do
    begin
      StringGrid1.RowCount := row + 1;
      StringGrid1.Cells[0,row] := Qry.FieldByName('JOB_CODE').AsString;

      ArrField := TIBArrayField(Qry.FieldByName('LANGUAGE_REQ'));
      dims := TIBArrayHelper.GetDimensions(ArrField);

      if dims = 1 then
      begin
        TIBArrayHelper.GetBounds(ArrField, 0, lower, upper);

        for i := lower to upper do
          StringGrid1.Cells[i,row] :=
            VarToStr(TIBArrayHelper.GetElement(ArrField, [i]));
      end;

      StringGrid1.Cells[6,row] := TIBArrayHelper.AsString(ArrField);

      Inc(row);
      Qry.Next;
    end;

    Trans.Commit;
  finally
    Qry.Free;
    Trans.Free;
    DB.Free;
  end;}
end;

end.

