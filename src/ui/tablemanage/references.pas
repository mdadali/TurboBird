unit References;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, Buttons,

  turbocommon,
  fbcommon,

  fsimpleobjextractor,

  uthemeselector;

type

  { TfmReferences }

  TfmReferences = class(TForm)
    bbRefreshReferences: TBitBtn;
    sgReferences: TStringGrid;

    procedure bbRefreshClick(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private
    FDBIndex: Integer;
    FTableName: string;
    FNodeInfos: TPNodeInfos;
    FExtractor: TSimpleObjExtractor;

  public
    procedure Init(ADBIndex: Integer; const ATableName: string;
                   ANodeInfos: TPNodeInfos; AExtractor: TSimpleObjExtractor);
    procedure FillReferences;
  end;


implementation

{$R references.lfm}

uses Main;

{ TfmReferences }

procedure TfmReferences.Init(ADBIndex: Integer; const ATableName: string;
  ANodeInfos: TPNodeInfos; AExtractor: TSimpleObjExtractor);
begin
  FDBIndex := ADBIndex;
  FTableName := ATableName;
  FNodeInfos := ANodeInfos;
  FExtractor := AExtractor;

  Caption := 'References: ' + FTableName;
end;

procedure TfmReferences.FillReferences;
var
  Refs: TForeignKeyInfoArray;
  i, Row: Integer;
begin
  sgReferences.RowCount := 1;

  Refs := FExtractor.GetTableReferences(MakeCaseSensitiveAuto(FTableName));

  for i := 0 to High(Refs) do
  begin
    sgReferences.RowCount := sgReferences.RowCount + 1;
    Row := sgReferences.RowCount - 1;

    sgReferences.Cells[0, Row] := MakeCaseSensitiveAuto(Refs[i].ConstraintName);    // FK-Name
    sgReferences.Cells[1, Row] := MakeCaseSensitiveAuto(Refs[i].ForeignTable);      // referenzierende Tabelle
    sgReferences.Cells[2, Row] := MakeCaseSensitiveAuto(Refs[i].ForeignFields);     // Feld(er) dort
    sgReferences.Cells[3, Row] := MakeCaseSensitiveAuto(Refs[i].MasterFields);      // Feld(er) bei uns
  end;

  if sgReferences.RowCount > 1 then
    sgReferences.Row := 1;
end;

procedure TfmReferences.bbRefreshClick(Sender: TObject);
begin
  FillReferences;
end;

procedure TfmReferences.FormShow(Sender: TObject);
begin
  FillReferences;
  frmThemeSelector.btnApplyClick(Self);
end;

end.
