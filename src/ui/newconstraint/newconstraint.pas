unit NewConstraint;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, CheckLst, QueryWindow,
  turbocommon,
  fmetaquerys,
  uthemeselector;

type

  { TfmNewConstraint }

  TfmNewConstraint = class(TForm)
    bbScript: TBitBtn;
    cbUpdateAction: TComboBox;
    cbTables: TComboBox;
    clxForFields: TCheckListBox;
    clxOnFields: TCheckListBox;
    edNewName: TEdit;
    cbDeleteAction: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    laTable: TLabel;
    procedure bbScriptClick(Sender: TObject);
    procedure cbTablesChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    DatabaseIndex: Integer;
    QWindow: TfmQueryWindow;
    { public declarations }
  end; 

var
  fmNewConstraint: TfmNewConstraint;

implementation

uses main;

{ TfmNewConstraint }

procedure TfmNewConstraint.cbTablesChange(Sender: TObject);
var FieldsList: TStringList;
    Iso: TIsolatedQuery;
begin
  // Get foreign table fields
  FieldsList:= TStringList.Create;
  try
    Iso := GetFieldsIsolated(RegisteredDatabases[DatabaseIndex].IBDatabase, cbTables.Text, FieldsList);
    clxForFields.Clear;
    clxForFields.Items.AddStrings(FieldsList);
  finally
    FieldsList.Free;
    Iso.Free;
  end;
end;

procedure TfmNewConstraint.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(self);
end;

procedure TfmNewConstraint.bbScriptClick(Sender: TObject);
var
  CurrFields, ForFields: string;
  i: Integer;
  TargetTable, RefTable: string;
begin
  CurrFields := '';
  ForFields := '';

  // Zusammenstellen der aktuellen Spalten
  for i := 0 to clxOnFields.Count - 1 do
  begin
    if clxOnFields.Checked[i] then
    begin
      if IsObjectNameCaseSensitive(clxOnFields.Items[i]) and
         not IsObjectNameQuoted(clxOnFields.Items[i]) then
        clxOnFields.Items[i] := MakeObjectNameQuoted(clxOnFields.Items[i]);
      CurrFields := CurrFields + clxOnFields.Items[i] + ', ';
    end;
  end;

  if CurrFields.EndsWith(', ') then
    CurrFields := Copy(CurrFields, 1, Length(CurrFields)-2);

  // Zusammenstellen der referenzierten Spalten
  for i := 0 to clxForFields.Count - 1 do
  begin
    if clxForFields.Checked[i] then
    begin
      if IsObjectNameCaseSensitive(clxForFields.Items[i]) and
         not IsObjectNameQuoted(clxForFields.Items[i]) then
        clxForFields.Items[i] := MakeObjectNameQuoted(clxForFields.Items[i]);
      ForFields := ForFields + clxForFields.Items[i] + ', ';
    end;
  end;

  if ForFields.EndsWith(', ') then
    ForFields := Copy(ForFields, 1, Length(ForFields)-2);

  // Tabellen
  TargetTable := laTable.Caption;
  if IsObjectNameCaseSensitive(TargetTable) and not IsObjectNameQuoted(TargetTable) then
    TargetTable := MakeObjectNameQuoted(TargetTable);

  RefTable := cbTables.Text;
  if IsObjectNameCaseSensitive(RefTable) and not IsObjectNameQuoted(RefTable) then
    RefTable := MakeObjectNameQuoted(RefTable);

  // Query erstellen
  QWindow := fmMain.ShowQueryWindow(DatabaseIndex, 'new constraint on table : ' + TargetTable);
  QWindow.meQuery.Lines.Text := 'ALTER TABLE ' + TargetTable +
                                ' ADD CONSTRAINT ' + edNewName.Text +
                                ' FOREIGN KEY (' + CurrFields + ')' +
                                ' REFERENCES ' + RefTable + ' (' + ForFields + ')';

  if cbUpdateAction.Text <> 'Restrict' then
    QWindow.meQuery.Lines.Add(' ON UPDATE ' + cbUpdateAction.Text);
  if cbDeleteAction.Text <> 'Restrict' then
    QWindow.meQuery.Lines.Add(' ON DELETE ' + cbDeleteAction.Text);

  fmMain.Show;
end;

initialization
  {$I newconstraint.lrs}

end.

