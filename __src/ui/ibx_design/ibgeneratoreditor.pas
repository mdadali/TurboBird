(*
 *  IBX For Lazarus (Firebird Express)
 *
 *  The contents of this file are subject to the Initial Developer's
 *  Public License Version 1.0 (the "License"); you may not use this
 *  file except in compliance with the License. You may obtain a copy
 *  of the License here:
 *
 *    http://www.firebirdsql.org/index.php?op=doc&id=idpl
 *
 *  Software distributed under the License is distributed on an "AS
 *  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 *  implied. See the License for the specific language governing rights
 *  and limitations under the License.
 *
 *  The Initial Developer of the Original Code is Tony Whyman.
 *
 *  The Original Code is (C) 2011-17 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)

unit IBGeneratorEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, db, IBDatabase, IBCustomDataSet, IBQuery, IBSQL,
  IBLookupComboEditBox, IB, IBTable;

type

  { TGeneratorEditor }

  TGeneratorEditor = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    GeneratorSource: TDataSource;
    GeneratorQuery: TIBQuery;
    GeneratorNames: TIBLookupComboEditBox;
    FieldNames: TIBLookupComboEditBox;
    IdentifyStatementSQL: TIBSQL;
    IncrementBy: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    OnNewRecord: TRadioButton;
    OnPost: TRadioButton;
    PrimaryKeys: TIBQuery;
    PrimaryKeySource: TDataSource;
    SQLTransaction: TIBTransaction;
    UpDown1: TUpDown;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure PrimaryKeysBeforeOpen(DataSet: TDataSet);
  private
    FGenerator: TIBGenerator;
    { private declarations }
    function GetTableName: string;
    procedure SetGenerator(const AValue: TIBGenerator);
    procedure SetDatabase(aDatabase: TIBDatabase);
  public
    { public declarations }
    property Generator: TIBGenerator read FGenerator write SetGenerator;
  end; 

function EditGenerator(AGenerator: TIBGenerator): boolean;

implementation


{$R *.lfm}

function EditGenerator(AGenerator: TIBGenerator): boolean;
var Database: TIBDatabase;
begin
  Result := false;
  if not (AGenerator.Owner is TIBTable) and
   (((AGenerator.Owner is TIBQuery) and ((AGenerator.Owner as TIBQuery).SQL.Text = '')) or
   ((AGenerator.Owner is TIBDataSet) and ((AGenerator.Owner as TIBDataSet).SelectSQL.Text = ''))) then
  begin
    ShowMessage('No Select SQL Found!');
    Exit
  end;
  Database := AGenerator.Owner.Database;

  if assigned(Database) then
    try
      Database.Connected := true;
    except on E: Exception do
      ShowMessage(E.Message)
    end;

    with TGeneratorEditor.Create(Application) do
    try
      Generator := AGenerator;
      Result := ShowModal = mrOK
    finally
      Free
    end;
end;

{ TGeneratorEditor }

procedure TGeneratorEditor.FormShow(Sender: TObject);
begin
  if (PrimaryKeys.Database = nil) or not PrimaryKeys.Database.Connected then Exit;
  SQLTransaction.Active := true;
  PrimaryKeys.Active := true;
  GeneratorQuery.Active := true;
  if Generator.Generator <> '' then
    GeneratorQuery.Locate('RDB$GENERATOR_NAME',Generator.Generator,[]);
  if Generator.Field <> '' then
    PrimaryKeys.Locate('ColumnName',UpperCase(Generator.Field),[]);

  if Generator.ApplyOnEvent = gaeOnNewRecord then
    OnNewRecord.Checked := true
  else
    OnPost.Checked := true;
  IncrementBy.Text := IntToStr(Generator.Increment);
end;

procedure TGeneratorEditor.PrimaryKeysBeforeOpen(DataSet: TDataSet);
begin
  PrimaryKeys.ParamByName('RDB$RELATION_NAME').AsString := GetTableName;
end;

function TGeneratorEditor.GetTableName: string;
begin
  Result := '';
  with IdentifyStatementSQL do
  begin
    Transaction.Active := true;
    if FGenerator.Owner is TIBTable then
    begin
      Result :=  TIBTable(FGenerator.Owner).TableName;
      Exit;
    end;
    if FGenerator.Owner is TIBQuery then
      SQL.Assign((FGenerator.Owner as TIBQuery).SQL)
    else
      SQL.Assign((FGenerator.Owner as TIBDataset).SelectSQL);
    try
      Prepare;
      if (SQLStatementType = SQLSelect) and (MetaData.Count > 0) then
        Result := MetaData[0].GetRelationName;
    except on E:EIBError do
  //      ShowMessage(E.Message);
    end;
  end;
end;

procedure TGeneratorEditor.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if ModalResult = mrOK then
  begin
    Generator.Generator := GeneratorNames.Text;
    Generator.Field := FieldNames.Text;
    if OnNewRecord.Checked then
      Generator.ApplyOnEvent := gaeOnNewRecord
    else
      Generator.ApplyOnEvent := gaeOnPostRecord;
    Generator.Increment := StrToInt(IncrementBy.Text)

  end;
end;

procedure TGeneratorEditor.SetGenerator(const AValue: TIBGenerator);
begin
  FGenerator := AValue;
  SetDatabase(Generator.Owner.Database);
end;

procedure TGeneratorEditor.SetDatabase(aDatabase: TIBDatabase);
begin
  if not assigned(ADatabase) then
    raise Exception.Create('A Database must be assigned');
  PrimaryKeys.Database := aDatabase;
  GeneratorQuery.Database := aDatabase;
  IdentifyStatementSQL.Database := aDatabase;
  SQLTransaction.DefaultDatabase := aDatabase;
end;

end.

