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
 *  The Original Code is (C) 2011 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*)
unit ibinsertsqleditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, ibselectsqleditor, IBSQLEditFrame, IBDatabase,
  IBDynamicGrid, IBCustomDataSet;

type

  { TIBInsertSQLEditorForm }

  TIBInsertSQLEditorForm = class(TIBSelectSQLEditorForm)
    IdentityGrid: TIBDynamicGrid;
    Label5: TLabel;
    Label6: TLabel;
    ReadOnlyGrid: TIBDynamicGrid;
    procedure GenerateBtnClick(Sender: TObject);
    procedure IdentityGridDblClick(Sender: TObject);
    procedure ReadOnlyGridDblClick(Sender: TObject);
  private

  protected
    procedure Loaded; override;

  public

  end;

function EditSQL(DataSet: TIBCustomDataSet; SelectSQL: TStrings): boolean;

var
  IBInsertSQLEditorForm: TIBInsertSQLEditorForm;

implementation

{$R *.lfm}

function EditSQL(DataSet: TIBCustomDataSet; SelectSQL: TStrings): boolean;
begin
  Result := false;
  if assigned(DataSet) and assigned(DataSet.Database) then
    try
      DataSet.Database.Connected := true;
    except on E: Exception do
      ShowMessage(E.Message)
    end;

  with TIBInsertSQLEditorForm.Create(Application) do
  try
    if assigned(DataSet) then
    begin
        IBSQLEditFrame1.Database := DataSet.Database;
        GenerateParams.Checked := DataSet.GenerateParamNames;
    end;
    with IBSQLEditFrame1 do
    begin
      IncludeReadOnlyFields := false;
      ExecuteOnlyProcs := true;
      ExcludeIdentityColumns := true;
      SQLText.Lines.Assign(SelectSQL);
    end;
    Result := ShowModal = mrOK;
    if Result then
    begin
     SelectSQL.Assign(IBSQLEditFrame1.SQLText.Lines);
     if assigned(DataSet) then
          DataSet.GenerateParamNames := GenerateParams.Checked
    end;
  finally
    Free
  end;
end;

{ TIBInsertSQLEditorForm }

procedure TIBInsertSQLEditorForm.GenerateBtnClick(Sender: TObject);
begin
  if PageControl.ActivePage = ExecutePage then
    IBSQLEditFrame1.GenerateExecuteSQL(QuoteFields.Checked)
  else
    IBSQLEditFrame1.GenerateInsertSQL(QuoteFields.Checked);
end;

procedure TIBInsertSQLEditorForm.IdentityGridDblClick(Sender: TObject);
begin
  IBSQLEditFrame1.InsertSelectedIdentityCol;
end;

procedure TIBInsertSQLEditorForm.ReadOnlyGridDblClick(Sender: TObject);
begin
  IBSQLEditFrame1.InsertSelectedReadOnlyField;
end;

procedure TIBInsertSQLEditorForm.Loaded;
begin
  inherited Loaded;
  if IBSQLEditFrame1 <> nil then
  begin
   if PrimaryKeysGrid <> nil then
     PrimaryKeysGrid.DataSource := IBSQLEditFrame1.PrimaryKeySource;
   if IdentityGrid <> nil then
     IdentityGrid.DataSource := IBSQLEditFrame1.IdentityColsSource;
    if ReadOnlyGrid <> nil then
      ReadOnlyGrid.DataSource := IBSQLEditFrame1.ReadOnlyFieldsSource;
  end;
end;


end.

