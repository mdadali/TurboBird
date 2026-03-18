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

unit ibselectsqleditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls , ExtCtrls, db, IBSQLEditFrame, IBDatabase, IBCustomDataSet,
  IBLookupComboEditBox, IBDynamicGrid, IB;

type

  { TIBSelectSQLEditorForm }

  TIBSelectSQLEditorForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    IBSQLEditFrame1 : TIBSQLEditFrame;
    Label3 : TLabel;
    PackageNames: TIBLookupComboEditBox;
    InputProcGrid: TIBDynamicGrid;
    PackageNameLabel: TLabel;
    OutputProcGrid: TIBDynamicGrid;
    IncludeSysTables: TCheckBox;
    GenerateBtn: TButton;
    GenerateParams: TCheckBox;
    FieldNamesGrid: TIBDynamicGrid;
    Panel1 : TPanel;
    PrimaryKeysGrid: TIBDynamicGrid;
    ProcedureNames: TIBLookupComboEditBox;
    SelectSelectAll: TCheckBox;
    SelectTableNames: TIBLookupComboEditBox;
    TestBtn: TButton;
    Label1: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    PageControl: TPageControl;
    QuoteFields: TCheckBox;
    SelectPage: TTabSheet;
    ExecutePage: TTabSheet;
    procedure FieldNamesGridDblClick(Sender: TObject);
    procedure GenerateBtnClick(Sender: TObject);
    procedure IncludeSysTablesChange(Sender: TObject);
    procedure InputProcGridDblClick(Sender: TObject);
    procedure OutputProcGridDblClick(Sender: TObject);
    procedure PackageNamesDblClick(Sender: TObject);
    procedure PrimaryKeysGridDblClick(Sender: TObject);
    procedure ProcedureNamesDblClick(Sender: TObject);
    procedure SelectSelectAllChange(Sender: TObject);
    procedure SelectTableNamesDblClick(Sender: TObject);
    procedure TestBtnClick(Sender: TObject);
    procedure ExecutePageShow(Sender: TObject);
    procedure FieldListDblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PrimaryKeyListDblClick(Sender: TObject);
    procedure SelectPageShow(Sender: TObject);
  private
    { private declarations }
    procedure HandleUserTablesOpened(Sender: TObject);
  protected
    procedure Loaded; override;
    procedure SetSQLStatementType(aType: TIBSQLStatementTypes); virtual;
  public
    { public declarations }
  end;


function EditSQL(DataSet: TIBCustomDataSet;  SelectSQL: TStrings): boolean;

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

  with TIBSelectSQLEditorForm.Create(Application) do
  try
    if assigned(DataSet) then
    begin
      IBSQLEditFrame1.Database := DataSet.Database;
      GenerateParams.Checked := DataSet.GenerateParamNames;
    end;
    IBSQLEditFrame1.SQLText.Lines.Assign(SelectSQL);
    IBSQLEditFrame1.SelectProcs := true;
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

{ TIBSelectSQLEditorForm }

procedure TIBSelectSQLEditorForm.FormShow(Sender: TObject);
begin
  GenerateBtn.Enabled := (IBSQLEditFrame1.Database <> nil) and IBSQLEditFrame1.Database.Connected;
  TestBtn.Enabled := (IBSQLEditFrame1.Database <> nil) and IBSQLEditFrame1.Database.Connected;
  PageControl.ActivePage := SelectPage;
  if Trim(IBSQLEditFrame1.SQLText.Text) <> '' then
  begin
    try
      SetSQLStatementType(IBSQLEditFrame1.SyncQueryBuilder);
      IncludeSysTables.Checked := IBSQLEditFrame1.IncludeSystemTables;
    except  end;
  end;
end;

procedure TIBSelectSQLEditorForm.PrimaryKeyListDblClick(Sender: TObject);
begin
  IBSQLEditFrame1.InsertSelectedPrimaryKey;
end;

procedure TIBSelectSQLEditorForm.SelectPageShow(Sender: TObject);
begin
  if (IBSQLEditFrame1.Database <> nil) and IBSQLEditFrame1.Database.Connected then
    IBSQLEditFrame1.UserTables.Active := true;
end;

procedure TIBSelectSQLEditorForm.FieldListDblClick(Sender: TObject);
begin
  IBSQLEditFrame1.InsertSelectedFieldName;
end;

procedure TIBSelectSQLEditorForm.GenerateBtnClick(Sender: TObject);
begin
  if PageControl.ActivePage = ExecutePage then
    IBSQLEditFrame1.GenerateExecuteSQL(QuoteFields.Checked)
  else
    IBSQLEditFrame1.GenerateSelectSQL(QuoteFields.Checked);
end;

procedure TIBSelectSQLEditorForm.FieldNamesGridDblClick(Sender: TObject);
begin
  IBSQLEditFrame1.InsertSelectedFieldName;
end;

procedure TIBSelectSQLEditorForm.IncludeSysTablesChange(Sender: TObject);
begin
  IBSQLEditFrame1.IncludeSystemTables := IncludeSysTables.Checked;
end;

procedure TIBSelectSQLEditorForm.InputProcGridDblClick(Sender: TObject);
begin
  IBSQLEditFrame1.InsertSelectedInputParam;
end;

procedure TIBSelectSQLEditorForm.OutputProcGridDblClick(Sender: TObject);
begin
  IBSQLEditFrame1.InsertSelectedOutputParam;
end;

procedure TIBSelectSQLEditorForm.PackageNamesDblClick(Sender: TObject);
begin
  IBSQLEditFrame1.InsertPackageName;
end;

procedure TIBSelectSQLEditorForm.PrimaryKeysGridDblClick(Sender: TObject);
begin
  IBSQLEditFrame1.InsertSelectedPrimaryKey;
end;

procedure TIBSelectSQLEditorForm.ProcedureNamesDblClick(Sender: TObject);
begin
    IBSQLEditFrame1.InsertProcName;
end;

procedure TIBSelectSQLEditorForm.SelectSelectAllChange(Sender: TObject);
begin
  IBSQLEditFrame1.SelectAllFields(SelectSelectAll.Checked);
end;

procedure TIBSelectSQLEditorForm.SelectTableNamesDblClick(Sender: TObject);
begin
  IBSQLEditFrame1.InsertTableName;
end;

procedure TIBSelectSQLEditorForm.TestBtnClick(Sender: TObject);
begin
  IBSQLEditFrame1.TestSQL(GenerateParams.Checked)
end;

procedure TIBSelectSQLEditorForm.ExecutePageShow(Sender: TObject);
begin
  if (IBSQLEditFrame1.Database <> nil) and IBSQLEditFrame1.Database.Connected then
    IBSQLEditFrame1.OpenUserProcedures;
end;

procedure TIBSelectSQLEditorForm.HandleUserTablesOpened(Sender: TObject);
begin
  SelectSelectAll.Checked := true;
end;

procedure TIBSelectSQLEditorForm.Loaded;
begin
  inherited Loaded;
  if IBSQLEditFrame1 <> nil then
  begin
    if PageControl <> nil then
      PageControl.ActivePage := SelectPage;
    IBSQLEditFrame1.OnUserTablesOpened := @HandleUserTablesOpened;
    if SelectTableNames <> nil then
      SelectTableNames.ListSource :=  IBSQLEditFrame1.UserTableSource;
    if FieldNamesGrid <> nil then
      FieldNamesGrid.DataSource := IBSQLEditFrame1.FieldsSource;
    if PrimaryKeysGrid <> nil then
      PrimaryKeysGrid.DataSource := IBSQLEditFrame1.PrimaryKeySource;
    if PackageNames <> nil then
      PackageNames.ListSource := IBSQLEditFrame1.PackageNameSource;
    if ProcedureNames <> nil then
      ProcedureNames.ListSource := IBSQLEditFrame1.UserProcSource;
    if InputProcGrid <> nil then
      InputProcGrid.DataSource := IBSQLEditFrame1.ProcInputSource;
    if OutputProcGrid <> nil then
      OutputProcGrid.DataSource := IBSQLEditFrame1.ProcOutputSource;
  end;
end;

procedure TIBSelectSQLEditorForm.SetSQLStatementType(aType: TIBSQLStatementTypes
  );
begin
  case aType of
  SQLExecProcedure:
    PageControl.ActivePage := ExecutePage;
  else
    PageControl.ActivePage := SelectPage;
  end;
end;

end.
