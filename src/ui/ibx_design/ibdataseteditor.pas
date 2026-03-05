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

unit IBDataSetEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, IBSQLEditFrame, IBCustomDataSet,
  IBDatabase, IBLookupComboEditBox, IBDynamicGrid;

type

  { TIBDataSetEditorForm }

  TIBDataSetEditorForm = class(TForm)
    FieldNamesGrid: TIBDynamicGrid;
    GenerateParams: TCheckBox;
    IBSQLEditFrame1: TIBSQLEditFrame;
    IncludeSysTables: TCheckBox;
    Label5: TLabel;
    Label6: TLabel;
    PrimaryKeysGrid: TIBDynamicGrid;
    IdentityGrid: TIBDynamicGrid;
    ReadOnlyGrid: TIBDynamicGrid;
    SelectSelectAll: TCheckBox;
    SelectTableNames: TIBLookupComboEditBox;
    TestBtn: TButton;
    CancelButton: TButton;
    FieldsPage: TTabSheet;
    GenerateButton: TButton;
    GroupBox1: TGroupBox;
    IncludePrimaryKeys: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    OkButton: TButton;
    PageControl: TPageControl;
    QuoteFields: TCheckBox;
    SQLPage: TTabSheet;
    StatementType: TRadioGroup;
    procedure IncludeSysTablesChange(Sender: TObject);
    procedure SelectSelectAllClick(Sender: TObject);
    procedure TestBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure GenerateButtonClick(Sender: TObject);
    procedure SQLMemoChange(Sender: TObject);
    procedure SQLPageShow(Sender: TObject);
    procedure StatementTypeClick(Sender: TObject);
  private
    { private declarations }
    FDataSet: TIBDataSet;
    FDirty: boolean;
    FCurrentStatement: integer;
    FSelectSQL: TStringList;
    FModifySQL: TStringList;
    FInsertSQL: TStringList;
    FDeleteSQL: TStringList;
    FRefreshSQL: TStringList;
    procedure UpdateSQLMemo;
    procedure HandleUserTablesOpened(Sender: TObject);
  protected
    procedure Loaded; override;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  IBDataSetEditorForm: TIBDataSetEditorForm;

function EditIBDataSet(DataSet: TIBDataSet): boolean;

implementation

{$R *.lfm}

function EditIBDataSet(DataSet: TIBDataSet): boolean;
begin
  Result := false;
  if assigned(DataSet) and assigned(DataSet.Database) then
  try
    DataSet.Database.Connected := true;
  except on E: Exception do
    ShowMessage(E.Message)
  end;

  with TIBDataSetEditorForm.Create(Application) do
  try
    if assigned(DataSet) then
    begin
      IBSQLEditFrame1.Database := DataSet.Database;
      GenerateParams.Checked := DataSet.GenerateParamNames;
    end;
    FDataSet := DataSet;
    with IBSQLEditFrame1 do
    begin
      IncludeReadOnlyFields := false;
    end;
    Result := ShowModal = mrOK;
    if Result and assigned(DataSet) then
      DataSet.GenerateParamNames := GenerateParams.Checked
  finally
    Free
  end;

end;

{ TIBDataSetEditorForm }

procedure TIBDataSetEditorForm.FormShow(Sender: TObject);
begin
  PageControl.ActivePage := FieldsPage;
  FModifySQL.Assign(FDataSet.ModifySQL);
  FInsertSQL.Assign(FDataSet.InsertSQL);
  FDeleteSQL.Assign(FDataSet.DeleteSQL);
  FRefreshSQL.Assign(FDataSet.RefreshSQL);
  FSelectSQL.Assign(FDataSet.SelectSQL);
  GenerateButton.Enabled := (IBSQLEditFrame1.Database <> nil) and IBSQLEditFrame1.Database.Connected;
  TestBtn.Enabled := (IBSQLEditFrame1.Database <> nil) and IBSQLEditFrame1.Database.Connected;
  FCurrentStatement := -1;
  if (IBSQLEditFrame1.Database <> nil) and IBSQLEditFrame1.Database.Connected then
  begin
    IBSQLEditFrame1.UserTables.Active := true;
    IBSQLEditFrame1.SyncQueryBuilder(FSelectSQL);
  end;
end;

procedure TIBDataSetEditorForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if ModalResult = mrOK then
  begin
    UpdateSQLMemo;
    FDataSet.ModifySQL.Assign(FModifySQL);
    FDataSet.InsertSQL.Assign(FInsertSQL);
    FDataSet.DeleteSQL.Assign(FDeleteSQL);
    FDataSet.RefreshSQL.Assign(FRefreshSQL);
    FDataSet.SelectSQL.Assign(FSelectSQL);
  end;
end;

procedure TIBDataSetEditorForm.TestBtnClick(Sender: TObject);
begin
  IBSQLEditFrame1.TestSQL(GenerateParams.Checked);
end;

procedure TIBDataSetEditorForm.IncludeSysTablesChange(Sender: TObject);
begin
  IBSQLEditFrame1.IncludeSystemTables := IncludeSysTables.Checked;
end;

procedure TIBDataSetEditorForm.SelectSelectAllClick(Sender: TObject);
begin
  IBSQLEditFrame1.SelectAllFields(SelectSelectAll.Checked);
end;

procedure TIBDataSetEditorForm.GenerateButtonClick(Sender: TObject);
begin
  IBSQLEditFrame1.GenerateSelectSQL(QuoteFields.Checked,FSelectSQL,true);
  IBSQLEditFrame1.GenerateRefreshSQL(QuoteFields.Checked,FRefreshSQL,true);
  IBSQLEditFrame1.GenerateDeleteSQL(QuoteFields.Checked,FDeleteSQL);
  IBSQLEditFrame1.GenerateInsertSQL(QuoteFields.Checked,FInsertSQL);
  IBSQLEditFrame1.GenerateModifySQL(QuoteFields.Checked,FModifySQL, IncludePrimaryKeys.Checked);
  FDirty := false;
  PageControl.ActivePage := SQLPage;
end;

procedure TIBDataSetEditorForm.SQLMemoChange(Sender: TObject);
begin
  FDirty := true
end;

procedure TIBDataSetEditorForm.SQLPageShow(Sender: TObject);
begin
  UpdateSQLMemo
end;

procedure TIBDataSetEditorForm.StatementTypeClick(Sender: TObject);
begin
  UpdateSQLMemo
end;

procedure TIBDataSetEditorForm.UpdateSQLMemo;
begin
  if FDirty then
    case FCurrentStatement of
    0: // Select
        FSelectSQL.Assign(IBSQLEditFrame1.SQLText.Lines);
    1: //Modify
        FModifySQL.Assign(IBSQLEditFrame1.SQLText.Lines);
    2: //Insert
        FInsertSQL.Assign(IBSQLEditFrame1.SQLText.Lines);
    3: // Delete
        FDeleteSQL.Assign(IBSQLEditFrame1.SQLText.Lines);
    4: //Refresh
        FRefreshSQL.Assign(IBSQLEditFrame1.SQLText.Lines);
    end;
  FDirty := false;
  case StatementType.ItemIndex of
  0: //Select
     IBSQLEditFrame1.SQLText.Lines.Assign(FSelectSQL);
  1: // Modify
    IBSQLEditFrame1.SQLText.Lines.Assign(FModifySQL)  ;
  2: //Insert
    IBSQLEditFrame1.SQLText.Lines.Assign(FInsertSQL)  ;
  3: // Delete
    IBSQLEditFrame1.SQLText.Lines.Assign(FDeleteSQL)  ;
  4: //Refresh
    IBSQLEditFrame1.SQLText.Lines.Assign(FRefreshSQL)  ;
  end;
  FCurrentStatement := StatementType.ItemIndex;
end;

procedure TIBDataSetEditorForm.HandleUserTablesOpened(Sender: TObject);
begin
  SelectSelectAll.Checked := true;
end;

procedure TIBDataSetEditorForm.Loaded;
begin
  inherited Loaded;
  if IBSQLEditFrame1 <> nil then
  begin
    IBSQLEditFrame1.OnUserTablesOpened := @HandleUserTablesOpened;
    if SelectTableNames <> nil then
      SelectTableNames.ListSource :=  IBSQLEditFrame1.UserTableSource;
    if FieldNamesGrid <> nil then
      FieldNamesGrid.DataSource := IBSQLEditFrame1.FieldsSource;
    if PrimaryKeysGrid <> nil then
      PrimaryKeysGrid.DataSource := IBSQLEditFrame1.PrimaryKeySource;
    if IdentityGrid <> nil then
      IdentityGrid.DataSource := IBSQLEditFrame1.IdentityColsSource;
    if ReadOnlyGrid <> nil then
      ReadOnlyGrid.DataSource := IBSQLEditFrame1.ReadOnlyFieldsSource;
  end;
end;

constructor TIBDataSetEditorForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FModifySQL := TStringList.Create;
  FInsertSQL := TStringList.Create;
  FDeleteSQL := TStringList.Create;
  FRefreshSQL := TStringList.Create;
  FSelectSQL := TStringList.Create;
end;

destructor TIBDataSetEditorForm.Destroy;
begin
  if assigned(FModifySQL) then FModifySQL.Free;
  if assigned(FInsertSQL) then FInsertSQL.Free;
  if assigned(FDeleteSQL) then FDeleteSQL.Free;
  if assigned(FRefreshSQL) then FRefreshSQL.Free;
  if assigned(FSelectSQL) then FSelectSQL.Free;
  inherited Destroy;
end;

end.
