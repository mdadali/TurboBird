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

unit ibupdatesqleditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls,  IBSQLEditFrame, IBCustomDataSet,
  IBDatabase, IBLookupComboEditBox, IBDynamicGrid, IBUpdateSQL;

type

  { TIBUpdateSQLEditorForm }

  TIBUpdateSQLEditorForm = class(TForm)
    FieldNamesGrid: TIBDynamicGrid;
    GenerateParams: TCheckBox;
    IBSQLEditFrame1: TIBSQLEditFrame;
    IdentityGrid: TIBDynamicGrid;
    IncludeSysTables: TCheckBox;
    Label5: TLabel;
    Label6: TLabel;
    PrimaryKeysGrid: TIBDynamicGrid;
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
    FUpdateObject: TIBUpdateSQL;
    FDirty: boolean;
    FCurrentStatement: integer;
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
  IBUpdateSQLEditorForm: TIBUpdateSQLEditorForm;

  function EditIBUpdateSQL(UpdateObject: TIBUpdateSQL): boolean;

implementation

{$R *.lfm}

function EditIBUpdateSQL(UpdateObject: TIBUpdateSQL): boolean;
begin
  Result := false;
  if assigned(UpdateObject) and assigned(UpdateObject.DataSet) and assigned(UpdateObject.DataSet.Database) then
  try
    UpdateObject.DataSet.Database.Connected := true;
  except on E: Exception do
    ShowMessage(E.Message)
  end;

  with TIBUpdateSQLEditorForm.Create(Application) do
  try
    if assigned(UpdateObject) and assigned(UpdateObject.DataSet) then
    begin
      IBSQLEditFrame1.Database := UpdateObject.DataSet.Database;
      GenerateParams.Checked := UpdateObject.DataSet.GenerateParamNames;
    end;
    with IBSQLEditFrame1 do
      IncludeReadOnlyFields := false;
    FUpdateObject := UpdateObject;
    Result := ShowModal = mrOK;
    if Result then
      UpdateObject.DataSet.GenerateParamNames := GenerateParams.Checked
  finally
    Free
  end;

end;

{ TIBUpdateSQLEditorForm }

procedure TIBUpdateSQLEditorForm.FormShow(Sender: TObject);
begin
  PageControl.ActivePage := FieldsPage;
  FModifySQL.Assign(FUpdateObject.ModifySQL);
  FInsertSQL.Assign(FUpdateObject.InsertSQL);
  FDeleteSQL.Assign(FUpdateObject.DeleteSQL);
  FRefreshSQL.Assign(FUpdateObject.RefreshSQL);
  GenerateButton.Enabled := (IBSQLEditFrame1.Database <> nil) and IBSQLEditFrame1.Database.Connected;
  TestBtn.Enabled := (IBSQLEditFrame1.Database <> nil) and IBSQLEditFrame1.Database.Connected;
  FCurrentStatement := -1;
  if (IBSQLEditFrame1.Database <> nil) and IBSQLEditFrame1.Database.Connected then
  begin
    IBSQLEditFrame1.UserTables.Active := true;
    IBSQLEditFrame1.SyncQueryBuilder(FRefreshSQL);
  end;
end;

procedure TIBUpdateSQLEditorForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if ModalResult = mrOK then
  begin
    UpdateSQLMemo;
    FUpdateObject.ModifySQL.Assign(FModifySQL);
    FUpdateObject.InsertSQL.Assign(FInsertSQL);
    FUpdateObject.DeleteSQL.Assign(FDeleteSQL);
    FUpdateObject.RefreshSQL.Assign(FRefreshSQL);
  end;
end;

procedure TIBUpdateSQLEditorForm.TestBtnClick(Sender: TObject);
begin
  IBSQLEditFrame1.TestSQL(GenerateParams.Checked);
end;

procedure TIBUpdateSQLEditorForm.IncludeSysTablesChange(Sender: TObject);
begin
  IBSQLEditFrame1.IncludeSystemTables := IncludeSysTables.Checked;
end;

procedure TIBUpdateSQLEditorForm.SelectSelectAllClick(Sender: TObject);
begin
  IBSQLEditFrame1.SelectAllFields(SelectSelectAll.Checked);
end;

procedure TIBUpdateSQLEditorForm.GenerateButtonClick(Sender: TObject);
begin
  IBSQLEditFrame1.GenerateRefreshSQL(QuoteFields.Checked,FRefreshSQL,true);
  IBSQLEditFrame1.GenerateDeleteSQL(QuoteFields.Checked,FDeleteSQL);
  IBSQLEditFrame1.GenerateInsertSQL(QuoteFields.Checked,FInsertSQL);
  IBSQLEditFrame1.GenerateModifySQL(QuoteFields.Checked,FModifySQL, IncludePrimaryKeys.Checked);
  FDirty := false;
  PageControl.ActivePage := SQLPage;
end;

procedure TIBUpdateSQLEditorForm.SQLMemoChange(Sender: TObject);
begin
  FDirty := true
end;

procedure TIBUpdateSQLEditorForm.SQLPageShow(Sender: TObject);
begin
  UpdateSQLMemo
end;

procedure TIBUpdateSQLEditorForm.StatementTypeClick(Sender: TObject);
begin
  UpdateSQLMemo
end;

procedure TIBUpdateSQLEditorForm.UpdateSQLMemo;
begin
  if FDirty then
    case FCurrentStatement of
    0: //Modify
        FModifySQL.Assign(IBSQLEditFrame1.SQLText.Lines);
    1: //Insert
        FInsertSQL.Assign(IBSQLEditFrame1.SQLText.Lines);
    2: // Delete
        FDeleteSQL.Assign(IBSQLEditFrame1.SQLText.Lines);
    3: //Refresh
        FRefreshSQL.Assign(IBSQLEditFrame1.SQLText.Lines);
    end;
  FDirty := false;
  case StatementType.ItemIndex of
  0: // Modify
    IBSQLEditFrame1.SQLText.Lines.Assign(FModifySQL)  ;
  1: //Insert
    IBSQLEditFrame1.SQLText.Lines.Assign(FInsertSQL)  ;
  2: // Delete
    IBSQLEditFrame1.SQLText.Lines.Assign(FDeleteSQL)  ;
  3: //Refresh
    IBSQLEditFrame1.SQLText.Lines.Assign(FRefreshSQL)  ;
  end;
  FCurrentStatement := StatementType.ItemIndex;
end;

procedure TIBUpdateSQLEditorForm.HandleUserTablesOpened(Sender: TObject);
begin
  SelectSelectAll.Checked := true;
end;

procedure TIBUpdateSQLEditorForm.Loaded;
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

constructor TIBUpdateSQLEditorForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FModifySQL := TStringList.Create;
  FInsertSQL := TStringList.Create;
  FDeleteSQL := TStringList.Create;
  FRefreshSQL := TStringList.Create;
end;

destructor TIBUpdateSQLEditorForm.Destroy;
begin
  if assigned(FModifySQL) then FModifySQL.Free;
  if assigned(FInsertSQL) then FInsertSQL.Free;
  if assigned(FDeleteSQL) then FDeleteSQL.Free;
  if assigned(FRefreshSQL) then FRefreshSQL.Free;
  inherited Destroy;
end;

end.
