unit TableManage;

{$mode objfpc}

interface

uses
  Classes, SysUtils,  Forms, Controls,   LResources, LCLType,
  Graphics, Dialogs, ComCtrls,  Buttons, StdCtrls, ExtCtrls,

  fbcommon,
  turbocommon,

  fsimpleobjextractor,

  uthemeselector,

  edit_primarykey,
  UniqueConstraints,
  CheckConstraints,
  NotNullConstraints,
  NewTable,
  References,
  ForeignKeys,
  Triggers,
  Permissions,
  indices,
  fields
;

type

  { TfmTableManage }

  TfmTableManage = class(TForm)
    btnClose: TButton;
    ImageList2: TImageList;
    ImageList1: TImageList;
    PageControl1: TPageControl;
    pnlBottom: TPanel;
    tsNotNullConstraints: TTabSheet;
    tsCheckConstraints: TTabSheet;
    tsUniqueConstraints: TTabSheet;
    tsPrimaryKey: TTabSheet;
    tsReferences: TTabSheet;
    tsPermissions: TTabSheet;
    tsTriggers: TTabSheet;
    tsIndices: TTabSheet;
    tsForeignKeys: TTabSheet;
    tsFields: TTabSheet;
    procedure btnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    FNodeInfos: TPNodeInfos;
    FDBIndex: Integer;
    FTableName: string;
    FExtractor: TSimpleObjExtractor;

    fmPrimaryKey: TfmPrimaryKey;
    fmUniqueConstraints: TfmUniqueConstraints;
    fmCheckConstraints: TfmCheckConstraints;
    fmNotNullConstraints: TfmNotNullConstraints;
    fmReferences: TfmReferences;
    fmForeignKeys: TfmForeignKeys;
    fmTriggers: TfmTriggers;
    fmTablePermissions: TfmTablePermissions;
    fmIndices: TfmIndices;
    fmFields: TfmFields;
  public
     procedure Init(dbIndex: Integer; TableName: string; ANodeInfos: TPNodeInfos);
  end;

var
  fmTableManage: TfmTableManage;

implementation

{ TfmTableManage }
procedure TfmTableManage.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(FNodeInfos) then
    FNodeInfos^.EditorForm := nil;

  if Assigned(fmPrimaryKey) then
    fmPrimaryKey.Close;

  if Assigned(fmUniqueConstraints) then
    fmUniqueConstraints.Close;

  if Assigned(fmCheckConstraints) then
    fmCheckConstraints.Close;

  if Assigned(fmNotNullConstraints) then
    fmNotNullConstraints.Close;

  if Assigned(fmReferences) then
     fmReferences.Close;

  if Assigned(fmForeignKeys) then
    fmForeignKeys.Close;

  if Assigned(fmTriggers) then
    fmTriggers.Close;

  if Assigned(fmTablePermissions) then
     fmTablePermissions.Close;

  if Assigned(fmIndices) then
     fmIndices.Close;

  if Assigned(fmFields) then
      fmFields.Close;


  if Assigned(FExtractor) then
    FreeAndNil(FExtractor);

  CloseAction := caFree;
  TTabSheet(Parent).Free;
end;

procedure TfmTableManage.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) and
    ((Key=VK_F4) or (Key=VK_W)) then
  begin
    if (MessageDlg('Do you want to close this query window?', mtConfirmation, [mbNo, mbYes], 0) = mrYes) then
    begin
      // Close when pressing Ctrl-W or Ctrl-F4 (Cmd-W/Cmd-F4 on OSX)
      Close;
      Parent.Free;
    end;
  end;
end;

procedure TfmTableManage.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(self);
end;

procedure TfmTableManage.Init(dbIndex: Integer; TableName: string; ANodeInfos: TPNodeInfos);
begin
  FNodeInfos := ANodeInfos;
  FDBIndex   := dbIndex;
  FTableName := TableName;

  if not Assigned(FExtractor) then
    FExtractor := TSimpleObjExtractor.Create(dbIndex);

  try
    // Primary Key
    if not Assigned(fmPrimaryKey) then
    begin
      fmPrimaryKey := TfmPrimaryKey.Create(Self);
      fmPrimaryKey.Parent      := tsPrimaryKey;
      fmPrimaryKey.Align       := alClient;
      fmPrimaryKey.BorderStyle := bsNone;
      fmPrimaryKey.Init(dbIndex, FTableName, FNodeInfos, FExtractor);
      fmPrimaryKey.Visible     := True;
      fmPrimaryKey.bbClose.Visible := False;
    end;

    // Unique Constraints
    if not Assigned(fmUniqueConstraints) then
    begin
      fmUniqueConstraints := TfmUniqueConstraints.Create(Self);
      fmUniqueConstraints.Parent      := tsUniqueConstraints;
      fmUniqueConstraints.Align       := alClient;
      fmUniqueConstraints.BorderStyle := bsNone;
      fmUniqueConstraints.Init(dbIndex, FTableName, FNodeInfos, FExtractor);
      fmUniqueConstraints.Visible     := True;
      fmUniqueConstraints.bbClose.Visible := False;
    end;

    // Check Constraints
    if not Assigned(fmCheckConstraints) then
    begin
      fmCheckConstraints := TfmCheckConstraints.Create(Self);
      fmCheckConstraints.Parent      := tsCheckConstraints;
      fmCheckConstraints.Align       := alClient;
      fmCheckConstraints.BorderStyle := bsNone;
      fmCheckConstraints.Init(dbIndex, FTableName, FNodeInfos, FExtractor);
      fmCheckConstraints.Visible     := True;
      fmCheckConstraints.bbClose.Visible := False;
    end;

    // Not-Null Constraints
    if not Assigned(fmNotNullConstraints) then
    begin
      fmNotNullConstraints := TfmNotNullConstraints.Create(Self);
      fmNotNullConstraints.Parent      := tsNotNullConstraints;
      fmNotNullConstraints.Align       := alClient;
      fmNotNullConstraints.BorderStyle := bsNone;
      fmNotNullConstraints.Init(dbIndex, FTableName, FNodeInfos, FExtractor);
      fmNotNullConstraints.Visible     := True;
      fmNotNullConstraints.bbClose.Visible := False;
    end;

    // References
    if not Assigned(fmReferences) then
    begin
      fmReferences := TfmReferences.Create(Self);
      fmReferences.Parent      := tsReferences;
      fmReferences.Align       := alClient;
      fmReferences.BorderStyle := bsNone;
      fmReferences.Init(dbIndex, FTableName, FNodeInfos, FExtractor);
      fmReferences.Visible     := True;
      fmReferences.bbRefreshReferences.Visible := True;
    end;

    // Foreign Keys
    if not Assigned(fmForeignKeys) then
    begin
      fmForeignKeys := TfmForeignKeys.Create(Self);
      fmForeignKeys.Parent      := tsForeignKeys;
      fmForeignKeys.Align       := alClient;
      fmForeignKeys.BorderStyle := bsNone;
      fmForeignKeys.Init(dbIndex, FTableName, FNodeInfos, FExtractor);
      fmForeignKeys.Visible     := True;
    end;

    // Triggers
    if not Assigned(fmTriggers) then
    begin
      fmTriggers := TfmTriggers.Create(Self);
      fmTriggers.Parent      := tsTriggers;
      fmTriggers.Align       := alClient;
      fmTriggers.BorderStyle := bsNone;
      fmTriggers.Init(dbIndex, FTableName, FNodeInfos, FExtractor);
      fmTriggers.Visible     := True;
    end;

    // Permissions
    if not Assigned(fmTablePermissions) then
    begin
      fmTablePermissions := TfmTablePermissions.Create(Self);
      fmTablePermissions.Parent      := tsPermissions;
      fmTablePermissions.Align       := alClient;
      fmTablePermissions.BorderStyle := bsNone;
      fmTablePermissions.Init(dbIndex, FTableName, FNodeInfos, FExtractor);
      fmTablePermissions.Visible     := True;
    end;

    // Indices
    if not Assigned(fmIndices) then
    begin
      fmIndices := TfmIndices.Create(Self);
      fmIndices.Parent      := tsIndices;
      fmIndices.Align       := alClient;
      fmIndices.BorderStyle := bsNone;
      fmIndices.Init(dbIndex, FTableName, FNodeInfos, FExtractor);
      fmIndices.Visible     := True;
    end;

    // Fields
    if not Assigned(fmFields) then
    begin
      fmFields := TfmFields.Create(Self);
      fmFields.Parent      := tsFields;
      fmFields.Align       := alClient;
      fmFields.BorderStyle := bsNone;
      fmFields.Init(dbIndex, FTableName, FNodeInfos, FExtractor);
      fmFields.Visible     := True;
    end;

  except
    on E: Exception do
    begin
      MessageDlg('Error while initializing Table Management: ' + E.Message,
                 mtError, [mbOk], 0);
    end;
  end;
end;

procedure TfmTableManage.btnCloseClick(Sender: TObject);
begin
  turbocommon.MetaDataChanged := true;
  Close;
  Parent.Free;
end;

initialization
  {$I tablemanage.lrs}

end.

