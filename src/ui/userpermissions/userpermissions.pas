unit UserPermissions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Grids, Buttons, StdCtrls, ComCtrls, LCLType, ExtCtrls, DB,

  turbocommon,
  uthemeselector,
  umessagedlg;

type

  { TfmUserPermissions }

  TfmUserPermissions = class(TForm)
    bbRefreshPermissions: TBitBtn;
    bbClose: TBitBtn;
    Label1: TLabel;
    laObject: TLabel;
    pnlBottom: TPanel;
    pnlTop: TPanel;
    StringGrid1: TStringGrid;
    procedure bbCloseClick(Sender: TObject);
    procedure bbRefreshPermissionsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    FNodeInfos: TPNodeInfos;
    FDBIndex: Integer;
    FUserName: string;
  public
    procedure Init(ANodeInfos: TPNodeInfos; ADBIndex: Integer; const AUserName: string);
    procedure FillPermissions;
  end;

var
  fmUserPermissions: TfmUserPermissions;

implementation

uses main, SysTables;

{ TfmUserPermissions }

procedure TfmUserPermissions.Init(ANodeInfos: TPNodeInfos; ADBIndex: Integer;
  const AUserName: string);
begin
  FNodeInfos := ANodeInfos;
  FDBIndex   := ADBIndex;
  FUserName  := AUserName;
  laObject.Caption := AUserName;
end;

procedure TfmUserPermissions.FillPermissions;
var
  ds: TDataSet;
  ObjName, ObjTypeName, PrivStr, Priv, CurrentObj: string;
  ObjType: Integer;
  RowIndex: Integer;
begin
  StringGrid1.RowCount := 1;
  RowIndex := 1;
  CurrentObj := '';
  PrivStr := '';
  ObjTypeName := '';

  ds := dmSysTables.GetAllUserPermissions(FDBIndex, FUserName);
  try
    while not ds.EOF do
    begin
      ObjName := Trim(ds.FieldByName('RDB$RELATION_NAME').AsString);
      Priv := Trim(ds.FieldByName('RDB$PRIVILEGE').AsString);
      if ds.FieldByName('RDB$GRANT_OPTION').AsInteger <> 0 then
        Priv := Priv + 'G';

      if (ObjName <> CurrentObj) and (CurrentObj <> '') then
      begin
        StringGrid1.RowCount := RowIndex + 1;
        StringGrid1.Cells[0, RowIndex] := ObjTypeName;
        StringGrid1.Cells[1, RowIndex] := CurrentObj;
        StringGrid1.Cells[2, RowIndex] := PrivStr;
        Inc(RowIndex);
        PrivStr := '';
      end;

      CurrentObj := ObjName;
      ObjType := ds.FieldByName('RDB$OBJECT_TYPE').AsInteger;

      case ObjType of
        0:  ObjTypeName := 'Table/View';
        5:  ObjTypeName := 'Procedure';
        13: ObjTypeName := 'Role';
      else
        ObjTypeName := IntToStr(ObjType);
      end;

      if PrivStr <> '' then
        PrivStr := PrivStr + ',';
      PrivStr := PrivStr + Priv;

      ds.Next;
    end;

    if CurrentObj <> '' then
    begin
      StringGrid1.RowCount := RowIndex + 1;
      StringGrid1.Cells[0, RowIndex] := ObjTypeName;
      StringGrid1.Cells[1, RowIndex] := CurrentObj;
      StringGrid1.Cells[2, RowIndex] := PrivStr;
    end;
  finally
    // Dataset wird vom Modul verwaltet
  end;
end;

procedure TfmUserPermissions.bbRefreshPermissionsClick(Sender: TObject);
begin
  FillPermissions;
end;

procedure TfmUserPermissions.bbCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfmUserPermissions.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(FNodeInfos) then
    FNodeInfos^.ViewForm := nil;
  CloseAction := caFree;

  if (Parent is TTabSheet) then
    Parent.Free;
end;

procedure TfmUserPermissions.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) and ((Key = VK_F4) or (Key = VK_W)) then
  begin
    if MsgConfirm('Do you want to close this window?', mkQuestion) then
    begin
      Close;
    end;
  end;
end;

procedure TfmUserPermissions.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(Self);
end;

initialization
  {$I userpermissions.lrs}

end.
