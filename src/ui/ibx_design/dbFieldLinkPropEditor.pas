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

unit dbFieldLinkPropEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, PropEdits, DB;

type
  {TFieldLinkProperty}

  TFieldLinkProperty = class(TStringProperty)
    private
      FDataSet: TDataSet;
    protected
      function GetDataSet: TDataSet;
      function GetIndexDefs: TIndexDefs; virtual;
      function GetIndexFieldNames: string; virtual; abstract;
      function GetMasterFields: string; virtual; abstract;
      procedure SetIndexFieldNames(const Value: string); virtual; abstract;
      procedure SetMasterFields(const Value: string); virtual; abstract;
    public
      procedure Edit; override;
      function GetAttributes: TPropertyAttributes; override;
      property DataSet: TDataSet read GetDataSet;
      property IndexDefs: TIndexDefs read GetIndexDefs;
      property DetailIndexFieldNames: string read GetIndexFieldNames write SetIndexFieldNames;
      property MasterIndexFieldNames: string read GetMasterFields write SetMasterFields;
    end;

  { TFieldLinkEditor }

  TFieldLinkEditor = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    OKButton: TButton;
    JoinButton: TButton;
    DeleteButton: TButton;
    JoinHint: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    DetailedFieldListBox: TListBox;
    JoinedFields: TListBox;
    MasterFieldListBox: TListBox;
    procedure DeleteButtonClick(Sender: TObject);
    procedure DetailedFieldListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure FormShow(Sender: TObject);
    procedure JoinButtonClick(Sender: TObject);
    procedure JoinedFieldsClick(Sender: TObject);
    procedure MasterFieldListBoxSelectionChange(Sender: TObject; User: boolean);
  private
    { private declarations }
    FDetailedFieldsList: TStringList;
    FMasterFieldsList: TStringList;
    FChanged: boolean;
    procedure ClearJoinList;
    procedure ExtractNames(List: TStringList; Names: string);
    procedure LoadDetailedFields;
    procedure LoadJoinedFields;
    procedure LoadMasterFields;
    procedure SetButtonState;
    procedure ShowJoins;
  public
    { public declarations }
    DetailIndexDefs: TIndexDefs;
    DetailIndexFieldNames: string;
    MasterIndexFieldNames: string;
    Master: TDataset;
    IndexDefs: TIndexDefs;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  FieldLinkEditor: TFieldLinkEditor;

function EditFieldLink(aMaster: TDataSet; aIndexDefs: TIndexDefs;
                       var aDetailIndexFieldNames: string;
                       var aMasterIndexFieldNames: string): boolean;

implementation

uses IBCustomDataSet;

function EditFieldLink(aMaster: TDataSet; aIndexDefs: TIndexDefs;
   var aDetailIndexFieldNames: string; var aMasterIndexFieldNames: string
   ): boolean;
begin
  with TFieldLinkEditor.Create(Application) do
  try
    Master := aMaster;
    IndexDefs := aIndexDefs;
    DetailIndexFieldNames := aDetailIndexFieldNames;
    MasterIndexFieldNames := aMasterIndexFieldNames;
    Result := ShowModal = mrOK;
    if Result then
    begin
      aDetailIndexFieldNames := DetailIndexFieldNames;
      aMasterIndexFieldNames := MasterIndexFieldNames
    end;
  finally
    Free
  end;

end;

{$R *.lfm}

{ TFieldLinkProperty }

 function TFieldLinkProperty.GetIndexDefs: TIndexDefs;
begin
  Result := nil
end;

function TFieldLinkProperty.GetDataSet: TDataSet;
begin
  if FDataSet = nil then
    FDataSet := TDataSet(GetComponent(0));
  Result := FDataSet;
end;

procedure TFieldLinkProperty.Edit;
var detailedFields, masterFields: string;
begin
  if not assigned(DataSet.DataSource) or not assigned(DataSet.DataSource.DataSet) then
     raise Exception.Create('No Master Dataset');

  detailedFields := DetailIndexFieldNames;
  masterFields := MasterIndexFieldNames;
  if EditFieldLink(DataSet.DataSource.DataSet,IndexDefs,detailedFields,masterFields) then
  Begin
    DetailIndexFieldNames := detailedFields;
    MasterIndexFieldNames := masterFields;
    Modified
  end;
end;

function TFieldLinkProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

{ TFieldLinkEditor }

 procedure TFieldLinkEditor.DeleteButtonClick(Sender: TObject);
begin
  if (JoinedFields.Items.Count = 0) or
     (DetailIndexFieldNames = '') or
     (MessageDlg('Delete the current Field Bindings?',mtConfirmation,[mbYes,mbNo],0) = mrYes) then
  begin
    ClearJoinList;
    if DetailedFieldListBox.ItemIndex > -1 then
        DetailIndexFieldNames := DetailedFieldListBox.Items[DetailedFieldListBox.ItemIndex]
    else
        DetailIndexFieldNames := '';
    MasterIndexFieldNames := '';
    LoadMasterFields;
    LoadJoinedFields;
    FChanged := true;
    SetButtonState
  end;
end;

 procedure TFieldLinkEditor.DetailedFieldListBoxSelectionChange(Sender: TObject;
   User: boolean);
begin
  if (DetailedFieldListBox.ItemIndex > -1) and
     (DetailedFieldListBox.Items[DetailedFieldListBox.ItemIndex] <> DetailIndexFieldNames) then
     DeleteButtonClick(nil)
end;

 procedure TFieldLinkEditor.FormShow(Sender: TObject);
begin
 JoinHint.Caption := '';
 LoadDetailedFields;
 LoadMasterFields;
 LoadJoinedFields;
 FChanged := false;
 SetButtonState
end;

 procedure TFieldLinkEditor.JoinButtonClick(Sender: TObject);
begin
  FMasterFieldsList.Add(MasterFieldListBox.Items[MasterFieldListBox.ItemIndex]);
  if FMasterFieldsList.Count = FDetailedFieldsList.Count then
     MasterIndexFieldNames :=  FMasterFieldsList.DelimitedText;
  FChanged := true;
  ShowJoins;
  SetButtonState
end;

 procedure TFieldLinkEditor.JoinedFieldsClick(Sender: TObject);
begin
  SetButtonState
end;

 procedure TFieldLinkEditor.MasterFieldListBoxSelectionChange(Sender: TObject;
   User: boolean);
begin
  SetButtonState
end;

 procedure TFieldLinkEditor.ClearJoinList;
 begin
   FDetailedFieldsList.Clear;
   FMasterFieldsList.Clear;
   JoinedFields.Items.Clear;
 end;

 procedure TFieldLinkEditor.ExtractNames(List: TStringList; Names: string);
 var idx: integer;
 begin
  idx := 1;
  List.Clear;
  while idx <= Length(Names) do
        List.Add(ExtractFieldName(Names,idx));
 end;

 procedure TFieldLinkEditor.LoadDetailedFields;
 var
     I: integer;
 begin
  DetailedFieldListBox.Clear;
  if Master is TIBCustomDataSet then
   with TIBCustomDataSet(Master) do
     if (Database = nil) or not Database.Connected then Exit;
  IndexDefs.Update;
  for I := 0 to IndexDefs.Count - 1 do
    with IndexDefs[I] do
      if (Options * [ixExpression, ixDescending] = []) and (Fields <> '') then
        DetailedFieldListBox.Items.Add(Fields);
  DetailedFieldListBox.ItemIndex :=  DetailedFieldListBox.Items.IndexOf(DetailIndexFieldNames)
 end;

 procedure TFieldLinkEditor.LoadJoinedFields;
 begin
   ClearJoinList;
   ExtractNames(FDetailedFieldsList,DetailIndexFieldNames);
   ExtractNames(FMasterFieldsList,MasterIndexFieldNames);
   while FDetailedFieldsList.Count < FMasterFieldsList.Count do
         FMasterFieldsList.Delete(FMasterFieldsList.Count - 1);
   ShowJoins
 end;

 procedure TFieldLinkEditor.LoadMasterFields;
 begin
   MasterFieldListBox.Clear;
   if assigned(Master) then
        Master.GetFieldNames(MasterFieldListBox.Items)
 end;

 procedure TFieldLinkEditor.SetButtonState;
 begin
   DeleteButton.Enabled := JoinedFields.Items.Count > 0;

   JoinButton.Enabled := (DetailedFieldListBox.ItemIndex > -1) and
                         (MasterFieldListBox.ItemIndex > -1) and
                         (JoinedFields.Items.Count < FDetailedFieldsList.Count);

   if JoinButton.Enabled then
      JoinHint.Caption := Format('Click on the Join Button to bind %s to %s',
                                        [FDetailedFieldsList[JoinedFields.Items.Count],
                                         MasterFieldListBox.Items[MasterFieldListBox.ItemIndex]])
   else
     JoinHint.Caption := '';
   OKButton.Enabled := FChanged and (MasterIndexFieldNames <> '');
 end;

 procedure TFieldLinkEditor.ShowJoins;
 var i: integer;
     idx: integer;
 begin
   JoinedFields.Clear;
   for i := 0 to FDetailedFieldsList.Count -1 do
       if i < FMasterFieldsList.Count then
       JoinedFields.Items.Add(FDetailedFieldsList[i] + ' => ' + FMasterFieldsList[i]);

   for i := 0 to FMasterFieldsList.Count - 1 do
   begin
     idx := MasterFieldListBox.Items.IndexOf(FMasterFieldsList[i]);
     if idx > -1 then
       MasterFieldListBox.Items.Delete(idx);
   end;
 end;

 constructor TFieldLinkEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FDetailedFieldsList := TStringList.Create;
  FMasterFieldsList := TStringList.Create;
  FMasterFieldsList.Delimiter := ';';
  FMasterFieldsList.StrictDelimiter := true
end;

 destructor TFieldLinkEditor.Destroy;
begin
  ClearJoinList;
  if assigned(FDetailedFieldsList) then
     FDetailedFieldsList.Free;
  if assigned(FMasterFieldsList) then
     FMasterFieldsList.Free;
  inherited Destroy;
end;

end.

