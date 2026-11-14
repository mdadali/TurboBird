unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  ExtCtrls, SynEdit, SynHighlighterSQL,
  uib, uibmetadata;

type
  { TMainForm }

  TMainForm = class(TForm)
    DataBase: TUIBDataBase;
    ImageList: TImageList;
    MainMenu: TMainMenu;
    Memo: TSynEdit;
    mFile: TMenuItem;
    mLoad: TMenuItem;
    mOpen: TMenuItem;
    mSave: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    SynSQLSyn1: TSynSQLSyn;
    Transaction: TUIBTransaction;
    TreeView: TTreeView;
    UIBQuery1: TUIBQuery;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mLoadClick(Sender: TObject);
    procedure mOpenClick(Sender: TObject);
    procedure mSaveClick(Sender: TObject);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
  private
    FMetaDB: TMetaDatabase;

    procedure ClearTree;
    procedure FillTree;
    procedure AddNodeWithChildren(ParentNode: TTreeNode; MetaNode: TMetaNode);
    function GetDDL(MetaNode: TMetaNode): string;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FMetaDB := nil;
  Memo.Highlighter := SynSQLSyn1;
  Memo.ReadOnly := True;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin

  FreeAndNil(FMetaDB);
end;

procedure TMainForm.mOpenClick(Sender: TObject);
begin
  try
    DataBase.Connected := False;
    DataBase.Connected := True;

    FreeAndNil(FMetaDB);

    Screen.Cursor := crHourGlass;
    try
      FMetaDB := TMetaDatabase(DataBase.GetMetadata(True));
      FillTree;
    finally
      Screen.Cursor := crDefault;
    end;

  except
    on E: Exception do
      MessageDlg('Fehler beim Laden der Metadaten', E.Message, mtError, [mbOK], 0);
  end;
end;

procedure TMainForm.mSaveClick(Sender: TObject);
var
  FS: TFileStream;
begin
  if FMetaDB = nil then
  begin
    MessageDlg('Keine Metadaten geladen.', mtInformation, [mbOK], 0);
    Exit;
  end;

  if SaveDialog.Execute then
  begin
    FS := TFileStream.Create(SaveDialog.FileName, fmCreate);
    try
      FMetaDB.SaveToStream(FS);
      MessageDlg('Metadaten gespeichert.', mtInformation, [mbOK], 0);
    finally
      FS.Free;
    end;
  end;
end;

procedure TMainForm.mLoadClick(Sender: TObject);
var
  FS: TFileStream;
begin
  if not OpenDialog.Execute then Exit;

  FreeAndNil(FMetaDB);

  FS := TFileStream.Create(OpenDialog.FileName, fmOpenRead or fmShareDenyWrite);
  try
    // Richtige Variante in UIB:
    FMetaDB := TMetaDatabase.CreateFromStream(nil, -1, FS);
    FillTree;
  finally
    FS.Free;
  end;
end;

procedure TMainForm.TreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  Memo.Clear;
  if (Node = nil) or (Node.Data = nil) then Exit;

  Memo.Text := GetDDL(TMetaNode(Node.Data));
end;

procedure TMainForm.ClearTree;
begin
  TreeView.Items.Clear;
end;

procedure TMainForm.FillTree;
var
  RootNode, GroupNode: TTreeNode;
  i: Integer;
begin
  ClearTree;
  if FMetaDB = nil then Exit;

  TreeView.Items.BeginUpdate;
  try
    RootNode := TreeView.Items.Add(nil, FMetaDB.Name);
    RootNode.Data := FMetaDB;

    // Tables
    GroupNode := TreeView.Items.AddChild(RootNode, 'Tables');
    for i := 0 to FMetaDB.TablesCount - 1 do
      AddNodeWithChildren(TreeView.Items.AddChild(GroupNode, FMetaDB.Tables[i].Name), FMetaDB.Tables[i]);

    // Views
    GroupNode := TreeView.Items.AddChild(RootNode, 'Views');
    for i := 0 to FMetaDB.ViewsCount - 1 do
      AddNodeWithChildren(TreeView.Items.AddChild(GroupNode, FMetaDB.Views[i].Name), FMetaDB.Views[i]);

    // Procedures
    GroupNode := TreeView.Items.AddChild(RootNode, 'Procedures');
    for i := 0 to FMetaDB.ProceduresCount - 1 do
      AddNodeWithChildren(TreeView.Items.AddChild(GroupNode, FMetaDB.Procedures[i].Name), FMetaDB.Procedures[i]);

    // Packages (Firebird 3.0+)
    GroupNode := TreeView.Items.AddChild(RootNode, 'Packages');
    for i := 0 to FMetaDB.PackagesCount - 1 do
      AddNodeWithChildren(TreeView.Items.AddChild(GroupNode, FMetaDB.Packages[i].Name), FMetaDB.Packages[i]);

    // Generators
    GroupNode := TreeView.Items.AddChild(RootNode, 'Generators');
    for i := 0 to FMetaDB.GeneratorsCount - 1 do
      with TreeView.Items.AddChild(GroupNode, FMetaDB.Generators[i].Name) do
        Data := FMetaDB.Generators[i];

    // Roles
    GroupNode := TreeView.Items.AddChild(RootNode, 'Roles');
    for i := 0 to FMetaDB.RolesCount - 1 do
      with TreeView.Items.AddChild(GroupNode, FMetaDB.Roles[i].Name) do
        Data := FMetaDB.Roles[i];

    // Exceptions
    GroupNode := TreeView.Items.AddChild(RootNode, 'Exceptions');
    for i := 0 to FMetaDB.ExceptionsCount - 1 do
      with TreeView.Items.AddChild(GroupNode, FMetaDB.Exceptions[i].Name) do
        Data := FMetaDB.Exceptions[i];

    RootNode.Expand(False);
  finally
    TreeView.Items.EndUpdate;
  end;
end;

procedure TMainForm.AddNodeWithChildren(ParentNode: TTreeNode; MetaNode: TMetaNode);
var
  i, j: Integer;
  ChildNode, CategoryNode: TTreeNode;
  ChildMeta: TMetaNode;
begin
  ParentNode.Data := MetaNode;

  for i := 0 to MetaNode.NodeCount - 1 do
  begin
    if MetaNode.Nodes[i].Childs.Count = 0 then
      Continue;

    CategoryNode := TreeView.Items.AddChild(ParentNode, MetaNode.Nodes[i].ClassID.NodeClass);

    for j := 0 to MetaNode.Nodes[i].Childs.Count - 1 do
    begin
      ChildMeta := TMetaNode(MetaNode.Nodes[i].Childs[j]);
      ChildNode := TreeView.Items.AddChild(CategoryNode, ChildMeta.Name);
      ChildNode.Data := ChildMeta;
    end;
  end;
end;

function TMainForm.GetDDL(MetaNode: TMetaNode): string;
begin
  Result := '';
  if MetaNode = nil then Exit;

  try
    Result := MetaNode.AsFullDDL;
    if Trim(Result) = '' then
      Result := MetaNode.AsDDL;
  except
    on E: Exception do
      Result := '-- Fehler beim Erzeugen des DDL: ' + E.Message;
  end;
end;

end.
{unit main;

//{$mode objfpc}{$H+}
{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Menus,
  ComCtrls, uibmetadata, StdCtrls, uib, uiblib, uibconst, SynEdit,
  SynHighlighterSQL;

type

  { TMainForm }

  TMainForm = class(TForm)
    ImageList: TImageList;
    DataBase: TUIBDataBase;
    Memo: TSynEdit;
    mSave: TMenuItem;
    mLoad: TMenuItem;
    SaveDialog: TSaveDialog;
    SynSQLSyn1: TSynSQLSyn;
    Transaction: TUIBTransaction;
    MainMenu: TMainMenu;
    mOpen: TMenuItem;
    mFile: TMenuItem;
    OpenDialog: TOpenDialog;
    TreeView: TTreeView;
    UIBQuery1: TUIBQuery;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mLoadClick(Sender: TObject);
    procedure mOpenClick(Sender: TObject);
    procedure mSaveClick(Sender: TObject);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
  private
    { private declarations }
  public
    { public declarations }
    procedure ShowNodes(node: TMetaNode; from: TTreeNode);
  end; 

var
  MainForm: TMainForm;

implementation

var
  MetaData: TMetaDataBase;

{ TMainForm }

type
  TNodeInfo = record
    icon: Integer;
    color: TColor;
  end;

const
   NodeInfos: array[TMetaNodeType] of TNodeInfo = (
     (icon: 0;  color : clBlack),   // MetaNode
     (icon: 1;  color : clBlack),   // MetaDatabase
     (icon: 7;  color : clRed),   // MetaException
     (icon: 6;  color : clMaroon),   // MetaGenerator
     (icon: 15;  color : clBlack),   // MetaCheck
     (icon: 14;  color : clBlack),   // MetaTrigger
     (icon: 8;  color : clBlue),   // MetaUDF
     (icon: 4;  color : clGreen),   // MetaView
     (icon: 5;  color : clTeal),   // MetaProcedure
     (icon: 5;  color : clTeal),   // MetaPackage
     (icon: 10;  color : clNavy),   // MetaPackageProc
     (icon: 10;  color : clNavy),   // MetaPackageFunc
     (icon: 9;  color : clBlack),   // MetaRole
     (icon: 3;  color : clGreen),   // MetaTable
     (icon: 10;  color : clNavy),   // MetaBaseField
     (icon: 10;  color : clNavy),   // MetaUDFField
     (icon: 10;  color : clNavy),   // MetaField
     (icon: 10;  color : clNavy),   // MetaProcField
     (icon: 10;  color : clNavy),   // MetaProcInField
     (icon: 10;  color : clNavy),   // MetaProcOutField
     (icon: 10;  color : clNavy),   // MetaTableField
     (icon: 2;  color : clOlive),   // MetaDomain
     (icon: 0;  color : clBlack),   // MetaConstraint
     (icon: 12;  color : clBlack),   // MetaForeign
     (icon: 17;  color : clBlack),   // MetaIndex
     (icon: 11;  color : clBlack),   // MetaPrimary
     (icon: 16;  color : clBlack),   // MetaUnique
     (icon: 9;  color : clBlack),   // MetaGrant
     (icon: 9;  color : clBlack),   // MetaRoleGrant
     (icon: 9;  color : clBlack),   // MetaTableGrant
     (icon: 9;  color : clBlack),   // MetaFieldGrant
     (icon: 9;  color : clBlack),   // MetaProcedureGrant
     (icon: 9;  color : clBlack),   // MetaGrantee
     (icon: 9;  color : clBlack),   // MetaUserGrantee
     (icon: 9;  color : clBlack),   // MetaRoleGrantee
     (icon: 9;  color : clBlack),   // MetaProcedureGrantee
     (icon: 9;  color : clBlack),   // MetaTriggerGrantee
     (icon: 9;  color : clBlack)    // MetaViewGrantee
   );

procedure TMainForm.mOpenClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    DataBase.Connected := false;
    TreeView.Items.Clear;

    //OK
    //DataBase.DatabaseName := '127.0.0.1/3030:/opt/firebird/examples/empbuild/employee.fdb';
    //Database.LibraryName  := '/opt/firebird/lib/libfbclient.so.3.0.13';

    //OK
    //DataBase.DatabaseName := 'Firebird25/3125:/var/lib/firebird/backups/employee.fdb';
    //Database.LibraryName  := '/opt/firebird/lib/libfbclient.so';

    //DataBase.DatabaseName := 'Firebird40/3140:/opt/firebird/examples/empbuild/employee.fdb';
    //Database.LibraryName  := '/opt/FireBird/Firebird-4.0.6.3221-0.amd64/lib/libfbclient.so';

    DataBase.DatabaseName := 'localhost/3060:/opt/firebird6/examples/empbuild/employee.fdb';
    Database.LibraryName := '/opt/firebird6/lib/libfbclient.so.6.0.0';

    DataBase.Connected := true;
    Transaction.StartTransaction;

    if Assigned(MetaData) then
      MetaData.Free;

    MetaData := TMetaDataBase.Create(nil, 0);
    //MetaData.OIDDatabases := [OIDTable];  // nur Tabellen
    //MetaData.OIDTables := [];             // keine Details!
    MetaData.LoadFromDatabase(Transaction);

    Transaction.Commit;  //PFLICHT!

    ShowNodes(MetaData, nil);
  end;
end;

procedure TMainForm.mSaveClick(Sender: TObject);
var FileStream: TFileStream;
begin
  if SaveDialog.Execute then
  begin
    FileStream := TFileStream.Create(SaveDialog.FileName, fmCreate);
    try
      MetaData.SaveToStream(FileStream);
    finally
      FileStream.Free;
    end;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  DataBase.LibraryName := uiblib.GetClientLibrary;
  with Memo do
  begin
    Parent := Self;
    Align := alClient;
    ScrollBars := ssBoth;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  MetaData.Free;
end;

procedure TMainForm.mLoadClick(Sender: TObject);
var FileStream: TFileStream;
begin
  OpenDialog.FileName := SaveDialog.FileName;
  if OpenDialog.Execute then
  begin
    TreeView.Items.Clear;
    FileStream := TFileStream.Create(OpenDialog.FileName, fmOpenRead);
    try
      MetaData.Free;
      MetaData := TMetaDataBase.CreateFromStream(nil, -1, FileStream);
      ShowNodes(MetaData, nil);
    finally
      FileStream.Free;
    end;
  end;
end;

procedure TMainForm.TreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  If (node.Data <> nil) then
    memo.Lines.Text := TMetaNode(Node.Data).AsDDL
  else
    memo.Lines.Text := '';

  //TMetaNode(Node.Data).NodeType;
end;

procedure TMainForm.ShowNodes(node: TMetaNode; from: TTreeNode);
var
  i, j: Integer;
  ClassNode: TTreeNode;
begin
  from := TreeView.Items.AddChild(from, node.Name);
  from.Data := Node;
  if (node.NodeType = MetaTableField) then
  with TMetaTableField(node) do
  begin
    if (FieldInfos >= [fiprimary, fiforeign]) then from.ImageIndex := 13 else
    if (FieldInfos >= [fiprimary]) then from.ImageIndex := NodeInfos[MetaPrimary].icon else
    if (FieldInfos >= [fiforeign]) then from.ImageIndex := NodeInfos[MetaForeign].icon else
      from.ImageIndex := NodeInfos[node.NodeType].icon;
  end else
    from.ImageIndex := NodeInfos[node.NodeType].icon;
  from.SelectedIndex := from.ImageIndex;
  for i := 0 to node.NodeCount - 1 do
    if node.Nodes[i].Childs.Count > 0 then
    begin
      ClassNode := TreeView.Items.AddChild(from, node.Nodes[i].ClassID.NodeClass + 's'
      + format(': %s(%d)', [node.Nodes[i].ClassID.ClassName, node.Nodes[i].Childs.Count]));
      ClassNode.ImageIndex := NodeInfos[node.Nodes[i].ClassID.NodeType].icon;
      ClassNode.SelectedIndex := ClassNode.ImageIndex;
      for j := 0 to node.Nodes[i].Childs.Count - 1 do
        ShowNodes(TMetaNode(node.Nodes[i].Childs[j]), ClassNode);
    end;
end;

initialization
  {$I main.lrs}

end. }

