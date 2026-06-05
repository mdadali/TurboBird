unit clone_table_to_external_table_dialog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfmCloneToExternalTable }

  TfmCloneToExternalTable = class(TForm)
    btnBrowseFile: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    edtExtTableName: TEdit;
    edtExternalFileName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    SaveDialog1: TSaveDialog;
    procedure btnBrowseFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function GetTableName: string;
    function GetExternalFileName: string;
    procedure SetTableName(const AValue: string);
  public
    property TableName: string read GetTableName write SetTableName;
    property ExternalFileName: string read GetExternalFileName;
  end;

var
  fmCloneToExternalTable: TfmCloneToExternalTable;

implementation

{$R *.lfm}

{ TfmCloneToExternalTable }

procedure TfmCloneToExternalTable.FormCreate(Sender: TObject);
begin
  SaveDialog1.Filter := 'External Table Files|*.dat;*.txt;*.csv|All Files|*.*';
  SaveDialog1.DefaultExt := 'dat';
end;

procedure TfmCloneToExternalTable.btnBrowseFileClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
    edtExternalFileName.Text := SaveDialog1.FileName;
end;

function TfmCloneToExternalTable.GetTableName: string;
begin
  Result := Trim(edtExtTableName.Text);
end;

function TfmCloneToExternalTable.GetExternalFileName: string;
begin
  Result := Trim(edtExternalFileName.Text);
end;

procedure TfmCloneToExternalTable.SetTableName(const AValue: string);
begin
  edtExtTableName.Text := AValue;
  // Automatisch Dateinamen vorschlagen
  edtExternalFileName.Text := ExtractFilePath(Application.ExeName) +
                              AValue + '.dat';
end;

end.
