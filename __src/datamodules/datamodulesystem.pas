unit datamodulesystem;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, BufDataset;

type

  { TdmSystem }

  TdmSystem = class(TDataModule)
    BufDsServers: TBufDataset;
    BufDsDBs: TBufDataset;
    BufDsServersAlias: TStringField;
    BufDsServersClientLibraryPath: TStringField;
    BufDsServersConfigFilePath: TStringField;
    BufDsServersDescription: TStringField;
    BufDsServersHost: TStringField;
    BufDsServersID: TAutoIncField;
    BufDsServersIsEmbedded: TBooleanField;
    BufDsServersLoadOwnClientLib: TBooleanField;
    BufDsServersPort: TSmallintField;
    BufDsServersProtokol: TStringField;
    BufDsServersRootPath: TStringField;
    BufDsServersServerInfos: TBlobField;
    BufDsServersServicePassword: TStringField;
    BufDsServersServiceSavePassword: TBooleanField;
    BufDsServersServiceUserName: TStringField;
    BufDsServersVersionMajor: TSmallintField;
    BufDsServersVersionMinor: TSmallintField;
    BufDsServersVersionString: TStringField;
    dsServers: TDataSource;
    dsDBs: TDataSource;
    procedure DataModuleCreate(Sender: TObject);
  private
    procedure InitServersDataset(DS: TBufDataset);
  public

  end;

var
  dmSystem: TdmSystem;

implementation

{$R *.lfm}

{ TdmSystem }

procedure TdmSystem.DataModuleCreate(Sender: TObject);
begin
end;

procedure TdmSystem.InitServersDataset(DS: TBufDataset);
begin
  DS.Close;

  DS.FieldDefs.Clear;
  DS.IndexDefs.Clear;

  DS.FieldDefs.Add('ID', ftAutoInc);
  DS.FieldDefs.Add('Alias', ftString, 50);
  DS.FieldDefs.Add('Host', ftString, 100);
  DS.FieldDefs.Add('Protocol', ftString, 20);
  DS.FieldDefs.Add('Port', ftWord);
  DS.FieldDefs.Add('IsEmbedded', ftBoolean);

  DS.FieldDefs.Add('ClientLibraryPath', ftString, 255);
  DS.FieldDefs.Add('LoadOwnClientLib', ftBoolean);
  DS.FieldDefs.Add('ConfigFilePath', ftString, 255);

  DS.FieldDefs.Add('ServiceUser', ftString, 50);
  DS.FieldDefs.Add('ServicePassword', ftString, 255);
  DS.FieldDefs.Add('ServiceSavePassword', ftBoolean);

  DS.FieldDefs.Add('RootPath', ftString, 255);
  DS.FieldDefs.Add('VersionMajor', ftWord);
  DS.FieldDefs.Add('VersionMinor', ftWord);
  DS.FieldDefs.Add('VersionString', ftString, 50);
  DS.FieldDefs.Add('Description', ftBlob);

  // UNIQUE Alias
  DS.IndexDefs.Add('UQ_SERVER_ALIAS', 'Alias', [ixUnique]);

  DS.CreateDataset;
  DS.IndexName := 'UQ_SERVER_ALIAS';
end;


end.

