unit Unit1;

{$mode objfpc}{$H+}

// Please define at project level (Project Options/Other, defines) either
//
// -dQBEZEOS: Use Zeos in Visual Query Builder
// Please add a project requirement to Zeos zcomponent
// The example uses a Firebird connection (see code below).
// You can of course change this
//
// or
//
// -dQBESQLDB: Use SQLDB in Visual Query Builder
// This example uses a Firebird ibconnection.
// You can of course change this.
//
// or
//
// -dQBEIBX: Use IBX objects in Visual Query Builder
// This example uses TIBDatabase/TIBTransaction
//

interface

uses
  SysUtils, Forms, StdCtrls, QBuilder
  {$IFDEF QBEIBX}
  ,IBDatabase, QBEIBX
  {$ENDIF}
  {$IFDEF QBESQLDB}
  , sqldb, QBESqlDb
  , IBConnection
  {$ENDIF}
  {$IFDEF QBEZEOS}
  , ZConnection, ZDatasetUtils, QBEZEOS
  {$ENDIF}
  , Classes;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    {$IFDEF QBESQLDB}
    FDbConnection: TIBConnection;
    FDBTrans: TSQLTransaction;
    {$ENDIF}
    {$IFDEF QBEZEOS}
    FDbConnection: TZConnection;
    {$ENDIF}
    {$IFDEF QBEIBX}
    FDbConnection: TIBDatabase;
    FDBTrans: TIBTransaction;
    {$ENDIF}
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  meuqb: TOQBuilderDialog;
  {$IFDEF QBEIBX}
  VisualQueryEngine: TOQBEngineIBX;
  {$ENDIF}
  {$IFDEF QBESQLDB}
  VisualQueryEngine: TOQBEngineSQLDB;
  {$ENDIF}
  {$IFDEF QBEZEOS}
  VisualQueryEngine: TOQBEngineZEOS;
  {$ENDIF}
begin
  try
    {$IFDEF QBESQLDB}
    FDbConnection.DatabaseName := '127.0.0.1:/opt/FireBird/Firebird-5.0.3.1683-0-linux-x64/examples/empbuild/employee.fdb';
    {$ENDIF}
    {$IFDEF QBEZEOS}
    FDbConnection.Database := '127.0.0.1:/opt/FireBird/Firebird-5.0.3.1683-0-linux-x64/examples/empbuild/employee.fdb';
    {$ENDIF}
    {$IFDEF QBEIBX}
    FDbConnection.DatabaseName := '127.0.0.1:/opt/FireBird/Firebird-5.0.3.1683-0-linux-x64/examples/empbuild/employee.fdb';
    {$ENDIF}

    meuqb := TOQBuilderDialog.Create(nil);

    {$IFDEF QBESQLDB}
    VisualQueryEngine := TOQBEngineSQLDB.Create(nil);
    VisualQueryEngine.ShowSystemTables := False;
    {$ENDIF}
    {$IFDEF QBEZEOS}
    VisualQueryEngine := TOQBEngineZEOS.Create(nil);
    VisualQueryEngine.ShowSystemTables := False;
    {$ENDIF}
    {$IFDEF QBEIBX}
    VisualQueryEngine := TOQBEngineIBX.Create(nil);
    VisualQueryEngine.ShowSystemTables := False;
    {$ENDIF}

    VisualQueryEngine.Connection := FDbConnection;
    meuqb.OQBEngine := VisualQueryEngine;

    {$IFDEF QBESQLDB}
    meuqb.OQBEngine.DatabaseName := FDbConnection.DatabaseName;
    FDbConnection.Open;
    {$ENDIF}
    {$IFDEF QBEZEOS}
    meuqb.OQBEngine.DatabaseName := FDbConnection.Database;
    //VisualQueryEngine.ShowSystemTables := False;
    FDbConnection.Connect;
    {$ENDIF}
    {$IFDEF QBEIBX}
    meuqb.OQBEngine.DatabaseName := FDbConnection.DatabaseName;
    FDbConnection.Connected := True;
    if not FDBTrans.InTransaction then
      FDBTrans.StartTransaction;
    {$ENDIF}

    if meuqb.Execute then
      Memo1.Text := meuqb.SQL.Text;
  finally
    meuqb.Free;
    VisualQueryEngine.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  {$IFDEF QBESQLDB}
  FDbConnection := TIBConnection.Create(nil);
  FDbConnection.HostName := '';
  FDbConnection.UserName := 'SYSDBA';
  FDBTrans := TSQLTransaction.Create(nil);
  FDbConnection.Transaction := FDBTrans;
  {$ENDIF}

  {$IFDEF QBEZEOS}
  FDbConnection := TZConnection.Create(nil);
  FDbConnection.Protocol := 'firebird';
  FDbConnection.Properties.Add('lc_ctype=UTF8');
  FDbConnection.ControlsCodePage := cCP_UTF8;
  FDbConnection.User := 'SYSDBA';
  {$ENDIF}

  {$IFDEF QBEIBX}
  FDbConnection := TIBDatabase.Create(nil);
  FDbConnection.LoginPrompt := false;
  FDBTrans := TIBTransaction.Create(nil);
  FDbConnection.DefaultTransaction := FDBTrans;
  FDbConnection.Params.Values['user_name'] := 'SYSDBA';
  FDbConnection.Params.Values['password'] := 'masterkey';
  FDbConnection.Params.Add('lc_ctype=UTF8');
  //FDbConnection.Params.Values['lc_ctype'] := 'WIN1252';
  {$ENDIF}
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  {$IFDEF QBESQLDB}
  FDBTrans.Free;
  {$ENDIF}
  {$IFDEF QBEIBX}
  if FDBTrans.InTransaction then
    FDBTrans.Rollback;
  FDBTrans.Free;
  {$ENDIF}
  FDbConnection.Free;
end;

{$R *.lfm}

end.

