{*******************************************************}
{                                                       }
{       Delphi Visual Component Library                 }
{       Open QBuilder Engine for IBX Sources            }
{                                                       }
{       Copyright (c) 2003 Fast Reports, Inc.           }
{                                                       }
{ Adapted to Lazarus by Reinier Olislagers October 2014 }
{*******************************************************}

//untested: ibx users, please test!

unit QBEIBX;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}

  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, DB, IBDatabase, IBQuery, QBuilder;

type
  TOQBEngineIBX = class(TOQBEngine)
  private
    FResultQuery: TIBQuery;
    FIBXConnection : TIBDatabase;
    FTransaction : TIBTransaction;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearQuerySQL; override;
    procedure CloseResultQuery; override;
    procedure OpenResultQuery; override;
    procedure ReadFieldList(const ATableName: string); override;
    procedure ReadTableList; override;
    procedure SaveResultQueryData; override;
    procedure SetConnection(Value: TIBDatabase);
    procedure SetQuerySQL(const Value: string); override;
    function ResultQuery: TDataSet; override;
    function SelectDatabase: Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    // Breaking change: used to be DatabaseName
    property Connection: TIBDatabase read FIBXConnection write SetConnection;
  end;

implementation

{ TOQBEngineIBX }

constructor TOQBEngineIBX.Create(AOwner: TComponent);
begin
  inherited;
  FResultQuery := TIBQuery.Create(Self);
  FTransaction := TIBTransaction.Create(Self);
  FResultQuery.Transaction := FTransaction;
end;

destructor TOQBEngineIBX.Destroy;
begin
  FResultQuery.Free;
  FTransaction.Free;
  inherited;
end;

procedure TOQBEngineIBX.SetConnection(Value: TIBDatabase);
begin
  FIBXConnection := Value;
  FResultQuery.Database := Value;
end;

function TOQBEngineIBX.SelectDatabase: Boolean;
begin
  Result := True;
end;

{procedure TOQBEngineIBX.ReadTableList;
begin
  TableList.Clear;
  FResultQuery.Database.GetTableNames(TableList, ShowSystemTables);
end;}

{
  TOQBEngineIBX.ReadTableList

  Modified to include Views in addition to Tables.

  Situation:
    - A model can contain both Tables and Views, even if it's very small.
    - Engines like SQLDB or Zeos return both Tables and Views.
    - IBX's original GetTableNames method returns only Tables, not Views.
    - If we try to load a model containing Views using IBX, the
      number of items in the model exceeds the number of tables retrieved
      by IBX, causing Range check / ListIndex(-1) errors.
    - This modification ensures that all Views are also added to TableList,
      making IBX compatible with models created with other engines.

  Steps:
    1. Retrieve all Tables using IBX's GetTableNames.
    2. Query the system table RDB$RELATIONS to find Views and add them to TableList.
}

procedure TOQBEngineIBX.ReadTableList;
var
  Q: TIBQuery;
  TableName: string;
begin
  TableList.Clear;

  // 1. First, get all normal tables
  FResultQuery.Database.GetTableNames(TableList, ShowSystemTables);

  // 2. Add views manually
  Q := TIBQuery.Create(nil);
  try
    Q.Database := FResultQuery.Database;

    // Firebird: SYSTEM_FLAG = 0 -> user tables/views
    Q.SQL.Text := 'SELECT RDB$RELATION_NAME FROM RDB$RELATIONS ' +
                  'WHERE RDB$SYSTEM_FLAG = 0 AND RDB$VIEW_BLR IS NOT NULL ' +
                  'ORDER BY RDB$RELATION_NAME';
    Q.Open;

    while not Q.EOF do
    begin
      TableName := Trim(Q.FieldByName('RDB$RELATION_NAME').AsString);
      if TableList.IndexOf(TableName) = -1 then
        TableList.Add(TableName);
      Q.Next;
    end;
  finally
    Q.Free;
  end;
end;


procedure TOQBEngineIBX.ReadFieldList(const ATableName: string);
begin
  FieldList.Clear;
  FResultQuery.Database.GetFieldNames(ATableName, FieldList);
  FieldList.Insert(0, '*');
end;

procedure TOQBEngineIBX.ClearQuerySQL;
begin
  FResultQuery.SQL.Clear;
end;

procedure TOQBEngineIBX.SetQuerySQL(const Value: string);
begin
  FResultQuery.SQL.Text := Value;
end;

function TOQBEngineIBX.ResultQuery: TDataSet;
begin
  Result := FResultQuery;
end;

procedure TOQBEngineIBX.OpenResultQuery;
begin
  FTransaction.DefaultDatabase := FIBXConnection;
  FTransaction.Active := True;
  FResultQuery.Open;
end;

procedure TOQBEngineIBX.CloseResultQuery;
begin
  FResultQuery.Close;
end;

{$WARNINGS OFF}
procedure TOQBEngineIBX.SaveResultQueryData;
begin
//
end;
{$WARNINGS ON}

procedure TOQBEngineIBX.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FIBXConnection) and (Operation = opRemove) then
  begin
    FIBXCOnnection := nil;
    FResultQuery.Database := nil;
  end;
end;


end.

