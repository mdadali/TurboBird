unit uPSI_uibdataset;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB,
  Variants,
  uPSComponent,
  uPSRuntime,
  uPSCompiler,
  uibase,
  uiberror,
  uiblib,
  uibdataset,
  uib;

const
  iptR = uPSCompiler.iptR;

procedure SIRegister_TUIBDataSet(Cl: TPSPascalCompiler);
procedure RIRegister_TUIBDataSet_Routines(Cl: TPSRuntimeClassImporter);

implementation

{ ================= COMPILE TIME ================= }

procedure SIRegister_TUIBDataSet(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TDataSet'), 'TUIBDataSet') do
  begin
    RegisterMethod('constructor Create(AOwner: TComponent)');
    RegisterMethod('procedure Execute');
    RegisterMethod('procedure ExecSQL');
    RegisterMethod('procedure BuildStoredProc(const StoredProc: string; forSelect: Boolean)');

    RegisterMethod('procedure Open');
    RegisterMethod('procedure Close');
    RegisterMethod('procedure Next');
    RegisterMethod('procedure First');

    RegisterProperty('Database', 'TUIBDataBase', iptRW);
    RegisterProperty('Transaction', 'TUIBTransaction', iptRW);
    RegisterProperty('SQL', 'TStrings', iptRW);
    RegisterProperty('Active', 'Boolean', iptRW);
    RegisterProperty('RowsAffected', 'Cardinal', iptR);
    RegisterProperty('Params', 'TSQLParams', iptRW);
  end;
end;

{ ================= RUNTIME ================= }

// Database
procedure TUIBDataSet_Database_W(Self: TUIBDataSet; const T: TUIBDataBase);
begin Self.Database := T; end;

procedure TUIBDataSet_Database_R(Self: TUIBDataSet; var T: TUIBDataBase);
begin T := Self.Database; end;

// Transaction
procedure TUIBDataSet_Transaction_W(Self: TUIBDataSet; const T: TUIBTransaction);
begin Self.Transaction := T; end;

procedure TUIBDataSet_Transaction_R(Self: TUIBDataSet; var T: TUIBTransaction);
begin T := Self.Transaction; end;

// SQL
procedure TUIBDataSet_SQL_R(Self: TUIBDataSet; var T: TStrings);
begin T := Self.SQL; end;

// Active
procedure TUIBDataSet_Active_W(Self: TUIBDataSet; const T: Boolean);
begin Self.Active := T; end;

procedure TUIBDataSet_Active_R(Self: TUIBDataSet; var T: Boolean);
begin T := Self.Active; end;

// RowsAffected
procedure TUIBDataSet_RowsAffected_R(Self: TUIBDataSet; var T: Cardinal);
begin T := Self.RowsAffected; end;

// Params
procedure TUIBDataSet_Params_R(Self: TUIBDataSet; var T: TSQLParams);
begin T := Self.Params; end;

procedure RIRegister_TUIBDataSet_Routines(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TUIBDataSet) do
  begin
    RegisterConstructor(@TUIBDataSet.Create, 'Create');

    RegisterMethod(@TUIBDataSet.Execute, 'Execute');
    RegisterMethod(@TUIBDataSet.ExecSQL, 'ExecSQL');
    RegisterMethod(@TUIBDataSet.BuildStoredProc, 'BuildStoredProc');

    RegisterMethod(@TDataSet.Open, 'Open');
    RegisterMethod(@TDataSet.Close, 'Close');
    RegisterMethod(@TDataSet.Next, 'Next');
    RegisterMethod(@TDataSet.First, 'First');

    RegisterPropertyHelper(@TUIBDataSet_Database_R, @TUIBDataSet_Database_W, 'Database');
    RegisterPropertyHelper(@TUIBDataSet_Transaction_R, @TUIBDataSet_Transaction_W, 'Transaction');
    RegisterPropertyHelper(@TUIBDataSet_SQL_R, nil, 'SQL');
    RegisterPropertyHelper(@TUIBDataSet_Active_R, @TUIBDataSet_Active_W, 'Active');
    RegisterPropertyHelper(@TUIBDataSet_RowsAffected_R, nil, 'RowsAffected');
    RegisterPropertyHelper(@TUIBDataSet_Params_R, nil, 'Params');
  end;
end;

end.

