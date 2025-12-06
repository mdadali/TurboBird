unit fdataexportersintrf;

{$mode ObjFPC}{$H+}

interface

uses
  Forms, Types, Classes, StdCtrls, ComCtrls, SysUtils, StrUtils, DateUtils, Dialogs, {$IFDEF WINDOWS} Windows, {$ENDIF}
  LCLType,
  interfaces, LCLPlatformDef,
  DB, sqldb, IBConnection,  IBDatabase,


  fpcstdexporters,
  fMarkDownTableExport,
  fhtmlexport;


procedure ExportStdFormat(ADataSet: TDataSet);
procedure ExportDataMarkDownTable(ADataSet: TDataSet);
procedure ExportDataHtml(ADataSet: TDataSet);


implementation

procedure ExportStdFormat(ADataSet: TDataSet);
begin
   _ExportStdFormat(ADataSet);
end;

procedure ExportDataMarkDownTable(ADataSet: TDataSet);
begin
  _ExportDataMarkDownTable(ADataSet);
end;

procedure ExportDataHtml(ADataSet: TDataSet);
begin
  _ExportDataHtml(ADataSet);
end;



end.

