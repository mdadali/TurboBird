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
  fhtmlexport,
  fClipboardExport;


procedure ExportStdFormat(ADataSet: TDataSet);
procedure ExportDataMarkDownTable(ADataSet: TDataSet);
procedure ExportDataHtml(ADataSet: TDataSet);
procedure ExportDataToClipboard(ADataSet: TDataSet; MaxRows: Integer);


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

procedure ExportDataToClipboard(ADataSet: TDataSet; MaxRows: Integer);
begin
  _ExportDataToClipboard(ADataSet, MaxRows);
end;

end.

