unit fpcstdexporters;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs,

  DB, IBDatabase,
  fpstdexports,
  fpDataExporter;


procedure _ExportStdFormat(ADataSet: TDataSet);


var StandardExportFormats: TStandardExportFormats; //DataSet export

implementation


procedure _ExportStdFormat(ADataSet: TDataSet);
var Exporter: TFPDataExporter;
begin
  if not Assigned(ADataSet) then
  begin
    ShowMessage('Query not assigned!');
    exit;
  end;

  Exporter := TFPDataExporter.Create(nil);
  try
    Exporter.Dataset := ADataSet;
    Exporter.ShowProgress := True;
    Exporter.ShowResult := True;
    if Exporter.Execute then
      //ShowMessage('Done')
    else;
      //
  finally
    Exporter.Free;
  end;
end;

initialization
StandardExportFormats := TStandardExportFormats.Create(nil);
StandardExportFormats.Active := true;


finalization
StandardExportFormats.Active := false;
StandardExportFormats.Free;

end.

