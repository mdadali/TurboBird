program DBReader;

uses
  Forms,
  MainForm in 'MainForm.pas' {FormMain},
  DBReaderFirebird in 'DBReaderFirebird.pas',
  ValueViewForm in 'ValueViewForm.pas' {FormRawValue},
  DBReaderBerkley in 'DBReaderBerkley.pas',
  DBReaderMidas in 'DBReaderMidas.pas',
  DBReaderParadox in 'DBReaderParadox.pas',
  DBReaderBase in 'DBReaderBase.pas',
  DBReaderDbf in 'DBReaderDbf.pas',
  FSReaderMtf in 'FSReaderMtf.pas',
  DBReaderMdf in 'DBReaderMdf.pas',
  DBReaderMdb in 'DBReaderMdb.pas',
  FSReaderPst in 'FSReaderPst.pas',
  FSReaderBase in 'FSReaderBase.pas',
  DBReaderEdb in 'DBReaderEdb.pas',
  DBReaderInno in 'DBReaderInno.pas',
  DBReaderSqlite in 'DBReaderSqlite.pas',
  DBReaderSybase in 'DBReaderSybase.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormRawValue, FormRawValue);
  Application.Run;
end.
