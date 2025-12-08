{***************************************************************************}
{  TurboBird: FireBird database administration and management tool          }
{  Started by  : Motaz Abdel Azeem http://code.sd/                          }
{  Start development :  5.Dec.2009                                          }
{  Last updated      : 18.Apr.2015                                          }
{  License           : GPL for GUI, LGPL for Units                          }
{***************************************************************************}

program TurboBird;

{$mode objfpc}{$H+}
{$Xs}

uses
  {$IFDEF UNIX}
  cthreads,
  cmem,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Dialogs, Controls, IniFiles, abbrevia, ibexpress, pkg_gifanim,
  lazrichview, indylaz, zcomponent, memdslaz, datetimectrls,
  runtimetypeinfocontrols, main, CreateDb, Reg, QueryWindow, ViewView,
  ViewTrigger, ViewSProc, ViewGen, NewTable, NewGen, EnterPass, About,
  CreateTrigger, fedittabledata, CallProc, UDFInfo, ViewDomain, NewDomain,
  SysTables, NewConstraint, NewEditField, Calen, Scriptdb, UserPermissions,
  TableManage, BackupRestore, CreateUser, ChangePass, PermissionManage,
  SQLHistory, CopyTable, dynlibs, ibase60dyn, dbInfo, sysutils, Comparison,
  topologicalsort, UnitFirebirdServices, turbocommon, importtable, fileimport,
  csvdocument, fServerSession, uthemeselector, lazdbexport,
  udb_firebird_struct_helper, udb_udf_Fetcher, udb_udr_func_fetcher, sqldblib,
  fbcommon, fTestFunction, fSetFBClient, fFirebirdConfig, updatechecker, QBEIBX,
  QBuilder, QBDirFrm, QBLnkFrm,
dmibx, fCheckDBIntegrity, fsqlmonitor,
  fdataexportersintrf, fMarkDownTableExport, fhtmlexport, fpcstdexporters,
  uArrayFormTest, fblobedit, floginservicemanager, fserverregistry,
  ftransactionconfig, tb_netutils,

  //DBAdmin
  MainFormUnit,
  DataModule,
  selectsqlresultsunit_ext,
  fScriptEngine,
  DBLoginDlgUnit,
  ShutdownDatabaseDlgUnit,
  ShutdownRegDlgUnit,
  BackupDlgUnit,
  RestoreDlgUnit,
  AddSecondaryFileDlgUnit,
  AddShadowFileDlgUnit,
  AddShadowSetDlgUnit,
  NewUserDlgUnit,
  ChgPasswordDlgUnit,
  ExecuteSQLScriptDlgUnit;
  //End-DBAdmin


const
  Major = 1;
  Minor = 2;
  Release = 1;

  VersionDate = '2010 - Jan 2015';
{$IFDEF Unix}
{$DEFINE extdecl:=cdecl}

{$ENDIF}
{$IFDEF Windows}
  {$DEFINE extdecl:=stdcall}
{$ENDIF}


{$R *.res}


var
  SAbout: TfmAbout;
  ErrorMessage: string;

begin
  Application.Scaled:=True;
  Application.Initialize;

  {$IFDEF DEBUG}
  // Requires the build mode to set -dDEBUG in Project Options/Other and
  // defining -gh/heaptrace on
  // This avoids interference when running a production/default build without -gh

  // Set up -gh output for the Leakview package:
  if FileExists('heap.trc') then
    DeleteFile('heap.trc');
  SetHeapTraceOutput('heap.trc');
  {$ENDIF DEBUG}


  if not LoadClientLibIBX(InitialFBClientLibPath) then
    SetInitialClientLib;


  Application.CreateForm(TfmMain, fmMain);
  frmThemeSelector := TfrmThemeSelector.Create(fmMain);
  Application.CreateForm(TdmSysTables, dmSysTables);

  SAbout:= TfmAbout.Create(nil);
  SAbout.BitBtn1.Visible:= False;
  SAbout.Show;
  Application.ProcessMessages;
  SAbout.Update;

  fmMain.Version:= Format('%d.%d.%d', [Major, Minor, Release]);
  fmMain.StatusBar1.Panels[1].Text:= 'Version: ' + fmMain.Version;
  fmMain.VersionDate:= VersionDate;
  fmMain.Major:= Major;
  fmMain.Minor:= Minor;
  fmMain.ReleaseVersion:= Release;

  Application.CreateForm(TfmCreateDB, fmCreateDB);
  Application.CreateForm(TfmReg, fmReg);
  Application.CreateForm(TfmNewGen, fmNewGen);
  Application.CreateForm(TfmEnterPass, fmEnterPass);
  Application.CreateForm(TfmCreateTrigger, fmCreateTrigger);
  //Application.CreateForm(TfrmSetFBClient, frmSetFBClient);
  //Application.CreateForm(TfmEditTable, fmEditTable);
  //Application.CreateForm(TfmCallProc, fmCallProc);
  Application.CreateForm(TfmNewDomain, fmNewDomain);
  Application.CreateForm(TfmNewConstraint, fmNewConstraint);
  Application.CreateForm(TfmCalen, fmCalen);
  //Application.CreateForm(TfmBackupRestore, fmBackupRestore);
  Application.CreateForm(TfmCreateUser, fmCreateUser);
  Application.CreateForm(TfmChangePass, fmChangePass);
  Application.CreateForm(TfmSQLHistory, fmSQLHistory);
  //Application.CreateForm(TfmCopyTable, fmCopyTable);

  //Application.CreateForm(TDataModuleIBX, DataModuleIBX);
  Application.CreateForm(TfmTransactionConfig, fmTransactionConfig);
  //Application.CreateForm(TfrmLoginServiceManager, frmLoginServiceManager);
  //Application.CreateForm(TfmServerRegistry, fmServerRegistry);
  //Application.CreateForm(TfrmBlobEdit, frmBlobEdit);
  //Application.CreateForm(TfrmArrayTest, frmArrayTest);
  //Application.CreateForm(TfmCheckDBIntegrity, fmCheckDBIntegrity);
  //Application.CreateForm(TfmSQLMonitor, fmSQLMonitor);
  SAbout.Free;

  //DBAdmin
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TDBDataModule, DBDataModule);
  Application.CreateForm(TDBLoginDlg, DBLoginDlg);
  Application.CreateForm(TShutdownDatabaseDlg, ShutdownDatabaseDlg);
  Application.CreateForm(TShutdownReqDlg, ShutdownReqDlg);
  Application.CreateForm(TBackupDlg, BackupDlg);
  Application.CreateForm(TRestoreDlg, RestoreDlg);
  Application.CreateForm(TAddSecondaryFileDlg, AddSecondaryFileDlg);
  Application.CreateForm(TAddShadowFileDlg, AddShadowFileDlg);
  Application.CreateForm(TAddShadowSetDlg, AddShadowSetDlg);
  Application.CreateForm(TNewUserDlg, NewUserDlg);
  Application.CreateForm(TChgPasswordDlg, ChgPasswordDlg);
  Application.CreateForm(TExecuteSQLScriptDlg, ExecuteSQLScriptDlg);

  Application.Run;
  //ReleaseIBase60;
end.
