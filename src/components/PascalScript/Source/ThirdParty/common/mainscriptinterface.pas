unit mainscriptinterface;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  math

  ,uPSComponent
 ,uPSRuntime
 ,uPSCompiler,
  uPSC_DB,

  //uPSI_DialogsScriptInterface, //old
  {$IFDEF CADSys4}
    upsi_cad2dscripinterface,
    upsi_cadsys4_mathscriptinterface.pas,
  {$ELSE}
    uPSI_MathScriptinterface,
  {$ENDIF}

  uPSI_Dialogs,
  uPSI_IBXConnection,


  uPSI_uibconst,
  uPSI_uib,
  uPSI_uiblib,
  uPSI_uibmetadata,
  uPSI_uibsqlparser,
  uPSI_uibdataset,

  uPSC_DBControls
  ;

procedure SIRegister_MainScriptInterface(CL: TPSPascalCompiler);
procedure RIRegister_MainScriptInterface_Routines(S: TPSExec; x: TPSRuntimeClassImporter);

implementation

procedure SIRegister_MainScriptInterface(CL: TPSPascalCompiler);
begin
  //SIRegister_DialogsScriptInterface(CL); //old

  {$IFDEF CADSys4}
    SIRegister_CAD2DScripInterface(CL);
  {$ENDIF}

  SIRegister_Dialogs(CL);

  SIRegister_DB(Cl);  //orig.  partner das wird ausgeführt
  SIRegister_DBControls(CL);

  SIRegister_TPSIBXConnection(CL);
  SIRegister_TIBXConnection(CL);

  SIRegister_uiblib(CL);
  SIRegister_uibconst(CL);
  SIRegister_uibmetadata(CL);
  SIRegister_uibsqlparser(CL);
  SIRegister_uib(CL);

  SIRegister_TUIBDataSet(Cl);
end;

procedure RIRegister_MainScriptInterface_Routines(S: TPSExec; x: TPSRuntimeClassImporter);
begin
  {$IFDEF CADSys4}
    RIRegister_CAD2DScripInterface_Routines(S);
  {$ENDIF}

  RIRegister_MathScriptInterface_Routines(S);
  //RIRegister_DialogsScriptInterface_Routines(S); //oöd
  RIRegister_Dialogs(x);

  RIRegister_Dialogs_Routines(S);
  RIRegister_TPSIBXConnection(x);
  RIRegister_TIBXConnection(x);

  RIRegister_uiblib(x);
  RIRegister_uibmetadata(x);
  RIRegister_uibsqlparser(x);
  RIRegister_uib(x);
  RIRegister_DB_Routines(x);  //und auch das wird ausgeführt
  RIRegister_TUIBDataSet_Routines(x);

  RIRegister_DBControls(x);
end;

end.
