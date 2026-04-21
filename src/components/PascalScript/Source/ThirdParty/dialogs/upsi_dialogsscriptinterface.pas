unit uPSI_DialogsScriptInterface;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Forms, Graphics, Printers, PrintersDlgs, ExtDlgs, // Für erweiterte Dialoge
  uPSComponent, uPSRuntime, uPSCompiler;

procedure SIRegister_DialogsScriptInterface(CL: TPSPascalCompiler);
procedure RIRegister_DialogsScriptInterface_Routines(S: TPSExec);

implementation

// --- Dialogfunktionen ---

// Datei öffnen
function ScriptOpenFileDialog(Title, InitialDir, Filter: string): string;
var
  Dialog: TOpenDialog;
begin
  Dialog := TOpenDialog.Create(nil);
  try
    Dialog.Title := Title;
    Dialog.InitialDir := InitialDir;
    Dialog.Filter := Filter;
    if Dialog.Execute then
      Result := Dialog.FileName
    else
      Result := '';
  finally
    Dialog.Free;
  end;
end;

// Datei speichern
function ScriptSaveFileDialog(Title, InitialDir, Filter: string): string;
var
  Dialog: TSaveDialog;
begin
  Dialog := TSaveDialog.Create(nil);
  try
    Dialog.Title := Title;
    Dialog.InitialDir := InitialDir;
    Dialog.Filter := Filter;
    if Dialog.Execute then
      Result := Dialog.FileName
    else
      Result := '';
  finally
    Dialog.Free;
  end;
end;

// Verzeichnis auswählen
function ScriptSelectDirectoryDialog(Title, InitialDir: string): string;
begin
  if SelectDirectory(Title, InitialDir, Result) then
    Exit;
  Result := '';
end;

// Schriftart wählen
function ScriptFontDialog: string;
var
  Dialog: TFontDialog;
begin
  Dialog := TFontDialog.Create(nil);
  try
    if Dialog.Execute then
      Result := Dialog.Font.Name + ',' + IntToStr(Dialog.Font.Size)
    else
      Result := '';
  finally
    Dialog.Free;
  end;
end;

// Farbe wählen
function ScriptColorDialog: TColor;
var
  Dialog: TColorDialog;
begin
  Dialog := TColorDialog.Create(nil);
  try
    if Dialog.Execute then
      Result := Dialog.Color
    else
      Result := clNone;
  finally
    Dialog.Free;
  end;
end;

// Farbe über Button auswählen
function ScriptColorButtonDialog: TColor;
var
  Button: TColorButton;
begin
  Button := TColorButton.Create(nil);
  try
    Result := Button.ButtonColor;
  finally
    Button.Free;
  end;
end;

// Drucken
function ScriptPrintDialog: Boolean;
var
  Dialog: TPrintDialog;
begin
  Dialog := TPrintDialog.Create(nil);
  try
    Result := Dialog.Execute;
  finally
    Dialog.Free;
  end;
end;

// Druckereinrichtung
function ScriptPrinterSetupDialog: Boolean;
var
  Dialog: TPrinterSetupDialog;
begin
  Dialog := TPrinterSetupDialog.Create(nil);
  try
    Result := Dialog.Execute;
  finally
    Dialog.Free;
  end;
end;

// Seite einrichten
function ScriptPageSetupDialog: Boolean;
var
  Dialog: TPageSetupDialog;
begin
  Dialog := TPageSetupDialog.Create(nil);
  try
    Result := Dialog.Execute;
  finally
    Dialog.Free;
  end;
end;

// Bild öffnen
function ScriptOpenPictureDialog: string;
var
  Dialog: TOpenPictureDialog;
begin
  Dialog := TOpenPictureDialog.Create(nil);
  try
    if Dialog.Execute then
      Result := Dialog.FileName
    else
      Result := '';
  finally
    Dialog.Free;
  end;
end;

// Bild speichern
function ScriptSavePictureDialog: string;
var
  Dialog: TSavePictureDialog;
begin
  Dialog := TSavePictureDialog.Create(nil);
  try
    if Dialog.Execute then
      Result := Dialog.FileName
    else
      Result := '';
  finally
    Dialog.Free;
  end;
end;

// Kalender auswählen
function ScriptCalendarDialog: TDate;
var
  Dialog: TCalendarDialog;
begin
  Dialog := TCalendarDialog.Create(nil);
  try
    if Dialog.Execute then
      Result := Dialog.Date
    else
      Result := 0;
  finally
    Dialog.Free;
  end;
end;

// Rechner
function ScriptCalculatorDialog: Double;
var
  Dialog: TCalculatorDialog;
begin
  Dialog := TCalculatorDialog.Create(nil);
  try
    if Dialog.Execute then
      Result := Dialog.Value
    else
      Result := 0;
  finally
    Dialog.Free;
  end;
end;

// --- Registrierungsprozeduren für den Compiler ---

procedure SIRegister_DialogsScriptInterface(CL: TPSPascalCompiler);
begin
  CL.AddDelphiFunction('function OpenFileDialog(const Title, InitialDir, Filter: string): string');
  CL.AddDelphiFunction('function SaveFileDialog(const Title, InitialDir, Filter: string): string');
  CL.AddDelphiFunction('function SelectDirectoryDialog(const Title, InitialDir: string): string');
  CL.AddDelphiFunction('function FontDialog: string');
  CL.AddDelphiFunction('function ColorDialog: TColor');
  CL.AddDelphiFunction('function ColorButtonDialog: TColor');
  CL.AddDelphiFunction('function PrintDialog: Boolean');
  CL.AddDelphiFunction('function PrinterSetupDialog: Boolean');
  CL.AddDelphiFunction('function PageSetupDialog: Boolean');
  CL.AddDelphiFunction('function OpenPictureDialog: string');
  CL.AddDelphiFunction('function SavePictureDialog: string');
  CL.AddDelphiFunction('function CalendarDialog: TDateTime');
  CL.AddDelphiFunction('function CalculatorDialog: Double');
end;

// --- Registrierungsprozeduren für die Laufzeit ---

procedure RIRegister_DialogsScriptInterface_Routines(S: TPSExec);
begin
  S.RegisterDelphiFunction(@ScriptOpenFileDialog, 'OpenFileDialog', cdRegister);
  S.RegisterDelphiFunction(@ScriptSaveFileDialog, 'SaveFileDialog', cdRegister);
  S.RegisterDelphiFunction(@ScriptSelectDirectoryDialog, 'SelectDirectoryDialog', cdRegister);
  S.RegisterDelphiFunction(@ScriptFontDialog, 'FontDialog', cdRegister);
  S.RegisterDelphiFunction(@ScriptColorDialog, 'ColorDialog', cdRegister);
  S.RegisterDelphiFunction(@ScriptColorButtonDialog, 'ColorButtonDialog', cdRegister);
  S.RegisterDelphiFunction(@ScriptPrintDialog, 'PrintDialog', cdRegister);
  S.RegisterDelphiFunction(@ScriptPrinterSetupDialog, 'PrinterSetupDialog', cdRegister);
  S.RegisterDelphiFunction(@ScriptPageSetupDialog, 'PageSetupDialog', cdRegister);
  S.RegisterDelphiFunction(@ScriptOpenPictureDialog, 'OpenPictureDialog', cdRegister);
  S.RegisterDelphiFunction(@ScriptSavePictureDialog, 'SavePictureDialog', cdRegister);
  S.RegisterDelphiFunction(@ScriptCalendarDialog, 'CalendarDialog', cdRegister);
  S.RegisterDelphiFunction(@ScriptCalculatorDialog, 'CalculatorDialog', cdRegister);
end;

end.

