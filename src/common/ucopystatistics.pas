unit uCopyStatistics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils;

type
  TCopyMethod = (cmLocal, cmCrossExecuteBlock, cmCrossRowByRow);

  TCopyStatistics = record
    Method: TCopyMethod;

    SourceServer: string;
    SourceDatabase: string;
    SourceTable: string;
    SourceIsExternal: Boolean;

    DestServer: string;
    DestDatabase: string;
    DestTable: string;
    DestIsExternal: Boolean;

    RowsCopied: Int64;
    BatchSize: Integer;
    FromRow: Integer;
    ToRow: Integer;
    UseRowRange: Boolean;

    ElapsedSeconds: Double;

    CreateTableSQL: string;      // Die CREATE TABLE-Anweisung (leer, wenn nicht erstellt)
    FormulasApplied: string;
  end;

function CopyMethodToStr(M: TCopyMethod): string;
function CopyStatsRowsPerSec(const Stats: TCopyStatistics): Double;
function FormatCopyReport(const Stats: TCopyStatistics): string;

implementation

function FormatNumberEN(const AValue: Int64): string;
var
  OldSep: Char;
begin
  OldSep := DefaultFormatSettings.ThousandSeparator;
  try
    DefaultFormatSettings.ThousandSeparator := ',';
    Result := FormatFloat('#,##0', AValue);
  finally
    DefaultFormatSettings.ThousandSeparator := OldSep;
  end;
end;

function CopyMethodToStr(M: TCopyMethod): string;
begin
  case M of
    cmLocal:             Result := 'Same Database (INSERT...SELECT)';
    cmCrossExecuteBlock: Result := 'Cross-Database (Execute Block)';
    cmCrossRowByRow:     Result := 'Cross-Database (Row-by-Row)';
  else
    Result := 'Unknown';
  end;
end;

function CopyStatsRowsPerSec(const Stats: TCopyStatistics): Double;
begin
  if Stats.ElapsedSeconds > 0 then
    Result := Stats.RowsCopied / Stats.ElapsedSeconds
  else
    Result := 0;
end;

function FormatCopyReport(const Stats: TCopyStatistics): string;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Add('═══════════════════════════════════════════════════');
    SL.Add('                   COPY REPORT');
    SL.Add('═══════════════════════════════════════════════════');
    SL.Add('');
    SL.Add('Copy Method:');
    SL.Add('  ' + CopyMethodToStr(Stats.Method));
    SL.Add('');

    SL.Add('Source:');
    SL.Add('  Server:    ' + Stats.SourceServer);
    SL.Add('  Database:  ' + Stats.SourceDatabase);
    SL.Add('  Table:     ' + Stats.SourceTable);
    if Stats.SourceIsExternal then
      SL.Add('  Type:      External Table')
    else
      SL.Add('  Type:      Firebird Table');
    SL.Add('');

    SL.Add('Destination:');
    SL.Add('  Server:    ' + Stats.DestServer);
    SL.Add('  Database:  ' + Stats.DestDatabase);
    SL.Add('  Table:     ' + Stats.DestTable);
    if Stats.DestIsExternal then
      SL.Add('  Type:      External Table')
    else
      SL.Add('  Type:      Firebird Table');
    SL.Add('');

    SL.Add('Options:');
    SL.Add('  Batch Size: ' + FormatNumberEN(Stats.BatchSize));
    if Stats.UseRowRange then
      SL.Add(Format('  Row Range:  %d .. %d', [Stats.FromRow, Stats.ToRow]));
    SL.Add('');

    // --- Formulas (nur wenn vorhanden) ---
    if Stats.FormulasApplied <> '' then
    begin
      SL.Add('Formulas Applied:');
      SL.Add(Stats.FormulasApplied);
      SL.Add('');
    end;

    SL.Add('Result:');
    SL.Add('  Rows Copied: ' + FormatNumberEN(Stats.RowsCopied));
    SL.Add('  Time:        ' + FormatDateTime('hh:nn:ss', Stats.ElapsedSeconds / SecsPerDay));
    SL.Add('  Speed:       ' + FormatNumberEN(Round(CopyStatsRowsPerSec(Stats))) + ' rows/sec');    SL.Add('');
    // --- Table Structure (nur wenn vorhanden) ---
    if Stats.CreateTableSQL <> '' then
    begin
      SL.Add('Table Structure:');
      SL.Add(Stats.CreateTableSQL);
      SL.Add('');
    end;
    SL.Add('═══════════════════════════════════════════════════');

    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

end.
