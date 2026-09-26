unit uCopyStatistics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils,
  uSystemInfo;

type
  TCopyMethod = (cmLocal, cmCrossExecuteBlock, cmCrossRowByRow);

  TCopyStatistics = record
    Method: TCopyMethod;

    SourceServer: string;
    SourceDatabase: string;
    SourceTable: string;
    SourceIsExternal: Boolean;
    SourceServerVersion: string;

    DestServer: string;
    DestDatabase: string;
    DestTable: string;
    DestIsExternal: Boolean;
    DestServerVersion: string;

    ClientLibVersion: string;

    RowsCopied: Int64;
    BatchSize: Integer;
    FromRow: Integer;
    ToRow: Integer;
    UseRowRange: Boolean;

    ElapsedSeconds: Double;

    CreateTableSQL: string;
    FormulasApplied: string;

    SystemInfo: TSystemInfo;
  end;

function CopyMethodToStr(M: TCopyMethod): string;
function CopyStatsRowsPerSec(const Stats: TCopyStatistics): Double;
function FormatNumberEN(const AValue: Int64): string;
function FormatCopyReport(const Stats: TCopyStatistics): string;

implementation

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

function FormatNumberEN(const AValue: Int64): string;
var
  S: string;
  i, Len: Integer;
begin
  S := IntToStr(AValue);
  Len := Length(S);
  Result := '';

  for i := 1 to Len do
  begin
    Result := Result + S[i];
    if ((Len - i) mod 3 = 0) and (i < Len) then
      Result := Result + ',';
  end;
end;

function FormatCopyReport(const Stats: TCopyStatistics): string;
var
  SL: TStringList;
  RowsPerSec: Double;
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
    if Stats.SourceServerVersion <> '' then
      SL.Add('  Version:   ' + Stats.SourceServerVersion);
    SL.Add('');

    SL.Add('Destination:');
    SL.Add('  Server:    ' + Stats.DestServer);
    SL.Add('  Database:  ' + Stats.DestDatabase);
    SL.Add('  Table:     ' + Stats.DestTable);
    if Stats.DestIsExternal then
      SL.Add('  Type:      External Table')
    else
      SL.Add('  Type:      Firebird Table');
    if Stats.DestServerVersion <> '' then
      SL.Add('  Version:   ' + Stats.DestServerVersion);
    SL.Add('');

    SL.Add('Options:');
    SL.Add('  Batch Size: ' + FormatNumberEN(Stats.BatchSize));
    if Stats.UseRowRange then
      SL.Add(Format('  Row Range:  %s .. %s',
        [FormatNumberEN(Stats.FromRow), FormatNumberEN(Stats.ToRow)]));
    SL.Add('');

    if Stats.FormulasApplied <> '' then
    begin
      SL.Add('Formulas Applied:');
      SL.Add(Stats.FormulasApplied);
      SL.Add('');
    end;

    SL.Add('Result:');
    SL.Add('  Rows Copied: ' + FormatNumberEN(Stats.RowsCopied));
    SL.Add('  Time:        ' + FormatDateTime('hh:nn:ss',
      Stats.ElapsedSeconds / SecsPerDay));

    RowsPerSec := CopyStatsRowsPerSec(Stats);
    SL.Add('  Speed:       ' + FormatNumberEN(Round(RowsPerSec)) + ' rows/sec');
    SL.Add('');

    if Stats.CreateTableSQL <> '' then
    begin
      SL.Add('Table Structure:');
      SL.Add(Stats.CreateTableSQL);
      SL.Add('');
    end;

    // === System / Environment ===
    SL.Add('Environment:');
    if Stats.ClientLibVersion <> '' then
      SL.Add('  Client Lib: ' + Stats.ClientLibVersion);
    SL.Add('  OS:        ' + Stats.SystemInfo.OSName);
    SL.Add('  CPU:       ' + Stats.SystemInfo.CPUModel);
    SL.Add('  Cores:     ' + IntToStr(Stats.SystemInfo.CPUCores));
    SL.Add('  RAM:       ' + FormatNumberEN(Stats.SystemInfo.RAMTotalMB) + ' MB');

    if Stats.SystemInfo.DiskPath <> '' then
    begin
      SL.Add('  Disk (' + ExtractFileName(Stats.SystemInfo.DiskPath) + '):');
      SL.Add('    Free:    ' + FormatNumberEN(Stats.SystemInfo.DiskFreeMB) + ' MB');
      SL.Add('    Total:   ' + FormatNumberEN(Stats.SystemInfo.DiskTotalMB) + ' MB');
      if Stats.SystemInfo.DiskType <> '' then
        SL.Add('    Type:    ' + Stats.SystemInfo.DiskType);
    end;

    SL.Add('');
    SL.Add('═══════════════════════════════════════════════════');

    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

end.
