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
  end;

function CopyMethodToStr(M: TCopyMethod): string;
function CopyStatsRowsPerSec(const Stats: TCopyStatistics): Double;
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
    SL.Add('  Batch Size: ' + FormatFloat('#,##0', Stats.BatchSize));
    if Stats.UseRowRange then
      SL.Add(Format('  Row Range:  %d .. %d', [Stats.FromRow, Stats.ToRow]));
    SL.Add('');

    SL.Add('Result:');
    SL.Add('  Rows Copied: ' + FormatFloat('#,##0', Stats.RowsCopied));
    SL.Add('  Time:        ' + FormatDateTime('hh:nn:ss', Stats.ElapsedSeconds / SecsPerDay));
    SL.Add('  Speed:       ' + FormatFloat('#,##0', CopyStatsRowsPerSec(Stats)) + ' rows/sec');
    SL.Add('');
    SL.Add('═══════════════════════════════════════════════════');

    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

end.
