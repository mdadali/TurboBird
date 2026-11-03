unit uCrossRDBMSReport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uCrossRDBMSTypes,
  uCrossRDBMSTypeMapping,
  uCrossRDBMSStrategies;

function FormatCrossTransferReport(const AStats: TCrossTransferStats; const AFields: TCrossFieldArray): string;


implementation

const
  SEP = '═══════════════════════════════════════════════════';

function FormatNumberEN(AValue: Int64): string;
var FS: TFormatSettings;
begin
  FS := DefaultFormatSettings;
  FS.DecimalSeparator  := '.';
  FS.ThousandSeparator := ',';
  Result := FormatFloat('#,##0', AValue, FS);
end;

function FormatBytesSmart(ABytes: Int64): string;
var FS: TFormatSettings;
begin
  FS := DefaultFormatSettings;
  FS.DecimalSeparator  := '.';
  FS.ThousandSeparator := ',';

  if ABytes >= (Int64(1024) * 1024 * 1024) then
    Result := FormatFloat('#,##0.00', ABytes / (1024.0 * 1024 * 1024), FS) + ' GB'
  else if ABytes >= (Int64(1024) * 1024) then
    Result := FormatFloat('#,##0.00', ABytes / (1024.0 * 1024), FS) + ' MB'
  else if ABytes >= 1024 then
    Result := FormatFloat('#,##0.00', ABytes / 1024.0, FS) + ' KB'
  else
    Result := IntToStr(ABytes) + ' Bytes';
end;

//Helpers for FormatCrossTransferReport
function PadRight(const S: string; AWidth: Integer): string;
begin
  Result := S;
  while Length(Result) < AWidth do
    Result := Result + ' ';
end;

function IsComputedStaysComputedReport(const AField: TCrossField;
  const AStats: TCrossTransferStats): Boolean;
begin
  Result := AField.IsComputed and
            (AStats.SourceDBMS = cdFirebird) and
            (AStats.DestDBMS = cdFirebird);
end;

function FormatCrossTransferReport(const AStats: TCrossTransferStats; const AFields: TCrossFieldArray): string;
var
  SL: TStringList;
  FS: TFormatSettings;
  i: integer;
begin
  FS := DefaultFormatSettings;
  FS.DecimalSeparator  := '.';
  FS.ThousandSeparator := ',';

  SL := TStringList.Create;
  try
    SL.Add(SEP);
    SL.Add('              CROSSRDBMS TRANSFER REPORT');
    SL.Add(SEP);
    SL.Add('');

    SL.Add('Source:');
    SL.Add('  DBMS:      ' + DBMSName(AStats.SourceDBMS));
    SL.Add('  Connector: ' + AStats.SourceConnectorType);
    SL.Add('  Database:  ' + AStats.SourceDatabase);
    SL.Add('  Table:     ' + AStats.SourceTable);
    SL.Add('');

    SL.Add('Destination:');
    SL.Add('  DBMS:      ' + DBMSName(AStats.DestDBMS));
    SL.Add('  Connector: ' + AStats.DestConnectorType);
    SL.Add('  Database:  ' + AStats.DestDatabase);
    SL.Add('  Table:     ' + AStats.DestTable);
    SL.Add('');

    SL.Add('Table Action:');
    if AStats.TableRecreated then
      SL.Add('  Recreated (drop + create)')
    else if AStats.TableCreated then
      SL.Add('  Created (new table)')
    else if AStats.TableExisted then
      SL.Add('  Appended (existing table)')
    else
      SL.Add('  No action');
    SL.Add('');

    SL.Add('Result:');
    if AStats.Cancelled then
      SL.Add('  Status:        CANCELLED')
    else
      SL.Add('  Status:        COMPLETED');
    SL.Add('  Strategy:      ' + StrategyName(AStats.StrategyUsed));
    SL.Add('  Rows:          ' + FormatNumberEN(AStats.RowsProcessed));
    SL.Add('  Rows total:    ' + FormatNumberEN(AStats.RowsTotal));
    SL.Add('  Total Time:    ' + FormatDateTime('hh:nn:ss.zzz',
      AStats.ElapsedSeconds / SecsPerDay));
    if AStats.RowsPerSec > 0 then
      SL.Add('  Speed:         ' + FormatFloat('#,##0', AStats.RowsPerSec, FS) + ' rows/sec');
    if AStats.BytesProcessed > 0 then
      SL.Add('  Bytes:         ' + FormatBytesSmart(AStats.BytesProcessed));
    if AStats.MBPerSec > 0 then
      SL.Add('  Data Rate:     ' + FormatFloat('#,##0.00', AStats.MBPerSec, FS) + ' MB/sec');
    SL.Add('  Commits:       ' + IntToStr(AStats.CommitsDone));
    SL.Add('');

    if Assigned(AStats.Warnings) and (AStats.Warnings.Count > 0) then
    begin
      SL.Add('Warnings:');
      SL.Add(AStats.Warnings.Text);
      SL.Add('');
    end;

    if Trim(AStats.CreateSQL) <> '' then
    begin
      SL.Add('Table Structure:');
      SL.Add(AStats.CreateSQL);
      SL.Add('');
    end;

    if Trim(AStats.InsertSQL) <> '' then
    begin
      SL.Add('Insert SQL (template):');
      SL.Add(AStats.InsertSQL);
      SL.Add('');
    end;

    if Trim(AStats.SystemInfo) <> '' then
    begin
      { SystemInfo beginnt mit "Environment:" }
      SL.Add(AStats.SystemInfo);
      SL.Add('');
    end;

    SL.Add(SEP);

    { ---- Source-Feldliste ---- }
    SL.Add('Source Structure:');
    SL.Add('  Table: ' + AStats.SourceTable);
    for i := 0 to High(AFields) do
    begin
      if not AFields[i].Selected then Continue;
      if AFields[i].IsProblemField then Continue;
      if IsComputedStaysComputedReport(AFields[i], AStats) then Continue;
      SL.Add('  ' + PadRight(AFields[i].SourceName, 24) + ' ' + AFields[i].SourceType);
    end;
    SL.Add('');

    { ---- Destination-Feldliste ---- }
    SL.Add('Destination Structure:');
    SL.Add('  Table: ' + AStats.DestTable);
    for i := 0 to High(AFields) do
    begin
      if not AFields[i].Selected then Continue;
      if AFields[i].IsProblemField then Continue;
      if IsComputedStaysComputedReport(AFields[i], AStats) then Continue;
      SL.Add('  ' + PadRight(AFields[i].DestName, 24) + ' ' + AFields[i].DestType);
    end;
    SL.Add('');

    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

end.
