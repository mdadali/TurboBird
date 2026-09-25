unit uSystemInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TSystemInfo = record
    CPUModel: string;
    CPUCores: Integer;
    RAMTotalMB: Int64;
    DiskFreeMB: Int64;
    DiskTotalMB: Int64;
    DiskPath: string;
    OSName: string;
  end;

function GetSystemInfo(const APath: string = ''): TSystemInfo;
function FormatSystemInfo(const AInfo: TSystemInfo): string;

implementation

{$IFDEF LINUX}
uses
  BaseUnix;
{$ENDIF}

{$IFDEF WINDOWS}
uses
  Windows;
{$ENDIF}

// ---------------------------------------------------------------------------
// CPU-Modell auslesen
// ---------------------------------------------------------------------------
function GetCPUModel: string;
{$IFDEF LINUX}
var
  SL: TStringList;
  i: Integer;
  Line: string;
begin
  Result := 'Unknown CPU';
  if not FileExists('/proc/cpuinfo') then Exit;

  SL := TStringList.Create;
  try
    SL.LoadFromFile('/proc/cpuinfo');
    for i := 0 to SL.Count - 1 do
    begin
      Line := SL[i];
      if Pos('model name', Line) = 1 then
      begin
        Result := Trim(Copy(Line, Pos(':', Line) + 1, MaxInt));
        Break;
      end;
    end;
  finally
    SL.Free;
  end;
end;
{$ENDIF}

{$IFDEF WINDOWS}
var
  Reg: TRegistry;
begin
  Result := 'Unknown CPU';
  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly(
      '\HARDWARE\DESCRIPTION\System\CentralProcessor\0') then
    begin
      Result := Reg.ReadString('ProcessorNameString');
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;
{$ENDIF}

{$IFDEF DARWIN}
var
  P: TProcess;
  SL: TStringList;
begin
  Result := 'Unknown CPU';
  P := TProcess.Create(nil);
  SL := TStringList.Create;
  try
    P.Executable := '/usr/sbin/sysctl';
    P.Parameters.Add('-n');
    P.Parameters.Add('machdep.cpu.brand_string');
    P.Options := [poUsePipes, poWaitOnExit];
    P.Execute;
    SL.LoadFromStream(P.Output);
    if SL.Count > 0 then
      Result := Trim(SL[0]);
  finally
    SL.Free;
    P.Free;
  end;
end;
{$ENDIF}

// ---------------------------------------------------------------------------
// RAM-Gesamt auslesen
// ---------------------------------------------------------------------------
function GetRAMTotalMB: Int64;
{$IFDEF LINUX}
var
  SL: TStringList;
  i: Integer;
  Line, ValStr: string;
  MemKB: Int64;
begin
  Result := 0;
  if not FileExists('/proc/meminfo') then Exit;

  SL := TStringList.Create;
  try
    SL.LoadFromFile('/proc/meminfo');
    for i := 0 to SL.Count - 1 do
    begin
      Line := SL[i];
      if Pos('MemTotal:', Line) = 1 then
      begin
        ValStr := Trim(Copy(Line, Pos(':', Line) + 1, MaxInt));
        ValStr := Copy(ValStr, 1, Pos(' ', ValStr) - 1);
        if TryStrToInt64(ValStr, MemKB) then
          Result := MemKB div 1024;   // KB → MB
        Break;
      end;
    end;
  finally
    SL.Free;
  end;
end;
{$ENDIF}

{$IFDEF WINDOWS}
var
  MemStatus: TMemoryStatusEx;
begin
  Result := 0;
  FillChar(MemStatus, SizeOf(MemStatus), 0);
  MemStatus.dwLength := SizeOf(MemStatus);
  if GlobalMemoryStatusEx(MemStatus) then
    Result := MemStatus.ullTotalPhys div (1024 * 1024);
end;
{$ENDIF}

{$IFDEF DARWIN}
var
  P: TProcess;
  SL: TStringList;
  ValStr: string;
begin
  Result := 0;
  P := TProcess.Create(nil);
  SL := TStringList.Create;
  try
    P.Executable := '/usr/sbin/sysctl';
    P.Parameters.Add('-n');
    P.Parameters.Add('hw.memsize');
    P.Options := [poUsePipes, poWaitOnExit];
    P.Execute;
    SL.LoadFromStream(P.Output);
    if SL.Count > 0 then
    begin
      ValStr := Trim(SL[0]);
      Result := StrToInt64Def(ValStr, 0) div (1024 * 1024);
    end;
  finally
    SL.Free;
    P.Free;
  end;
end;
{$ENDIF}

// ---------------------------------------------------------------------------
// OS-Name
// ---------------------------------------------------------------------------
function GetOSName: string;
{$IFDEF LINUX}
var
  SL: TStringList;
  i: Integer;
  Line: string;
begin
  Result := 'Linux';
  if FileExists('/etc/os-release') then
  begin
    SL := TStringList.Create;
    try
      SL.LoadFromFile('/etc/os-release');
      for i := 0 to SL.Count - 1 do
      begin
        Line := SL[i];
        if Pos('PRETTY_NAME=', Line) = 1 then
        begin
          Result := Copy(Line, Pos('=', Line) + 1, MaxInt);
          Result := StringReplace(Result, '"', '', [rfReplaceAll]);
          Break;
        end;
      end;
    finally
      SL.Free;
    end;
  end;
end;
{$ENDIF}

{$IFDEF WINDOWS}
begin
  Result := 'Windows';
end;
{$ENDIF}

{$IFDEF DARWIN}
begin
  Result := 'macOS';
end;
{$ENDIF}

// ---------------------------------------------------------------------------
// Hauptfunktion
// ---------------------------------------------------------------------------
function GetSystemInfo(const APath: string): TSystemInfo;
var
  PathToCheck: string;
begin
  Result.CPUModel := GetCPUModel;
  Result.CPUCores := System.CpuCount;   // ← Einfach und plattformübergreifend
  Result.RAMTotalMB := GetRAMTotalMB;
  Result.OSName := GetOSName;

  // Festplatte
  Result.DiskPath := APath;
  Result.DiskFreeMB := 0;
  Result.DiskTotalMB := 0;

  if APath <> '' then
  begin
    PathToCheck := IncludeTrailingPathDelimiter(ExtractFileDir(APath));
    if PathToCheck = '' then
      PathToCheck := ExtractFilePath(APath);

    try
      Result.DiskFreeMB := DiskFree(AddDisk(PathToCheck)) div (1024 * 1024);
      Result.DiskTotalMB := DiskSize(AddDisk(PathToCheck)) div (1024 * 1024);
    except
      // Ignorieren – manche Pfade lassen sich nicht auflösen
    end;
  end;
end;

// ---------------------------------------------------------------------------
// Formatierung für den Report
// ---------------------------------------------------------------------------
function FormatSystemInfo(const AInfo: TSystemInfo): string;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Add('System:');
    SL.Add('  OS:        ' + AInfo.OSName);
    SL.Add('  CPU:       ' + AInfo.CPUModel);
    SL.Add('  Cores:     ' + IntToStr(AInfo.CPUCores));
    SL.Add('  RAM:       ' + FormatFloat('#,##0', AInfo.RAMTotalMB) + ' MB');

    if AInfo.DiskPath <> '' then
    begin
      SL.Add('  Disk (' + ExtractFileName(AInfo.DiskPath) + '):');
      SL.Add('    Free:    ' + FormatFloat('#,##0', AInfo.DiskFreeMB) + ' MB');
      SL.Add('    Total:   ' + FormatFloat('#,##0', AInfo.DiskTotalMB) + ' MB');
    end;

    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

end.
