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
    DiskType: string;
    OSName: string;
  end;

function GetSystemInfo(const APath: string = ''): TSystemInfo;
function FormatSystemInfo(const AInfo: TSystemInfo): string;

implementation

{$IFDEF WINDOWS}
uses
  Windows, Registry;
{$ENDIF}

{$IFDEF LINUX}
uses
  BaseUnix;
{$ENDIF}

// ---------------------------------------------------------------------------
// CPU-Modell
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
// CPU-Kerne (logisch)
// ---------------------------------------------------------------------------
function GetCPUCores: Integer;
{$IFDEF LINUX}
var
  SL: TStringList;
  i, Count: Integer;
begin
  Result := System.CpuCount;   // Fallback
  if not FileExists('/proc/cpuinfo') then Exit;

  SL := TStringList.Create;
  try
    SL.LoadFromFile('/proc/cpuinfo');
    Count := 0;
    for i := 0 to SL.Count - 1 do
      if (Pos('processor', SL[i]) = 1) and
         (Length(SL[i]) > 9) and
         (SL[i][10] in [#9, ' ', ':']) then
        Inc(Count);
    if Count > 0 then
      Result := Count;
  finally
    SL.Free;
  end;
end;
{$ENDIF}

{$IFDEF WINDOWS}
begin
  Result := System.CpuCount;
end;
{$ENDIF}

{$IFDEF DARWIN}
begin
  Result := System.CpuCount;
end;
{$ENDIF}

// ---------------------------------------------------------------------------
// RAM-Gesamt
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
          Result := MemKB div 1024;
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
// Festplatten-Typ (SSD/HDD)
// ---------------------------------------------------------------------------
function GetDiskType(const APath: string): string;
{$IFDEF LINUX}
var
  DeviceName, SysPath, Rotational: string;
  SL: TStringList;
begin
  Result := '';
  if APath = '' then Exit;

  // Bekannte Block-Devices durchsuchen
  if FileExists('/sys/block/nvme0n1/queue/rotational') then
    DeviceName := 'nvme0n1'
  else if FileExists('/sys/block/sda/queue/rotational') then
    DeviceName := 'sda'
  else if FileExists('/sys/block/vda/queue/rotational') then
    DeviceName := 'vda'
  else
    Exit;

  SysPath := '/sys/block/' + DeviceName + '/queue/rotational';
  SL := TStringList.Create;
  try
    SL.LoadFromFile(SysPath);
    Rotational := Trim(SL.Text);
    if Rotational = '0' then
    begin
      if Pos('nvme', DeviceName) = 1 then
        Result := 'NVMe SSD'
      else
        Result := 'SSD';
    end
    else if Rotational = '1' then
      Result := 'HDD (rotational)';
  finally
    SL.Free;
  end;
end;
{$ENDIF}

{$IFDEF WINDOWS}
begin
  Result := 'Unknown (Windows)';
end;
{$ENDIF}

{$IFDEF DARWIN}
begin
  Result := 'Unknown (macOS)';
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
  Result.CPUCores := GetCPUCores;
  Result.RAMTotalMB := GetRAMTotalMB;
  Result.OSName := GetOSName;

  Result.DiskPath := APath;
  Result.DiskFreeMB := 0;
  Result.DiskTotalMB := 0;
  Result.DiskType := '';

  if APath <> '' then
  begin
    PathToCheck := ExtractFileDir(APath);
    if PathToCheck = '' then
      PathToCheck := ExtractFilePath(APath);

    try
      Result.DiskFreeMB := DiskFree(AddDisk(PathToCheck)) div (1024 * 1024);
      Result.DiskTotalMB := DiskSize(AddDisk(PathToCheck)) div (1024 * 1024);
      Result.DiskType := GetDiskType(PathToCheck);
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
    SL.Add('Environment:');
    SL.Add('  OS:        ' + AInfo.OSName);
    SL.Add('  CPU:       ' + AInfo.CPUModel);
    SL.Add('  Cores:     ' + IntToStr(AInfo.CPUCores));
    SL.Add('  RAM:       ' + FormatFloat('#,##0', AInfo.RAMTotalMB) + ' MB');

    if AInfo.DiskPath <> '' then
    begin
      SL.Add('  Disk (' + ExtractFileName(AInfo.DiskPath) + '):');
      SL.Add('    Free:    ' + FormatFloat('#,##0', AInfo.DiskFreeMB) + ' MB');
      SL.Add('    Total:   ' + FormatFloat('#,##0', AInfo.DiskTotalMB) + ' MB');
      if AInfo.DiskType <> '' then
        SL.Add('    Type:    ' + AInfo.DiskType);
    end;

    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

end.
