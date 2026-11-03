unit uSystemTools;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, Process;

function IsDangerousCommand(const Cmd: string): Boolean;
function ExecuteShellCommand(const Cmd: string; const WorkingDir: string; out Output: string; out ErrorMsg: string): Integer;
function ReadFileContent(const Path: string; out Content: string; out ErrorMsg: string): Boolean;
function WriteFileContent(const Path: string; const Content: string; out ErrorMsg: string): Boolean;
function ListDirectory(const Path: string; out DirResult: string; out ErrorMsg: string): Boolean;
function SearchFiles(const Pattern: string; const Path: string; out SearchResult: string; out ErrorMsg: string): Boolean;

implementation

const
  DANGEROUS_PATTERNS: array[0..41] of string = (
    'mkfs', 'fdisk', 'parted', 'gdisk', 'cfdisk', 'sfdisk', 'wipefs',
    'dd if=/dev/zero', 'dd if=/dev/random', 'dd if=/dev/urandom',
    'shred /dev/', 'shred /', 'shred /boot', 'shred /etc', 'shred /usr',
    'rm -rf /', 'rm -rf /*', 'rm -rf /boot', 'rm -rf /etc', 'rm -rf /usr',
    'rm -rf /bin', 'rm -rf /sbin', 'rm -rf /lib', 'rm -rf /var', 'rm -rf /root',
    'rm -rf /home', 'rm -rf /proc', 'rm -rf /sys', 'rm -rf /dev',
    'shutdown', 'reboot', 'halt', 'poweroff', 'init 0', 'init 6',
    ':(){ :|:& };:', '> /dev/sd', '> /dev/hd', '> /dev/nvme', '> /dev/vd',
    'chmod -R 777 /', 'chown -R'
  );

function IsDangerousCommand(const Cmd: string): Boolean;
var
  Lower: string;
  i: Integer;
begin
  Result := False;
  Lower := LowerCase(Trim(Cmd));
  if Lower = '' then Exit;
  for i := 0 to High(DANGEROUS_PATTERNS) do
  begin
    if Pos(DANGEROUS_PATTERNS[i], Lower) > 0 then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function ExecuteShellCommand(const Cmd: string; const WorkingDir: string; out Output: string; out ErrorMsg: string): Integer;
var
  Process: TProcess;
  OutputStream: TStream;
  Buffer: array[0..4095] of AnsiChar;
  BytesRead: Integer;
  StartTime: QWord;
  TimeoutMs: Integer;
  Str: string;
begin
  Result := -1;
  Output := '';
  ErrorMsg := '';

  if Trim(Cmd) = '' then
  begin
    ErrorMsg := 'Empty command';
    Exit;
  end;

  if IsDangerousCommand(Cmd) then
  begin
    ErrorMsg := 'BLOCKED: This command is considered dangerous and has been blocked for safety. ' +
                'Blocked patterns include: disk formatting, system file deletion, shutdown/reboot, etc.';
    Exit;
  end;

  Process := TProcess.Create(nil);
  try
    Process.Executable := '/bin/sh';
    Process.Parameters.Add('-c');
    Process.Parameters.Add(Cmd);
    Process.Options := [poUsePipes, poStderrToOutPut];
    Process.ShowWindow := swoHIDE;

    if (WorkingDir <> '') and DirectoryExists(WorkingDir) then
      Process.CurrentDirectory := WorkingDir;

    StartTime := GetTickCount64;
    TimeoutMs := 120000; // 2 minutes

    try
      Process.Execute;
    except
      on E: Exception do
      begin
        ErrorMsg := 'Failed to execute command: ' + E.Message;
        Exit;
      end;
    end;

    OutputStream := Process.Output;
    while True do
    begin
      if GetTickCount64 - StartTime > TimeoutMs then
      begin
        Process.Terminate(9);
        ErrorMsg := 'TIMEOUT: Command exceeded ' + IntToStr(TimeoutMs div 1000) + ' seconds and was terminated.';
        Break;
      end;

      BytesRead := OutputStream.Read(Buffer, SizeOf(Buffer));
      if BytesRead > 0 then
      begin
        SetString(Str, PAnsiChar(@Buffer), BytesRead);
        Output := Output + Str;
      end
      else if not Process.Running then
        Break
      else
        Sleep(10);
    end;

    Result := Process.ExitStatus;

    if ErrorMsg = '' then
    begin
      if Output = '' then
        Output := '(no output)';
    end;
  finally
    Process.Free;
  end;
end;

function ReadFileContent(const Path: string; out Content: string; out ErrorMsg: string): Boolean;
var
  Stream: TFileStream;
  Size: Int64;
  Buffer: array of Byte;
begin
  Content := '';
  ErrorMsg := '';
  if not FileExists(Path) then
  begin
    ErrorMsg := 'File not found: ' + Path;
    Exit(False);
  end;

  try
    Stream := TFileStream.Create(Path, fmOpenRead or fmShareDenyWrite);
    try
      Size := Stream.Size;
      if Size > 0 then
      begin
        SetLength(Buffer, Size);
        Stream.ReadBuffer(Buffer[0], Size);
        SetString(Content, PAnsiChar(@Buffer[0]), Size);
      end;
    finally
      Stream.Free;
    end;
    Result := True;
  except
    on E: Exception do
    begin
      ErrorMsg := 'Error reading file: ' + E.Message;
      Result := False;
    end;
  end;
end;

function WriteFileContent(const Path: string; const Content: string; out ErrorMsg: string): Boolean;
var
  Stream: TFileStream;
  Dir: string;
begin
  ErrorMsg := '';
  Dir := ExtractFileDir(Path);
  if (Dir <> '') and not DirectoryExists(Dir) then
  begin
    try
      ForceDirectories(Dir);
    except
      on E: Exception do
      begin
        ErrorMsg := 'Cannot create directory: ' + E.Message;
        Exit(False);
      end;
    end;
  end;

  try
    Stream := TFileStream.Create(Path, fmCreate);
    try
      if Length(Content) > 0 then
        Stream.WriteBuffer(Content[1], Length(Content));
    finally
      Stream.Free;
    end;
    Result := True;
  except
    on E: Exception do
    begin
      ErrorMsg := 'Error writing file: ' + E.Message;
      Result := False;
    end;
  end;
end;

function ListDirectory(const Path: string; out DirResult: string; out ErrorMsg: string): Boolean;
var
  SearchRec: TSearchRec;
  FullPath: string;
  Count: Integer;
  IsDir: Boolean;
begin
  DirResult := '';
  ErrorMsg := '';
  FullPath := Path;
  if FullPath = '' then FullPath := '.';
  if not DirectoryExists(FullPath) then
  begin
    ErrorMsg := 'Directory not found: ' + FullPath;
    Exit(False);
  end;

  Count := 0;
  if FindFirst(IncludeTrailingPathDelimiter(FullPath) + '*', faAnyFile, SearchRec) = 0 then
  begin
    try
      repeat
        if (SearchRec.Name = '.') or (SearchRec.Name = '..') then Continue;
        IsDir := (SearchRec.Attr and faDirectory) <> 0;
        if IsDir then
          DirResult := DirResult + Format('[DIR]  %s', [SearchRec.Name]) + LineEnding
        else
          DirResult := DirResult + Format('[FILE] %s (%d bytes)', [SearchRec.Name, SearchRec.Size]) + LineEnding;
        Inc(Count);
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;

  if DirResult = '' then
    DirResult := '(empty directory)';
  DirResult := 'Directory: ' + FullPath + LineEnding +
            'Entries: ' + IntToStr(Count) + LineEnding + DirResult;
  Result := True;
  Exit;
end;

procedure SearchFilesRecursive(const Pattern: string; const DirPath: string; var SearchRes: string; var Count: Integer; MaxDepth: Integer);
var
  SearchRec: TSearchRec;
  FullPath: string;
  LowerPattern, LowerName: string;
begin
  if MaxDepth <= 0 then Exit;
  LowerPattern := LowerCase(Pattern);

  if FindFirst(IncludeTrailingPathDelimiter(DirPath) + '*', faAnyFile, SearchRec) = 0 then
  begin
    try
      repeat
        if (SearchRec.Name = '.') or (SearchRec.Name = '..') then Continue;
        FullPath := IncludeTrailingPathDelimiter(DirPath) + SearchRec.Name;
        LowerName := LowerCase(SearchRec.Name);
        if Pos(LowerPattern, LowerName) > 0 then
        begin
          if (SearchRec.Attr and faDirectory) <> 0 then
            SearchRes := SearchRes + '[DIR]  ' + FullPath + LineEnding
          else
            SearchRes := SearchRes + '[FILE] ' + FullPath + ' (' + IntToStr(SearchRec.Size) + ' bytes)' + LineEnding;
          Inc(Count);
          if Count >= 500 then Exit;
        end;
        if ((SearchRec.Attr and faDirectory) <> 0) then
          SearchFilesRecursive(Pattern, FullPath, SearchRes, Count, MaxDepth - 1);
      until (FindNext(SearchRec) <> 0) or (Count >= 500);
    finally
      FindClose(SearchRec);
    end;
  end;
end;

function SearchFiles(const Pattern: string; const Path: string; out SearchResult: string; out ErrorMsg: string): Boolean;
var
  DirPath: string;
  Count: Integer;
begin
  SearchResult := '';
  ErrorMsg := '';
  DirPath := Path;
  if DirPath = '' then DirPath := '.';
  if not DirectoryExists(DirPath) then
  begin
    ErrorMsg := 'Directory not found: ' + DirPath;
    Exit(False);
  end;

  Count := 0;
  SearchFilesRecursive(Pattern, DirPath, SearchResult, Count, 5);

  if SearchResult = '' then
    SearchResult := 'No files matching "' + Pattern + '" found in ' + DirPath
  else
    SearchResult := 'Search results for "' + Pattern + '" in ' + DirPath + ' (' + IntToStr(Count) + ' matches):' + LineEnding + SearchResult;
  Result := True;
  Exit;
end;

end.