{
Sergey Bodrov (serbod@gmail.com) 2016
}
unit MciPlay;

interface

uses Windows, Messages, MMSystem, Classes, SysUtils;

type
  TMciPlayer = class(TObject)
  private
    FMemory: Pointer;
    FFourCC: array[0..3] of AnsiChar;
    FFilename: string;
    FAlias: string;
    FLoop: Boolean;
    {$ifdef MSWINDOWS}
    {$ifndef FPC}
    FWindowHandle: HWND;
    procedure WndProc(var Msg: TMessage);
    procedure SendMCICommand(Cmd: string);
    {$endif}
    {$endif}
    procedure SetLoop(const Value: Boolean);
  public
    { AFileName - sound file name or pseudoname for sample in memory
      AMem - pointer to sample in memory (or nil)
      AMemSize - size of sampe in memory }
    constructor Create(const AFileName: string; AMem: Pointer = nil; AMemSize: Integer = 0);
    destructor Destroy(); override;

    procedure Play();
    procedure Stop();

    { repeat playback from beginning after reaching an end of file/sample }
    property Loop: Boolean read FLoop write SetLoop;
    property Alias: string read FAlias write FAlias;
    property Filename: string read FFilename;
  end;

implementation

type
  TMemFileInfo = record
    FourCC: array[0..3] of AnsiChar;
    Mem: Pointer;
    Size: Integer;
    Opened: Boolean;
  end;

var
  glMemFileInfoArray: array of TMemFileInfo;

function GetMemFileInfo(AFourCC: DWord): TMemFileInfo;
var
  i: Integer;
begin
  DWord(Result.FourCC) := AFourCC;
  Result.Mem := nil;
  Result.Size := 0;
  Result.Opened := False;
  for i := 0 to Length(glMemFileInfoArray)-1 do
  begin
    if DWord(glMemFileInfoArray[i].FourCC) = AFourCC then
    begin
      Result := glMemFileInfoArray[i];
      Break;
    end;
  end;
end;

procedure SetMemFileInfo(AMemFileInfo: TMemFileInfo);
var
  i: Integer;
begin
  for i := 0 to Length(glMemFileInfoArray)-1 do
  begin
    if DWord(glMemFileInfoArray[i].FourCC) = DWord(AMemFileInfo.FourCC) then
    begin
      glMemFileInfoArray[i] := AMemFileInfo;
      Exit;
    end;
  end;
  // not found, appending
  i := Length(glMemFileInfoArray);
  SetLength(glMemFileInfoArray, i+1);
  glMemFileInfoArray[i] := AMemFileInfo;
end;


{ Callback for reading sample from memory }
function MyIOCallback(Info: PChar; Msg: UINT; LParam1, LParam2: LPARAM): Longint stdcall;
var
  MMIOInfo: PMMIOInfo;
  MemFileInfo: TMemFileInfo;
begin
  MMIOInfo := PMMIOInfo(Info);
  MemFileInfo := GetMemFileInfo(MMIOInfo.fccIOProc);
  case Msg of
    MMIOM_OPEN:
      begin
        if MemFileInfo.Opened then
        begin
          Result := 0;
          Exit;
        end;
        MemFileInfo.Opened := true;
        MMIOInfo.lDiskOffset := 0;
        Result := 0;
      end;
    MMIOM_CLOSE:
      Result := 0;
    MMIOM_READ:
      begin
        Move(Pointer(Integer(MemFileInfo.Mem) + MMIOInfo.lDiskOffset)^, Pointer(LParam1)^, LParam2);
        MMIOInfo.lDiskOffset := MMIOInfo.lDiskOffset + LParam2;
        Result := LParam2;
      end;
    MMIOM_SEEK:
      begin
        case LParam2 of
          0: MMIOInfo.lDiskOffset := LParam1;
          1: MMIOInfo.lDiskOffset := MMIOInfo.lDiskOffset + LParam1;
          2: MMIOInfo.lDiskOffset := MemFileInfo.Size - 1 - LParam1;
        end;
        Result := MMIOInfo.lDiskOffset;
      end;
    else Result := -1;
  end;
  SetMemFileInfo(MemFileInfo);
end;

{ TMciPlayer }

constructor TMciPlayer.Create(const AFileName: string; AMem: Pointer; AMemSize: Integer);
var
  MemFileInfo: TMemFileInfo;
  s: AnsiString;
begin
  inherited Create;
  {$ifdef MSWINDOWS}
  {$ifndef FPC}
  // virtual window handle for MCI notifications
  FWindowHandle := Classes.AllocateHWnd(WndProc);
  {$endif}
  {$endif}

  FMemory := AMem;
  FFilename := AFileName;
  FFourCC := '0000';

  if Assigned(FMemory) then
  begin
    // mediafile from memory
    // set FourCC from filename
    s := UpperCase(Copy(AFileName, 1, 4));
    Move(PAnsiChar(s)^, FFourCC, Length(s));
    // alias name
    FAlias := Trim(LowerCase(Copy(AFileName, 1, 4)));

    MemFileInfo := GetMemFileInfo(DWord(FFourCC));
    MemFileInfo.Mem := FMemory;
    MemFileInfo.Size := AMemSize;
    MemFileInfo.Opened := False;
    SetMemFileInfo(MemFileInfo);

    // set reader callback
    mmioInstallIOProc(DWORD(FFourCC), @MyIOCallback, MMIO_INSTALLPROC);
    // open
    SendMCICommand('open ".' + FFourCC + '+" type waveaudio alias ' + FAlias);
  end
  else
  begin
    // alias name
    s := ExtractFileName(AFileName);
    FAlias := Trim(LowerCase(Copy(s, 1, 4)));
    // open mediafile from disk/resource
    SendMCICommand('open waveaudio!"' + FFilename + '" alias ' + FAlias);
  end;
end;

destructor TMciPlayer.Destroy;
begin
  SendMCICommand('stop ' + FAlias);
  SendMCICommand('close ' + FAlias);

  if Assigned(FMemory) then
  begin
    // remove reader callback
    mmioInstallIOProc(DWORD(FFourCC), @MyIOCallback, MMIO_REMOVEPROC);
    FMemory := nil;
  end;

  {$ifdef MSWINDOWS}
  {$ifndef FPC}
  Classes.DeallocateHWnd(FWindowHandle);
  {$endif}
  {$endif}
  inherited;
end;

procedure TMciPlayer.Play;
begin
  SendMCICommand('play ' + FAlias + ' from 0 notify');
end;

procedure TMciPlayer.SetLoop(const Value: Boolean);
begin
  FLoop := Value;
end;

procedure TMciPlayer.Stop;
begin
  SendMCICommand('stop ' + FAlias);
end;

{$ifdef MSWINDOWS}
{$ifndef FPC}
procedure TMciPlayer.SendMCICommand(Cmd: string);
var
  RetVal: Integer;
  ErrMsg: array[0..254] of char;
begin
  RetVal := mciSendString(PChar(Cmd), nil, 0, Self.FWindowHandle);
  if RetVal <> 0 then
  begin
    { get message for returned value }
    mciGetErrorString(RetVal, ErrMsg, 255);
    //MessageDlg(StrPas(ErrMsg), mtError, [mbOK], 0);
  end;
end;

procedure TMciPlayer.WndProc(var Msg: TMessage);
begin
  if Msg.Msg = MM_MCINOTIFY then
  begin
    case Msg.WParam of
      MCI_NOTIFY_SUCCESSFUL:
      begin
        if FLoop then
        begin
          SendMCICommand('play ' + FAlias + ' from 0 notify');
        end;
      end;
      MCI_NOTIFY_ABORTED,
      MCI_NOTIFY_FAILURE,
      MCI_NOTIFY_SUPERSEDED:
      begin
        // 
      end;
    end;
  end
  else
    Msg.Result := DefWindowProc(FWindowHandle, Msg.Msg, Msg.wParam, Msg.lParam);
end;
{$endif}
{$endif}

initialization

SetLength(glMemFileInfoArray, 0);

finalization

SetLength(glMemFileInfoArray, 0);

end.
