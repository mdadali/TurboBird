unit FSReaderBase;

(*
Filesytem data reader base classes and defines
for embedded filesystems, archives and packed data storages

Author: Sergey Bodrov, 2024 Minsk
License: MIT

*)

interface

uses
  SysUtils, Classes;

type
  { FS reader base class }

  TFSReader = class(TComponent)
  protected
    FFile: TStream;
    FOnLog: TGetStrProc;
    FOnPageReaded: TNotifyEvent;

  public
    IsDebugPages: Boolean;     // debug messages for pages
    FileName: string;

    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;

    procedure LogInfo(AStr: string); virtual;
    function OpenFile(AFileName: string; AStream: TStream = nil): Boolean; virtual;
    // get progress value 0..1000
    function GetProgress(): Integer; virtual;

    // messages from reader
    property OnLog: TGetStrProc read FOnLog write FOnLog;
    // after portion of data readed, for progress update
    property OnPageReaded: TNotifyEvent read FOnPageReaded write FOnPageReaded;
  end;

implementation

{ TFSReader }

procedure TFSReader.AfterConstruction;
begin
  inherited;
end;

procedure TFSReader.BeforeDestruction;
begin
  FreeAndNil(FFile);
  inherited;
end;

function TFSReader.GetProgress: Integer;
begin
  Result := Trunc(FFile.Position / (FFile.Size + 1) * 1000);
end;

procedure TFSReader.LogInfo(AStr: string);
begin
  if Assigned(OnLog) then OnLog(AStr);
end;

function TFSReader.OpenFile(AFileName: string; AStream: TStream): Boolean;
begin
  Result := False;
  FreeAndNil(FFile);
  if not FileExists(AFileName) and (not Assigned(AStream)) then Exit;
  if Assigned(AStream) then
    FFile := AStream
  else
    FFile := TFileStream.Create(AFileName, fmOpenRead + fmShareDenyNone);
  FileName := AFileName;
  Result := True;
end;

end.
