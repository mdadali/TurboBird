unit dbInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, LCLType, ComCtrls,
  turbocommon,
  fbcommon;

type

  { TfmDBInfo }

  TfmDBInfo = class(TForm)
    bbClose: TSpeedButton;
    bbRefresh: TBitBtn;
    edCharset: TEdit;
    edConnections: TEdit;
    edCreationDate: TEdit;
    edDBSize: TEdit;
    edName: TEdit;
    edODSVer: TEdit;
    edPageSize: TEdit;
    edServerString: TEdit;
    edServerTime: TEdit;
    GroupBox1: TGroupBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    meClients: TMemo;
    Panel1: TPanel;
    Panel13: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel9: TPanel;
    Splitter1: TSplitter;
    TabSheet1: TTabSheet;
    procedure bbClose1Click(Sender: TObject);
    procedure bbCloseClick(Sender: TObject);
    procedure bbRefreshClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
  private
    { private declarations }
    FDBIndex: Integer;
    FNodeInfos: TPNodeInfos;
  public
    procedure Init(dbIndex: Integer; ANodeInfos: TPNodeInfos);
    { public declarations }
  end; 

//var
  //fmDBInfo: TfmDBInfo;

implementation

{$R *.lfm}

{ TfmDBInfo }

uses SysTables;


procedure TfmDBInfo.bbRefreshClick(Sender: TObject);
begin
  Init(FDBIndex, FNodeInfos);
end;

procedure TfmDBInfo.bbClose1Click(Sender: TObject);
begin
  TTabSheet(Parent).Free;
  Close;
end;

procedure TfmDBInfo.bbCloseClick(Sender: TObject);
begin
  Close;
  Parent.Free;
end;

procedure TfmDBInfo.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(FNodeInfos) then
    FNodeInfos^.ViewForm := nil;
  CloseAction:= caFree;
end;

procedure TfmDBInfo.FormCreate(Sender: TObject);
begin

end;

procedure TfmDBInfo.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) and
    ((key=VK_F4) or (key=VK_W)) then
  begin
    // Close when pressing Ctrl-W or Ctrl-F4 (Cmd-W/Cmd-F4 on OSX)
    Close;
    Parent.Free;
  end;
end;

procedure TfmDBInfo.FormResize(Sender: TObject);
begin
  Image1.Top := 40;
  Image1.Left := self.Width - Image1.Width - 10;
end;

procedure TfmDBInfo.Init(dbIndex: Integer; ANodeInfos: TPNodeInfos);
var
  dbName, CreationDate, ACharSet: string;
  MajorVer, MinorVer, Pages, PageSize: Integer;
  ProcessList: TStringList;
  dbSize: Double;
  AType: string;
  ServerTime: string;
  ErrorMsg: string;
begin
  FNodeInfos := ANodeInfos;
  FDBIndex:= dbIndex;
  ProcessList:= TStringList.Create;
  try
    // Read database info
    if dmSysTables.GetDatabaseInfo(dbIndex, dbName, ACharSet, CreationDate, ServerTime,
      MajorVer, MinorVer, Pages, PageSize, ProcessList, ErrorMsg) then
    begin
      //lbServerVersion.Caption := IntToStr(FBVersionMajor) + '.' + IntToStr(FBVersionMinor);
      edServerString.Text :=  FBVersionString;
      edName.Text:= dbName;
      edODSVer.Text:= IntToStr(MajorVer) + '.' + IntToStr(MinorVer);
      edCharset.Text:= ACharSet;
      edCreationDate.Text:= CreationDate;
      edPageSize.Text:= IntToStr(PageSize);
      edConnections.Text:= IntToStr(ProcessList.Count);
      dbSize:= Double(Pages) * Double(PageSize);     

      // Display database size in readable format
      if dbSize > (1024*1024*1024) then
      begin
        dbSize:= ((dbSize / 1024) / 1024) / 1024;
        AType:= 'Gigabytes';
      end
      else
      if dbSize > (1024*1024) then
      begin
        dbSize:= ((dbSize / 1024) / 1024);
        AType:= 'Megabytes';
      end
      else
      if dbSize > 1024 then
      begin
        dbSize:= (dbSize / 1024);
        AType:= 'Kilobytes';
      end
      else
      begin
        AType:= 'Bytes';
      end;

      edDBSize.Text:= Format('%3.1n %s', [dbSize, AType]);
      //fmDBInfo.edServerTime.Text:= ServerTime;
      self.edServerTime.Text:= ServerTime;
      meClients.Lines.Text:= ProcessList.Text;
      meClients.Lines.Insert(0, '');
      Show;
    end
    else
      ShowMessage('Unable to get database information' + LineEnding +
        ErrorMsg);
  finally
    ProcessList.Free;
  end;

end;



end.

