unit frmCopyReport;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Dialogs, Clipbrd,

    uthemeselector;

type
  { TfrmCopyReport }

  TfrmCopyReport = class(TForm)
    btnCopy: TButton;
    btnSave: TButton;
    btnClose: TButton;
    meReport: TMemo;
    pnlBottom: TPanel;
procedure btnCopyClick(Sender: TObject);
procedure btnSaveClick(Sender: TObject);
procedure btnCloseClick(Sender: TObject);
procedure FormShow(Sender: TObject);
private
    { private declarations }
public
    { public declarations }
procedure SetReportText(const AText: string);
end;

//var
    //frmCopyReport: TfrmCopyReport;

implementation

{$R *.lfm}

{ TfrmCopyReport }

procedure TfrmCopyReport.SetReportText(const AText: string);
begin
    meReport.Text := AText;
end;

procedure TfrmCopyReport.btnCopyClick(Sender: TObject);
begin
    Clipboard.AsText := meReport.Text;
    ShowMessage('Report copied to clipboard.');
end;

procedure TfrmCopyReport.btnSaveClick(Sender: TObject);
var
    SD: TSaveDialog;
begin
    SD := TSaveDialog.Create(nil);
    try
    SD.Title := 'Save Copy Report';
    SD.DefaultExt := 'txt';
    SD.Filter := 'Text Files|*.txt|All Files|*.*';
    SD.FileName := 'copy_report_' + FormatDateTime('yyyymmdd_hhnnss', Now) + '.txt';
    if SD.Execute then
    begin
        meReport.Lines.SaveToFile(SD.FileName);
        ShowMessage('Report saved to:' + sLineBreak + SD.FileName);
    end;
    finally
    SD.Free;
end;
end;

procedure TfrmCopyReport.btnCloseClick(Sender: TObject);
begin
    Close;
end;

procedure TfrmCopyReport.FormShow(Sender: TObject);
begin
    frmThemeSelector.btnApplyClick(Self);
end;

end.
