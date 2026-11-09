(*
 * ShutdownRegDlgUnit.pas
 * Copyright (C) 2018 Tony Whyman <tony@mwasoftware.co.uk>
 *
 * DBAdmin is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * DBAdmin is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along
 * with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)
unit ShutdownRegDlgUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, IBXServices;

type

  { TShutdownReqDlg }

  TShutdownReqDlg = class(TForm)
    Bevel1: TBevel;
    CancelBtn: TButton;
    DatabaseName: TEdit;
    Delay: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    OKBtn: TButton;
    ShutdownOptions: TRadioGroup;
    procedure FormShow(Sender: TObject);
  private

  public
    function ShowModal(const aDatabaseName: string; var aShutDownmode: TDBShutdownMode;
      var aDelay: integer): TModalResult;
  end;

var
  ShutdownReqDlg: TShutdownReqDlg;

implementation

{$R *.lfm}

{ TShutdownReqDlg }

procedure TShutdownReqDlg.FormShow(Sender: TObject);
begin
  Delay.Text := '60';
end;

function TShutdownReqDlg.ShowModal(const aDatabaseName: string;
  var aShutDownmode: TDBShutdownMode; var aDelay: integer): TModalResult;
begin
  ShutdownOptions.ItemIndex := ord(aShutDownmode);
  DatabaseName.Text := aDatabaseName;
  Result := inherited ShowModal;
  if Result = mrOK then
  begin
    aDelay := StrToInt(Delay.Text);
    aShutDownmode := TDBShutdownMode(ShutdownOptions.ItemIndex);
  end;
end;

end.

