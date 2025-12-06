(*
 * DBLoginDlgUnit.pas
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
unit DBLoginDlgUnit;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF WINDOWS }
  Windows,
{$ELSE}
  unix,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Dialogs,
  Forms, StdCtrls, ExtCtrls, Buttons, IB, IBDialogs;

type
  { TDBLoginDlg }

  TDBLoginDlg = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    CreateIfNotExist: TCheckBox;
    Label1: TLabel;
    TargetCaption: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Password: TEdit;
    UserName: TEdit;
    DatabaseName: TEdit;
  private
    { private declarations }
  public
    { public declarations }
    function ShowModal(var aDatabaseName, aUserName, aPassword: string;
      var aCreateIfNotExist: boolean): TModalResult;
  end;

var DBLoginDlg: TDBLoginDlg;

implementation

{$R *.lfm}

{ TDBLoginDlg }

function TDBLoginDlg.ShowModal(var aDatabaseName, aUserName, aPassword: string;
  var aCreateIfNotExist: boolean): TModalResult;
begin
  DatabaseName.Text := aDatabaseName;
  UserName.Text := aUserName;
  Password.Text := '';
  CreateIfNotExist.Checked := false;
  Result := inherited ShowModal;
  if Result = mrOK then
  begin
    aDatabaseName := DatabaseName.Text;
    aUserName := UserName.Text;
    aPassword := Password.Text;
    aCreateIfNotExist := CreateIfNotExist.Checked;
  end;
end;


end.
