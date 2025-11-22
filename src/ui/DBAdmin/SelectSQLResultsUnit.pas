(*
 * SelectSQLResultsUnit.pas
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
            
unit SelectSQLResultsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, db,
  IBDynamicGrid, IBQuery, IBDatabase, IBXScript;

type

  { TSelectSQLResults }

  TSelectSQLResults = class(TForm)
    DataSource1: TDataSource;
    IBDynamicGrid1: TIBDynamicGrid;
    IBTransaction1: TIBTransaction;
    SelectQuery: TIBQuery;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure SelectQueryAfterOpen(DataSet: TDataSet);
  private
    { private declarations }
    FScriptEngine: TIBXScript;
    procedure Add2Log(const Msg: string; IsError: boolean);
  public
    { public declarations }
    procedure Show(ScriptEngine: TIBXScript; SelectSQLText: string; EnableStatistics: boolean);
  end;

var
  SelectSQLResults: TSelectSQLResults;

implementation

{$R *.lfm}

uses IBDataOutput;

{ TSelectSQLResults }

procedure TSelectSQLResults.FormShow(Sender: TObject);
begin
  SelectQuery.Active := true;
end;

type
  THackedIBXScript = class(TCustomIBXScript)
  end;

procedure TSelectSQLResults.SelectQueryAfterOpen(DataSet: TDataSet);
begin
  SelectQuery.Last;
  if SelectQuery.EnableStatistics then
    TIBCustomDataOutput.ShowPerfStats(SelectQuery.StmtHandle, @Add2Log );
end;

procedure TSelectSQLResults.Add2Log(const Msg: string; IsError: boolean);
begin
  THackedIBXScript(FScriptEngine).Add2Log(msg,IsError);
end;

procedure TSelectSQLResults.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  SelectQuery.Active := false;
  CloseAction := caFree;
end;

procedure TSelectSQLResults.Show(ScriptEngine: TIBXScript;
  SelectSQLText: string; EnableStatistics: boolean);
begin
  FScriptEngine := ScriptEngine;
  SelectQuery.SQL.Text := SelectSQLText;
  SelectQuery.EnableStatistics := EnableStatistics;
  inherited Show;
end;

end.

