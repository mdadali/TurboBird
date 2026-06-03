(*
 *  IBX For Lazarus (Firebird Express)
 *
 *  The contents of this file are subject to the Initial Developer's
 *  Public License Version 1.0 (the "License"); you may not use this
 *  file except in compliance with the License. You may obtain a copy
 *  of the License here:
 *
 *    http://www.firebirdsql.org/index.php?op=doc&id=idpl
 *
 *  Software distributed under the License is distributed on an "AS
 *  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 *  implied. See the License for the specific language governing rights
 *  and limitations under the License.
 *
 *  The Initial Developer of the Original Code is Tony Whyman.
 *
 *  The Original Code is (C) 2015 Tony Whyman, MWA Software
 *  (http://www.mwasoftware.co.uk).
 *
 *  All Rights Reserved.
 *
 *  Contributor(s): ______________________________________.
 *
*) 
            
unit fSQLParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterSQL, Forms, Controls,
  Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TfmSQLParser }

  TfmSQLParser = class(TForm)
    btnApply: TButton;
    btnClose: TButton;
    HavingAllUnions: TCheckBox;
    Button1: TButton;
    GeneratedSQL: TSynEdit;
    SynSQLSyn1: TSynSQLSyn;
    WhereAllUnions: TCheckBox;
    WhereCondition: TEdit;
    HavingCondition: TEdit;
    HavingConditionType: TRadioGroup;
    OrderBy: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    OriginalSQL: TMemo;
    WhereConditionType: TRadioGroup;
    procedure btnApplyClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { private declarations }
  public
    { public declarations }
    FStrings: TStrings;
  end;

//var
  //fmSQLParser: TfmSQLParser;

implementation

{$R *.lfm}

uses IBSQLParser;

{ TfmSQLParser }

procedure TfmSQLParser.Button1Click(Sender: TObject);
var Parser: TSelectSQLParser;
begin
  Parser := TSelectSQLParser.Create(OriginalSQL.Lines);
  try
    if WhereCondition.Text <> '' then
      Parser.Add2WhereClause(WhereCondition.Text,WhereConditionType.ItemIndex <> 0,WhereAllUnions.Checked);
    if HavingCondition.Text <> '' then
      Parser.Add2HavingClause(HavingCondition.Text,HavingConditionType.ItemIndex <> 0,HavingAllUnions.Checked);
    if OrderBy.Text <> ''then
      Parser.OrderByClause := OrderBy.Text;
    GeneratedSQL.Lines.Text := Parser.SQLText
  finally
  end;
end;

procedure TfmSQLParser.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  FStrings := nil;
  CloseAction := caFree;
end;

procedure TfmSQLParser.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfmSQLParser.btnApplyClick(Sender: TObject);
begin
  FStrings.Assign(GeneratedSQL.Lines);
end;

end.

