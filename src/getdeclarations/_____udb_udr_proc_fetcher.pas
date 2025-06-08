unit udb_udr_proc_fetcher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, SQLDB,
  udb_field_type_helper;

type
  TParamDirection = (pdIn, pdOut);

  PUDRParam = ^TUDRParam;
  TUDRParam = record
    Name: string;
    Direction: TParamDirection;
    DataType: string;
  end;

  TUDRParamArray = array of TUDRParam;

function GetUDRProcedureParams(Conn: TIBConnection; const ProcedureName: string): TUDRParamArray;
function GetUDRProcedureDeclaration(Conn: TIBConnection; const ProcedureName: string): string;

implementation

uses DB;

function GetUDRProcedureParams(Conn: TIBConnection; const ProcedureName: string): TUDRParamArray;
var
  Q: TSQLQuery;
  Param: PUDRParam;
  ParamType: Integer;
  TmpList: TList;
  i: Integer;
begin
  TmpList := TList.Create;
  try
    Q := TSQLQuery.Create(nil);
    try
      Q.DataBase := Conn;
      Q.SQL.Text :=
        'SELECT RDB$PARAMETER_NAME, RDB$FIELD_SOURCE, RDB$PARAMETER_TYPE ' +
        'FROM RDB$PROCEDURE_PARAMETERS ' +
        'WHERE RDB$PROCEDURE_NAME = :PROC_NAME ' +
        'ORDER BY RDB$PARAMETER_TYPE, RDB$PARAMETER_NUMBER';
      Q.Params.ParamByName('PROC_NAME').AsString := UpperCase(ProcedureName);
      Q.Open;

      while not Q.EOF do
      begin
        New(Param);
        Param^.Name := Trim(Q.FieldByName('RDB$PARAMETER_NAME').AsString);
        ParamType := Q.FieldByName('RDB$PARAMETER_TYPE').AsInteger;
        if ParamType = 0 then
          Param^.Direction := pdIn
        else
          Param^.Direction := pdOut;
        Param^.DataType := FieldSourceToStr(Trim(Q.FieldByName('RDB$FIELD_SOURCE').AsString), Conn);
        TmpList.Add(Param);
        Q.Next;
      end;

      SetLength(Result, TmpList.Count);
      for i := 0 to TmpList.Count - 1 do
        Result[i] := PUDRParam(TmpList[i])^;

    finally
      Q.Free;
    end;
  finally
    for i := 0 to TmpList.Count - 1 do
      Dispose(PUDRParam(TmpList[i]));
    TmpList.Free;
  end;
end;

function GetUDRProcedureDeclaration(Conn: TIBConnection; const ProcedureName: string): string;
var
  Params: TUDRParamArray;
  InParams, OutParams: TStringList;
  i: Integer;
  Line, ExternalName: string;
  Q: TSQLQuery;
begin
  InParams := TStringList.Create;
  OutParams := TStringList.Create;
  Q := TSQLQuery.Create(nil);
  try
    // External name holen
    Q.DataBase := Conn;
    Q.SQL.Text := 'SELECT RDB$ENTRYPOINT FROM RDB$PROCEDURES WHERE RDB$PROCEDURE_NAME = :NAME';
    Q.Params.ParamByName('NAME').AsString := UpperCase(ProcedureName);
    Q.Open;
    ExternalName := Trim(Q.FieldByName('RDB$ENTRYPOINT').AsString);

    Params := GetUDRProcedureParams(Conn, ProcedureName);

    for i := 0 to High(Params) do
    begin
      Line := Format('%s %s', [Params[i].Name, Params[i].DataType]);
      if Params[i].Direction = pdIn then
        InParams.Add(Line)
      else
        OutParams.Add(Line);
    end;

    Result := Format('-- DROP PROCEDURE %s;%s', [ProcedureName, LineEnding]) +
              Format('CREATE OR ALTER PROCEDURE %s%s', [ProcedureName, LineEnding]);

    if InParams.Count > 0 then
      Result := Result + '(' + LineEnding + '  ' +
                StringReplace(TrimRight(InParams.Text), LineEnding, ',' + LineEnding + '  ', [rfReplaceAll]) +
                LineEnding + ')' + LineEnding;

    if OutParams.Count > 0 then
      Result := Result + 'RETURNS (' + LineEnding + '  ' +
                StringReplace(TrimRight(OutParams.Text), LineEnding, ',' + LineEnding + '  ', [rfReplaceAll]) +
                LineEnding + ')' + LineEnding;

    Result := Result +
              Format('EXTERNAL NAME ''%s''%s', [ExternalName, LineEnding]) +
              'ENGINE UDR;' + LineEnding;

  finally
    InParams.Free;
    OutParams.Free;
    Q.Free;
  end;
end;

end.

