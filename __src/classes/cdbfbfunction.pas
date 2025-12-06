unit cDBFBFunction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, IBConnection, cDBRoutine, udb_firebird_struct_helper;

type
  TDBFBFunction = class(TDBRoutine)
  protected
    function GetFirebirdFunctionHeader: string;
    function GetFirebirdFunctionBody: string;
    function GetFirebirdFunctionDeclaration: string;
  public
    constructor Create(AConn: TSQLConnection; const AName: string); override;

    function LoadFromDatabase: Boolean; override;
    function SaveToDatabase: Boolean; override;
    function DropFromDatabase: Boolean; override;

    property Header: string read GetFirebirdFunctionHeader;
    property Body: string read GetFirebirdFunctionBody;
    property Declaration: string read GetFirebirdFunctionDeclaration;
  end;

implementation

{ TDBFBFunction }

constructor TDBFBFunction.Create(AConn: TSQLConnection; const AName: string);
begin
  inherited Create(AConn, AName, otFBFunctions);
end;

function TDBFBFunction.GetFirebirdFunctionHeader: string;
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.DataBase := FConnection;
    Q.Transaction := FConnection.Transaction;

    // Lade den reinen "CREATE OR ALTER FUNCTION" Header inkl. Parameter und RETURNS
    Q.SQL.Text :=
      'SELECT FIRST 1 RDB$FUNCTION_NAME FROM RDB$FUNCTIONS WHERE UPPER(RDB$FUNCTION_NAME) = :FUNC';
    Q.Params.ParamByName('FUNC').AsString := UpperCase(FName);
    Q.Open;
    if Q.EOF then
      Exit('');

    // Original Header aus Funktion (ist nicht als Spalte direkt gespeichert, sondern muss zusammengebaut werden)
    // Hier nehme ich an, wir speichern Header in FHeader beim LoadFromDatabase, oder lade ihn zB über
    // eine Abfrage auf RDB$FUNCTION_ARGUMENTS plus RDB$FUNCTIONS-RetType.

    // Damit keine Typumwandlungen hier — stattdessen nur Originaltext
    // Hole original Header aus RDB$FUNCTIONS-Tabellenfeldern + RDB$FUNCTION_ARGUMENTS

    // Oder: Lade direkt Header aus FCurrentSQL (siehe LoadFromDatabase).
    // Da wir nicht direkt in DB so einen Text haben, hole ich ihn beim LoadFromDatabase.

    Result := FHeader; // Wird beim LoadFromDatabase gesetzt
  finally
    Q.Free;
  end;
end;

function TDBFBFunction.GetFirebirdFunctionBody: string;
var
  Q: TSQLQuery;
  Src: string;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.DataBase := FConnection;
    Q.Transaction := FConnection.Transaction;

    Q.SQL.Text :=
      'SELECT RDB$FUNCTION_SOURCE FROM RDB$FUNCTIONS ' +
      'WHERE UPPER(RDB$FUNCTION_NAME) = :FUNC';
    Q.Params.ParamByName('FUNC').AsString := UpperCase(FName);
    Q.Open;

    if Q.EOF then
      Result := ''
    else
    begin
      Src := Q.FieldByName('RDB$FUNCTION_SOURCE').AsString;
      Result := CleanupBodyText(Src);
    end;
  finally
    Q.Free;
  end;
end;

function TDBFBFunction.GetFirebirdFunctionDeclaration: string;
var
  Decl: string;
  BodyText: string;
begin
  // Verwende Header + Body unverändert, füge AS BEGIN ... END hinzu

  Decl := Header;

  BodyText := Body;
  if BodyText = '' then
    BodyText := 'BEGIN' + LineEnding + '  -- no body' + LineEnding + 'END'
  else
  begin
    // Prüfe, ob Body schon mit BEGIN/END umgeben ist
    if (Pos('BEGIN', UpperCase(BodyText)) = 0) or (Pos('END', UpperCase(BodyText)) = 0) then
      BodyText := 'BEGIN' + LineEnding + BodyText + LineEnding + 'END';
  end;

  Result :=
    'SET TERM ^;' + LineEnding + LineEnding +
    Format('-- DROP FUNCTION %s;%s', [FName, LineEnding]) +
    Decl + LineEnding +
    'AS' + LineEnding +
    BodyText + LineEnding +
    '^' + LineEnding +
    'SET TERM ;^';
end;

function TDBFBFunction.LoadFromDatabase: Boolean;
var
  Q: TSQLQuery;
  HeaderBuilder: TStringList;
  ArgName, ArgSource: string;
  ArgPos: Integer;
  ArgsList: TStringList;
  RetType: string;
  i: Integer;
begin
  Result := False;
  Q := TSQLQuery.Create(nil);
  HeaderBuilder := TStringList.Create;
  ArgsList := TStringList.Create;
  try
    Q.DataBase := FConnection;
    Q.Transaction := FConnection.Transaction;

    // Lade Argumente + Rückgabetyp um Header komplett original zu bauen
    Q.SQL.Text :=
      'SELECT RDB$ARGUMENT_NAME, RDB$FIELD_SOURCE, RDB$ARGUMENT_POSITION ' +
      'FROM RDB$FUNCTION_ARGUMENTS ' +
      'WHERE UPPER(RDB$FUNCTION_NAME) = :FUNC ' +
      'AND RDB$PACKAGE_NAME IS NULL ' +
      'ORDER BY RDB$ARGUMENT_POSITION NULLS FIRST';

    Q.Params.ParamByName('FUNC').AsString := UpperCase(FName);
    Q.Open;

    // Suche Rückgabetyp (Position = 0)
    RetType := 'UNKNOWN';
    while not Q.EOF do
    begin
      ArgName := Trim(Q.FieldByName('RDB$ARGUMENT_NAME').AsString);
      ArgSource := Trim(Q.FieldByName('RDB$FIELD_SOURCE').AsString);
      ArgPos := Q.FieldByName('RDB$ARGUMENT_POSITION').AsInteger;

      if ArgPos = 0 then
        RetType := FieldSourceToStr(ArgSource, IBConnection(FConnection))
      else
      begin
        if ArgName <> '' then
          ArgsList.Add(Format('%s %s', [ArgName, FieldSourceToStr(ArgSource, IBConnection(FConnection))]))
        else
          ArgsList.Add(FieldSourceToStr(ArgSource, IBConnection(FConnection)));
      end;
      Q.Next;
    end;

    // Header original zusammenbauen (wie aus DB)
    HeaderBuilder.Add(Format('CREATE OR ALTER FUNCTION %s(', [FName]));
    for i := 0 to ArgsList.Count - 1 do
    begin
      if i < ArgsList.Count - 1 then
        HeaderBuilder.Add('  ' + ArgsList[i] + ',')
      else
        HeaderBuilder.Add('  ' + ArgsList[i]);
    end;
    HeaderBuilder.Add(')');
    HeaderBuilder.Add('RETURNS ' + RetType);

    FHeader := HeaderBuilder.Text;

    FCurrentSQL := GetFirebirdFunctionDeclaration;
    FEditedSQL := FCurrentSQL;
    FIsModified := False;
    Result := True;
  finally
    Q.Free;
    HeaderBuilder.Free;
    ArgsList.Free;
  end;
end;

function TDBFBFunction.SaveToDatabase: Boolean;
var
  Q: TSQLQuery;
begin
  Result := False;
  try
    Q := TSQLQuery.Create(nil);
    try
      Q.DataBase := FConnection;
      Q.Transaction := FConnection.Transaction;
      Q.SQL.Text := FEditedSQL;
      Q.ExecSQL;
      Q.Transaction.Commit;
      FCurrentSQL := FEditedSQL;
      FIsModified := False;
      Result := True;
    finally
      Q.Free;
    end;
  except
    Result := False;
  end;
end;

function TDBFBFunction.DropFromDatabase: Boolean;
var
  Q: TSQLQuery;
  DropSQL: string;
begin
  Result := False;
  try
    DropSQL := Format('DROP FUNCTION %s', [FName]);

    Q := TSQLQuery.Create(nil);
    try
      Q.DataBase := FConnection;
      Q.Transaction := FConnection.Transaction;
      Q.SQL.Text := DropSQL;
      Q.ExecSQL;
      Q.Transaction.Commit;
      Result := True;
    finally
      Q.Free;
    end;
  except
    Result := False;
  end;
end;

end.

