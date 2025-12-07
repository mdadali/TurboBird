unit usqlqueryext;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB,
  SynEditHighlighter, SynHighlighterSQL,

  Dialogs;

type
  { TSQLQueryExt }

  TSQLQueryExt = class(TSQLQuery)
  private
    FArrayInfoList: TStringList;
  protected
    procedure InternalOpen; override;
    procedure FilterOutArrayFields;
    procedure AddVirtualArrayField(const FieldName: string);
    procedure VirtualArrayGetText(Sender: TField; var Text: string; DisplayText: Boolean);
    function ExtractFirstTableNameWithSynSQLSyn(const SQLText: string): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetArrayInfo(const FieldName: string): string;
  end;



implementation
uses turbocommon;

{ TSQLQueryExt }

function TSQLQueryExt.ExtractFirstTableNameWithSynSQLSyn(const SQLText: string): string;
var
  SynSQL: TSynSQLSyn;
  Line: string;
  Token: string;
  TokenKind: integer;
  FoundFrom: Boolean;
begin
  Result := '';
  Line := SQLText;
  SynSQL := TSynSQLSyn.Create(nil);
  try
    SynSQL.SetLine(Line, 0);
    FoundFrom := False;

    while not SynSQL.GetEol do
    begin
      Token := SynSQL.GetToken;
      TokenKind := SynSQL.GetTokenKind;

      if FoundFrom then
      begin
        if TokenKind = Ord(tkIdentifier) then
        begin
          Result := Token;
          Exit;
        end
        else if TokenKind = Ord(tkSpace) then
          // weitersuchen
        else
          Break;
      end
      else if UpperCase(Token) = 'FROM' then
        FoundFrom := True;

      SynSQL.Next;
    end;
  finally
    SynSQL.Free;
  end;
end;


constructor TSQLQueryExt.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FArrayInfoList := TStringList.Create;
end;

procedure TSQLQueryExt.InternalOpen;
begin
  FilterOutArrayFields;
  inherited InternalOpen;
end;

destructor TSQLQueryExt.Destroy;
begin
  FArrayInfoList.Free;
  inherited Destroy;
end;

{procedure TSQLQueryExt.FilterOutArrayFields;
var
  CleanFieldList, ArrayFields, TZFields: TStringList;
  FieldName, TempRelationName: string[63];
  MetaQuery, DimQuery: TSQLQuery;
  DimStr, FieldTypeStr: string;
  FieldType, FieldSubType: Integer;
  FieldLength, Precision, Scale, CharLen: Integer;
  CharSetName: string;
begin
  if not SQL.Text.Trim.ToUpper.StartsWith('SELECT * FROM ') then Exit;

  //TempRelationName := Trim(Copy(SQL.Text, Length('SELECT * FROM ') + 1, MaxInt));
  TempRelationName := ExtractFirstTableNameWithSynSQLSyn(SQL.Text);
  //ShowMessage('SQL.Text = ' + SQL.Text);
  //ShowMessage('TempRelationName = ' + TempRelationName);

  ArrayFields := TStringList.Create;
  CleanFieldList := TStringList.Create;
  TZFields := TStringList.Create;
  MetaQuery := TSQLQuery.Create(nil);
  DimQuery := TSQLQuery.Create(nil);

  try
    MetaQuery.DataBase := Self.DataBase;
    MetaQuery.Transaction := Self.Transaction;
    DimQuery.DataBase := Self.DataBase;
    DimQuery.Transaction := Self.Transaction;

    FArrayInfoList.Clear;

    MetaQuery.SQL.Text :=
      'SELECT rf.RDB$FIELD_NAME, ' +
      '       f.RDB$FIELD_NAME AS TYPENAME, ' +
      '       f.RDB$FIELD_TYPE, f.RDB$FIELD_SUB_TYPE, f.RDB$FIELD_LENGTH, ' +
      '       f.RDB$FIELD_PRECISION, f.RDB$FIELD_SCALE, ' +
      '       f.RDB$CHARACTER_LENGTH, ' +
      '       cs.RDB$CHARACTER_SET_NAME AS RDB$CHARACTER_SET_NAME, ' +
      '       f.RDB$DIMENSIONS ' +
      'FROM RDB$RELATION_FIELDS rf ' +
      'JOIN RDB$FIELDS f ON rf.RDB$FIELD_SOURCE = f.RDB$FIELD_NAME ' +
      'LEFT JOIN RDB$CHARACTER_SETS cs ON f.RDB$CHARACTER_SET_ID = cs.RDB$CHARACTER_SET_ID ' +
      'WHERE rf.RDB$RELATION_NAME = :TBL';
    MetaQuery.Params.ParamByName('TBL').AsString := UpperCase(TempRelationName);
    MetaQuery.Open;

    while not MetaQuery.EOF do
    begin
      FieldName := Trim(MetaQuery.FieldByName('RDB$FIELD_NAME').AsString);
      FieldType := MetaQuery.FieldByName('RDB$FIELD_TYPE').AsInteger;
      FieldSubType := MetaQuery.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger;
      FieldLength := MetaQuery.FieldByName('RDB$FIELD_LENGTH').AsInteger;
      Precision := MetaQuery.FieldByName('RDB$FIELD_PRECISION').AsInteger;
      Scale := MetaQuery.FieldByName('RDB$FIELD_SCALE').AsInteger;
      CharLen := MetaQuery.FieldByName('RDB$CHARACTER_LENGTH').AsInteger;
      CharSetName := Trim(MetaQuery.FieldByName('RDB$CHARACTER_SET_NAME').AsString);

      // TimeZone-Felder (optional)
      if (FieldType = 35) or (FieldType = 29) then
        TZFields.Add(FieldName)

      // Array-Feld
      else if not MetaQuery.FieldByName('RDB$DIMENSIONS').IsNull then
      begin
        ArrayFields.Add(FieldName);

        // Dimensionen laden
        DimQuery.SQL.Text :=
          'SELECT RDB$DIMENSION, RDB$LOWER_BOUND, RDB$UPPER_BOUND ' +
          'FROM RDB$FIELD_DIMENSIONS WHERE RDB$FIELD_NAME = :FN ORDER BY RDB$DIMENSION';
        DimQuery.Params.ParamByName('FN').AsString :=
          Trim(MetaQuery.FieldByName('TYPENAME').AsString);
        DimQuery.Open;
        DimStr := '';
        while not DimQuery.EOF do
        begin
          if DimStr <> '' then DimStr += ' x ';
          DimStr += Format('%d..%d', [
            DimQuery.FieldByName('RDB$LOWER_BOUND').AsInteger,
            DimQuery.FieldByName('RDB$UPPER_BOUND').AsInteger
          ]);
          DimQuery.Next;
        end;
        DimQuery.Close;

        // Type-Name berechnen
        FieldTypeStr := GetFBTypeName(FieldType, FieldSubType, FieldLength, Precision, Scale, CharSetName, CharLen);

        // Info z.B. "FIELD1 [VARCHAR(50)] Array [1..5 x 1..3]"
        FArrayInfoList.Values[FieldName] :=
          Format('%s [%s] Array [%s]', [FieldName, FieldTypeStr, DimStr]);
      end
      else
        CleanFieldList.Add(FieldName);

      MetaQuery.Next;
    end;
    MetaQuery.Close;

    // TimeZone-Felder mit CAST behandeln
    for FieldName in TZFields do
      CleanFieldList.Add(Format('CAST(%s AS VARCHAR(255)) AS %s', [FieldName, FieldName]));


    // Fallback-Spalte, falls nix übrig
    if CleanFieldList.Count = 0 then
      CleanFieldList.Add(' * ');
    // Neue SQL setzen
    SQL.Text := 'SELECT ' + String.Join(', ', CleanFieldList.ToStringArray) + ' FROM ' + TempRelationName;

    // Platzhalter für Array-Felder
    for FieldName in ArrayFields do
      AddVirtualArrayField(FieldName);

  finally
    ArrayFields.Free;
    CleanFieldList.Free;
    TZFields.Free;
    MetaQuery.Free;
    DimQuery.Free;
  end;
end;
}

function TruncateFBIdentifier(const S: string): string;
var
  Bytes: TBytes;
begin
  // Firebird-Standard: max. 63 Bytes pro Identifier
  Bytes := TEncoding.UTF8.GetBytes(S);
  if Length(Bytes) > 63 then
    SetLength(Bytes, 63);
  Result := TEncoding.UTF8.GetString(Bytes);
end;

procedure TSQLQueryExt.FilterOutArrayFields;
var
  CleanFieldList, ArrayFields, TZFields: TStringList;
  FieldName, TempRelationName: string;
  MetaQuery, DimQuery: TSQLQuery;
  DimStr, FieldTypeStr: string;
  FieldType, FieldSubType: Integer;
  FieldLength, Precision, Scale, CharLen: Integer;
  CharSetName: string;
begin
  if not SQL.Text.Trim.ToUpper.StartsWith('SELECT * FROM ') then Exit;

  TempRelationName := TruncateFBIdentifier(
    ExtractFirstTableNameWithSynSQLSyn(SQL.Text)
  );

  ArrayFields := TStringList.Create;
  CleanFieldList := TStringList.Create;
  TZFields := TStringList.Create;
  MetaQuery := TSQLQuery.Create(nil);
  DimQuery := TSQLQuery.Create(nil);

  try
    MetaQuery.DataBase := Self.DataBase;
    MetaQuery.Transaction := Self.Transaction;
    DimQuery.DataBase := Self.DataBase;
    DimQuery.Transaction := Self.Transaction;

    FArrayInfoList.Clear;

    MetaQuery.SQL.Text :=
      'SELECT rf.RDB$FIELD_NAME, ' +
      '       f.RDB$FIELD_NAME AS TYPENAME, ' +
      '       f.RDB$FIELD_TYPE, f.RDB$FIELD_SUB_TYPE, f.RDB$FIELD_LENGTH, ' +
      '       f.RDB$FIELD_PRECISION, f.RDB$FIELD_SCALE, ' +
      '       f.RDB$CHARACTER_LENGTH, ' +
      '       cs.RDB$CHARACTER_SET_NAME AS RDB$CHARACTER_SET_NAME, ' +
      '       f.RDB$DIMENSIONS ' +
      'FROM RDB$RELATION_FIELDS rf ' +
      'JOIN RDB$FIELDS f ON rf.RDB$FIELD_SOURCE = f.RDB$FIELD_NAME ' +
      'LEFT JOIN RDB$CHARACTER_SETS cs ON f.RDB$CHARACTER_SET_ID = cs.RDB$CHARACTER_SET_ID ' +
      'WHERE rf.RDB$RELATION_NAME = :TBL';
    MetaQuery.Params.ParamByName('TBL').AsString := UpperCase(TempRelationName);
    MetaQuery.Open;

    while not MetaQuery.EOF do
    begin
      FieldName := TruncateFBIdentifier(
        Trim(MetaQuery.FieldByName('RDB$FIELD_NAME').AsString)
      );
      FieldType := MetaQuery.FieldByName('RDB$FIELD_TYPE').AsInteger;
      FieldSubType := MetaQuery.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger;
      FieldLength := MetaQuery.FieldByName('RDB$FIELD_LENGTH').AsInteger;
      Precision := MetaQuery.FieldByName('RDB$FIELD_PRECISION').AsInteger;
      Scale := MetaQuery.FieldByName('RDB$FIELD_SCALE').AsInteger;
      CharLen := MetaQuery.FieldByName('RDB$CHARACTER_LENGTH').AsInteger;
      CharSetName := TruncateFBIdentifier(
        Trim(MetaQuery.FieldByName('RDB$CHARACTER_SET_NAME').AsString)
      );

      // TimeZone-Felder
      if (FieldType = 35) or (FieldType = 29) then
        TZFields.Add(FieldName)

      // Array-Feld
      else if not MetaQuery.FieldByName('RDB$DIMENSIONS').IsNull then
      begin
        ArrayFields.Add(FieldName);

        DimQuery.SQL.Text :=
          'SELECT RDB$DIMENSION, RDB$LOWER_BOUND, RDB$UPPER_BOUND ' +
          'FROM RDB$FIELD_DIMENSIONS WHERE RDB$FIELD_NAME = :FN ORDER BY RDB$DIMENSION';
        DimQuery.Params.ParamByName('FN').AsString :=
          Trim(MetaQuery.FieldByName('TYPENAME').AsString);
        DimQuery.Open;
        DimStr := '';
        while not DimQuery.EOF do
        begin
          if DimStr <> '' then DimStr += ' x ';
          DimStr += Format('%d..%d', [
            DimQuery.FieldByName('RDB$LOWER_BOUND').AsInteger,
            DimQuery.FieldByName('RDB$UPPER_BOUND').AsInteger
          ]);
          DimQuery.Next;
        end;
        DimQuery.Close;

        FieldTypeStr := GetFBTypeName(FieldType, FieldSubType, FieldLength, Precision, Scale, CharSetName, CharLen);

        FArrayInfoList.Values[FieldName] :=
          Format('%s [%s] Array [%s]', [FieldName, FieldTypeStr, DimStr]);
      end
      else
        CleanFieldList.Add(FieldName);

      MetaQuery.Next;
    end;
    MetaQuery.Close;

    // TimeZone-Felder umwandeln
    for FieldName in TZFields do
      CleanFieldList.Add(Format('CAST(%s AS VARCHAR(255)) AS %s', [FieldName, FieldName]));

    if CleanFieldList.Count = 0 then
      CleanFieldList.Add(' * ');

    SQL.Text := 'SELECT ' + String.Join(', ', CleanFieldList.ToStringArray) +
                ' FROM ' + TempRelationName;

    for FieldName in ArrayFields do
      AddVirtualArrayField(FieldName);

  finally
    ArrayFields.Free;
    CleanFieldList.Free;
    TZFields.Free;
    MetaQuery.Free;
    DimQuery.Free;
  end;
end;

procedure TSQLQueryExt.AddVirtualArrayField(const FieldName: string);
var
  F: TStringField;
begin
  F := TStringField.Create(Self);
  F.FieldName := FieldName;
  F.FieldKind := fkCalculated;
  F.ReadOnly := True;
  F.Size := 255;
  F.DisplayLabel := FArrayInfoList.Values[FieldName];  // zeigt z.?B. "FELDNAME Array [1..3 x 1..2]"
  F.DataSet := Self;
  F.OnGetText := @VirtualArrayGetText;
end;

procedure TSQLQueryExt.VirtualArrayGetText(Sender: TField; var Text: string; DisplayText: Boolean);
begin
  Text := FArrayInfoList.Values[Sender.FieldName];
end;

function TSQLQueryExt.GetArrayInfo(const FieldName: string): string;
begin
  Result := FArrayInfoList.Values[FieldName];
end;

end.

