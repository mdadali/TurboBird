unit usqlqueryext;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB;

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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetArrayInfo(const FieldName: string): string;
  end;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('SQLdb_Ext', [TSQLQueryExt]);
end;


{ TSQLQueryExt }

constructor TSQLQueryExt.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FArrayInfoList := TStringList.Create;
end;

destructor TSQLQueryExt.Destroy;
begin
  FArrayInfoList.Free;
  inherited Destroy;
end;

procedure TSQLQueryExt.InternalOpen;
begin
  FilterOutArrayFields;
  inherited InternalOpen;
end;

{procedure TSQLQueryExt.FilterOutArrayFields;
var
  CleanFieldList, ArrayFields: TStringList;
  FieldName, ArrayInfo: string;
  TempTableName: string;
  MetaQuery, DimQuery: TSQLQuery;
  DimStr: string;
begin
  if not SQL.Text.Trim.ToUpper.StartsWith('SELECT * FROM ') then
    Exit;

  TempTableName := Trim(Copy(SQL.Text, Length('SELECT * FROM ') + 1, MaxInt));

  ArrayFields := TStringList.Create;
  CleanFieldList := TStringList.Create;
  MetaQuery := TSQLQuery.Create(nil);
  DimQuery := TSQLQuery.Create(nil);
  try
    MetaQuery.DataBase := Self.DataBase;
    MetaQuery.Transaction := Self.Transaction;
    DimQuery.DataBase := Self.DataBase;
    DimQuery.Transaction := Self.Transaction;

    FArrayInfoList.Clear;

    // 1. Array-Felder erkennen
    MetaQuery.SQL.Text :=
      'SELECT rf.RDB$FIELD_NAME, f.RDB$FIELD_NAME AS TYPENAME ' +
      'FROM RDB$RELATION_FIELDS rf ' +
      'JOIN RDB$FIELDS f ON rf.RDB$FIELD_SOURCE = f.RDB$FIELD_NAME ' +
      'WHERE rf.RDB$RELATION_NAME = :TBL ' +
      'AND f.RDB$DIMENSIONS IS NOT NULL';
    MetaQuery.Params.ParamByName('TBL').AsString := UpperCase(TempTableName);
    MetaQuery.Open;
    while not MetaQuery.EOF do
    begin
      FieldName := Trim(MetaQuery.FieldByName('RDB$FIELD_NAME').AsString);
      ArrayFields.Add(FieldName);

      // Dimensionen abfragen
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

      ArrayInfo := Format('%s Array [%s]', [FieldName, DimStr]);
      //FArrayInfoList.Values['ARRAY_' + FieldName] := ArrayInfo;
      FArrayInfoList.Values[FieldName] := ArrayInfo;

      MetaQuery.Next;
    end;
    MetaQuery.Close;

    // 2. Normale Felder ohne Arrays holen
    MetaQuery.SQL.Text :=
      'SELECT rf.RDB$FIELD_NAME FROM RDB$RELATION_FIELDS rf ' +
      'WHERE rf.RDB$RELATION_NAME = :TBL';
    MetaQuery.Params.ParamByName('TBL').AsString := UpperCase(TempTableName);
    MetaQuery.Open;
    while not MetaQuery.EOF do
    begin
      FieldName := Trim(MetaQuery.Fields[0].AsString);
      if ArrayFields.IndexOf(FieldName) < 0 then
        CleanFieldList.Add(FieldName);
      MetaQuery.Next;
    end;
    MetaQuery.Close;

    // 3. Neue SQL
    SQL.Text := 'SELECT ' + CleanFieldList.CommaText + ' FROM ' + TempTableName;

    // 4. Platzhalter-Felder erzeugen
    for FieldName in ArrayFields do
      //AddVirtualArrayField('ARRAY_' + FieldName);
      AddVirtualArrayField(FieldName);

  finally
    ArrayFields.Free;
    CleanFieldList.Free;
    MetaQuery.Free;
    DimQuery.Free;
  end;
end;
}

procedure TSQLQueryExt.FilterOutArrayFields;
var
  CleanFieldList, ArrayFields: TStringList;
  FieldName: string[63];
  DimStr: string[63];
  TempRelationName: string;
  MetaQuery, DimQuery: TSQLQuery;
  FieldType, FieldSubType: Integer;
  TZFields: TStringList;
begin
  //if not SQL.Text.Trim.ToUpper.StartsWith('SELECT * FROM ') then
    //Exit;

  TempRelationName := Trim(Copy(SQL.Text, Length('SELECT * FROM ') + 1, MaxInt));

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

    // 1. TZ- und Array-Felder analysieren
    MetaQuery.SQL.Text :=
      'SELECT rf.RDB$FIELD_NAME, f.RDB$FIELD_NAME AS TYPENAME, ' +
      'f.RDB$FIELD_TYPE, f.RDB$FIELD_SUB_TYPE, f.RDB$DIMENSIONS ' +
      'FROM RDB$RELATION_FIELDS rf ' +
      'JOIN RDB$FIELDS f ON rf.RDB$FIELD_SOURCE = f.RDB$FIELD_NAME ' +
      'WHERE rf.RDB$RELATION_NAME = :TBL';
    MetaQuery.Params.ParamByName('TBL').AsString := UpperCase(TempRelationName);
    MetaQuery.Open;
    while not MetaQuery.EOF do
    begin
      FieldName := Trim(MetaQuery.FieldByName('RDB$FIELD_NAME').AsString);
      FieldType := MetaQuery.FieldByName('RDB$FIELD_TYPE').AsInteger;
      FieldSubType := MetaQuery.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger;

      // Timezone
      //if ((FieldType = 13) or (FieldType = 35)) and (FieldSubType = 3) then
      if (FieldType = 35) or (FieldType = 29) then
        TZFields.Add(FieldName)

      // Arrays
      else if not MetaQuery.FieldByName('RDB$DIMENSIONS').IsNull then
      begin
        ArrayFields.Add(FieldName);

        // Dimensionen abfragen
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

        FArrayInfoList.Values[FieldName] := Format('%s Array [%s]', [FieldName, DimStr]);
      end
      else
        CleanFieldList.Add(FieldName);

      MetaQuery.Next;
    end;
    MetaQuery.Close;

    // 2. CASTs für TimeZone einfügen
    for FieldName in TZFields do
      CleanFieldList.Add(Format('CAST(%s AS VARCHAR(255)) AS %s', [FieldName, FieldName]));

    // 3. Neue SQL setzen (ohne Anführungszeichen!)

    // Fallback-Spalte, falls nix übrig
    if CleanFieldList.Count = 0 then
      CleanFieldList.Add(' * ');
    SQL.Text := 'SELECT ' + String.Join(', ', CleanFieldList.ToStringArray) + ' FROM ' + TempRelationName;

    // (optional) Debug speichern
    //SQL.SaveToFile('sql.txt');

    // 4. Virtuelle Array-Felder einbauen
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

