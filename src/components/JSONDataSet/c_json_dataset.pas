unit c_json_dataset;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, BufDataset, DB;

type
  TJSONDataSet = class(TBufDataset)
  private
    function JSONTypeToFieldType(AType: TJSONType): TFieldType;
  public
    procedure LoadFromJSON(const JSONText: string);
    procedure LoadFromFile(const AFileName: string);
    function SaveToJSON: string;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Data Access', [TJSONDataSet]);
end;

{ TJSONDataSet }

function TJSONDataSet.JSONTypeToFieldType(AType: TJSONType): TFieldType;
begin
  case AType of
    jtNumber: Result := ftFloat;
    jtBoolean: Result := ftBoolean;
    jtString: Result := ftString;
  else
    Result := ftString;
  end;
end;

procedure TJSONDataSet.LoadFromJSON(const JSONText: string);
var
  Data: TJSONData;
  Arr: TJSONArray;
  Obj: TJSONObject;
  i, j: Integer;
  FieldName: String;
  FieldType: TFieldType;
begin
  Data := GetJSON(JSONText);
  try
    if Data.JSONType <> jtArray then
      raise Exception.Create('JSON muss ein Array sein');

    Arr := TJSONArray(Data);
    if Arr.Count = 0 then Exit;

    Close;
    FieldDefs.Clear;

    Obj := Arr.Objects[0];
    for j := 0 to Obj.Count - 1 do
    begin
      FieldName := Obj.Names[j];
      FieldType := JSONTypeToFieldType(Obj.Items[j].JSONType);

      // ✅ Size nur bei Strings angeben
      if FieldType = ftString then
        FieldDefs.Add(FieldName, FieldType, 255)
      else
        FieldDefs.Add(FieldName, FieldType);
    end;

    CreateDataset;

    // Daten laden
    for i := 0 to Arr.Count - 1 do
    begin
      Obj := Arr.Objects[i];
      Append;

      for j := 0 to Obj.Count - 1 do
      begin
        FieldName := Obj.Names[j];
        case Obj.Items[j].JSONType of
          jtNumber:
            FieldByName(FieldName).AsFloat := Obj.Items[j].AsFloat;
          jtBoolean:
            FieldByName(FieldName).AsBoolean := Obj.Items[j].AsBoolean;
        else
          FieldByName(FieldName).AsString := Obj.Items[j].AsString;
        end;
      end;

      Post;
    end;

  finally
    Data.Free;
  end;
end;

procedure TJSONDataSet.LoadFromFile(const AFileName: string);
var
  FS: TFileStream;
  Bytes: TBytes;
  JSONText: string;
begin
  FS := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Bytes, FS.Size);
    FS.ReadBuffer(Bytes[0], FS.Size);
    JSONText := TEncoding.UTF8.GetString(Bytes); // oder TEncoding.Default
    LoadFromJSON(JSONText);
  finally
    FS.Free;
  end;
end;

function TJSONDataSet.SaveToJSON: string;
var
  Arr: TJSONArray;
  Obj: TJSONObject;
  i, j: Integer;
begin
  Arr := TJSONArray.Create;
  try
    First;
    while not EOF do
    begin
      Obj := TJSONObject.Create;
      for j := 0 to FieldCount - 1 do
      begin
        case Fields[j].DataType of
          ftFloat, ftInteger:
            Obj.Add(Fields[j].FieldName, Fields[j].AsFloat);
          ftBoolean:
            Obj.Add(Fields[j].FieldName, Fields[j].AsBoolean);
        else
          Obj.Add(Fields[j].FieldName, Fields[j].AsString);
        end;
      end;
      Arr.Add(Obj);
      Next;
    end;
    Result := Arr.AsJSON;
  finally
    Arr.Free;
  end;
end;

end.
