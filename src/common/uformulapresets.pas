unit uFormulaPresets;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, Forms;

type
  TFieldInfoArray = array of record
    FieldName: string;
    FieldType: string;
    IsComputed: Boolean;
    Checked: Boolean;
    Formula: string;
  end;

  { TFormulaPreset }

  TFormulaPreset = class
  private
    FName: string;
    FDescription: string;
    FSeparator: string;
    FQuoteChar: string;
    FIncludeHeader: Boolean;
    FFormulas: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    function LoadFromFile(const AFileName: string): Boolean;
    function SaveToFile(const AFileName: string): Boolean;

    function GetFormulaForFieldType(const AFieldType: string): string;

    property Name: string read FName write FName;
    property Description: string read FDescription write FDescription;
    property Separator: string read FSeparator write FSeparator;
    property QuoteChar: string read FQuoteChar write FQuoteChar;
    property IncludeHeader: Boolean read FIncludeHeader write FIncludeHeader;
  end;

  { TFormulaPresetManager }

  TFormulaPresetManager = class
  private
    FPresets: TStringList;
    FPresetDir: string;
    function GetPresetDir: string;
    procedure CreateDefaultPresets;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadAllPresets;
    procedure Reload;
    function GetPreset(const AName: string): TFormulaPreset;
    function GetPresetByIndex(AIndex: Integer): TFormulaPreset;
    function PresetCount: Integer;
    function PresetName(AIndex: Integer): string;
  end;

var
  FormulaPresetManager: TFormulaPresetManager;

implementation

uses
  FileUtil;

{ TFormulaPreset }

constructor TFormulaPreset.Create;
begin
  inherited Create;
  FFormulas := TStringList.Create;
  FSeparator := ',';
  FQuoteChar := '"';
  FIncludeHeader := True;
end;

destructor TFormulaPreset.Destroy;
begin
  FFormulas.Free;
  inherited Destroy;
end;

function TFormulaPreset.LoadFromFile(const AFileName: string): Boolean;
var
  Ini: TIniFile;
  i: Integer;
  KeyName: string;
begin
  Result := False;
  if not FileExists(AFileName) then Exit;

  Ini := TIniFile.Create(AFileName);
  try
    FName := Ini.ReadString('Preset', 'Name', '');
    FDescription := Ini.ReadString('Preset', 'Description', '');
    FSeparator := Ini.ReadString('Preset', 'Separator', ',');
    FQuoteChar := Ini.ReadString('Preset', 'QuoteChar', '"');
    FIncludeHeader := Ini.ReadBool('Preset', 'IncludeHeader', True);

    FFormulas.Clear;
    Ini.ReadSectionValues('Formulas', FFormulas);

    Result := (FName <> '');
  finally
    Ini.Free;
  end;
end;

function TFormulaPreset.SaveToFile(const AFileName: string): Boolean;
var
  Ini: TIniFile;
  i: Integer;
  KeyName: string;
begin
  Result := False;
  try
    Ini := TIniFile.Create(AFileName);
    try
      Ini.WriteString('Preset', 'Name', FName);
      Ini.WriteString('Preset', 'Description', FDescription);
      Ini.WriteString('Preset', 'Separator', FSeparator);
      Ini.WriteString('Preset', 'QuoteChar', FQuoteChar);
      Ini.WriteBool('Preset', 'IncludeHeader', FIncludeHeader);

      for i := 0 to FFormulas.Count - 1 do
      begin
        KeyName := FFormulas.Names[i];
        if KeyName <> '' then
          Ini.WriteString('Formulas', KeyName, FFormulas.Values[KeyName]);
      end;

      Result := True;
    finally
      Ini.Free;
    end;
  except
  end;
end;

function TFormulaPreset.GetFormulaForFieldType(const AFieldType: string): string;
var
  CleanType: string;
  i: Integer;
  KeyName: string;
begin
  Result := '$1';  // Default: 1:1 Copy

  CleanType := UpperCase(Trim(AFieldType));

  // Numerische Typen
  if Pos('SMALLINT', CleanType) > 0 then Result := FFormulas.Values['INTEGER']
  else if Pos('INTEGER', CleanType) > 0 then Result := FFormulas.Values['INTEGER']
  else if Pos('BIGINT', CleanType) > 0 then Result := FFormulas.Values['INTEGER']
  else if Pos('FLOAT', CleanType) > 0 then Result := FFormulas.Values['FLOAT']
  else if Pos('DOUBLE', CleanType) > 0 then Result := FFormulas.Values['FLOAT']
  else if Pos('NUMERIC', CleanType) > 0 then Result := FFormulas.Values['FLOAT']
  else if Pos('DECIMAL', CleanType) > 0 then Result := FFormulas.Values['FLOAT']

  // String-Typen
  else if Pos('CHAR', CleanType) > 0 then Result := FFormulas.Values['STRING']
  else if Pos('VARCHAR', CleanType) > 0 then Result := FFormulas.Values['STRING']

  // Datum/Zeit
  else if Pos('DATE', CleanType) > 0 then Result := FFormulas.Values['DATE']
  else if Pos('TIME', CleanType) > 0 then Result := FFormulas.Values['TIMESTAMP']
  else if Pos('TIMESTAMP', CleanType) > 0 then Result := FFormulas.Values['TIMESTAMP']

  // Boolean
  else if Pos('BOOLEAN', CleanType) > 0 then Result := FFormulas.Values['BOOLEAN']

  // BLOB
  else if Pos('BLOB', CleanType) > 0 then Result := FFormulas.Values['BLOB'];

  // Fallback: wenn kein spezifischer Eintrag → $1
  if Result = '' then
    Result := '$1';
end;

{ TFormulaPresetManager }

constructor TFormulaPresetManager.Create;
begin
  inherited Create;
  FPresets := TStringList.Create;
  FPresets.Sorted := False;
  FPresetDir := GetPresetDir;

  if not DirectoryExists(FPresetDir) then
    ForceDirectories(FPresetDir);

  CreateDefaultPresets;
  LoadAllPresets;
end;

destructor TFormulaPresetManager.Destroy;
var
  i: Integer;
begin
  for i := 0 to FPresets.Count - 1 do
    FPresets.Objects[i].Free;
  FPresets.Free;
  inherited Destroy;
end;

function TFormulaPresetManager.GetPresetDir: string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) +
            'data' + PathDelim + 'formula_presets' + PathDelim;
end;

procedure TFormulaPresetManager.CreateDefaultPresets;
var
  Preset: TFormulaPreset;
begin
  // CSV Export
  if not FileExists(FPresetDir + 'csv_export.ini') then
  begin
    Preset := TFormulaPreset.Create;
    try
      Preset.Name := 'CSV Export';
      Preset.Description := 'Export all fields as quoted CSV values (numbers unquoted)';
      Preset.Separator := ',';
      Preset.QuoteChar := '"';
      Preset.FFormulas.Values['STRING'] := '''''''' + ' || $1 || ' + '''''''';
      Preset.FFormulas.Values['INTEGER'] := '$1';
      Preset.FFormulas.Values['FLOAT'] := '$1';
      Preset.FFormulas.Values['DATE'] := '''''''' + ' || CAST($1 AS VARCHAR(10)) || ' + '''''''';
      Preset.FFormulas.Values['TIMESTAMP'] := '''''''' + ' || CAST($1 AS VARCHAR(19)) || ' + '''''''';
      Preset.FFormulas.Values['BOOLEAN'] := '$1';
      Preset.FFormulas.Values['BLOB'] := '''''''' + ' || CAST(SUBSTRING($1 FROM 1 FOR 32765) AS VARCHAR(32765)) || ' + '''''''';
      Preset.SaveToFile(FPresetDir + 'csv_export.ini');
    finally
      Preset.Free;
    end;
  end;

  // Fixed Format
  if not FileExists(FPresetDir + 'fixed_format.ini') then
  begin
    Preset := TFormulaPreset.Create;
    try
      Preset.Name := 'Fixed Format';
      Preset.Description := 'Export all fields as fixed-width columns';
      Preset.FFormulas.Values['STRING'] := 'CAST($1 AS CHAR(50))';
      Preset.FFormulas.Values['INTEGER'] := 'CAST($1 AS CHAR(10))';
      Preset.FFormulas.Values['FLOAT'] := 'CAST($1 AS CHAR(20))';
      Preset.FFormulas.Values['DATE'] := 'CAST($1 AS CHAR(10))';
      Preset.FFormulas.Values['TIMESTAMP'] := 'CAST($1 AS CHAR(19))';
      Preset.FFormulas.Values['BOOLEAN'] := 'CAST($1 AS CHAR(5))';
      Preset.SaveToFile(FPresetDir + 'fixed_format.ini');
    finally
      Preset.Free;
    end;
  end;

  // JSON Export
  if not FileExists(FPresetDir + 'json_export.ini') then
  begin
    Preset := TFormulaPreset.Create;
    try
      Preset.Name := 'JSON Export';
      Preset.Description := 'Export all fields as JSON values';
      Preset.FFormulas.Values['STRING'] := '''"'' || $1 || ''"''';
      Preset.FFormulas.Values['INTEGER'] := '$1';
      Preset.FFormulas.Values['FLOAT'] := '$1';
      Preset.FFormulas.Values['BOOLEAN'] := '$1';
      Preset.FFormulas.Values['DATE'] := '''"'' || CAST($1 AS VARCHAR(10)) || ''"''';
      Preset.FFormulas.Values['TIMESTAMP'] := '''"'' || CAST($1 AS VARCHAR(19)) || ''"''';
      Preset.SaveToFile(FPresetDir + 'json_export.ini');
    finally
      Preset.Free;
    end;
  end;
end;

procedure TFormulaPresetManager.LoadAllPresets;
var
  i: integer;
  SR: TSearchRec;
  Preset: TFormulaPreset;
  FullPath: string;
begin
  for i := 0 to FPresets.Count - 1 do
    FPresets.Objects[i].Free;
  FPresets.Clear;

  if FindFirst(FPresetDir + '*.ini', faAnyFile, SR) = 0 then
  begin
    repeat
      FullPath := FPresetDir + SR.Name;
      Preset := TFormulaPreset.Create;
      if Preset.LoadFromFile(FullPath) then
        FPresets.AddObject(Preset.Name, Preset)
      else
        Preset.Free;
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
end;

procedure TFormulaPresetManager.Reload;
begin
  LoadAllPresets;
end;

function TFormulaPresetManager.GetPreset(const AName: string): TFormulaPreset;
var
  idx: Integer;
begin
  idx := FPresets.IndexOf(AName);
  if idx >= 0 then
    Result := TFormulaPreset(FPresets.Objects[idx])
  else
    Result := nil;
end;

function TFormulaPresetManager.GetPresetByIndex(AIndex: Integer): TFormulaPreset;
begin
  if (AIndex >= 0) and (AIndex < FPresets.Count) then
    Result := TFormulaPreset(FPresets.Objects[AIndex])
  else
    Result := nil;
end;

function TFormulaPresetManager.PresetCount: Integer;
begin
  Result := FPresets.Count;
end;

function TFormulaPresetManager.PresetName(AIndex: Integer): string;
begin
  if (AIndex >= 0) and (AIndex < FPresets.Count) then
    Result := FPresets[AIndex]
  else
    Result := '';
end;

initialization
  FormulaPresetManager := TFormulaPresetManager.Create;

finalization
  FormulaPresetManager.Free;

end.
