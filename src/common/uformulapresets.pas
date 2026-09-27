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
    FFileName: string;
    FName: string;
    FDescription: string;
    FSeparator: string;
    FQuoteChar: string;
    FIncludeHeader: Boolean;
    FHeaderQuoted: Boolean;
    FDataQuoted: Boolean;
    FFormulas: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    function LoadFromFile(const AFileName: string): Boolean;
    function SaveToFile(const AFileName: string): Boolean;

    function GetFormulaForFieldType(const AFieldType: string): string;

    property FileName: string read FFileName;
    property Name: string read FName write FName;
    property Description: string read FDescription write FDescription;
    property Separator: string read FSeparator write FSeparator;
    property QuoteChar: string read FQuoteChar write FQuoteChar;
    property IncludeHeader: Boolean read FIncludeHeader write FIncludeHeader;
    property HeaderQuoted: Boolean read FHeaderQuoted write FHeaderQuoted;
    property DataQuoted: Boolean read FDataQuoted write FDataQuoted;
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
    function GetDefaultPresetIndex: Integer;
    function PresetCount: Integer;
    function PresetName(AIndex: Integer): string;
  end;

var
  FormulaPresetManager: TFormulaPresetManager;

implementation

uses
  FileUtil,
  turbocommon;   // für BulkExportDefaultPreset

{ TFormulaPreset }

constructor TFormulaPreset.Create;
begin
  inherited Create;
  FFormulas := TStringList.Create;
  FSeparator := ',';
  FQuoteChar := '"';
  FIncludeHeader := True;
  FHeaderQuoted := True;
  FDataQuoted := True;
  FFileName := '';
end;

destructor TFormulaPreset.Destroy;
begin
  FFormulas.Free;
  inherited Destroy;
end;

function TFormulaPreset.LoadFromFile(const AFileName: string): Boolean;
var
  Ini: TIniFile;
begin
  Result := False;
  if not FileExists(AFileName) then Exit;

  Ini := TIniFile.Create(AFileName);
  try
    FFileName    := ExtractFileName(AFileName);
    FName        := Ini.ReadString('Preset', 'Name', '');
    FDescription := Ini.ReadString('Preset', 'Description', '');
    FSeparator   := Ini.ReadString('Preset', 'Separator', ',');
    FQuoteChar   := Ini.ReadString('Preset', 'QuoteChar', '"');
    FIncludeHeader := Ini.ReadBool('Preset', 'IncludeHeader', True);
    FHeaderQuoted  := Ini.ReadBool('Preset', 'HeaderQuoted', True);
    FDataQuoted    := Ini.ReadBool('Preset', 'DataQuoted', True);

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
      Ini.WriteBool('Preset', 'HeaderQuoted', FHeaderQuoted);
      Ini.WriteBool('Preset', 'DataQuoted', FDataQuoted);

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
begin
  Result := '$1';

  CleanType := UpperCase(Trim(AFieldType));

  // BLOB zuerst (enthält manchmal CHAR)
  if Pos('BLOB', CleanType) > 0 then
    Result := FFormulas.Values['BLOB']

  else if Pos('SMALLINT', CleanType) > 0 then Result := FFormulas.Values['INTEGER']
  else if Pos('BIGINT',   CleanType) > 0 then Result := FFormulas.Values['INTEGER']
  else if Pos('INTEGER',  CleanType) > 0 then Result := FFormulas.Values['INTEGER']
  else if Pos('FLOAT',    CleanType) > 0 then Result := FFormulas.Values['FLOAT']
  else if Pos('DOUBLE',   CleanType) > 0 then Result := FFormulas.Values['FLOAT']
  else if Pos('NUMERIC',  CleanType) > 0 then Result := FFormulas.Values['FLOAT']
  else if Pos('DECIMAL',  CleanType) > 0 then Result := FFormulas.Values['FLOAT']

  else if Pos('TIMESTAMP', CleanType) > 0 then Result := FFormulas.Values['TIMESTAMP']
  else if Pos('DATE',      CleanType) > 0 then Result := FFormulas.Values['DATE']
  else if Pos('TIME',      CleanType) > 0 then Result := FFormulas.Values['TIMESTAMP']

  else if Pos('CSTRING', CleanType) > 0 then Result := FFormulas.Values['STRING']
  else if Pos('VARCHAR', CleanType) > 0 then Result := FFormulas.Values['STRING']
  else if Pos('CHAR',    CleanType) > 0 then Result := FFormulas.Values['STRING']

  else if Pos('BOOLEAN', CleanType) > 0 then Result := FFormulas.Values['BOOLEAN'];

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
  // ---------- 1. CSV Quoted (Header + Daten) ----------
  if not FileExists(FPresetDir + 'csv_quoted.ini') then
  begin
    Preset := TFormulaPreset.Create;
    try
      Preset.Name          := 'CSV Quoted';
      Preset.Description   := 'CSV mit Quotes auf Strings/Datum, Zahlen roh';
      Preset.Separator     := ',';
      Preset.QuoteChar     := '"';
      Preset.IncludeHeader := True;
      Preset.HeaderQuoted  := True;
      Preset.DataQuoted    := True;
      Preset.FFormulas.Values['STRING']    := '$1';
      Preset.FFormulas.Values['INTEGER']   := '$1';
      Preset.FFormulas.Values['FLOAT']     := '$1';
      Preset.FFormulas.Values['DATE']      := '$1';
      Preset.FFormulas.Values['TIMESTAMP'] := '$1';
      Preset.FFormulas.Values['BOOLEAN']   := '$1';
      Preset.FFormulas.Values['BLOB']      := 'SUBSTRING($1 FROM 1 FOR 32765)';
      Preset.SaveToFile(FPresetDir + 'csv_quoted.ini');
    finally
      Preset.Free;
    end;
  end;

  // ---------- 2. CSV Unquoted (Header + Daten roh) ----------
  if not FileExists(FPresetDir + 'csv_unquoted.ini') then
  begin
    Preset := TFormulaPreset.Create;
    try
      Preset.Name          := 'CSV Unquoted';
      Preset.Description   := 'CSV ohne Quotes, alles roh';
      Preset.Separator     := ',';
      Preset.QuoteChar     := '"';
      Preset.IncludeHeader := True;
      Preset.HeaderQuoted  := False;
      Preset.DataQuoted    := False;
      Preset.FFormulas.Values['STRING']    := '$1';
      Preset.FFormulas.Values['INTEGER']   := '$1';
      Preset.FFormulas.Values['FLOAT']     := '$1';
      Preset.FFormulas.Values['DATE']      := '$1';
      Preset.FFormulas.Values['TIMESTAMP'] := '$1';
      Preset.FFormulas.Values['BOOLEAN']   := '$1';
      Preset.FFormulas.Values['BLOB']      := 'SUBSTRING($1 FROM 1 FOR 32765)';
      Preset.SaveToFile(FPresetDir + 'csv_unquoted.ini');
    finally
      Preset.Free;
    end;
  end;

  // ---------- 3. CSV Header Quoted (Data Unquoted) ----------
  if not FileExists(FPresetDir + 'csv_header_quoted.ini') then
  begin
    Preset := TFormulaPreset.Create;
    try
      Preset.Name          := 'CSV Header Quoted';
      Preset.Description   := 'Header mit Quotes, Daten roh';
      Preset.Separator     := ',';
      Preset.QuoteChar     := '"';
      Preset.IncludeHeader := True;
      Preset.HeaderQuoted  := True;
      Preset.DataQuoted    := False;
      Preset.FFormulas.Values['STRING']    := '$1';
      Preset.FFormulas.Values['INTEGER']   := '$1';
      Preset.FFormulas.Values['FLOAT']     := '$1';
      Preset.FFormulas.Values['DATE']      := '$1';
      Preset.FFormulas.Values['TIMESTAMP'] := '$1';
      Preset.FFormulas.Values['BOOLEAN']   := '$1';
      Preset.FFormulas.Values['BLOB']      := 'SUBSTRING($1 FROM 1 FOR 32765)';
      Preset.SaveToFile(FPresetDir + 'csv_header_quoted.ini');
    finally
      Preset.Free;
    end;
  end;

  // ---------- 4. CSV Data Quoted (Header Unquoted) ----------
  if not FileExists(FPresetDir + 'csv_data_quoted.ini') then
  begin
    Preset := TFormulaPreset.Create;
    try
      Preset.Name          := 'CSV Data Quoted';
      Preset.Description   := 'Header roh, Daten mit Quotes';
      Preset.Separator     := ',';
      Preset.QuoteChar     := '"';
      Preset.IncludeHeader := True;
      Preset.HeaderQuoted  := False;
      Preset.DataQuoted    := True;
      Preset.FFormulas.Values['STRING']    := '$1';
      Preset.FFormulas.Values['INTEGER']   := '$1';
      Preset.FFormulas.Values['FLOAT']     := '$1';
      Preset.FFormulas.Values['DATE']      := '$1';
      Preset.FFormulas.Values['TIMESTAMP'] := '$1';
      Preset.FFormulas.Values['BOOLEAN']   := '$1';
      Preset.FFormulas.Values['BLOB']      := 'SUBSTRING($1 FROM 1 FOR 32765)';
      Preset.SaveToFile(FPresetDir + 'csv_data_quoted.ini');
    finally
      Preset.Free;
    end;
  end;

  // ---------- 5. TSV Quoted ----------
  if not FileExists(FPresetDir + 'tsv_quoted.ini') then
  begin
    Preset := TFormulaPreset.Create;
    try
      Preset.Name          := 'TSV Quoted';
      Preset.Description   := 'Tab-separiert mit Quotes';
      Preset.Separator     := '\t';
      Preset.QuoteChar     := '"';
      Preset.IncludeHeader := True;
      Preset.HeaderQuoted  := True;
      Preset.DataQuoted    := True;
      Preset.FFormulas.Values['STRING']    := '$1';
      Preset.FFormulas.Values['INTEGER']   := '$1';
      Preset.FFormulas.Values['FLOAT']     := '$1';
      Preset.FFormulas.Values['DATE']      := '$1';
      Preset.FFormulas.Values['TIMESTAMP'] := '$1';
      Preset.FFormulas.Values['BOOLEAN']   := '$1';
      Preset.FFormulas.Values['BLOB']      := 'SUBSTRING($1 FROM 1 FOR 32765)';
      Preset.SaveToFile(FPresetDir + 'tsv_quoted.ini');
    finally
      Preset.Free;
    end;
  end;

  // ---------- 6. Pipe Quoted ----------
  if not FileExists(FPresetDir + 'pipe_quoted.ini') then
  begin
    Preset := TFormulaPreset.Create;
    try
      Preset.Name          := 'Pipe Quoted';
      Preset.Description   := 'Pipe-separiert mit Quotes';
      Preset.Separator     := '|';
      Preset.QuoteChar     := '"';
      Preset.IncludeHeader := True;
      Preset.HeaderQuoted  := True;
      Preset.DataQuoted    := True;
      Preset.FFormulas.Values['STRING']    := '$1';
      Preset.FFormulas.Values['INTEGER']   := '$1';
      Preset.FFormulas.Values['FLOAT']     := '$1';
      Preset.FFormulas.Values['DATE']      := '$1';
      Preset.FFormulas.Values['TIMESTAMP'] := '$1';
      Preset.FFormulas.Values['BOOLEAN']   := '$1';
      Preset.FFormulas.Values['BLOB']      := 'SUBSTRING($1 FROM 1 FOR 32765)';
      Preset.SaveToFile(FPresetDir + 'pipe_quoted.ini');
    finally
      Preset.Free;
    end;
  end;

  // ---------- 7. Fixed Format ----------
  if not FileExists(FPresetDir + 'fixed_format.ini') then
  begin
    Preset := TFormulaPreset.Create;
    try
      Preset.Name          := 'Fixed Format';
      Preset.Description   := 'Fixed-Width-Format, kein Separator';
      Preset.Separator     := '';
      Preset.QuoteChar     := '';
      Preset.IncludeHeader := True;
      Preset.HeaderQuoted  := False;
      Preset.DataQuoted    := False;
      Preset.FFormulas.Values['STRING']    := 'CAST($1 AS CHAR(50))';
      Preset.FFormulas.Values['INTEGER']   := 'CAST($1 AS CHAR(10))';
      Preset.FFormulas.Values['FLOAT']     := 'CAST($1 AS CHAR(20))';
      Preset.FFormulas.Values['DATE']      := 'CAST($1 AS CHAR(10))';
      Preset.FFormulas.Values['TIMESTAMP'] := 'CAST($1 AS CHAR(19))';
      Preset.FFormulas.Values['BOOLEAN']   := 'CAST($1 AS CHAR(5))';
      Preset.SaveToFile(FPresetDir + 'fixed_format.ini');
    finally
      Preset.Free;
    end;
  end;

  // ---------- 8. JSON Export ----------
  if not FileExists(FPresetDir + 'json_export.ini') then
  begin
    Preset := TFormulaPreset.Create;
    try
      Preset.Name          := 'JSON Export';
      Preset.Description   := 'JSON-Werte, Strings/Datum in Doppelquotes';
      Preset.Separator     := ',';
      Preset.QuoteChar     := '"';
      Preset.IncludeHeader := True;
      Preset.HeaderQuoted  := True;
      Preset.DataQuoted    := True;
      Preset.FFormulas.Values['STRING']    := '$1';
      Preset.FFormulas.Values['INTEGER']   := '$1';
      Preset.FFormulas.Values['FLOAT']     := '$1';
      Preset.FFormulas.Values['DATE']      := '$1';
      Preset.FFormulas.Values['TIMESTAMP'] := '$1';
      Preset.FFormulas.Values['BOOLEAN']   := '$1';
      Preset.SaveToFile(FPresetDir + 'json_export.ini');
    finally
      Preset.Free;
    end;
  end;
end;

procedure TFormulaPresetManager.LoadAllPresets;
var
  i: Integer;
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

function TFormulaPresetManager.GetDefaultPresetIndex: Integer;
var
  i: Integer;
  DefaultName, PresetFileBase: string;
  P: TFormulaPreset;
begin
  Result := -1;

  DefaultName := Trim(BulkExportDefaultPreset);
  if DefaultName <> '' then
  begin
    if LowerCase(ExtractFileExt(DefaultName)) = '.ini' then
      DefaultName := ChangeFileExt(DefaultName, '');

    for i := 0 to FPresets.Count - 1 do
    begin
      P := TFormulaPreset(FPresets.Objects[i]);
      PresetFileBase := ChangeFileExt(P.FileName, '');
      if SameText(PresetFileBase, DefaultName) then
        Exit(i);
    end;
  end;

  if FPresets.Count > 0 then
    Result := 0;
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
