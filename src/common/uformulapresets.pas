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

  { TPresetIssueSeverity }

  TPresetIssueSeverity = (isInfo, isWarning, isError);

  { TPresetIssue }

  TPresetIssue = record
    Severity: TPresetIssueSeverity;
    Message: string;
  end;

  TPresetIssues = array of TPresetIssue;

  { TFormulaPreset }

  TFormulaPreset = class
  private
    FFileName: string;
    FName: string;
    FDescription: string;
    FSeparator: string;
    FDecimalPoint: string;
    FQuoteChar: string;
    FIncludeHeader: Boolean;
    FHeaderQuoted: Boolean;
    FDataQuoted: Boolean;
    FSkipTypes: string;
    FBooleanTrue: string;
    FBooleanFalse: string;
    FFormulas: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    function LoadFromFile(const AFileName: string): Boolean;
    function SaveToFile(const AFileName: string): Boolean;

    function GetFormulaForFieldType(const AFieldType: string): string;
    function ShouldSkipType(const AFieldType: string): Boolean;
    function GetBooleanLiteral(AValue: Boolean): string;

    property FileName: string read FFileName;
    property Name: string read FName write FName;
    property Description: string read FDescription write FDescription;
    property Separator: string read FSeparator write FSeparator;
    property DecimalPoint: string read FDecimalPoint write FDecimalPoint;
    property QuoteChar: string read FQuoteChar write FQuoteChar;
    property IncludeHeader: Boolean read FIncludeHeader write FIncludeHeader;
    property HeaderQuoted: Boolean read FHeaderQuoted write FHeaderQuoted;
    property DataQuoted: Boolean read FDataQuoted write FDataQuoted;
    property SkipTypes: string read FSkipTypes write FSkipTypes;
    property BooleanTrue: string read FBooleanTrue write FBooleanTrue;
    property BooleanFalse: string read FBooleanFalse write FBooleanFalse;
  end;

  { TPresetValidator }

  TPresetValidator = class
  public
    class function Validate(APreset: TFormulaPreset): TPresetIssues;
    class function HasErrors(const AIssues: TPresetIssues): Boolean;
    class function BuildMessage(const AIssues: TPresetIssues): string;
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
  turbocommon;

{ TFormulaPreset }

constructor TFormulaPreset.Create;
begin
  inherited Create;
  FFormulas := TStringList.Create;
  FSeparator    := ',';
  FDecimalPoint := '.';
  FQuoteChar    := '"';
  FIncludeHeader := True;
  FHeaderQuoted  := True;
  FDataQuoted    := True;
  FSkipTypes     := '';
  FBooleanTrue   := 'TRUE';
  FBooleanFalse  := 'FALSE';
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
    FFileName     := ExtractFileName(AFileName);
    FName         := Ini.ReadString('Preset', 'Name', '');
    FDescription  := Ini.ReadString('Preset', 'Description', '');
    FSeparator    := Ini.ReadString('Preset', 'Separator', ',');
    FDecimalPoint := Ini.ReadString('Preset', 'DecimalPoint', '.');
    FQuoteChar    := Ini.ReadString('Preset', 'QuoteChar', '"');
    FIncludeHeader := Ini.ReadBool('Preset', 'IncludeHeader', True);
    FHeaderQuoted  := Ini.ReadBool('Preset', 'HeaderQuoted', True);
    FDataQuoted    := Ini.ReadBool('Preset', 'DataQuoted', True);
    FSkipTypes     := Ini.ReadString('Preset', 'SkipTypes', '');
    FBooleanTrue   := Ini.ReadString('Preset', 'BooleanTrue', 'TRUE');
    FBooleanFalse  := Ini.ReadString('Preset', 'BooleanFalse', 'FALSE');

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
      Ini.WriteString('Preset', 'DecimalPoint', FDecimalPoint);
      Ini.WriteString('Preset', 'QuoteChar', FQuoteChar);
      Ini.WriteBool('Preset', 'IncludeHeader', FIncludeHeader);
      Ini.WriteBool('Preset', 'HeaderQuoted', FHeaderQuoted);
      Ini.WriteBool('Preset', 'DataQuoted', FDataQuoted);
      if FSkipTypes <> '' then
        Ini.WriteString('Preset', 'SkipTypes', FSkipTypes);
      Ini.WriteString('Preset', 'BooleanTrue', FBooleanTrue);
      Ini.WriteString('Preset', 'BooleanFalse', FBooleanFalse);

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

  if Pos('BLOB', CleanType) > 0 then
    Result := FFormulas.Values['BLOB']

  else if Pos('SMALLINT', CleanType) > 0 then Result := FFormulas.Values['INTEGER']
  else if Pos('BIGINT',   CleanType) > 0 then Result := FFormulas.Values['INTEGER']
  else if Pos('INTEGER',  CleanType) > 0 then Result := FFormulas.Values['INTEGER']
  else if Pos('FLOAT',    CleanType) > 0 then Result := FFormulas.Values['FLOAT']
  else if Pos('DOUBLE',   CleanType) > 0 then Result := FFormulas.Values['FLOAT']
  else if Pos('NUMERIC',  CleanType) > 0 then Result := FFormulas.Values['FLOAT']
  else if Pos('DECIMAL',  CleanType) > 0 then Result := FFormulas.Values['FLOAT']
  else if Pos('DECFLOAT', CleanType) > 0 then Result := FFormulas.Values['FLOAT']
  else if Pos('INT128',   CleanType) > 0 then Result := FFormulas.Values['INTEGER']

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

function TFormulaPreset.ShouldSkipType(const AFieldType: string): Boolean;
var
  SL: TStringList;
  i: Integer;
  FieldTypeUC, SkipToken: string;
  IsBlobField, IsBinaryBlob, IsTextBlob: Boolean;
begin
  Result := False;
  if FSkipTypes = '' then Exit;

  FieldTypeUC := UpperCase(AFieldType);

  // BLOB-Klassifizierung
  IsBlobField  := Pos('BLOB', FieldTypeUC) > 0;
  IsBinaryBlob := IsBlobField and (Pos('BINARY', FieldTypeUC) > 0);
  IsTextBlob   := IsBlobField and
                  ((Pos('TEXT', FieldTypeUC) > 0) or
                   (Pos('SUBTYPE 1', FieldTypeUC) > 0));

  SL := TStringList.Create;
  try
    SL.Delimiter := ',';
    SL.StrictDelimiter := True;
    SL.DelimitedText := FSkipTypes;

    for i := 0 to SL.Count - 1 do
    begin
      SkipToken := UpperCase(Trim(SL[i]));
      if SkipToken = '' then Continue;

      // Spezialfall: ARRAY — matcht auch auf eckige Klammern
      if SkipToken = 'ARRAY' then
      begin
        if (Pos('ARRAY', FieldTypeUC) > 0) or (Pos('[', AFieldType) > 0) then
          Exit(True);
      end
      // BLOB_BINARY — nur binary BLOBs
      else if SkipToken = 'BLOB_BINARY' then
      begin
        if IsBinaryBlob then
          Exit(True);
      end
      // BLOB_TEXT — nur text BLOBs
      else if SkipToken = 'BLOB_TEXT' then
      begin
        if IsTextBlob then
          Exit(True);
      end
      // BLOB — alle BLOBs (backward compatible)
      else if SkipToken = 'BLOB' then
      begin
        if IsBlobField then
          Exit(True);
      end
      // Normaler Token: Substring-Match
      else
      begin
        if Pos(SkipToken, FieldTypeUC) > 0 then
          Exit(True);
      end;
    end;
  finally
    SL.Free;
  end;
end;

function TFormulaPreset.GetBooleanLiteral(AValue: Boolean): string;
begin
  if AValue then
    Result := FBooleanTrue
  else
    Result := FBooleanFalse;
end;

{ TPresetValidator }

class function TPresetValidator.Validate(APreset: TFormulaPreset): TPresetIssues;
var
  Issue: TPresetIssue;
  i: Integer;
  FormulaValue: string;
  SepUsedInFormula: Boolean;
begin
  SetLength(Result, 0);

  if APreset = nil then
  begin
    Issue.Severity := isError;
    Issue.Message := 'Preset is nil.';
    SetLength(Result, 1);
    Result[0] := Issue;
    Exit;
  end;

  // ===========================================================
  // ERROR: Separator = QuoteChar
  // ===========================================================
  if (APreset.Separator <> '') and (APreset.QuoteChar <> '') and
     (APreset.Separator = APreset.QuoteChar) then
  begin
    Issue.Severity := isError;
    Issue.Message :=
      'Separator and QuoteChar are identical: "' + APreset.Separator + '"' + sLineBreak +
      sLineBreak +
      'Problem:' + sLineBreak +
      '  Both use the same character. The CSV parser cannot' + sLineBreak +
      '  distinguish between field separators and quoted values.' + sLineBreak +
      sLineBreak +
      'Example of broken output:' + sLineBreak +
      '  "value1";"value2"    -> looks fine' + sLineBreak +
      '  abc,def,ghi          -> is this 3 fields or 1?' + sLineBreak +
      sLineBreak +
      'Solution:' + sLineBreak +
      '  Choose a different QuoteChar (e.g. ' + QuotedStr('''') + ')' + sLineBreak +
      '  or a different Separator (e.g. ";")';
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := Issue;
  end;

  // ===========================================================
  // ERROR: Separator = DecimalPoint
  // ===========================================================
  if (APreset.Separator <> '') and (APreset.DecimalPoint <> '') and
     (APreset.Separator = APreset.DecimalPoint) then
  begin
    Issue.Severity := isError;
    Issue.Message :=
      'Separator and DecimalPoint are identical: "' + APreset.Separator + '"' + sLineBreak +
      sLineBreak +
      'Problem:' + sLineBreak +
      '  The same character is used both for separating fields' + sLineBreak +
      '  AND for the decimal mark. The result is unparseable.' + sLineBreak +
      sLineBreak +
      'Example of broken output:' + sLineBreak +
      '  123.45,678.90,1000.50    -> is this 3 fields or 6?' + sLineBreak +
      sLineBreak +
      'Solution:' + sLineBreak +
      '  Use ";" as Separator if you want "," as DecimalPoint' + sLineBreak +
      '  (this is the German/European CSV convention)' + sLineBreak +
      sLineBreak +
      '  Or use "." as DecimalPoint with "," as Separator' + sLineBreak +
      '  (this is the US/International CSV convention)';
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := Issue;
  end;

  // ===========================================================
  // WARNING: Separator in Formel
  // ===========================================================
  SepUsedInFormula := False;
  if (APreset.Separator <> '') then
  begin
    for i := 0 to APreset.FFormulas.Count - 1 do
    begin
      FormulaValue := APreset.FFormulas.ValueFromIndex[i];
      if Pos(APreset.Separator, FormulaValue) > 0 then
      begin
        SepUsedInFormula := True;
        Break;
      end;
    end;
  end;

  if SepUsedInFormula then
  begin
    Issue.Severity := isWarning;
    Issue.Message :=
      'Separator ("' + APreset.Separator + '") appears in at least one formula' + sLineBreak +
      sLineBreak +
      'Problem:' + sLineBreak +
      '  Values produced by a formula may contain the Separator.' + sLineBreak +
      '  Fields will be split incorrectly if not quoted.' + sLineBreak +
      sLineBreak +
      'Solution:' + sLineBreak +
      '  Either ensure the formula does not produce the Separator,' + sLineBreak +
      '  or enable quoting (DataQuoted=1, QuoteChar set)' + sLineBreak +
      '  so fields are safely wrapped.';
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := Issue;
  end;

  // ===========================================================
  // WARNING: Formeln ohne $1
  // ===========================================================
  for i := 0 to APreset.FFormulas.Count - 1 do
  begin
    FormulaValue := APreset.FFormulas.ValueFromIndex[i];
    if (Trim(FormulaValue) <> '') and (Pos('$1', FormulaValue) = 0) then
    begin
      Issue.Severity := isWarning;
      Issue.Message :=
        'Formula for "' + APreset.FFormulas.Names[i] + '" does not use $1' + sLineBreak +
        sLineBreak +
        'Problem:' + sLineBreak +
        '  Without $1, the formula does not reference the field value.' + sLineBreak +
        '  Every row will get the same constant output.' + sLineBreak +
        sLineBreak +
        'Example:' + sLineBreak +
        '  Formula: ''hello''' + sLineBreak +
        '  Result:  every row shows "hello"' + sLineBreak +
        sLineBreak +
        'Solution:' + sLineBreak +
        '  Use $1 for the original field value:' + sLineBreak +
        '  Formula: ''hello '' || $1';
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := Issue;
    end;
  end;

  // ===========================================================
  // WARNING: Unbalancierte Quotes in Formulas
  // ===========================================================
  for i := 0 to APreset.FFormulas.Count - 1 do
  begin
    FormulaValue := APreset.FFormulas.ValueFromIndex[i];
    if Odd(Length(FormulaValue) - Length(StringReplace(FormulaValue, '''', '', [rfReplaceAll]))) then
    begin
      Issue.Severity := isWarning;
      Issue.Message :=
        'Formula for "' + APreset.FFormulas.Names[i] + '" has unbalanced single quotes' + sLineBreak +
        sLineBreak +
        'Problem:' + sLineBreak +
        '  The formula contains an odd number of single quotes (' + QuotedStr('''') + ').' + sLineBreak +
        '  This is usually a syntax error in SQL.' + sLineBreak +
        sLineBreak +
        'Example of broken formula:' + sLineBreak +
        '  ''text || $1 || ''text      -> missing closing quote' + sLineBreak +
        sLineBreak +
        'Solution:' + sLineBreak +
        '  Check that all quotes are properly paired:' + sLineBreak +
        '  ''text'' || $1 || ''text''';
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := Issue;
    end;
  end;

  // ===========================================================
  // INFO: SkipTypes aktiv
  // ===========================================================
  if APreset.SkipTypes <> '' then
  begin
    Issue.Severity := isInfo;
    Issue.Message :=
      'SkipTypes active: ' + APreset.SkipTypes + sLineBreak +
      sLineBreak +
      'Info:' + sLineBreak +
      '  Fields matching these types will be automatically' + sLineBreak +
      '  deselected when the table is loaded.' + sLineBreak +
      sLineBreak +
      'Common values:' + sLineBreak +
      '  ARRAY        -> skip array fields' + sLineBreak +
      '  BLOB BINARY  -> skip binary BLOBs';
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := Issue;
  end;
end;

class function TPresetValidator.HasErrors(const AIssues: TPresetIssues): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to High(AIssues) do
    if AIssues[i].Severity = isError then
      Exit(True);
end;

class function TPresetValidator.BuildMessage(const AIssues: TPresetIssues): string;
var
  i: Integer;
  Prefix: string;
begin
  Result := '';
  for i := 0 to High(AIssues) do
  begin
    case AIssues[i].Severity of
      isError:   Prefix := '[ERROR] ';
      isWarning: Prefix := '[WARNING] ';
      isInfo:    Prefix := '[INFO] ';
    else
      Prefix := '';
    end;
    Result := Result + Prefix + AIssues[i].Message + sLineBreak;

    // Leerzeile zwischen Issues (nicht nach dem letzten)
    if i < High(AIssues) then
      Result := Result + sLineBreak;
  end;
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
  // ---------- 1. CSV Quoted ----------
  if not FileExists(FPresetDir + 'csv_quoted.ini') then
  begin
    Preset := TFormulaPreset.Create;
    try
      Preset.Name          := 'CSV Quoted';
      Preset.Description   := 'CSV mit Quotes auf Strings/Datum, Zahlen roh';
      Preset.Separator     := ',';
      Preset.DecimalPoint  := '.';
      Preset.QuoteChar     := '"';
      Preset.IncludeHeader := True;
      Preset.HeaderQuoted  := True;
      Preset.DataQuoted    := True;
      Preset.SkipTypes     := 'ARRAY,BLOB_BINARY,BLOB_TEXT';
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

  // ---------- 2. CSV Unquoted ----------
  if not FileExists(FPresetDir + 'csv_unquoted.ini') then
  begin
    Preset := TFormulaPreset.Create;
    try
      Preset.Name          := 'CSV Unquoted';
      Preset.Description   := 'CSV ohne Quotes, alles roh';
      Preset.Separator     := ',';
      Preset.DecimalPoint  := '.';
      Preset.QuoteChar     := '';
      Preset.IncludeHeader := True;
      Preset.HeaderQuoted  := False;
      Preset.DataQuoted    := False;
      Preset.SkipTypes     := 'ARRAY,BLOB_BINARY,BLOB_TEXT';
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

  // ---------- 3. CSV Header Quoted ----------
  if not FileExists(FPresetDir + 'csv_header_quoted.ini') then
  begin
    Preset := TFormulaPreset.Create;
    try
      Preset.Name          := 'CSV Header Quoted';
      Preset.Description   := 'Header mit Quotes, Daten roh';
      Preset.Separator     := ',';
      Preset.DecimalPoint  := '.';
      Preset.QuoteChar     := '"';
      Preset.IncludeHeader := True;
      Preset.HeaderQuoted  := True;
      Preset.DataQuoted    := False;
      Preset.SkipTypes     := 'ARRAY,BLOB_BINARY,BLOB_TEXT';
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

  // ---------- 4. CSV Data Quoted ----------
  if not FileExists(FPresetDir + 'csv_data_quoted.ini') then
  begin
    Preset := TFormulaPreset.Create;
    try
      Preset.Name          := 'CSV Data Quoted';
      Preset.Description   := 'Header roh, Daten mit Quotes';
      Preset.Separator     := ',';
      Preset.DecimalPoint  := '.';
      Preset.QuoteChar     := '"';
      Preset.IncludeHeader := True;
      Preset.HeaderQuoted  := False;
      Preset.DataQuoted    := True;
      Preset.SkipTypes     := 'ARRAY,BLOB_BINARY,BLOB_TEXT';
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

  // ---------- 5. CSV German ----------
  if not FileExists(FPresetDir + 'csv_german.ini') then
  begin
    Preset := TFormulaPreset.Create;
    try
      Preset.Name          := 'CSV German';
      Preset.Description   := 'Deutsches CSV: Semikolon als Trenner, Komma als Dezimal';
      Preset.Separator     := ';';
      Preset.DecimalPoint  := ',';
      Preset.QuoteChar     := '"';
      Preset.IncludeHeader := True;
      Preset.HeaderQuoted  := True;
      Preset.DataQuoted    := True;
      Preset.SkipTypes     := 'ARRAY,BLOB_BINARY,BLOB_TEXT';
      Preset.FFormulas.Values['STRING']    := '$1';
      Preset.FFormulas.Values['INTEGER']   := '$1';
      Preset.FFormulas.Values['FLOAT']     := 'REPLACE(CAST($1 AS VARCHAR(50)), ''.'', '','')';
      Preset.FFormulas.Values['DATE']      := '$1';
      Preset.FFormulas.Values['TIMESTAMP'] := '$1';
      Preset.FFormulas.Values['BOOLEAN']   := '$1';
      Preset.FFormulas.Values['BLOB']      := 'SUBSTRING($1 FROM 1 FOR 32765)';
      Preset.SaveToFile(FPresetDir + 'csv_german.ini');
    finally
      Preset.Free;
    end;
  end;

  // ---------- 6. TSV Quoted ----------
  if not FileExists(FPresetDir + 'tsv_quoted.ini') then
  begin
    Preset := TFormulaPreset.Create;
    try
      Preset.Name          := 'TSV Quoted';
      Preset.Description   := 'Tab-separiert mit Quotes';
      Preset.Separator     := '\t';
      Preset.DecimalPoint  := '.';
      Preset.QuoteChar     := '"';
      Preset.IncludeHeader := True;
      Preset.HeaderQuoted  := True;
      Preset.DataQuoted    := True;
      Preset.SkipTypes     := 'ARRAY,BLOB_BINARY,BLOB_TEXT';
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

  // ---------- 7. Pipe Quoted ----------
  if not FileExists(FPresetDir + 'pipe_quoted.ini') then
  begin
    Preset := TFormulaPreset.Create;
    try
      Preset.Name          := 'Pipe Quoted';
      Preset.Description   := 'Pipe-separiert mit Quotes';
      Preset.Separator     := '|';
      Preset.DecimalPoint  := '.';
      Preset.QuoteChar     := '"';
      Preset.IncludeHeader := True;
      Preset.HeaderQuoted  := True;
      Preset.DataQuoted    := True;
      Preset.SkipTypes     := 'ARRAY,BLOB_BINARY,BLOB_TEXT';
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

  // ---------- 8. Fixed Format ----------
  if not FileExists(FPresetDir + 'fixed_format.ini') then
  begin
    Preset := TFormulaPreset.Create;
    try
      Preset.Name          := 'Fixed Format';
      Preset.Description   := 'Fixed-Width-Format, kein Separator, kein Header';
      Preset.Separator     := '';
      Preset.DecimalPoint  := '.';
      Preset.QuoteChar     := '';
      Preset.IncludeHeader := False;
      Preset.HeaderQuoted  := False;
      Preset.DataQuoted    := False;
      Preset.SkipTypes     := 'ARRAY,BLOB_BINARY,BLOB_TEXT';
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

  // ---------- 9. JSON Export ----------
  if not FileExists(FPresetDir + 'json_export.ini') then
  begin
    Preset := TFormulaPreset.Create;
    try
      Preset.Name          := 'JSON Export';
      Preset.Description   := 'JSON-Werte, Strings/Datum in Doppelquotes';
      Preset.Separator     := ',';
      Preset.DecimalPoint  := '.';
      Preset.QuoteChar     := '"';
      Preset.IncludeHeader := True;
      Preset.HeaderQuoted  := True;
      Preset.DataQuoted    := True;
      Preset.SkipTypes     := 'ARRAY,BLOB_BINARY,BLOB_TEXT';
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

  DefaultName := Trim(CSVExportDefaultPreset);
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
