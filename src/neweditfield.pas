unit NewEditField;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, Buttons, turbocommon, fbcommon;

type
  TFormMode = (foNew, foEdit);

  { TfmNewEditField }

  TfmNewEditField = class(TForm)
    bbAdd: TBitBtn;
    cbCharset: TComboBox;
    cbCollation: TComboBox;
    cbType: TComboBox;
    cxAllowNull: TCheckBox;
    edDescription: TEdit;
    edFieldName: TEdit;
    edDefault: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    lblCharset: TLabel;
    lblCollation: TLabel;
    sePrecision: TSpinEdit;
    seSize: TSpinEdit;
    seOrder: TSpinEdit;
    seScale: TSpinEdit;
    procedure bbAddClick(Sender: TObject);
    procedure cbCharsetEditingDone(Sender: TObject);
    procedure cbTypeChange(Sender: TObject);
    procedure cbTypeEditingDone(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FDBIndex: Integer;
    FTableName: string;
    FFormMode: TFormMode;
    FRefreshButton: TBitBtn;
  public
    OldFieldName: string;
    OldFieldType: string;
    OldFieldSize: integer;
    OldFieldPrecision: integer;
    OldFieldScale: integer;
    OldAllowNull: Boolean;
    OldOrder: integer;
    OldDefault: string;
    OldCharacterSet: string;
    OldCollation: string;
    OldDescription: string;
    procedure Init(dbIndex: Integer; TableName: string;
      FormMode: TFormMode;
      FieldName, FieldType,
      CharacterSet, Collation,
      DefaultValue, Description: string;
      Size, Precision, Scale, Order: Integer;
      AllowNull: Boolean;
      RefreshButton: TBitBtn);
    procedure EnableDisableControls;
    { public declarations }
  end; 

var
  fmNewEditField: TfmNewEditField;

implementation

{ TfmNewEditField }

uses Main, SysTables;


{procedure TfmNewEditField.bbAddClick(Sender: TObject);
var
  Line, NewCharset, NewCollation, FieldDef, TempFieldName: string;
  Clk: TNotifyEvent;
begin
  if FRefreshButton = nil then
    Clk := nil
  else
    Clk := FRefreshButton.OnClick;

  NewCharset := cbCharset.Text;
  NewCollation := cbCollation.Text;

  if fFormMode = foNew then  // New field
  begin
    FieldDef := cbType.Text;
    if (FieldDef = 'CHAR') or (FieldDef = 'CSTRING') or (FieldDef = 'VARCHAR') then
      FieldDef := FieldDef + '(' + IntToStr(seSize.Value) + ')';

    if (FieldDef = 'DECIMAL') or (FieldDef = 'NUMERIC') then
      FieldDef := FieldDef + '(' + IntToStr(sePrecision.Value) + ',' + IntToStr(seScale.Value) + ')';

    // Charset & Collation
    if NewCharset <> '' then
      FieldDef := FieldDef + ' CHARACTER SET ' + NewCharset;
    if NewCollation <> '' then
      FieldDef := FieldDef + ' COLLATE ' + NewCollation;

    // Default value
    if Trim(edDefault.Text) <> '' then
    begin
      if ((cbType.Text = 'CHAR') or (cbType.Text = 'CSTRING') or (cbType.Text = 'VARCHAR')) and
         (Pos('''', edDefault.Text) = 0) then
        FieldDef := FieldDef + ' DEFAULT ' + QuotedStr(edDefault.Text)
      else
        FieldDef := FieldDef + ' DEFAULT ' + edDefault.Text;
    end;

    // Null / Not null
    if not cxAllowNull.Checked then
      FieldDef := FieldDef + ' NOT NULL';

    Line := 'ALTER TABLE ' + FTableName + ' ADD ' + edFieldName.Text + ' ' + FieldDef + ';';

    fmMain.ShowCompleteQueryWindow(FDBIndex, 'Add new field to Table: ' + FTableName, Line, Clk);
  end
  else  // Edit existing field
  begin
    Line := '';

    // Name geändert?
    if UpperCase(Trim(edFieldName.Text)) <> OldFieldName then
      Line := Line + 'ALTER TABLE ' + FTableName + ' ALTER ' + OldFieldName + ' TO ' + edFieldName.Text + ';' + LineEnding;

    // Typ geändert?
    if (cbType.Text <> OldFieldType) or
       (seSize.Value <> OldFieldSize) or
       (sePrecision.Value <> OldFieldPrecision) or
       (seScale.Value <> OldFieldScale) then
    begin
      Line := Line + 'ALTER TABLE ' + FTableName + ' ALTER ' + edFieldName.Text + ' TYPE ' + cbType.Text;

      if (cbType.Text = 'NUMERIC') or (cbType.Text = 'DECIMAL') then
        Line := Line + '(' + IntToStr(sePrecision.Value) + ',' + IntToStr(seScale.Value) + ')'
      else if (cbType.Text = 'CHAR') or (cbType.Text = 'CSTRING') or (cbType.Text = 'VARCHAR') then
        Line := Line + '(' + IntToStr(seSize.Value) + ')';

      Line := Line + ';' + LineEnding;
    end;

    // Charset / Collation geändert?
    if (NewCharset <> OldCharacterSet) or (NewCollation <> OldCollation) then
    begin
      TempFieldName := edFieldName.Text + '_NEW';

      FieldDef := cbType.Text;
      if (FieldDef = 'CHAR') or (FieldDef = 'CSTRING') or (FieldDef = 'VARCHAR') then
        FieldDef := FieldDef + '(' + IntToStr(seSize.Value) + ')';

      FieldDef := FieldDef + ' CHARACTER SET ' + NewCharset;
      if NewCollation <> '' then
        FieldDef := FieldDef + ' COLLATE ' + NewCollation;

      Line := Line + '-- Charset or Collation change requires field recreation:' + LineEnding;
      Line := Line + 'ALTER TABLE ' + FTableName + ' ADD ' + TempFieldName + ' ' + FieldDef + ';' + LineEnding;
      Line := Line + 'UPDATE ' + FTableName + ' SET ' + TempFieldName + ' = ' + edFieldName.Text + ';' + LineEnding;
      Line := Line + 'ALTER TABLE ' + FTableName + ' DROP ' + edFieldName.Text + ';' + LineEnding;
      Line := Line + 'ALTER TABLE ' + FTableName + ' ALTER ' + TempFieldName + ' TO ' + edFieldName.Text + ';' + LineEnding;
    end;

    // Feldposition
    if seOrder.Value <> OldOrder then
    begin
      Line := Line + 'ALTER TABLE ' + FTableName + ' ALTER ' + edFieldName.Text +
              ' POSITION ' + IntToStr(seOrder.Value) + ';' + LineEnding;
    end;

    // NOT NULL
    if cxAllowNull.Checked <> OldAllowNull then
    begin
      if cxAllowNull.Checked then
        Line := Line + 'ALTER TABLE ' + FTableName + ' ALTER ' + edFieldName.Text + ' DROP NOT NULL;' + LineEnding
      else
        Line := Line + 'ALTER TABLE ' + FTableName + ' ALTER ' + edFieldName.Text + ' SET NOT NULL;' + LineEnding;
    end;

    // Default-Wert
    if edDefault.Text <> OldDefault then
    begin
      if Trim(edDefault.Text) <> '' then
        Line := Line + 'ALTER TABLE ' + FTableName + ' ALTER ' + edFieldName.Text +
                ' SET DEFAULT ' + QuotedStr(edDefault.Text) + ';' + LineEnding
      else
        Line := Line + 'ALTER TABLE ' + FTableName + ' ALTER ' + edFieldName.Text +
                ' DROP DEFAULT;' + LineEnding;
    end;

    // Beschreibung
    if edDescription.Text <> OldDescription then
    begin
      Line := Line + 'COMMENT ON COLUMN ' + FTableName + '.' + edFieldName.Text +
              ' IS ' + QuotedStr(edDescription.Text) + ';' + LineEnding;
    end;

    if Line <> '' then
      fmMain.ShowCompleteQueryWindow(FDBIndex, 'Edit field: ' + OldFieldName, Line, Clk);
  end;

  Close;
end;
}

procedure TfmNewEditField.bbAddClick(Sender: TObject);
var
  Line, NewCharset, NewCollation, FieldDef, TempFieldName, BaseType: string;
  Clk: TNotifyEvent;
  TmpFloat: Double;
  TmpInt: Int64;
begin
  if FRefreshButton = nil then
    Clk := nil
  else
    Clk := FRefreshButton.OnClick;

  NewCharset := cbCharset.Text;
  NewCollation := cbCollation.Text;

  if fFormMode = foNew then  // Neues Feld
  begin
    BaseType := cbType.Text;

    // UUID-Spezialbehandlung
    if BaseType = 'UUID' then
    begin
      FieldDef := 'CHAR(16) CHARACTER SET OCTETS';
    end
    else
    begin
      FieldDef := BaseType;

      if (FieldDef = 'CHAR') or (FieldDef = 'CSTRING') or (FieldDef = 'VARCHAR') then
        FieldDef := FieldDef + '(' + IntToStr(seSize.Value) + ')';

      if (FieldDef = 'DECIMAL') or (FieldDef = 'NUMERIC') then
        FieldDef := FieldDef + '(' + IntToStr(sePrecision.Value) + ',' + IntToStr(seScale.Value) + ')';

      if NewCharset <> '' then
        FieldDef := FieldDef + ' CHARACTER SET ' + NewCharset;

      if NewCollation <> '' then
        FieldDef := FieldDef + ' COLLATE ' + NewCollation;
    end;

    // Default value
    if Trim(edDefault.Text) <> '' then
    begin
      try
        if (BaseType = 'INTEGER') or (BaseType = 'SMALLINT') or (BaseType = 'BIGINT') then
        begin
          TmpInt := StrToInt(edDefault.Text);
          FieldDef := FieldDef + ' DEFAULT ' + IntToStr(TmpInt);
        end
        else if (BaseType = 'NUMERIC') or (BaseType = 'DECIMAL') or
                (BaseType = 'FLOAT') or (BaseType = 'DOUBLE PRECISION') or
                (BaseType = 'REAL') then
        begin
          TmpFloat := StrToFloat(edDefault.Text);
          FieldDef := FieldDef + ' DEFAULT ' + StringReplace(FloatToStrF(TmpFloat, ffGeneral, 15, 0), ',', '.', [rfReplaceAll]);
        end
        else if (BaseType = 'CHAR') or (BaseType = 'VARCHAR') or (BaseType = 'CSTRING') then
        begin
          FieldDef := FieldDef + ' DEFAULT ' + QuotedStr(edDefault.Text);
        end
        else
        begin
          // Default fallback for other types
          FieldDef := FieldDef + ' DEFAULT ' + edDefault.Text;
        end;
      except
        on E: Exception do
        begin
          raise Exception.Create('Invalid default value for field type "' + BaseType + '": ' + E.Message);
        end;
      end;
    end;

    // NOT NULL
    if not cxAllowNull.Checked then
      FieldDef := FieldDef + ' NOT NULL';

    Line := 'ALTER TABLE ' + FTableName + ' ADD ' + edFieldName.Text + ' ' + FieldDef + ';';

    fmMain.ShowCompleteQueryWindow(FDBIndex, 'Add new field to Table: ' + FTableName, Line, Clk);
  end
  {else  // Existierendes Feld bearbeiten
  begin
    Line := '';
    BaseType := cbType.Text;

    // Name geändert?
    if UpperCase(Trim(edFieldName.Text)) <> OldFieldName then
      Line := Line + 'ALTER TABLE ' + FTableName + ' ALTER ' + OldFieldName + ' TO ' + edFieldName.Text + ';' + LineEnding;

    // Typ geändert?
    if (cbType.Text <> OldFieldType) or
       (seSize.Value <> OldFieldSize) or
       (sePrecision.Value <> OldFieldPrecision) or
       (seScale.Value <> OldFieldScale) then
    begin
      Line := Line + 'ALTER TABLE ' + FTableName + ' ALTER ' + edFieldName.Text + ' TYPE ';

      if BaseType = 'UUID' then
      begin
        Line := Line + 'CHAR(16) CHARACTER SET OCTETS';
      end else
      begin
        Line := Line + cbType.Text;

        if (cbType.Text = 'NUMERIC') or (cbType.Text = 'DECIMAL') then
          Line := Line + '(' + IntToStr(sePrecision.Value) + ',' + IntToStr(seScale.Value) + ')'
        else if (cbType.Text = 'CHAR') or (cbType.Text = 'CSTRING') or (cbType.Text = 'VARCHAR') then
          Line := Line + '(' + IntToStr(seSize.Value) + ')';
      end;

      Line := Line + ';' + LineEnding;
    end;

    // Charset / Collation geändert?
    if (NewCharset <> OldCharacterSet) or (NewCollation <> OldCollation) then
    begin
      TempFieldName := edFieldName.Text + '_NEW';

      if BaseType = 'UUID' then
        FieldDef := 'CHAR(16) CHARACTER SET OCTETS'
      else
      begin
        FieldDef := cbType.Text;
        if (FieldDef = 'CHAR') or (FieldDef = 'CSTRING') or (FieldDef = 'VARCHAR') then
          FieldDef := FieldDef + '(' + IntToStr(seSize.Value) + ')';

        if NewCharset <> '' then
          FieldDef := FieldDef + ' CHARACTER SET ' + NewCharset;
        if NewCollation <> '' then
          FieldDef := FieldDef + ' COLLATE ' + NewCollation;
      end;

      Line := Line + '-- Charset or Collation change requires field recreation:' + LineEnding;
      Line := Line + 'ALTER TABLE ' + FTableName + ' ADD ' + TempFieldName + ' ' + FieldDef + ';' + LineEnding;
      Line := Line + 'UPDATE ' + FTableName + ' SET ' + TempFieldName + ' = ' + edFieldName.Text + ';' + LineEnding;
      Line := Line + 'ALTER TABLE ' + FTableName + ' DROP ' + edFieldName.Text + ';' + LineEnding;
      Line := Line + 'ALTER TABLE ' + FTableName + ' ALTER ' + TempFieldName + ' TO ' + edFieldName.Text + ';' + LineEnding;
    end;

    // Feldposition
    if seOrder.Value <> OldOrder then
    begin
      Line := Line + 'ALTER TABLE ' + FTableName + ' ALTER ' + edFieldName.Text +
              ' POSITION ' + IntToStr(seOrder.Value) + ';' + LineEnding;
    end;

    // NOT NULL
    if cxAllowNull.Checked <> OldAllowNull then
    begin
      if cxAllowNull.Checked then
        Line := Line + 'ALTER TABLE ' + FTableName + ' ALTER ' + edFieldName.Text + ' DROP NOT NULL;' + LineEnding
      else
        Line := Line + 'ALTER TABLE ' + FTableName + ' ALTER ' + edFieldName.Text + ' SET NOT NULL;' + LineEnding;
    end;

    // Default-Wert
    if edDefault.Text <> OldDefault then
    begin
      if Trim(edDefault.Text) <> '' then
        Line := Line + 'ALTER TABLE ' + FTableName + ' ALTER ' + edFieldName.Text +
                ' SET DEFAULT ' + QuotedStr(edDefault.Text) + ';' + LineEnding
      else
        Line := Line + 'ALTER TABLE ' + FTableName + ' ALTER ' + edFieldName.Text +
                ' DROP DEFAULT;' + LineEnding;
    end;

    // Beschreibung
    if edDescription.Text <> OldDescription then
    begin
      Line := Line + 'COMMENT ON COLUMN ' + FTableName + '.' + edFieldName.Text +
              ' IS ' + QuotedStr(edDescription.Text) + ';' + LineEnding;
    end;

    if Line <> '' then
      fmMain.ShowCompleteQueryWindow(FDBIndex, 'Edit field: ' + OldFieldName, Line, Clk);
  end;}
  else  // Existierendes Feld bearbeiten
  begin
    Line := '';
    BaseType := cbType.Text;

    // Name geändert?
    if UpperCase(Trim(edFieldName.Text)) <> OldFieldName then
      Line := Line + 'ALTER TABLE ' + FTableName + ' ALTER ' + OldFieldName + ' TO ' + edFieldName.Text + ';' + LineEnding;

    // Typ geändert?
    if (cbType.Text <> OldFieldType) or
       (seSize.Value <> OldFieldSize) or
       (sePrecision.Value <> OldFieldPrecision) or
       (seScale.Value <> OldFieldScale) then
    begin
      Line := Line + 'ALTER TABLE ' + FTableName + ' ALTER ' + edFieldName.Text + ' TYPE ';

      if BaseType = 'UUID' then
        Line := Line + 'CHAR(16) CHARACTER SET OCTETS'
      else
      begin
        Line := Line + cbType.Text;

        if (cbType.Text = 'NUMERIC') or (cbType.Text = 'DECIMAL') then
          Line := Line + '(' + IntToStr(sePrecision.Value) + ',' + IntToStr(seScale.Value) + ')'
        else if (cbType.Text = 'CHAR') or (cbType.Text = 'CSTRING') or (cbType.Text = 'VARCHAR') then
          Line := Line + '(' + IntToStr(seSize.Value) + ')';

        if NewCharset <> '' then
          Line := Line + ' CHARACTER SET ' + NewCharset;

        if NewCollation <> '' then
          Line := Line + ' COLLATE ' + NewCollation;
      end;

      Line := Line + ';' + LineEnding;
    end;

    // Charset / Collation geändert?
    if (NewCharset <> OldCharacterSet) or (NewCollation <> OldCollation) then
    begin
      TempFieldName := edFieldName.Text + '_NEW';

      if BaseType = 'UUID' then
        FieldDef := 'CHAR(16) CHARACTER SET OCTETS'
      else
      begin
        FieldDef := cbType.Text;
        if (FieldDef = 'CHAR') or (FieldDef = 'CSTRING') or (FieldDef = 'VARCHAR') then
          FieldDef := FieldDef + '(' + IntToStr(seSize.Value) + ')';

        if NewCharset <> '' then
          FieldDef := FieldDef + ' CHARACTER SET ' + NewCharset;

        if NewCollation <> '' then
          FieldDef := FieldDef + ' COLLATE ' + NewCollation;
      end;

      Line := Line + '-- Charset or Collation change requires field recreation:' + LineEnding;
      Line := Line + 'ALTER TABLE ' + FTableName + ' ADD ' + TempFieldName + ' ' + FieldDef + ';' + LineEnding;
      Line := Line + 'UPDATE ' + FTableName + ' SET ' + TempFieldName + ' = ' + edFieldName.Text + ';' + LineEnding;
      Line := Line + 'ALTER TABLE ' + FTableName + ' DROP ' + edFieldName.Text + ';' + LineEnding;
      Line := Line + 'ALTER TABLE ' + FTableName + ' ALTER ' + TempFieldName + ' TO ' + edFieldName.Text + ';' + LineEnding;
    end;

    // Feldposition
    if seOrder.Value <> OldOrder then
    begin
      Line := Line + 'ALTER TABLE ' + FTableName + ' ALTER ' + edFieldName.Text +
              ' POSITION ' + IntToStr(seOrder.Value) + ';' + LineEnding;
    end;

    // NOT NULL
    if cxAllowNull.Checked <> OldAllowNull then
    begin
      if cxAllowNull.Checked then
        Line := Line + 'ALTER TABLE ' + FTableName + ' ALTER ' + edFieldName.Text + ' DROP NOT NULL;' + LineEnding
      else
        Line := Line + 'ALTER TABLE ' + FTableName + ' ALTER ' + edFieldName.Text + ' SET NOT NULL;' + LineEnding;
    end;

    // Default value
    if edDefault.Text <> OldDefault then
    begin
      if Trim(edDefault.Text) <> '' then
      begin
        try
          if (cbType.Text = 'INTEGER') or (cbType.Text = 'SMALLINT') or (cbType.Text = 'BIGINT') then
          begin
            TmpInt := StrToInt(edDefault.Text);
            Line := Line + 'ALTER TABLE ' + FTableName + ' ALTER ' + edFieldName.Text +
                    ' SET DEFAULT ' + IntToStr(TmpInt) + ';' + LineEnding;
          end
          else if (cbType.Text = 'NUMERIC') or (cbType.Text = 'DECIMAL') or
                  (cbType.Text = 'FLOAT') or (cbType.Text = 'DOUBLE PRECISION') or
                  (cbType.Text = 'REAL') then
          begin
            TmpFloat := StrToFloat(edDefault.Text);
            Line := Line + 'ALTER TABLE ' + FTableName + ' ALTER ' + edFieldName.Text +
                    ' SET DEFAULT ' + StringReplace(FloatToStrF(TmpFloat, ffGeneral, 15, 0), ',', '.', [rfReplaceAll]) + ';' + LineEnding;
          end
          else if (cbType.Text = 'CHAR') or (cbType.Text = 'VARCHAR') or (cbType.Text = 'CSTRING') then
          begin
            Line := Line + 'ALTER TABLE ' + FTableName + ' ALTER ' + edFieldName.Text +
                    ' SET DEFAULT ' + QuotedStr(edDefault.Text) + ';' + LineEnding;
          end
          else
          begin
            // Fallback for other types without validation
            Line := Line + 'ALTER TABLE ' + FTableName + ' ALTER ' + edFieldName.Text +
                    ' SET DEFAULT ' + edDefault.Text + ';' + LineEnding;
          end;
        except
          on E: Exception do
            raise Exception.Create('Invalid default value for type "' + cbType.Text + '": ' + E.Message);
        end;
      end
      else
        Line := Line + 'ALTER TABLE ' + FTableName + ' ALTER ' + edFieldName.Text +
                ' DROP DEFAULT;' + LineEnding;
    end;

    // Beschreibung
    if edDescription.Text <> OldDescription then
    begin
      Line := Line + 'COMMENT ON COLUMN ' + FTableName + '.' + edFieldName.Text +
              ' IS ' + QuotedStr(edDescription.Text) + ';' + LineEnding;
    end;

    if Line <> '' then
      fmMain.ShowCompleteQueryWindow(FDBIndex, 'Edit field: ' + OldFieldName, Line, Clk);
  end;

  Close;
end;

procedure TfmNewEditField.cbCharsetEditingDone(Sender: TObject);
var
  Collations: TStringList;
begin
  // Available collations depend on the chosen character set,
  // so update that whenever user changes character set
  Collations:= TStringList.Create;
  try
    GetCollations(cbCharSet.Text,Collations);
    cbCollation.Items.Assign(Collations);
    cbCollation.ItemIndex := 0;

  finally
    Collations.Free;
  end;
end;

procedure TfmNewEditField.cbTypeChange(Sender: TObject);
begin
  seSize.Value:= dmSysTables.GetDefaultTypeSize(FDBIndex, cbType.Text);
  EnableDisableControls;
end;

procedure TfmNewEditField.cbTypeEditingDone(Sender: TObject);
begin
  seSize.Value:= dmSysTables.GetDefaultTypeSize(FDBIndex, cbType.Text);

  {todo: (low priority) allow/disallow gui elements when using domain datatypes.
   Check what can be overridden (e.g. collate for text-type domain fields)}
  // Allow character set, lblCollation for text type fields; otherwise disable
{  case cbType.Text of
    'CHAR','CSTRING','VARCHAR':
    begin
      // Allow character set/lblCollation for text type fields
      cbCharset.Enabled:= true;
      cbCollation.Enabled:= true;
      seScale.Enabled:= false;
    end;
    'DECIMAL','NUMERIC':
    begin
      // Allow scale for numeric, decimal
      seScale.Enabled:= true;
      cbCharset.Enabled:= false;
      cbCollation.Enabled:= false;
    end
    else
    begin
      cbCharset.Enabled:= false;
      cbCollation.Enabled:= false;
      seScale.Enabled:= false;
    end;
  end;
}
end;

procedure TfmNewEditField.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
end;

procedure TfmNewEditField.FormCreate(Sender: TObject);
begin
  // Load available character sets
  // todo: (low priority) character sets should be retrieved from current database server
  CbCharSet.Items.AddStrings(FBCharacterSets);
  // Do not set a default value, but leave it empty, because specifying a charset
  // is not mandatory - it should only be done if the charset differs from the
  // db charset
end;

procedure TfmNewEditField.EnableDisableControls;
var
  FieldType: string;
  FieldSize: integer;
  FieldCharSet: string;
  IsTextType,
  IsNumericType,
  IsScaledType,
  IsUUID: boolean;
begin
  // Bestimme den aktuell gewählten Datentyp (Großbuchstaben für Vergleich)
  FieldType := Trim(UpperCase(cbType.Text));
  IsTextType := (FieldType = 'CHAR') or (FieldType = 'VARCHAR');

  // Reihenfolge evtl. fix, aber meist bearbeitbar
  seOrder.Enabled := True; // oder abhängig von Logik

  // Field name darf nur im Neuanlage-Modus geändert werden
  edFieldName.Enabled := FFormMode = foNew;
  // Beschreibung und Default-Wert sind immer editierbar

  edDescription.Enabled := True;
  edDefault.Enabled := True;
  cxAllowNull.Enabled := True;

  // Größe nur bei CHAR, VARCHAR und evtl. bei BLOB subtypes sinnvoll
  seSize.Enabled := IsTextType;

  // Charset und Collation nur bei TEXT-Typen
  IsUUID := (FieldType = 'UUID'); // ((FieldType = 'CHAR') and (FieldSize = 16) and (FieldCharSet = 'OCTETS'));
  cbCharset.Enabled := (IsTextType or (IsUUID and (FFormMode = foNew)));
  if cbCharset.Enabled and IsUUID then
  begin
    cbCharSet.ItemIndex := cbCharSet.Items.IndexOf('OCTETS');
    cbCharSet.Enabled := false;
  end;

  cbCollation.Enabled := IsTextType;

  // Typgruppen bestimmen
  //IsNumericType := (FieldType = 'INTEGER') or (FieldType = 'BIGINT') or (FieldType = 'SMALLINT');
  IsScaledType := ((FieldType = 'NUMERIC') or (FieldType = 'DECIMAL'));

  // Scale nur bei NUMERIC, DECIMAL, FLOAT, DOUBLE
  sePrecision.Enabled := IsScaledType;
  seScale.Enabled := IsScaledType;

  FieldSize    := seSize.Value;
  FieldCharSet := cbCharSet.Text;

  cbCollation.Enabled := ((cbCharSet.Enabled) and  (not IsUUID));
end;

procedure TfmNewEditField.Init(dbIndex: Integer; TableName: string;
  FormMode: TFormMode;
  FieldName, FieldType,
  CharacterSet, Collation,
  DefaultValue, Description: string;
  Size, Precision, Scale, Order: integer;
  AllowNull: Boolean;
  RefreshButton: TBitBtn);
begin
  fFormMode:= FormMode;
  seScale.MaxValue := Abs(Scale);
  cbType.Clear;

  // Load basic datatypes for fields into combobox....
  dmSysTables.GetAllTypes(cbType.Items);

  // ... add domain types for fields
  dmSysTables.GetDomainTypes(dbIndex, cbType.Items);

  FDBIndex:= dbIndex;
  FTableName:= TableName;

  FRefreshButton:= RefreshButton;

  OldFieldName:= FieldName;
  OldFieldSize:= Size;
  OldFieldPrecision := Precision;
  OldFieldScale:= Scale;
  OldFieldType:= FieldType;
  OldAllowNull:= AllowNull;
  cxAllowNull.Checked:= OldAllowNull;
  OldOrder:= Order;
  seOrder.Value:= OldOrder;
  OldDefault:= DefaultValue;
  edDefault.Text:= OldDefault;
  OldCharacterSet:= CharacterSet;
  cbCharset.ItemIndex := cbCharset.items.IndexOf(OldCharacterSet);
  cbCharsetEditingDone(nil); //fill collation combobox
  OldCollation:= Collation;
  if OldCharacterSet <> 'OCTETS' then
    cbCollation.ItemIndex := cbCollation.items.IndexOf(OldCollation)
  else
   cbCollation.ItemIndex := -1;
  OldDescription:= Description;
  edDescription.Text:= OldDescription;
  edFieldName.Text:= OldFieldName;
  seSize.Value:= OldFieldSize;
  sePrecision.Value := Precision;
  seScale.Value := Abs(Scale);

  if IsSizedTypeName(OldFieldType) then
    cbType.ItemIndex := cbType.Items.IndexOf(GetNameFromSizedTypeName(OldFieldType))
  else
    cbType.ItemIndex := cbType.Items.IndexOf(OldFieldType);

  if FFormMode = foEdit then
  begin
    cbType.Enabled := false;
    if cbType.Text = 'UUID' then
      cbCharSet.Enabled := false;
  end else
  begin
   cbType.Enabled := true;
  end;

  if FormMode = foEdit then
  begin
    bbAdd.Caption:= 'Update';
    Caption:= 'Edit field: ' + FieldName + ' on : ' + TableName;
  end
  else
  begin
    bbAdd.Caption:= 'Add';
    Caption:= 'Add new field in : ' + TableName;
  end;
  EnableDisableControls;
end;

initialization
  {$I neweditfield.lrs}

end.

