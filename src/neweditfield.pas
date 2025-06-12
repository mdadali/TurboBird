unit NewEditField;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, Buttons, turbocommon;

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
  Line: string;
  Nullflag: string;
  Clk: TNotifyEvent;
begin
  if FRefreshButton = nil then
    clk:= nil
  else
    clk:= FRefreshButton.OnClick;

  if fFormMode = foNew then  // New field
  begin
    Line:= cbType.Text;
    if (Line='CHAR') or
      (Line='CSTRING') or
       (Line='VARCHAR') then
      Line:= Line + '(' + IntToStr(seSize.Value) + ')';

    if (Line='DECIMAL') or
       (Line='NUMERIC') then
       Line:= Line + '(' + IntToStr(sePrecision.Value) + ',' +
         IntToStr(seScale.Value)+');' + LineEnding;

    // Default value
    if Trim(edDefault.Text) <> '' then
    begin
      if ((cbType.Text='CHAR') or
        (cbType.Text='CSTRING') or
        (cbType.Text='VARCHAR')) and
        (Pos('''', edDefault.Text) = 0) then
        Line:= Line + ' default ' + QuotedStr(edDefault.Text)
      else
        Line:= Line + ' default ' + edDefault.Text;
    end;

    // Null/Not null
    if not cxAllowNull.Checked then
      Line:= Line + ' not null';

    fmMain.ShowCompleteQueryWindow(FDBIndex, 'Add new field to Table: ' + FTableName,
      'ALTER TABLE ' + FTableName +
      ' ADD ' + edFieldName.Text + ' ' + Line,
      Clk);
  end
  else  // Update
  begin
    Line:= '';
    // Check name change
    if UpperCase(Trim(edFieldName.Text)) <> OldFieldName then
      Line:= 'ALTER TABLE ' + FTableName + ' ALTER ' + OldFieldName + ' TO ' +
      edFieldName.Text + ';' + LineEnding;

    // Check type/size/scale change
    if (cbType.Text <> OldFieldType) or
      (seSize.Value <> OldFieldSize) or
      (sePrecision.Value <> OldFieldPrecision) or
      (seScale.Value <> OldFieldScale) then
    begin
      Line:= Line + 'ALTER TABLE ' + FTableName +
        ' ALTER ' + UpperCase(Trim(edFieldName.Text)) +
        ' TYPE ' + cbType.Text;

      if (cbType.Text='NUMERIC') or
       (cbType.Text='DECIMAL') then
        Line:= Line + '(' + IntToStr(sePrecision.Value) + ',' +
          IntToStr(seScale.Value)+');' + LineEnding
      else
      if (cbType.Text='CHAR') or
        (cbType.Text='CSTRING') or
        (cbType.Text='VARCHAR') then
        Line:= Line + '(' + IntToStr(seSize.Value) + ');' + LineEnding;
    end;

    //Character set and collation
    Line:= Line + '-- warning: character set changed for field ' + edFieldName.Text + '. Please do this manually (e.g. with the fbclone tool)' + LineEnding;
    Line:= Line + '-- warning: collation changed for field ' + edFieldName.Text + '. Please do this manually (e.g. with the fbclone tool)' + LineEnding;

    // Field Order
    if seOrder.Value <> OldOrder then
    begin
      Line:= Line + 'ALTER TABLE ' + FTableName + ' ALTER ' + edFieldName.Text + ' POSITION ' +
        IntToStr(seOrder.Value) + ';' + LineEnding;
    end;

    // Allow Null
    if cxAllowNull.Checked <> OldAllowNull then
    begin
      if cxAllowNull.Checked then
        NullFlag:= 'NULL'
      else
        NullFlag:= '1';
        Line:= Line + 'UPDATE RDB$RELATION_FIELDS SET RDB$NULL_FLAG = ' + NullFlag + LineEnding +
          'WHERE RDB$FIELD_NAME = ' + QuotedStr(UpperCase(Trim(edFieldName.Text))) + ' ' +
          'AND RDB$RELATION_NAME = ' + QuotedStr(FTableName) + LineEnding;
    end;

    // Description
    if edDescription.Text <> OldDescription then
    begin
      Line:= Line + 'UPDATE RDB$RELATION_FIELDS ' +
        'set RDB$DESCRIPTION = ' + QuotedStr(edDescription.Text) + ' ' +
        'where RDB$FIELD_NAME = ' + QuotedStr(UpperCase(Trim(edFieldName.Text))) + ' ' +
        'and RDB$RELATION_NAME = ' + QuotedStr(FTableName) + ';' + LineEnding;
    end;

    // Default value
    if edDefault.Text <> OldDefault then
    begin
      Line:= Line + 'UPDATE RDB$RELATION_FIELDS set RDB$Default_Source = ''' + edDefault.Text +
        '''  where RDB$FIELD_NAME = ''' + UpperCase(Trim(edFieldName.Text)) +
        ''' and RDB$RELATION_NAME = ''' + FTableName + ''';' + LineEnding;
    end;

    if Line <> '' then
      fmMain.ShowCompleteQueryWindow(FDBIndex, 'Edit field: ' + OldFieldName, Line, clk);
  end;
  Close;
end;


procedure TfmNewEditField.bbAddClick(Sender: TObject);
var
  Line: string;
  Clk: TNotifyEvent;
begin
  if FRefreshButton = nil then
    Clk := nil
  else
    Clk := FRefreshButton.OnClick;

  if fFormMode = foNew then  // New field
  begin
    Line := cbType.Text;
    if (Line = 'CHAR') or (Line = 'CSTRING') or (Line = 'VARCHAR') then
      Line := Line + '(' + IntToStr(seSize.Value) + ')';

    if (Line = 'DECIMAL') or (Line = 'NUMERIC') then
      Line := Line + '(' + IntToStr(sePrecision.Value) + ',' + IntToStr(seScale.Value) + ')';

    // Default value
    if Trim(edDefault.Text) <> '' then
    begin
      if ((cbType.Text = 'CHAR') or (cbType.Text = 'CSTRING') or (cbType.Text = 'VARCHAR')) and
         (Pos('''', edDefault.Text) = 0) then
        Line := Line + ' DEFAULT ' + QuotedStr(edDefault.Text)
      else
        Line := Line + ' DEFAULT ' + edDefault.Text;
    end;

    // Null / Not null
    if not cxAllowNull.Checked then
      Line := Line + ' NOT NULL';

    fmMain.ShowCompleteQueryWindow(FDBIndex, 'Add new field to Table: ' + FTableName,
      'ALTER TABLE ' + FTableName + ' ADD ' + edFieldName.Text + ' ' + Line + ';',
      Clk);
  end
  else  // Edit existing field
  begin
    Line := '';

    // Feldname geändert?
    if UpperCase(Trim(edFieldName.Text)) <> OldFieldName then
      Line := Line + 'ALTER TABLE ' + FTableName + ' ALTER ' + OldFieldName + ' TO ' +
              edFieldName.Text + ';' + LineEnding;

    // Typ, Größe oder Skalierung geändert?
    if (cbType.Text <> OldFieldType) or
       (seSize.Value <> OldFieldSize) or
       (sePrecision.Value <> OldFieldPrecision) or
       (seScale.Value <> OldFieldScale) then
    begin
      Line := Line + 'ALTER TABLE ' + FTableName + ' ALTER ' +
              edFieldName.Text + ' TYPE ' + cbType.Text;

      if (cbType.Text = 'NUMERIC') or (cbType.Text = 'DECIMAL') then
        Line := Line + '(' + IntToStr(sePrecision.Value) + ',' + IntToStr(seScale.Value) + ')'
      else if (cbType.Text = 'CHAR') or (cbType.Text = 'CSTRING') or (cbType.Text = 'VARCHAR') then
        Line := Line + '(' + IntToStr(seSize.Value) + ')';

      Line := Line + ';' + LineEnding;
    end;

    // Zeichensatz / Collation Hinweis
    Line := Line + '-- warning: character set changed for field ' + edFieldName.Text + '. Please do this manually (e.g. with the fbclone tool)' + LineEnding;
    Line := Line + '-- warning: collation changed for field ' + edFieldName.Text + '. Please do this manually (e.g. with the fbclone tool)' + LineEnding;

    // Feldposition geändert?
    if seOrder.Value <> OldOrder then
    begin
      Line := Line + 'ALTER TABLE ' + FTableName + ' ALTER ' + edFieldName.Text +
              ' POSITION ' + IntToStr(seOrder.Value) + ';' + LineEnding;
    end;

    // NULL / NOT NULL geändert?
    if cxAllowNull.Checked <> OldAllowNull then
    begin
      if cxAllowNull.Checked then
        Line := Line + 'ALTER TABLE ' + FTableName + ' ALTER ' + edFieldName.Text + ' DROP NOT NULL;' + LineEnding
      else
        Line := Line + 'ALTER TABLE ' + FTableName + ' ALTER ' + edFieldName.Text + ' SET NOT NULL;' + LineEnding;
    end;

    // Standardwert geändert?
    if edDefault.Text <> OldDefault then
    begin
      if Trim(edDefault.Text) <> '' then
        Line := Line + 'ALTER TABLE ' + FTableName + ' ALTER ' + edFieldName.Text +
                ' SET DEFAULT ' + QuotedStr(edDefault.Text) + ';' + LineEnding
      else
        Line := Line + 'ALTER TABLE ' + FTableName + ' ALTER ' + edFieldName.Text +
                ' DROP DEFAULT;' + LineEnding;
    end;

    // Beschreibung geändert?
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
  IsTextType, IsNumericType, IsScaledType: Boolean;
begin
  // Bestimme den aktuell gewählten Datentyp (Großbuchstaben für Vergleich)
  FieldType := UpperCase(cbType.Text);
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
  cbCharset.Enabled := IsTextType;
  cbCollation.Enabled := IsTextType;

  // Typgruppen bestimmen
  //IsNumericType := (FieldType = 'INTEGER') or (FieldType = 'BIGINT') or (FieldType = 'SMALLINT');
  IsScaledType := ((FieldType = 'NUMERIC') or (FieldType = 'DECIMAL'));

  // Scale nur bei NUMERIC, DECIMAL, FLOAT, DOUBLE
  sePrecision.Enabled := IsScaledType;
  seScale.Enabled := IsScaledType;
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
  cbCollation.ItemIndex := cbCollation.items.IndexOf(OldCollation);
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
    cbType.Enabled := false
  else
   cbType.Enabled := true;

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

