unit importtable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ExtCtrls, Grids, SynEdit, SynHighlighterSQL,
  turbocommon, fileimport,
  IB, IBDatabase, IBQuery,
  uthemeselector;

type

  { TfmImportTable }

  TfmImportTable = class(TForm)
    bbImport: TBitBtn;
    btnAddMapping: TButton;
    btnDeleteMapping: TButton;
    btnPrepare: TButton;
    btnSourceFileOpen: TButton;
    btnClose: TButton;
    cbDestField: TComboBox;
    cbSourceField: TComboBox;
    chkSkipFirstRow: TCheckBox;
    chkTabDelimiter: TCheckBox;
    dlgSourceOpen: TOpenDialog;
    edSourceFile: TEdit;
    edDelimiter: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    MappingGrid: TStringGrid;
    MappingPanel: TPanel;
    Panel2: TPanel;
    pnlBottom: TPanel;
    SourcePanel: TPanel;
    procedure bbImportClick(Sender: TObject);
    procedure btnAddMappingClick(Sender: TObject);
    procedure btnDeleteMappingClick(Sender: TObject);
    procedure btnPrepareClick(Sender: TObject);
    procedure btnSourceFileOpenClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure chkTabDelimiterEditingDone(Sender: TObject);
    procedure edDelimiterEditingDone(Sender: TObject);
    procedure edSourceFileEditingDone(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FNodeInfos: TPNodeInfos;
    FDestinationQuery: TIBQuery;
    FImporter: TFileImport;
    FDestDB: string; //destination database
    FDestTable: string; //destination table
    FDestIndex: Integer; //index of destination database
    // Load source and destination fields in mapping comboboxes
    procedure LoadMappingCombos;
    // Opens destination table query
    procedure OpenDestinationTable;
    // Load/update mapping grid
    procedure UpdateMappingGrid;

    procedure ResetImporter;
    procedure PrepareInsertSQLFromMapping;

    { private declarations }
  public
    { public declarations }
    procedure Init(DestinationIndex: Integer; DestinationTableName: string; ANodeInfos: TPNodeInfos);
  end; 

//var
  //fmImportTable: TfmImportTable;

implementation

{ TfmImportTable }

uses SysTables, EnterPass, Reg;


procedure TfmImportTable.edSourceFileEditingDone(Sender: TObject);
begin
  if edSourceFile.Text='' then exit;
  try
    FImporter.FileName:=edSourceFile.Text;
    if FImporter.Delimiter = #9 then //tab
    begin
      chkTabDelimiter.Checked:=true;
      edDelimiter.Text:='<TAB>';
      edDelimiter.Enabled:=false;
    end
    else
    begin
      edDelimiter.Text:=FImporter.Delimiter;
      edDelimiter.Enabled:=true;
    end;
  except
    edDelimiter.Enabled:=true;
    edDelimiter.Text:='';
    chkTabDelimiter.Checked:=false;
  end;
end;

procedure TfmImportTable.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if Assigned(FNodeInfos) then
    FNodeInfos^.ViewForm := nil;

  FImporter.Free;
  if assigned(FDestinationQuery) then
    FreeAndNil(FDestinationQuery);
end;

procedure TfmImportTable.FormCreate(Sender: TObject);
begin
  FImporter:=TFileImport.Create;
end;

procedure TfmImportTable.FormDestroy(Sender: TObject);
begin
end;

procedure TfmImportTable.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(self);
end;

procedure TfmImportTable.LoadMappingCombos;
var
  i: integer;
begin
  cbSourceField.Clear;
  cbDestField.Clear;
  for i:=0 to FImporter.SourceFields.Count-1 do
  begin
    cbSourceField.Items.Add(FImporter.SourceFields[i]);
  end;
  if cbSourceField.Items.Count > -1 then
    cbSourceField.ItemIndex := 0;

  if assigned(FDestinationQuery) then
  begin
    if not(FDestinationQuery.Active) then FDestinationQuery.Open;
    for i:=0 to FDestinationQuery.FieldCount-1 do
    begin
      cbDestField.Items.Add(FDestinationQuery.Fields[i].FieldName);
    end;
  end;
  if cbDestField.Items.Count > -1 then
    cbDestField.ItemIndex := 0;
end;

{procedure TfmImportTable.OpenDestinationTable;
var
  i: Integer;
  Statement: string;
  Num: Integer;
  ServerName: string;
begin
  // Enter password if it is not saved
  with RegisteredDatabases[FDestIndex] do
  begin
    // If client/server password is empty, get it from user:
    //if (IBConnection.HostName <>'') and
      //(IBConnection.Password = '') then
    ServerName := GetServerName(RegRec.DatabaseName);
    if  ((ServerName <> '') and (ServerName <> 'localhost')) and  (RegRec.Password = '') then
    begin
      if fmEnterPass.ShowModal = mrOk then
      begin
        if fmReg.TestConnection(RegRec.DatabaseName, fmEnterPass.edUser.Text, fmEnterPass.edPassword.Text,
          RegRec.Charset) then
        begin
          RegisteredDatabases[FDestIndex].RegRec.UserName:= fmEnterPass.edUser.Text;
          RegisteredDatabases[FDestIndex].RegRec.Password:= fmEnterPass.edPassword.Text;
          RegisteredDatabases[FDestIndex].RegRec.Role:= fmEnterPass.cbRole.Text;
        end
        else exit;
      end;
    end;
    if not(assigned(FDestinationQuery)) then
      FDestinationQuery:=TIBQuery.Create(nil);
    FDestinationQuery.Close;
    FDestinationQuery.DataBase:=IBConnection;
    FDestinationQuery.Transaction:=SQLTrans;
    FDestinationQuery.ParseSQL; //belts and braces - generate InsertSQL
    FDestinationQuery.SQL.Text:='select * from ' + FDestTable;
    FDestinationQuery.Open;
  end;
end;}

procedure TfmImportTable.OpenDestinationTable;
var
  ServerName: string;
begin
  // Enter password if it is not saved
  with RegisteredDatabases[FDestIndex] do
  begin
    // Prüfen, ob Passwort für Client/Server leer ist
    ServerName := GetServerName(RegRec.DatabaseName);
    if ((ServerName <> '') and (ServerName <> 'localhost')) and (RegRec.Password = '') then
    begin
      if fmEnterPass.ShowModal = mrOk then
      begin
        if fmReg.TestConnection(RegRec.DatabaseName, fmEnterPass.edUser.Text, fmEnterPass.edPassword.Text,
          RegRec.Charset, RegRec.FireBirdClientLibPath, RegRec.SQLDialect, RegRec.Port, RegRec.ServerName, RegRec.OverwriteLoadedClientLib) then
        begin
          RegisteredDatabases[FDestIndex].RegRec.UserName := fmEnterPass.edUser.Text;
          RegisteredDatabases[FDestIndex].RegRec.Password := fmEnterPass.edPassword.Text;
          RegisteredDatabases[FDestIndex].RegRec.Role := fmEnterPass.cbRole.Text;
        end
        else
          Exit;
      end;
    end;

    // IBQuery anlegen, falls noch nicht vorhanden
    if not Assigned(FDestinationQuery) then
      FDestinationQuery := TIBQuery.Create(nil);

   FDestinationQuery.AllowAutoActivateTransaction := true;

    // Verbindung und Transaktion zuweisen
    FDestinationQuery.Close;
    FDestinationQuery.Database := IBDatabase;
    FDestinationQuery.Transaction := IBTransaction;

    // SQL vorbereiten
    FDestinationQuery.SQL.Text := 'SELECT * FROM ' + FDestTable;

    // Tabelle öffnen
    FDestinationQuery.Open;
  end;
end;

procedure TfmImportTable.UpdateMappingGrid;
var
  i: integer;
  MappingCount: integer;
begin
  // MappingCount will map fields if necessary so we need destination fields
  if FImporter.DestinationFields.Count=0 then
  begin
    if not(assigned(FDestinationQuery)) then
      raise Exception.Create('Cannot update mapping info without valid destination query.');
    MappingCount := FDestinationQuery.Fields.Count;
    for i := 0 to MappingCount - 1 do
    begin
      FImporter.DestinationFields.Add(FDestinationQuery.Fields[i].FieldName);
    end;
  end;

  MappingCount:=FImporter.MappingCount;
  MappingGrid.RowCount:=MappingCount;
  for i:=0 to MappingCount-1 do
  begin
    MappingGrid.Cells[0,i]:=FImporter.Mapping[i].SourceField;
    MappingGrid.Cells[1,i]:=FImporter.Mapping[i].DestinationField;
    {
    // Grid InsertRowWithValues ssems to fail if there are no columns or rows
    // even if there are it doesn't always work
    MappingGrid.InsertRowWithValues(0,
      [FImporter.Mapping[i].SourceField,
      FImporter.Mapping[i].DestinationField]);
    }
  end;
end;


procedure TfmImportTable.btnAddMappingClick(Sender: TObject);
begin
  if FImporter.AddMapping(cbSourceField.Text, cbDestField.Text) then
    UpdateMappingGrid;
end;

procedure TfmImportTable.btnDeleteMappingClick(Sender: TObject);
begin
  // Delete mapping for selected destination field
  if FImporter.DeleteMapping(MappingGrid.Cells[1,MappingGrid.Row]) then
    UpdateMappingGrid;
end;

{procedure TfmImportTable.btnPrepareClick(Sender: TObject);
begin
  // Only try if valid import file specified
  if (FImporter.FileName<>'') and
    (FImporter.Delimiter<>#0) then
  begin
    OpenDestinationTable;
    LoadMappingCombos;
    UpdateMappingGrid;
  end;
end; }

procedure TfmImportTable.ResetImporter;
var
  FileName: string;
  Delim: Char;
begin
  if not Assigned(FImporter) then Exit;

  FileName := FImporter.FileName;
  Delim := FImporter.Delimiter;

  FreeAndNil(FImporter);
  FImporter := TFileImport.Create;

  FImporter.FileName := FileName;
  FImporter.Delimiter := Delim;
end;


procedure TfmImportTable.btnPrepareClick(Sender: TObject);
begin
  // Grundprüfungen
  if FImporter.FileName = '' then
  begin
    MessageDlg('No source file selected.', mtWarning, [mbOK], 0);
    Exit;
  end;

  if FImporter.Delimiter = #0 then
  begin
    MessageDlg('No delimiter specified.', mtWarning, [mbOK], 0);
    Exit;
  end;

  ResetImporter;

  try
    // Ziel-Tabelle öffnen (nur für Feldinfos)
    OpenDestinationTable;

    // Mapping-Combos neu laden
    LoadMappingCombos;

    // Mapping-Grid aktualisieren
    UpdateMappingGrid;

    // INSERT-Statement aus Mapping erzeugen
    PrepareInsertSQLFromMapping;

    MessageDlg('Preparation successful.', mtInformation, [mbOK], 0);
  except
    on E: Exception do
      MessageDlg('Prepare failed: ' + E.Message, mtError, [mbOK], 0);
  end;
end;


procedure TfmImportTable.btnSourceFileOpenClick(Sender: TObject);
begin
  if dlgSourceOpen.Execute then
    edSourceFile.Text:=dlgSourceOpen.FileName;
  // Process delimiters etc
  edSourceFileEditingDone(Sender);
end;

procedure TfmImportTable.btnCloseClick(Sender: TObject);
begin
  Close;
  Parent.Free;
end;

procedure TfmImportTable.chkTabDelimiterEditingDone(Sender: TObject);
begin
  if chkTabDelimiter.Checked then
  begin
    edDelimiter.Text:='<TAB>';
    edDelimiter.Enabled:=false;
    FImporter.Delimiter:=#9;
  end
  else
  begin
    edDelimiter.Enabled:=true;
    FImporter.Delimiter:=#0; //more or less a placeholder
  end;
end;

procedure TfmImportTable.edDelimiterEditingDone(Sender: TObject);
begin
  case length(edDelimiter.Text) of
    0: FImporter.Delimiter:=#0; //placeholder
    1: FImporter.Delimiter:=edDelimiter.Text[1];
    else
    begin
      ShowMessage('Delimiter must be 1 character only.');
      exit;
    end;
  end;
end;

procedure TfmImportTable.PrepareInsertSQLFromMapping;
var
  i: Integer;
  FieldList: string;
  ParamList: string;
begin
  if not Assigned(FDestinationQuery) then
    raise Exception.Create('DestinationQuery not assigned');

  if FImporter.MappingCount = 0 then
    raise Exception.Create('No field mapping defined');

  FieldList := '';
  ParamList := '';

  for i := 0 to FImporter.MappingCount - 1 do
  begin
    if i > 0 then
    begin
      FieldList := FieldList + ', ';
      ParamList := ParamList + ', ';
    end;

    FieldList := FieldList + FImporter.Mapping[i].DestinationField;
    ParamList := ParamList + ':' + FImporter.Mapping[i].DestinationField;
  end;

  FDestinationQuery.Close;
  FDestinationQuery.SQL.Clear;
  FDestinationQuery.SQL.Text :=
    'INSERT INTO ' + FDestTable +
    ' (' + FieldList + ') VALUES (' + ParamList + ')';

  FDestinationQuery.Prepare;
end;

procedure TfmImportTable.bbImportClick(Sender: TObject);
var
  DestColumn: string;
  i: Integer;
  Num: Integer;
  ServerName: string;
begin
  if not Assigned(FDestinationQuery) then
  begin
    MessageDlg('Import not prepared.', mtWarning, [mbOK], 0);
    Exit;
  end;

  Screen.Cursor := crHourGlass;
  btnClose.Enabled := False;
  bbImport.Enabled := False;

  try
    // Passwort prüfen (Client/Server)
    with RegisteredDatabases[FDestIndex] do
    begin
      ServerName := GetServerName(RegRec.DatabaseName);
      if ((ServerName <> '') and (ServerName <> 'localhost')) and
         (RegRec.Password = '') then
      begin
        if fmEnterPass.ShowModal <> mrOk then
          Exit;

        if not fmReg.TestConnection(
          RegRec.DatabaseName,
          fmEnterPass.edUser.Text,
          fmEnterPass.edPassword.Text,
          RegRec.Charset,
          RegRec.FireBirdClientLibPath,
          RegRec.SQLDialect,
          RegRec.Port,
          RegRec.ServerName,
          RegRec.OverwriteLoadedClientLib
        ) then
          Exit;

        RegRec.UserName := fmEnterPass.edUser.Text;
        RegRec.Password := fmEnterPass.edPassword.Text;
        RegRec.Role     := fmEnterPass.cbRole.Text;
      end;
    end;

    // Skip Header-Zeile
    if chkSkipFirstRow.Checked then
      FImporter.ReadRow;

    // Transaktion sauber starten
    if RegisteredDatabases[FDestIndex].IBTransaction.InTransaction then
      RegisteredDatabases[FDestIndex].IBTransaction.Rollback;

    RegisteredDatabases[FDestIndex].IBTransaction.StartTransaction;

    Num := 0;

    try
      while FImporter.ReadRow do
      begin
        for i := 0 to FImporter.MappingCount - 1 do
        begin
          DestColumn := FImporter.Mapping[i].DestinationField;

          // CSV liefert Strings – Firebird konvertiert
          FDestinationQuery.ParamByName(DestColumn).AsString :=
            FImporter.GetData(i);
        end;

        FDestinationQuery.ExecSQL;
        Inc(Num);

        // Performance-Commit
        if (Num mod 1000) = 0 then
          RegisteredDatabases[FDestIndex].IBTransaction.CommitRetaining;
      end;

      RegisteredDatabases[FDestIndex].IBTransaction.Commit;

      Screen.Cursor := crDefault;
      MessageDlg(IntToStr(Num) + ' record(s) imported successfully.',
                 mtInformation, [mbOK], 0);
    except
      on E: Exception do
      begin
        RegisteredDatabases[FDestIndex].IBTransaction.Rollback;
        raise;
      end;
    end;

  except
    on E: Exception do
      MessageDlg('Import failed: ' + E.Message, mtError, [mbOK], 0);
  end;

  btnClose.Enabled := True;
  bbImport.Enabled := True;
  Screen.Cursor := crDefault;
end;


{procedure TfmImportTable.bbImportClick(Sender: TObject);
var
  DestColumn: string;
  i: Integer;
  Num: Integer;
  ServerName: string;
begin
  if not(assigned(FDestinationQuery)) and (FDestinationQuery.Active=false) then
    exit; //no destination fields

  Screen.Cursor:=crHourGlass;
  btnClose.Enabled:=false;
  bbImport.Enabled:=false;
  try
    // Skip first row if necessary
    if chkSkipFirstRow.Checked then
    begin
      if not(FImporter.ReadRow) then
        exit; //error: end of file?
    end;

    // Enter password if it is not saved... and we're not connected to an embedded
    // database
    with RegisteredDatabases[FDestIndex] do
    begin
      //if (RegisteredDatabases[cbDestDatabase.ItemIndex].IBConnection.HostName<>'') and (IBConnection.Password = '') then
      ServerName := GetServerName(RegRec.DatabaseName);
      if  ((ServerName <> '') and (ServerName <> 'localhost')) and  (RegRec.Password = '') then
      begin
        if fmEnterPass.ShowModal = mrOk then
        begin
          if fmReg.TestConnection(RegRec.DatabaseName, fmEnterPass.edUser.Text, fmEnterPass.edPassword.Text,
            RegRec.Charset, RegRec.FireBirdClientLibPath, RegRec.SQLDialect, RegRec.Port, RegRec.ServerName, RegRec.OverwriteLoadedClientLib) then
          begin
              RegisteredDatabases[FDestIndex].RegRec.UserName:= fmEnterPass.edUser.Text;
              RegisteredDatabases[FDestIndex].RegRec.Password:= fmEnterPass.edPassword.Text;
              RegisteredDatabases[FDestIndex].RegRec.Role:= fmEnterPass.cbRole.Text;
          end
          else exit;
        end;
      end;

      // Start import
      if IBTransaction.InTransaction then
        IBTransaction.RollBack;

      IBTransaction.StartTransaction;

      FDestinationQuery.Open;
      Num:=0;
      try
        while FImporter.ReadRow do
        begin
          FDestinationQuery.Insert;
          for I:=0 to FImporter.MappingCount-1 do
          begin
            DestColumn:=FImporter.Mapping[I].DestinationField;
            // Note: csv import sees everything as strings so let the db convert if possible
            FDestinationQuery.Fields.FieldByName(DestColumn).AsString:=FImporter.GetData(i);
          end;
          FDestinationQuery.Post;
          Inc(Num);
        end;
        FDestinationQuery.ApplyUpdates;
        // could be also done after e.g. every 1000 records for
        // higher performance
        IBTransaction.Commit;
        FDestinationQuery.Close;
        Screen.Cursor:=crDefault; // for message
        ShowMessage(IntToStr(Num) + ' record(s) have been imported');
      except
        on E: Exception do
        begin
          MessageDlg('Error while importing: ' + e.Message, mtError, [mbOk], 0);
          IBTransaction.Rollback;
        end;
      end;
    end;
  finally
    btnClose.Enabled:=true;
    bbImport.Enabled:=true;
    Screen.Cursor:=crDefault;
  end;
end;}

procedure TfmImportTable.Init(DestinationIndex: Integer; DestinationTableName: string; ANodeInfos: TPNodeInfos);
var
  i: Integer;
  Count: Integer;
begin
  FNodeInfos := ANodeInfos;
  FDestIndex:=DestinationIndex;
  FDestDB := RegisteredDatabases[FDestIndex].RegRec.Title;
  FDestTable:=DestinationTableName;
  Caption:='Import '+FDestTable;
end;

initialization
  {$I ImportTable.lrs}

end.

