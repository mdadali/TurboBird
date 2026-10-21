unit CreateDb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls,
  Graphics, Dialogs, StdCtrls, Buttons, ExtCtrls,
  IBDatabase, IB, IBQuery, FBIntf, IBXServices,
  turbocommon,
  uthemeselector;

type

  { TfmCreateDB }

  TfmCreateDB = class(TForm)
    bbCancel: TBitBtn;
    bbCreate: TBitBtn;
    bbReg: TBitBtn;
    btBrowse: TButton;
    cbCharset: TComboBox;
    chkboxReserveSpace: TCheckBox;
    chkboxForcedWrites: TCheckBox;
    cmbBoxServers: TComboBox;
    comboxPageSize: TComboBox;
    comboxSQLDialect: TComboBox;
    edtNumBuffers: TEdit;
    edtServerType: TEdit;
    edtRole: TEdit;
    edPassword: TEdit;
    edtSweepInterval: TEdit;
    edNewDatabase: TEdit;
    edtServerVersion: TEdit;
    edUserName: TEdit;
    grboxDatabase: TGroupBox;
    grboxServer: TGroupBox;
    grboxUser: TGroupBox;
    IBDatabase1: TIBDatabase;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    pnlTop: TPanel;
    pnlBottom: TPanel;
    SaveDialog1: TSaveDialog;
    procedure bbCreateClick(Sender: TObject);
    procedure bbRegClick(Sender: TObject);
    procedure btBrowseClick(Sender: TObject);
    procedure cmbBoxServersChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FUserName,
    FPassword,
    FCharSet,
    FClientLib,
    FDatabase,
    FPort: string;

    FServerRec: TServerRecord;

    procedure UpdateControlsForProtocol;
  public
    { public declarations }
     procedure Init;
  end; 

var
  fmCreateDB: TfmCreateDB;

implementation

uses Reg;

{ TfmCreateDB }

{ ============================================================
  UpdateControlsForProtocol: Enabled-Flags je nach Server-Typ.

  NEU:
    - Defensive Prüfung: Wenn FServerRec leer ist (Server nicht
      gefunden, z.B. nach fehlgeschlagenem Init), werden die
      Felder deaktiviert statt mit leerem Record zu arbeiten.
      Vorher hätte IsEmbedded auf einem leeren Record immer
      False ergeben — der User hätte Felder befüllen können,
      die später ins Leere laufen.

    - Erklärung der Logik:
        Embedded (Local): kein Passwort, keine Rolle nötig
                          (Firebird vertraut dem OS-User).
        TCP/Remote:       Passwort und Rolle aktiv.

    - edUserName bleibt bewusst immer enabled (unverändert
      zur alten Version). Der User kann auch bei Embedded
      einen anderen User angeben — Firebird ignoriert das
      zwar meist, aber es schadet nicht.
  ============================================================ }
procedure TfmCreateDB.UpdateControlsForProtocol;
var
  IsLocal: Boolean;
begin
  // Server nicht gefunden → Felder sperren statt blind zu arbeiten
  if Trim(FServerRec.ServerName) = '' then
  begin
    edPassword.Enabled := False;
    edtRole.Enabled    := False;
    Exit;
  end;

  IsLocal := FServerRec.IsEmbedded;

  // Passwort nur bei Remote-Verbindungen
  edPassword.Enabled := not IsLocal;
  if not edPassword.Enabled then
    edPassword.Text := '';

  // Rolle nur bei Remote-Verbindungen
  edtRole.Enabled := not IsLocal;
  if not edtRole.Enabled then
    edtRole.Text := '';
end;

procedure TfmCreateDB.btBrowseClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    edNewDatabase.Text:= SaveDialog1.FileName;
  end;
end;

procedure TfmCreateDB.cmbBoxServersChange(Sender: TObject);
begin
  Init;
end;

procedure TfmCreateDB.FormCreate(Sender: TObject);
begin
  // Load available character sets. Because we are not connected to a server,
  // we cannot retrieve the available charater sets.
  // Perhaps this can be done through the Services API but not high priority
  CbCharSet.Items.AddStrings(FBCharacterSets);
  CbCharSet.ItemIndex:=DefaultFBCharacterSet;
end;

{ ============================================================
  FormShow: Wird bei jedem ShowModal aufgerufen.

  NEU:
    - Bug behoben: FServerRec.Password := '' bei Embedded
      entfernt. Das war eine sinnlose Modifikation am lokalen
      Record — das Passwort im Edit-Feld wurde dadurch NICHT
      geleert (das macht UpdateControlsForProtocol), aber der
      Record im Speicher wurde verfälscht.

    - Bug behoben: edtRole wurde gar nicht aus dem Server-Record
      befüllt. Jetzt ja (bei TCP/Remote).

    - Defensive Prüfung: Wenn FServerRec leer ist (Server nicht
      gefunden, z.B. verwaiste Konfiguration), werden die Felder
      nicht mit leeren Werten überschrieben.

    - Reihenfolge: UpdateControlsForProtocol ZUERST, weil es
      edPassword und edtRole bei Embedded leert. Danach werden
      sie — nur bei TCP/Remote — aus dem Record befüllt.

  Was NICHT geändert wird:
    - Server-Felder wie FBClient oder Port. Die stehen bereits
      in IBDatabase1.FirebirdLibraryPathName (von Init gesetzt)
      und werden nicht separat im UI angezeigt.

  Verhalten bei erneutem Öffnen:
    FormShow läuft bei jedem ShowModal. Das ist gewollt: Wenn
    der User abbrechen, erneut öffnen, sollen die Felder
    wieder aus dem Record befüllt werden, nicht den alten
    manuellen Zustand zeigen.
  ============================================================ }
procedure TfmCreateDB.FormShow(Sender: TObject);
begin
  frmThemeSelector.btnApplyClick(self);

  UpdateControlsForProtocol;

  // User-Felder aus dem Server-Record befüllen
  if Trim(FServerRec.ServerName) <> '' then
  begin
    edUserName.Text := FServerRec.UserName;

    if not FServerRec.IsEmbedded then
    begin
      edPassword.Text := FServerRec.Password;
      edtRole.Text    := FServerRec.Role;
    end;
  end;

  edtServerVersion.Text := FServerRec.VersionString;
end;

{ ============================================================
  Init: Wird beim Öffnen der Maske UND bei jedem Serverwechsel
  (cmbBoxServersChange) aufgerufen.

  PageSize-ComboBox wird hier DYNAMISCH befüllt, je nach
  Firebird-Version des Servers:

    Eintrag 0: "Default (X)" — X ist der FB-Default für die
               erkannte Version (4096 für FB < 3.0, 8192 für
               FB 3.0+). Bei Auswahl wird PAGE_SIZE NICHT im
               CREATE DATABASE übergeben → Firebird nutzt
               seinen Default.

    Eintrag 1-3: 4096, 8192, 16384 — von allen FB-Versionen
               seit 2.1 unterstützt.

    Eintrag 4 (nur FB 4.0+): 32768 — erst ab FB 4.0 gültig,
               davor würde CREATE DATABASE mit Fehler abbrechen.

  Server-Lookup und Connection-String-Bau bleiben unverändert
  zur vorherigen Version.
  ============================================================ }
procedure TfmCreateDB.Init;
var
  ServerRec: TServerRecord;
  FBVersionMajor, DefaultPageSize: Integer;
begin
  ServerRec := GetServerRecordFromFileByAlias(Trim(cmbBoxServers.Text));

  if Trim(ServerRec.ServerName) = '' then
  begin
    FServerRec := ServerRec;
    Exit;
  end;

  FServerRec := ServerRec;

  IBDatabase1.FirebirdLibraryPathName := FServerRec.ClientLibraryPath;

  if FServerRec.IsEmbedded then
    IBDatabase1.DatabaseName := edNewDatabase.Text
  else
  begin
    if StrToIntDef(FServerRec.Port, 0) <> 0 then
      IBDatabase1.DatabaseName := FServerRec.ServerName + '/' +
                                  FServerRec.Port + ':' + edNewDatabase.Text
    else
      IBDatabase1.DatabaseName := FServerRec.ServerName + ':' + edNewDatabase.Text;
  end;

  FPort := FServerRec.Port;

  edtServerVersion.Text := FServerRec.VersionString;

  if Trim(FServerRec.Charset) <> '' then
    if cbCharset.Items.IndexOf(FServerRec.Charset) >= 0 then
      cbCharset.ItemIndex := cbCharset.Items.IndexOf(FServerRec.Charset);

  // ------------------------------------------------------------
  // PageSize-ComboBox dynamisch befüllen
  // ------------------------------------------------------------
  FBVersionMajor := FServerRec.VersionMajor;

  DefaultPageSize := 4096;
  if FBVersionMajor >= 3 then
    DefaultPageSize := 8192;

  comboxPageSize.Items.Clear;
  comboxPageSize.Items.Add('Default (' + IntToStr(DefaultPageSize) + ')');
  comboxPageSize.Items.Add('4096');
  comboxPageSize.Items.Add('8192');
  comboxPageSize.Items.Add('16384');
  if FBVersionMajor >= 4 then
    comboxPageSize.Items.Add('32768');

  comboxPageSize.ItemIndex := 0;  // Default vorauswählen

  // ------------------------------------------------------------
  // Verbindungsparameter für IBDatabase1 (nur für Create)
  // ------------------------------------------------------------
  IBDatabase1.Params.Clear;
  IBDatabase1.LoginPrompt := false;

  IBDatabase1.Params.Add('user_name=' + edUserName.Text);
  IBDatabase1.Params.Add('password=' + edPassword.Text);
  IBDatabase1.Params.Add('lc_ctype=' + cbCharset.Text);

  IBDatabase1.SQLDialect := StrToInt(Trim(comboxSQLDialect.Text));

  IBDatabase1.CreateIfNotExists := true;
end;

{ ============================================================
  bbCreateClick: Erstellt eine neue Firebird-Datenbank.

  WICHTIG:
    - PageSize-ComboBox Eintrag 0 ist "Default (X)".
      Bei Auswahl dieses Eintrags wird PAGE_SIZE NICHT ins
      CREATE DATABASE-Statement aufgenommen → Firebird nutzt
      seinen Versions-Default (4096 bei FB < 3.0, 8192 ab 3.0).

    - FORCE WRITE und NO RESERVE werden NICHT mehr verwendet,
      weil sie über die CREATE DATABASE-SQL-Syntax nicht
      zuverlässig funktionieren.

    - Die Erfolgsmeldung zeigt bei "Default" den Wert
      "(server default)" statt einer Zahl.
  ============================================================ }
procedure TfmCreateDB.bbCreateClick(Sender: TObject);
var
  TempDB: TIBDatabase;
  CreateSQL: string;
  DBName, Charset, PageSizeText: string;
  PageSize: Integer;
begin
  bbReg.Enabled := False;
  TempDB := nil;

  try
    Init;

    if Trim(FServerRec.ServerName) = '' then
    begin
      ShowMessage('Server not found: ' + cmbBoxServers.Text);
      Exit;
    end;

    DBName := IBDatabase1.DatabaseName;
    Charset := Trim(cbCharset.Text);
    if Charset = '' then
      Charset := 'UTF8';

    // PageSize: 0 = "Default" → PAGE_SIZE weglassen
    PageSize := 0;
    if comboxPageSize.ItemIndex > 0 then
    begin
      PageSize := StrToIntDef(Trim(comboxPageSize.Text), 0);
      if PageSize < 1024 then
        PageSize := 0;
    end;

    // --------------------------------------------------------
    // CREATE DATABASE SQL-Statement
    // --------------------------------------------------------
    CreateSQL := 'CREATE DATABASE ' + QuotedStr(DBName);
    CreateSQL := CreateSQL + ' USER ' + QuotedStr(edUserName.Text);
    if Trim(edPassword.Text) <> '' then
      CreateSQL := CreateSQL + ' PASSWORD ' + QuotedStr(edPassword.Text);
    if PageSize > 0 then
      CreateSQL := CreateSQL + ' PAGE_SIZE ' + IntToStr(PageSize);
    CreateSQL := CreateSQL + ' DEFAULT CHARACTER SET ' + Charset;

    // --------------------------------------------------------
    // Temporäre TIBDatabase zum Erstellen
    // --------------------------------------------------------
    TempDB := TIBDatabase.Create(nil);
    TempDB.FirebirdLibraryPathName := FServerRec.ClientLibraryPath;
    TempDB.LoginPrompt := False;
    TempDB.SQLDialect := StrToIntDef(Trim(comboxSQLDialect.Text), 3);

    TempDB.CreateDatabase(CreateSQL);

    if PageSize > 0 then
      PageSizeText := IntToStr(PageSize)
    else
      PageSizeText := '(server default)';

    ShowMessage('Database created successfully.' + sLineBreak +
                'Charset: ' + Charset + sLineBreak +
                'Page Size: ' + PageSizeText);

    bbReg.Enabled := True;

  except
    on E: Exception do
      ShowMessage('Error creating database:' + sLineBreak + E.Message);
  end;

  if Assigned(TempDB) then
  begin
    try
      if TempDB.Connected then
        TempDB.Connected := False;
    except
    end;
    TempDB.Free;
  end;
end;

{ ============================================================
  bbRegClick: Öffnet die DB-Registrierungs-Maske (fmReg) mit
  vorbefüllten Werten aus der gerade erstellten Datenbank.

  NEU:
    - ComboBox in fmReg wird mit ALIASEN gefüllt
      (GetServerAliasListFromTreeView statt GetServerListFromTreeView).
      Vorher wurden echte Servernamen gezeigt — inkonsistent zur
      TreeView und zum Rest der Maske.

    - Server-Auswahl in fmReg über DisplayAlias aus FServerRec
      (Alias mit Fallback auf echten Namen), nicht über
      cmbBoxServers.Text-IndexOf. Damit funktioniert die Auswahl
      auch dann, wenn zwischen CreateDB und fmReg-Öffnen
      irgendetwas den Anzeige-Namen verändert.

    - fmReg.cboxServers.Enabled := False — wie in mnRegDBClick.
      Der Server ist durch die CreateDB-Maske fest vorgegeben;
      der User soll ihn hier nicht mehr wechseln können.

    - fmReg.bbReg.Caption wird auf 'Register Database' gesetzt
      (konsistent zu mnRegDBClick).

  Was NICHT geändert wird:
    - ModalResult := fmReg.ShowModal — bleibt so, damit der
      Aufrufer in lmCreateDBClick (main.pas) den mrOK-Fall
      behandelt und die TreeView neu lädt.
  ============================================================ }
procedure TfmCreateDB.bbRegClick(Sender: TObject);
var
  AliasList: TStringList;
  FileNameOnly: string;
  DisplayAlias: string;
  Idx: Integer;
begin
  AliasList := GetServerAliasListFromTreeView;
  if AliasList = nil then
    Exit;

  try
    // fmReg-ComboBox mit ALIASEN füllen
    fmReg.cboxServers.Items.Assign(AliasList);

    // Deaktivieren — der Server ist durch die CreateDB-Maske
    // fest vorgegeben
    fmReg.cboxServers.Enabled := False;

    // Aktuellen Server (aus FServerRec) in der ComboBox auswählen
    DisplayAlias := Trim(FServerRec.ServerAlias);
    if DisplayAlias = '' then
      DisplayAlias := Trim(FServerRec.ServerName);

    Idx := fmReg.cboxServers.Items.IndexOf(DisplayAlias);
    if Idx >= 0 then
      fmReg.cboxServers.ItemIndex := Idx;

    // Prefill der restlichen Felder
    fmReg.edtPort.Text := FPort;
    fmReg.edtFBClient.Text := IBDatabase1.FirebirdLibraryPathName;

    FileNameOnly := edNewDatabase.Text;
    FileNameOnly := ExtractFileName(FileNameOnly);
    FileNameOnly := ChangeFileExt(FileNameOnly, '');
    fmReg.edTitle.Text := FileNameOnly;

    fmReg.edDatabaseName.Text := IBDatabase1.DatabaseName;
    fmReg.edtUserName.Text := edUserName.Text;
    fmReg.edtPassword.Text := edPassword.Text;
    fmReg.cbCharset.Text := cbCharset.Text;

    // Neue Registrierung
    fmReg.NewReg := True;
    fmReg.bbReg.Caption := 'Register Database';

    ModalResult := fmReg.ShowModal;
  finally
    AliasList.Free;
  end;
end;


initialization
  {$I createdb.lrs}

end.

