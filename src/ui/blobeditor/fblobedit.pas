unit fblobedit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  ComCtrls, StdCtrls, Menus, Grids, Math, SynEdit, ATBinHex, HtmlView, GifAnim,
  RichView, PtblRV, AbZBrows, IBDynamicGrid, DB, IBDatabase, IBQuery, IBTable,
  CADSys4, turbocommon, DBGrids, DBCtrls, LCLIntf; // LCLIntf für OpenDocument

type

  TBlobFormat = (bfUnknown, bfText, bfBinHex, bfHTML, bfImage, bfPDF, bfGIF, bfRTF, bfArchive, bfCAD);

  { TfrmBlobEdit }

  TfrmBlobEdit = class(TForm)
    AbZipBrowser1: TAbZipBrowser;
    ATBinHex1: TATBinHex;
    bbClose: TSpeedButton;
    btnLoadFromFile: TButton;
    btnSaveToFile: TButton;
    CADCmp2D1: TCADCmp2D;
    CADViewport2D1: TCADViewport2D;
    GifAnim1: TGifAnim;
    HtmlViewer1: THtmlViewer;
    Image1: TImage;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    pnlTop: TPanel;
    Panel13: TPanel;
    PopupMenu1: TPopupMenu;
    RichView1: TRichView;
    RVPrint1: TRVPrint;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;
    SynEdit1: TSynEdit;
    tsCAD: TTabSheet;
    tsArchive: TTabSheet;
    tsText: TTabSheet;
    tsRTF: TTabSheet;
    tsGIF: TTabSheet;
    tsPDF: TTabSheet;
    tsImage: TTabSheet;
    tsHTML: TTabSheet;
    tsBinHex: TTabSheet;
    procedure bbCloseClick(Sender: TObject);
    procedure btnLoadFromFileClick(Sender: TObject);
    procedure btnSaveToFileClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure tsGIFResize(Sender: TObject);
  private
    FNodeInfos: TPNodeInfos;
    FSelectedField: TField;
    FSelectedFieldBlobFormat: TBlobFormat;
    procedure ShowBlobField;
    function  GetBlobFormatFromStream(AStream: TMemoryStream): TBlobFormat;
    function  BlobFormatToFileExtension(ABlobFormat: TBlobFormat): string;
    procedure OpenTextBlob;
    procedure OpenBinHexBlob;
    //procedure OpenHTMLBlob;
    ///procedure OpenImageBlob;
    //procedure OpenPDFBlob;
    procedure OpenGIFBlob;
    //procedure OpenArchiveBlob;
    //procedure OpenCADBlob;

    function  CreateTempFileName(const AExt: string): string;
    procedure ShowSelectedTab(ABlobFormat: TBlobFormat);
  public
    //function IsPDF(ALoadedStream: TMemoryStream): boolean;
    function IsGIF(ALoadedStream: TMemoryStream): boolean;
    //function IsJPEG(ALoadedStream: TMemoryStream): boolean;
    //function IsPNG(ALoadedStream: TMemoryStream): boolean;
    //function IsHTML(ALoadedStream: TMemoryStream): boolean;
    //function IsRTF(ALoadedStream: TMemoryStream): boolean;
    //function IsZIP(ALoadedStream: TMemoryStream): boolean;
    //function IsCAD(ALoadedStream: TMemoryStream): boolean;


    procedure Init(ABlobField: TField; ANodeInfos: TPNodeInfos);
  end;

var
  MemoryStream: TMemoryStream;

implementation

{$R *.lfm}

{ TfrmBlobEdit }

procedure TfrmBlobEdit.FormCreate(Sender: TObject);
begin
  MemoryStream := TMemoryStream.Create;
end;

procedure TfrmBlobEdit.Init(ABlobField: TField; ANodeInfos: TPNodeInfos);
begin
  Randomize;
  FNodeInfos   := ANodeInfos;
  if Assigned(ABlobField) then
  begin
    FSelectedField :=  ABlobField;
    MemoryStream.Clear;
    TBlobField(FSelectedField).SaveToStream(MemoryStream);
    MemoryStream.Position := 0;
    FSelectedFieldBlobFormat := GetBlobFormatFromStream(MemoryStream);
    MemoryStream.Position := 0;
    ShowSelectedTab(FSelectedFieldBlobFormat);
    ShowBlobField;
  end;
end;

procedure TfrmBlobEdit.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(MemoryStream) then
    MemoryStream.Free;
  // Hinweis: FNodeInfos ist ein Pointer (wie von dir vorgegeben). Stelle sicher,
  // dass er gültig ist und das Feld ViewForm existiert.
  if Assigned(FNodeInfos) then
    FNodeInfos^.ViewForm := nil;
  // Parent ist TabSheet in deinem vorherigen Code; das freigeben sorgt für sauberes Entfernen
  //if Assigned(Parent) and (Parent is TTabSheet) then
    //TTabSheet(Parent).Free;
  CloseAction := caFree;
end;

procedure TfrmBlobEdit.tsGIFResize(Sender: TObject);
begin
  GifAnim1.Top    := 0;
  GifAnim1.Left   := 0;
  GifAnim1.Width  := tsGIF.Width;
  GifAnim1.Height := tsGIF.Height;
end;

procedure TfrmBlobEdit.bbCloseClick(Sender: TObject);
begin
  Close;
  Parent.Free;
end;

procedure TfrmBlobEdit.btnLoadFromFileClick(Sender: TObject);
var
  filename: string;
  BlobFormat: TBlobFormat;
begin
  if not Assigned(FSelectedField) then Exit;

  // FileDialog vorschlagen, Filter abhängig vom Tab
  case PageControl1.ActivePageIndex of
    0: filename := 'Select text file (*.txt)|*.txt';
    1: filename := 'All files (*.*)|*.*';
    2: filename := 'Select HTML file (*.html)|*.html';
    3: filename := 'Select image (*.png;*.jpg)|*.png;*.jpg';
    4: filename := 'Select PDF file (*.pdf)|*.pdf';
    5: filename := 'Select GIF file (*.gif)|*.gif';
    6: filename := 'Select RTF file (*.rtf)|*.rtf';
    7: filename := 'Select ZIP file (*.zip)|*.zip';
    8: filename := 'Select DXF file (*.dxf)|*.dxf';
  else
    filename := 'All files (*.*)|*.*';
  end;

  if not OpenDialog1.Execute then Exit;

  try

    FSelectedField.DataSet.Edit;
    MemoryStream.LoadFromFile(OpenDialog1.FileName);
    MemoryStream.Position := 0;

    TBlobField(FSelectedField).LoadFromStream(MemoryStream);

    MemoryStream.Position := 0;
    BlobFormat := GetBlobFormatFromStream(MemoryStream);
    ShowSelectedTab(BlobFormat);
  finally
    FSelectedField.DataSet.Post;
  end;

  ShowBlobField; // neu laden, damit SynEdit/Image etc. aktualisiert werden
end;

procedure TfrmBlobEdit.btnSaveToFileClick(Sender: TObject);
var
  filename: string;
begin
  if not Assigned(FSelectedField) then Exit;

  // FileDialog vorschlagen, Filter abhängig vom Tab

  filename := BlobFormatToFileExtension(FSelectedFieldBlobFormat);
  if not SaveDialog1.Execute then Exit;

  try
    MemoryStream.Clear;
    TBlobField(FSelectedField).SaveToStream(MemoryStream);
    MemoryStream.Position := 0;
    MemoryStream.SaveToFile(SaveDialog1.FileName);
  finally
  end;
end;

function TfrmBlobEdit.GetBlobFormatFromStream(AStream: TMemoryStream): TBlobFormat;
begin
  Result := bfUnknown;
  if not Assigned(FSelectedField) then Exit;

  // Für ftBlob oder ftGraphic oder ftTypedBinary prüfen wir die Signatur
  if FSelectedField.DataType in [ftBlob, ftGraphic, ftTypedBinary] then
  begin

    try
      if FSelectedField is TBlobField then

      else
        Exit; // kein Stream vorhanden

      AStream.Position := 0;

      // Zuerst komplexe Typen prüfen
      //if IsPDF(AStream)  then
        //Exit(bfPDF);
      if IsGIF(AStream)  then
        Exit(bfGIF);
      //if IsJPEG(AStream) then
        //Exit(bfImage);
      //if IsPNG(AStream)  then
        //Exit(bfImage);
      //if IsHTML(AStream) then
        //Exit(bfHTML);
      //if IsRTF(AStream)  then
        //Exit(bfRTF);
      //if IsZIP(AStream)  then
        //Exit(bfArchive);
      //if IsCAD(AStream)  then
        //Exit(bfCAD);

      // alles andere → BinHex
      Result := bfBinHex;

    finally

    end;

    Exit;
  end;

  // Einfachste Typen danach
  if FSelectedField.DataType in [ftMemo, ftFmtMemo, ftWideMemo] then
  begin
    Result := bfText;
    Exit;
  end;

  if FSelectedField.DataType = ftGraphic then
  begin
    Result := bfImage;
    Exit;
  end;

  if FSelectedField.DataType = ftTypedBinary then
  begin
    Result := bfBinHex;
    Exit;
  end;

  // alles andere
  Result := bfUnknown;
end;

procedure TfrmBlobEdit.ShowBlobField;
var BlobFormat: TBlobFormat;
begin
  BlobFormat := GetBlobFormatFromStream(MemoryStream);
  case BlobFormat of
    bfText    :OpenTextBlob;
    bfBinHex  :OpenBinHexBlob;
    //bfHTML    :OpenHTMLBlob;
    //bfImage   :OpenImageBlob;
    //bfPDF     :OpenPDFBlob;
    bfGIF     :OpenGIFBlob;
    //bfArchive :OpenArchiveBlob;
    //bfCAD     :OpenCADBlob;
    bfUnknown : begin
      // Fallback: zeige als BinHex
      OpenBinHexBlob;
    end;
  end;
end;

procedure TfrmBlobEdit.OpenTextBlob;
var
  s: TStringList;
begin
  try
    MemoryStream.Clear;
    TBlobField(FSelectedField).SaveToStream(MemoryStream);
    MemoryStream.Position := 0;

    s := TStringList.Create;
    try
      // probiere zuerst UTF-8, FPC kann die Erkennung automatisch machen, aber wir versuchen
      // LoadFromStream ohne Encoding (wird von TStringList erkannt falls BOM vorhanden)
      s.LoadFromStream(MemoryStream);
      SynEdit1.Lines.Assign(s);
      PageControl1.ActivePage := tsText;
      tsText.TabVisible := true;
      tsText.Visible := true;
    finally
      s.Free;
    end;
  finally

  end;
end;

procedure TfrmBlobEdit.OpenBinHexBlob;
begin

  try
    MemoryStream.Clear;
    TBlobField(FSelectedField).SaveToStream(MemoryStream);
    MemoryStream.Position := 0; // wichtig!

    ATBinHex1.OpenStream(MemoryStream);

    PageControl1.ActivePage := tsBinHex;
    tsBinHex.TabVisible := true;
    tsBinHex.Visible := true;

    // Hinweis: ATBinHex1.OpenStream erwartet den Stream während der Lebensdauer des Controls.
    // In vielen Implementationen kopiert die Komponente aber den Stream-Inhalt intern.
    // Falls ATBinHex1 wirklich den Stream später noch benötigt, müsstest Du den Stream
    // als Feld der Form speichern und ihn beim Schließen freigeben.
  except
    on E: Exception do
    begin
      // Fallback: falls OpenStream fehlschlägt, zeige als Text (evtl. leere)
      ShowMessage('Fehler beim Öffnen als BinHex: ' + E.Message);
    end;
  end;
end;

{procedure TfrmBlobEdit.OpenHTMLBlob;
var
  blobStream: TMemoryStream;
begin
  blobStream := TMemoryStream.Create;
  try
    TBlobField(FSelectedField).SaveToStream(blobStream);
    blobStream.Position := 0;

    try
      // THtmlViewer unterstützt LoadFromStream
      HtmlViewer1.LoadFromStream(blobStream);
      PageControl1.ActivePage := tsHTML;
      tsHTML.TabVisible := true;
      tsHTML.Visible := true;
    except
      on E: Exception do
      begin
        // Fallback: zeige als Text
        blobStream.Position := 0;
        SynEdit1.Lines.LoadFromStream(blobStream);
        PageControl1.ActivePage := tsText;
        tsText.TabVisible := true;
        tsText.Visible := true;
      end;
    end;
  finally
    blobStream.Free;
  end;
end;

procedure TfrmBlobEdit.OpenImageBlob;
var
  blobStream: TMemoryStream;
begin
  blobStream := TMemoryStream.Create;
  try
    TBlobField(FSelectedField).SaveToStream(blobStream);
    blobStream.Position := 0;
    try
      Image1.Picture.LoadFromStream(blobStream);
      Image1.Stretch := True;
      PageControl1.ActivePage := tsImage;
      tsImage.TabVisible := true;
      tsImage.Visible := true;
    except
      on E: Exception do
      begin
        // Falls Laden als Bild fehlschlägt, zeige als BinHex
        blobStream.Position := 0;
        ATBinHex1.OpenStream(blobStream);
        PageControl1.ActivePage := tsBinHex;
        tsBinHex.TabVisible := true;
        tsBinHex.Visible := true;
      end;
    end;
  finally
    blobStream.Free;
  end;
end;

procedure TfrmBlobEdit.OpenPDFBlob;
var
  blobStream: TMemoryStream;
  tempName: string;
begin
  blobStream := TMemoryStream.Create;
  try
    TBlobField(FSelectedField).SaveToStream(blobStream);
    blobStream.Position := 0;

    tempName := CreateTempFileName('.pdf');
    blobStream.SaveToFile(tempName);

    // Versuche, die PDF extern zu öffnen (plattformübergreifend)
    OpenDocument(tempName);

    // Markiere das Tab sichtbar (In diesem Fall wird die PDF extern geöffnet,
    // innerhalb der Form wird meist kein PDF-Viewer angezeigt)
    PageControl1.ActivePage := tsPDF;
    tsPDF.TabVisible := true;
    tsPDF.Visible := true;
    StatusBar1.SimpleText := 'PDF in externem Viewer geöffnet: ' + tempName;
  finally
    blobStream.Free;
  end;
end;

}

procedure TfrmBlobEdit.OpenGIFBlob;
var TempName: string;
begin
  // Eindeutigen Dateinamen erzeugen
  TempName := IncludeTrailingPathDelimiter(TmpDir) +
              'gif_' + IntToStr(GetTickCount) + '.gif';

  ForceDirectories(TmpDir); // sicherstellen, dass temp-Verzeichnis existiert

  try
    // BLOB ins MemoryStream laden
    TBlobField(FSelectedField).SaveToStream(MemoryStream);
    MemoryStream.Position := 0;
    // Stream in Datei schreiben
    MemoryStream.SaveToFile(TempName);
    MemoryStream.Position := 0;
  finally
  end;

  // GIF in Anzeige-Komponente laden
  GifAnim1.FileName := TempName;
  GifAnim1.Animate := True;

  // Tab sichtbar machen
  PageControl1.ActivePage := tsGIF;
  tsGIF.TabVisible := True;
  tsGIF.Visible := True;
end;

{
procedure TfrmBlobEdit.OpenArchiveBlob;
var
  blobStream: TMemoryStream;
  tempName: string;
begin
  blobStream := TMemoryStream.Create;
  try
    TBlobField(FSelectedField).SaveToStream(blobStream);
    blobStream.Position := 0;

    // Speichere in temporäre Zip-Datei und versuche, sie mit dem System-Handler zu öffnen.
    tempName := CreateTempFileName('.zip');
    blobStream.SaveToFile(tempName);

    // Wenn AbZipBrowser1 eine Methode hat um Dateien zu öffnen, könnte man versuchen:
    // AbZipBrowser1.ArchiveName := tempName; AbZipBrowser1.Open; // <-- evtl. komponentenspezifisch
    // Um Kompatibilitätsprobleme zu vermeiden, öffnen wir die Datei extern:
    OpenDocument(tempName);

    PageControl1.ActivePage := tsArchive;
    tsArchive.TabVisible := true;
    tsArchive.Visible := true;
    StatusBar1.SimpleText := 'Archiv in externem Programm geöffnet: ' + tempName;
  finally
    blobStream.Free;
  end;
end;

procedure TfrmBlobEdit.OpenCADBlob;
var
  blobStream: TMemoryStream;
  tempName: string;
begin
  blobStream := TMemoryStream.Create;
  try
    TBlobField(FSelectedField).SaveToStream(blobStream);
    blobStream.Position := 0;

    // Versuch: falls CAD-Komponente Dateien laden kann, könnte man sie hier direkt füttern.
    // Da wir das konkrete API nicht 100% annehmen können, speichere temporär und öffne extern.
    tempName := CreateTempFileName('.dxf'); // dxf als häufiger CAD-Export, evtl anpassen
    blobStream.SaveToFile(tempName);

    // Versuche internes Laden (falls Komponenten-API vorhanden). Falls nicht, extern öffnen:
    try
      // Beispiel: falls CADCmp2D1 eine LoadFromFile hat:
      // CADCmp2D1.LoadFromFile(tempName);
      // PageControl1.ActivePage := tsCAD;
      // tsCAD.TabVisible := true;
      // tsCAD.Visible := true;

      // Da das API variieren kann, fallback auf externes öffnen:
      OpenDocument(tempName);
      PageControl1.ActivePage := tsCAD;
      tsCAD.TabVisible := true;
      tsCAD.Visible := true;
      StatusBar1.SimpleText := 'CAD-Datei extern geöffnet: ' + tempName;
    except
      // Falls internes Laden fehlschlägt, öffne extern
      OpenDocument(tempName);
    end;
  finally
    blobStream.Free;
  end;
end;

}
function TfrmBlobEdit.CreateTempFileName(const AExt: string): string;
var
  base: string;
begin
  base := GetTempDir(false);
  if (base = '') then
    base := ExtractFilePath(Application.ExeName);
  Result := IncludeTrailingPathDelimiter(base) + 'blob_' + IntToStr(Random(MaxInt)) + AExt;
end;
{
function TfrmBlobEdit.IsPDF(ALoadedStream: TMemoryStream): boolean;
var
  buffer: array[0..3] of Byte;
  readBytes: LongInt;
begin
  Result := False;
  if not Assigned(ALoadedStream) then Exit;
  ALoadedStream.Position := 0;
  readBytes := ALoadedStream.Read(buffer, SizeOf(buffer));
  if readBytes < 4 then Exit;
  Result := (buffer[0] = $25) and (buffer[1] = $50) and
            (buffer[2] = $44) and (buffer[3] = $46); // "%PDF"
end;
}
function TfrmBlobEdit.IsGIF(ALoadedStream: TMemoryStream): boolean;
var
  buffer: array[0..2] of Byte;
  readBytes: LongInt;
begin
  Result := False;
  if not Assigned(ALoadedStream) then Exit;
  ALoadedStream.Position := 0;
  readBytes := ALoadedStream.Read(buffer, SizeOf(buffer));
  if readBytes < 3 then
    Exit;
  Result := (buffer[0] = $47) and (buffer[1] = $49) and (buffer[2] = $46); // "GIF"
end;
{
function TfrmBlobEdit.IsJPEG(ALoadedStream: TMemoryStream): boolean;
var
  buffer: array[0..1] of Byte;
  readBytes: LongInt;
begin
  Result := False;
  if not Assigned(ALoadedStream) then Exit;
  ALoadedStream.Position := 0;
  readBytes := ALoadedStream.Read(buffer, SizeOf(buffer));
  if readBytes < 2 then Exit;
  Result := (buffer[0] = $FF) and (buffer[1] = $D8);
end;

function TfrmBlobEdit.IsPNG(ALoadedStream: TMemoryStream): boolean;
var
  buffer: array[0..3] of Byte;
  readBytes: LongInt;
begin
  Result := False;
  if not Assigned(ALoadedStream) then Exit;
  ALoadedStream.Position := 0;
  readBytes := ALoadedStream.Read(buffer, SizeOf(buffer));
  if readBytes < 4 then Exit;
  Result := (buffer[0] = $89) and (buffer[1] = $50) and
            (buffer[2] = $4E) and (buffer[3] = $47);
end;

function TfrmBlobEdit.IsHTML(ALoadedStream: TMemoryStream): boolean;
var
  buffer: array[0..15] of Byte;
  readBytes: LongInt;
  s: string;
begin
  Result := False;
  if not Assigned(ALoadedStream) then Exit;
  ALoadedStream.Position := 0;
  readBytes := ALoadedStream.Read(buffer, SizeOf(buffer));
  if readBytes = 0 then Exit;
  SetString(s, PChar(@buffer[0]), Min(readBytes, Length(buffer)));
  s := LowerCase(Trim(s));
  Result := (Length(s) > 0) and (s[1] = '<');
end;

function TfrmBlobEdit.IsRTF(ALoadedStream: TMemoryStream): boolean;
var
  buffer: array[0..3] of Byte;
  readBytes: LongInt;
  s: string;
begin
  Result := False;
  if not Assigned(ALoadedStream) then Exit;
  ALoadedStream.Position := 0;
  readBytes := ALoadedStream.Read(buffer, SizeOf(buffer));
  if readBytes < 4 then Exit;
  SetString(s, PChar(@buffer[0]), readBytes);
  s := Trim(s);
  // RTF-Dateien beginnen normalerweise mit "{\rtf"
  Result := Copy(s,1,5) = '{\rtf';
end;

function TfrmBlobEdit.IsZIP(ALoadedStream: TMemoryStream): boolean;
var
  buffer: array[0..3] of Byte;
  readBytes: LongInt;
begin
  Result := False;
  if not Assigned(ALoadedStream) then Exit;
  ALoadedStream.Position := 0;
  readBytes := ALoadedStream.Read(buffer, SizeOf(buffer));
  if readBytes < 4 then Exit;
  // ZIP-Dateien beginnen mit PK 0x03 0x04
  Result := (buffer[0] = $50) and (buffer[1] = $4B) and
            (buffer[2] = $03) and (buffer[3] = $04);
end;

function TfrmBlobEdit.IsCAD(ALoadedStream: TMemoryStream): boolean;
var
  buffer: array[0..4] of Byte;
  readBytes: LongInt;
  s: string;
begin
  Result := False;
  if not Assigned(ALoadedStream) then Exit;
  ALoadedStream.Position := 0;
  readBytes := ALoadedStream.Read(buffer, SizeOf(buffer));
  if readBytes = 0 then Exit;
  SetString(s, PChar(@buffer[0]), readBytes);
  s := Trim(s);
  // DXF-Dateien beginnen normalerweise mit "0\nSECTION"
  Result := Copy(s,1,9) = '0\nSECTION';
end;
}
}}
procedure TfrmBlobEdit.ShowSelectedTab(ABlobFormat: TBlobFormat);
begin
  // Alle Tabs unsichtbar machen
  tsText.TabVisible := False;
  tsBinHex.TabVisible := False;
  tsHTML.TabVisible := False;
  tsImage.TabVisible := False;
  tsPDF.TabVisible := False;
  tsGIF.TabVisible := False;
  tsRTF.TabVisible := False;
  tsArchive.TabVisible := False;
  tsCAD.TabVisible := False;

  tsText.Visible := False;
  tsBinHex.Visible := False;
  tsHTML.Visible := False;
  tsImage.Visible := False;
  tsPDF.Visible := False;
  tsGIF.Visible := False;
  tsRTF.Visible := False;
  tsArchive.Visible := False;
  tsCAD.Visible := False;

  // Passendes Tab aktivieren
  case ABlobFormat of
    bfText:
      begin
        tsText.TabVisible := True;
        tsText.Visible := True;
        PageControl1.ActivePage := tsText;
      end;
    bfBinHex:
      begin
        tsBinHex.TabVisible := True;
        tsBinHex.Visible := True;
        PageControl1.ActivePage := tsBinHex;
      end;
    bfHTML:
      begin
        tsHTML.TabVisible := True;
        tsHTML.Visible := True;
        PageControl1.ActivePage := tsHTML;
      end;
    bfImage:
      begin
        tsImage.TabVisible := True;
        tsImage.Visible := True;
        PageControl1.ActivePage := tsImage;
      end;
    bfPDF:
      begin
        tsPDF.TabVisible := True;
        tsPDF.Visible := True;
        PageControl1.ActivePage := tsPDF;
      end;
    bfGIF:
      begin
        tsGIF.TabVisible := True;
        tsGIF.Visible := True;
        PageControl1.ActivePage := tsGIF;
      end;
    bfRTF:
      begin
        tsRTF.TabVisible := True;
        tsRTF.Visible := True;
        PageControl1.ActivePage := tsRTF;
      end;
    bfArchive:
      begin
        tsArchive.TabVisible := True;
        tsArchive.Visible := True;
        PageControl1.ActivePage := tsArchive;
      end;
    bfCAD:
      begin
        tsCAD.TabVisible := True;
        tsCAD.Visible := True;
        PageControl1.ActivePage := tsCAD;
      end;
    bfUnknown: ; // nichts anzeigen
  end;
end;

function TfrmBlobEdit.BlobFormatToFileExtension(ABlobFormat: TBlobFormat): string;
begin
  case ABlobFormat of
    bfText:     Result := '*.txt';
    bfBinHex:   Result := '*.*';
    bfHTML:     Result := '*.html;*.htm';
    bfImage:    Result := '*.png;*.jpg;*.jpeg;*.bmp';
    bfPDF:      Result := '*.pdf';
    bfGIF:      Result := '*.gif';
    bfRTF:      Result := '*.rtf';
    bfArchive:  Result := '*.zip';
    bfCAD:      Result := '*.dxf';
  else
    Result := '*.*'; // Fallback für unbekannte Formate
  end;
end;


end.

