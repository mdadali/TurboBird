(********************************************************************************)
(*                        UNIFIED INTERBASE (UIB)                               *)
(*                                                                              *)
(* The contents of this file are subject to the Mozilla Public License Version  *)
(* 1.1 (the "License"); you may not use this file except in compliance with the *)
(* License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ *)
(*                                                                              *)
(* Software distributed under the License is distributed on an "AS IS" basis,   *)
(* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for *)
(* the specific language governing rights and limitations under the License.    *)
(*                                                                              *)
(* Unit owner : Henri Gourvest <hgourvest@progdigy.com>                         *)
(* contributor: Olivier Guilbaud <oguilb@free.fr>                               *)
(*                                                                              *)
(********************************************************************************)

{$I uib.inc}

unit uibconst;

interface

{$IFNDEF DELPHI6_UP}
{$IFNDEF BCB}
const
  S_OK    = $00000000;
  S_FALSE = $00000001;
{$ENDIF BCB}
{$ENDIF DELPHI6_UP}


type
  // uib Server Commands
  TServerCommand = (scGetClassObject, scInvokeMethod);

  // Metadata Object Identifiers
  TOIDDatabase = (OIDDomain, OIDTable, OIDView, OIDProcedure, OIDPackage,
    OIDGenerator, OIDException, OIDUDF, OIDRole, OIDDBCharset, OIDDependencies);
  TOIDDatabases = set of TOIDDatabase;

  TOIDTable = (OIDTableField, OIDPrimary, OIDForeign, OIDTableTrigger,
    OIDUnique, OIDIndex, OIDCheck, OIDTableGrant, OIDTableFieldGrant);
  TOIDTables = set of TOIDTable;

  TOIDView = (OIDViewField, OIDViewTrigers, OIDViewGrant, OIDViewFieldGrant);
  TOIDViews = set of TOIDView;

  TOIDProcedure = (OIDProcFieldIn, OIDProcFieldOut, OIDProcedureGrant);
  TOIDProcedures = set of TOIDProcedure;

  TOIDPackage = (OIDPackageProc, OIDPackageFunc);
  TOIDPackages = set of TOIDPackage;

  TOIDUDF = (OIDUDFField);
  TOIDUDFs = set of TOIDUDF;

  TOIDRole = (OIDRoleGrant);
  TOIDRoles = set of TOIDRole;

  { Table Grants Privileges }
  TTablePrivilege = (tpSelect, tpInsert, tpUpdate, tpDelete, tpReference);
  TTablePrivileges = set of TTablePrivilege;

  { Field Grants Privileges }
  TFieldPrivilege = (fpUpdate, fpReference);
  TFieldPrivileges = set of TFieldPrivilege;

const
  ALLOBjects = [OIDDomain, OIDTable, OIDView, OIDProcedure, OIDPackage,
    OIDGenerator, OIDException, OIDUDF, OIDRole, OIDDBCharset, OIDDependencies];
  ALLTables = [OIDTableField, OIDPrimary, OIDForeign, OIDTableTrigger,
    OIDUnique,OIDIndex, OIDCheck, OIDTableGrant, OIDTableFieldGrant];
  ALLViews = [OIDViewField, OIDViewTrigers, OIDViewGrant, OIDViewFieldGrant];
  ALLProcedures = [OIDProcFieldIn, OIDProcFieldOut, OIDProcedureGrant];
  ALLPackages = [OIDPackageProc, OIDPackageFunc];
  ALLUDFs = [OIDUDFField];
  ALLRoles = [OIDRoleGrant];
  { All Grantable Privileges }
  ALLTablePrivileges = [tpSelect, tpInsert, tpUpdate, tpDelete, tpReference];
  ALLFieldPrivileges = [fpUpdate, fpReference];
{$IFDEF UNIX}
  BreakLine = #10;
  NewLine = #10#10; //BreakLine + BreakLine;
{$ELSE}
  BreakLine = #13;
  NewLine = #13#10;
{$ENDIF}

const
  {$IFDEF UIBLANG_EN}
  sUIBTrue  = 'True';
  sUIBFalse = 'False';

  // UIB Errors
  EUIB_INVALIDEIBVERSION   = 'Incorrect Database Server version, check compiler options.';
  EUIB_CANTLOADLIB         = 'Can''t load library: %s.';
  EUIB_DBHANDLEALREADYSET  = 'Database handle already assigned, first disconnect database.';
  EUIB_TRANSACTIONNOTDEF   = 'Transaction not assigned.';
  EUIB_DATABASENOTDEF      = 'Database not assigned.';
  EUIB_QUERYNOTOPEN        = 'Query not open.';
  EUIB_CASTERROR           = 'Cast error.';
  EUIB_UNEXPECTEDERROR     = 'Unexpected error.';
  EUIB_FIELDNUMNOTFOUND    = 'Field num: %d not found.';
  EUIB_FIELDSTRNOTFOUND    = 'Field "%s" not found.';
  EUIB_PARAMSTRNOTFOUND    = 'Parameter "%s" not found.';
  EUIB_BLOBFIELDNOTFOUND   = 'Blob field num: %d not found.';
  EUIB_FETCHBLOBNOTSET     = 'FetchBlob property must be set to use this method.';
  EUIB_INDEXERROR          = 'Index out of bound (%d)';
  EUIB_SIZENAME            = 'Size name too big (%s)';
  EUIB_MUSTBEPREPARED      = 'The query must be prepared first.';
  EUIB_MUSTBEOPEN          = 'The query must be opened first.';
  EUIB_EXPLICITTRANS       = 'Transaction must be started explicitly.';
  EUIB_EXCEPTIONNOTFOUND   = 'Exception name %s, not found.';
  EUIB_EXPTIONREGISTERED   = 'Exception: %d already registered';
  EUIB_NOAUTOSTOP          = 'Transaction must be closed explicitly.';
  EUIB_NOGENERATOR         = 'Generator %s not found.';
  EUIB_NOFIELD             = 'Field not found.';
  EUIB_TABLESTRNOTFOUND    = 'Table "%s" not found.';
  EUIB_DOMAINSTRNOTFOUND   = 'Domain %s not found.';
  EUIB_PROCSTRNOTFOUND     = 'Procedure %s not found.';
  EUIB_CACHEDFETCHNOTSET   = 'CachedFetch property not set to True.';
  EUIB_PARSESQLDIALECT     = 'Parse error: SET SQL DIALECT';
  EUIB_PARSESETNAMES       = 'Parse error: SET NAMES';
  EUIB_CHARSETNOTFOUND     = 'CharacterSet %s not found.';
  EUIB_UNEXPECTEDCASTERROR = 'Unexpected cast error.';
  EUIB_INVALIDUSERNAME     = 'Invalid user name : "%s".';
  EUIB_SERVICESPARSING     = 'Error while parsing Services API output.';
  EUIB_NOT_NULLABLE        = 'Field "%s" is not nullable.';
  {$ENDIF UIBLANG_EN}

  {$IFDEF UIBLANG_FR}
  sUIBTrue  = 'Vrai';
  sUIBFalse = 'Faux';

  // UIB Errors
  EUIB_INVALIDEIBVERSION   = 'Version de base de donn�es incorrecte, v�rifiez les options de compilation.';
  EUIB_CANTLOADLIB         = 'Impossible de charger la DLL: %s.';
  EUIB_DBHANDLEALREADYSET  = 'Le handle de la base de donn�es est d�j� d�fini, d�connectez d''abord la base de donn�es.';
  EUIB_TRANSACTIONNOTDEF   = 'La transaction n''est pas d�finie';
  EUIB_DATABASENOTDEF      = 'La base de donn�es n''est pas d�finie.';
  EUIB_QUERYNOTOPEN        = 'La requ�te n''est pas encore ouverte.';
  EUIB_CASTERROR           = 'Transtypage incorrect.';
  EUIB_UNEXPECTEDERROR     = 'Erreur inattendue.';
  EUIB_FIELDNUMNOTFOUND    = 'Le champ num�ro: %d n''existe pas.';
  EUIB_FIELDSTRNOTFOUND    = 'Le champ "%s" n''existe pas.';
  EUIB_PARAMSTRNOTFOUND    = 'Le Parametre "%s" n''existe pas.';
  EUIB_BLOBFIELDNOTFOUND   = 'Le champ Blob num�ro: %d n''existe pas.';
  EUIB_FETCHBLOBNOTSET     = 'La propri�t� FetchBlob doit �tre activ�e pour utiliser cette m�thode.';
  EUIB_INDEXERROR          = 'Indice de liste hors limites (%d)';
  EUIB_SIZENAME            = 'La taille du nom est trop grande (%s)';
  EUIB_MUSTBEPREPARED      = 'La requ�te doit d''abord �tre pr�par�e.';
  EUIB_MUSTBEOPEN          = 'La requ�te doit d''abord �tre ouverte.';
  EUIB_EXPLICITTRANS       = 'La transaction doit �tre d�marr�e explicitement.';
  EUIB_EXCEPTIONNOTFOUND   = 'L''exception %s, n''existe pas.';
  EUIB_EXPTIONREGISTERED   = 'L''exception %d a d�j� �t� enregistr�e.';
  EUIB_NOAUTOSTOP          = 'La transaction doit �tre ferm�e explicitement.';
  EUIB_NOGENERATOR         = 'Generator %s not found.';
  EUIB_NOFIELD             = 'Field not found.';
  EUIB_TABLESTRNOTFOUND    = 'Table "%s" not found.';
  EUIB_DOMAINSTRNOTFOUND   = 'Domain %s not found.';
  EUIB_PROCSTRNOTFOUND     = 'Procedure %s not found.';
  EUIB_CACHEDFETCHNOTSET   = 'CachedFetch property not set to True.';
  EUIB_PARSESQLDIALECT     = 'Parse error: SET SQL DIALECT';
  EUIB_PARSESETNAMES       = 'Parse error: SET NAMES';
  EUIB_CHARSETNOTFOUND     = 'CharacterSet %s not found.';
  EUIB_UNEXPECTEDCASTERROR = 'Unexpected cast error.';
  EUIB_INVALIDUSERNAME     = 'Invalid user name : "%s".';
  EUIB_SERVICESPARSING     = 'Error while parsing Services API output.';
  EUIB_NOT_NULLABLE        = 'Field "%s" is not nullable.';

  {$ENDIF UIBLANG_FR}

  {$IFDEF UIBLANG_CZ}
  sUIBTrue  = 'Ano';
  sUIBFalse = 'Ne';

  // UIB Errors
  EUIB_INVALIDEIBVERSION   = 'Nekorektn� verze datab�zov�ho serveru, zkontrolujte nastaven� kompileru.';
  EUIB_CANTLOADLIB         = 'Nelze na��st knihovnu: %s.';
  EUIB_DBHANDLEALREADYSET  = 'Kan�l datab�ze je ji� p�ipraven, nejd��ve odpojte datab�zi.';
  EUIB_TRANSACTIONNOTDEF   = 'Transakce nen� p�i�azena.';
  EUIB_DATABASENOTDEF      = 'Datab�ze nen� p�i�azena.';
  EUIB_QUERYNOTOPEN        = 'Dotaz nen� otev�en.';
  EUIB_CASTERROR           = 'Chyba bsazen�.';
  EUIB_UNEXPECTEDERROR     = 'Nezn�m� chyba.';
  EUIB_FIELDNUMNOTFOUND    = 'Polo�ka ��slo: %d neexistuje.';
  EUIB_FIELDSTRNOTFOUND    = 'Polo�ka "%s" neexistuje.';
  EUIB_PARAMSTRNOTFOUND    = 'Parametr "%s" neexistuje.';
  EUIB_BLOBFIELDNOTFOUND   = 'Blob z�znam ��slo: %d neexistuje.';
  EUIB_FETCHBLOBNOTSET     = 'Vlastnost FetchBlob mus� b�t nastavena pro pou�it� t�to metody.';
  EUIB_INDEXERROR          = 'Index je mimo rozsah (%d)';
  EUIB_SIZENAME            = 'Velikost n�zvu je p��li� velk� (%s)';
  EUIB_MUSTBEPREPARED      = 'Dotaz mus� b�t nejd��ve p�ipraven (prepared).';
  EUIB_MUSTBEOPEN          = 'The query must be opened first.';
  EUIB_EXPLICITTRANS       = 'Transaction must be started explicitly.';
  EUIB_EXCEPTIONNOTFOUND   = 'Exception name %s, not found.';
  EUIB_EXPTIONREGISTERED   = 'Exception: %d already registered';
  EUIB_NOAUTOSTOP          = 'Transaction must be closed explicitly.';
  EUIB_NOGENERATOR         = 'Generator %s not found.';
  EUIB_NOFIELD             = 'Field not found.';
  EUIB_TABLESTRNOTFOUND    = 'Table "%s" not found.';
  EUIB_DOMAINSTRNOTFOUND   = 'Domain %s not found.';
  EUIB_PROCSTRNOTFOUND     = 'Procedure %s not found.';
  EUIB_CACHEDFETCHNOTSET   = 'CachedFetch property not set to True.';
  EUIB_PARSESQLDIALECT     = 'Parse error: SET SQL DIALECT';
  EUIB_PARSESETNAMES       = 'Parse error: SET NAMES';
  EUIB_CHARSETNOTFOUND     = 'CharacterSet %s not found.';
  EUIB_UNEXPECTEDCASTERROR = 'Unexpected cast error.';
  EUIB_INVALIDUSERNAME     = 'Invalid user name : "%s".';
  EUIB_SERVICESPARSING     = 'Error while parsing Services API output.';
  EUIB_NOT_NULLABLE        = 'Field "%s" is not nullable.';

  {$ENDIF UIBLANG_CZ}

  {$IFDEF UIBLANG_DE}
  sUIBTrue  = 'Wahr';
  sUIBFalse = 'Falsch';

  // UIB Errors
  EUIB_INVALIDEIBVERSION   = 'Falsche Version des Datenbankservers. Bitte ueberpr�fen sie die Compileroptionen.';
  EUIB_CANTLOADLIB         = 'Kann Bibliothek %s nicht laden.';
  EUIB_DBHANDLEALREADYSET  = 'Datenbank-Handle bereits zugewiesen. Bitte erst Verbindung zur Datenbank trennen.';
  EUIB_TRANSACTIONNOTDEF   = 'Transaktion nicht zugewiesen.';
  EUIB_DATABASENOTDEF      = 'Datenbank nicht zugewiesen.';
  EUIB_QUERYNOTOPEN        = 'Abfrage nicht ge�ffnet.';
  EUIB_CASTERROR           = 'Fehler bei Typumwandlung.';
  EUIB_UNEXPECTEDERROR     = 'Unerwarteter Fehler.';
  EUIB_FIELDNUMNOTFOUND    = 'Feld Nummer %d nicht gefunden.';
  EUIB_FIELDSTRNOTFOUND    = 'Feld "%s" nicht gefunden.';
  EUIB_PARAMSTRNOTFOUND    = 'Parameter "%s" nicht gefunden.';
  EUIB_BLOBFIELDNOTFOUND   = 'Blob-Feld Nummer: %d nicht gefunden.';
  EUIB_FETCHBLOBNOTSET     = 'Die Eigenschaft FetchBlob muss gesetzt sein um diese Methode zu nutzen.';
  EUIB_INDEXERROR          = 'Index ausserhalb des g�ltigen Bereichs (%d).';
  EUIB_SIZENAME            = 'Name ist zu lang (%s).';
  EUIB_MUSTBEPREPARED      = 'Die Abfrage muss erst vorbereitet werden.';
  EUIB_MUSTBEOPEN          = 'Die Abfrage muss erst ge�ffnet werden.';
  EUIB_EXPLICITTRANS       = 'Die Transaktion muss explizit gestartet werden.';
  EUIB_EXCEPTIONNOTFOUND   = 'Ausnahme "%s" nicht gefunden.';
  EUIB_EXPTIONREGISTERED   = 'Ausnahme %d bereits registriert.';
  EUIB_NOAUTOSTOP          = 'Die Transaktion muss explizit beendet werden.';
  EUIB_NOGENERATOR         = 'Generator %s nicht gefunden.';
  EUIB_NOFIELD             = 'Feld nicht gefunden.';
  EUIB_TABLESTRNOTFOUND    = 'Tabelle "%s" nicht gefunden.';
  EUIB_DOMAINSTRNOTFOUND   = 'Dom�ne %s nicht gefunden.';
  EUIB_PROCSTRNOTFOUND     = 'Prozedur %s nicht gefunden.';
  EUIB_CACHEDFETCHNOTSET   = 'CachedFetch property not set to True.';
  EUIB_PARSESQLDIALECT     = 'Parse error: SET SQL DIALECT';
  EUIB_PARSESETNAMES       = 'Parse error: SET NAMES';
  EUIB_CHARSETNOTFOUND     = 'CharacterSet %s not found.';
  EUIB_UNEXPECTEDCASTERROR = 'Unexpected cast error.';
  EUIB_INVALIDUSERNAME     = 'Invalid user name : "%s".';
  EUIB_SERVICESPARSING     = 'Error while parsing Services API output.';
  EUIB_NOT_NULLABLE        = 'Field "%s" is not nullable.';

  {$ENDIF UIBLANG_DE}

  {$IFDEF UIBLANG_RU}
  sUIBTrue  = '��';
  sUIBFalse = '���';

  // UIB Errors
  EUIB_INVALIDEIBVERSION   = '������������ ������ ������� ��, ��������� ����� �����������.';
  EUIB_CANTLOADLIB         = '�� ���� ��������� ���������� DLL: %s.';
  EUIB_DBHANDLEALREADYSET  = '���������� ���� ������ ��� ����������, ������� ��������� ������� ���������� � ����� ������.';
  EUIB_TRANSACTIONNOTDEF   = '�������� Transaction �� �����������.';
  EUIB_DATABASENOTDEF      = '�������� Database �� �����������.';
  EUIB_QUERYNOTOPEN        = '������ �� ������.';
  EUIB_CASTERROR           = '������ ���������� ����.';
  EUIB_UNEXPECTEDERROR     = '����������� ������.';
  EUIB_FIELDNUMNOTFOUND    = '���� �����: %d �� �������.';
  EUIB_FIELDSTRNOTFOUND    = '���� "%s" �� �������.';
  EUIB_PARAMSTRNOTFOUND    = '�������� "%s" �� ������.';
  EUIB_BLOBFIELDNOTFOUND   = 'Blob ���� �����: %d �� �������.';
  EUIB_FETCHBLOBNOTSET     = '�������� FetchBlob ������ ���� ����������� ��� ���������� ����� ������.';
  EUIB_INDEXERROR          = 'Index �� ��������� ���������� �������� (%d)';
  EUIB_SIZENAME            = '������� ������� �������� (%s)';
  EUIB_MUSTBEPREPARED      = '���������� ������� ����������� (prepare) ������.';
  EUIB_MUSTBEOPEN          = '���������� ������� ������� ������.';
  EUIB_EXPLICITTRANS       = '�� �������� ����� ����������.';
  EUIB_EXCEPTIONNOTFOUND   = '���������� %s �� �������.';
  EUIB_EXPTIONREGISTERED   = '����������: %d ��� ����������������';
  EUIB_NOAUTOSTOP          = 'Transaction must be closed explicitly.';
  EUIB_NOGENERATOR         = 'Generator %s not found.';
  EUIB_NOFIELD             = 'Field not found.';
  EUIB_TABLESTRNOTFOUND    = 'Table "%s" not found.';
  EUIB_DOMAINSTRNOTFOUND   = 'Domain %s not found.';
  EUIB_PROCSTRNOTFOUND     = 'Procedure %s not found.';
  EUIB_CACHEDFETCHNOTSET   = 'CachedFetch property not set to True.';
  EUIB_PARSESQLDIALECT     = 'Parse error: SET SQL DIALECT';
  EUIB_PARSESETNAMES       = 'Parse error: SET NAMES';
  EUIB_CHARSETNOTFOUND     = 'CharacterSet %s not found.';
  EUIB_UNEXPECTEDCASTERROR = 'Unexpected cast error.';
  EUIB_INVALIDUSERNAME     = 'Invalid user name : "%s".';
  EUIB_SERVICESPARSING     = 'Error while parsing Services API output.';
  EUIB_NOT_NULLABLE        = 'Field "%s" is not nullable.';

  {$ENDIF UIBLANG_RU}

  {$IFDEF UIBLANG_ES}
  sUIBTrue             = 'Si';
  sUIBFalse            = 'No';

  // UIB Errors
  EUIB_INVALIDEIBVERSION   = 'Versi�n incorrecta del Servidor de Base de Datos, verifica las opciones del compilador.';
  EUIB_CANTLOADLIB         = 'Imposible cargar la DLL: %s.';
  EUIB_DBHANDLEALREADYSET  = 'El handle de la base de datos est� asignado, primero desconecte la base de datos.';
  EUIB_TRANSACTIONNOTDEF   = 'La transacci�n no est� asignada.';
  EUIB_DATABASENOTDEF      = 'La Base de Datos no est� asignada.';
  EUIB_QUERYNOTOPEN        = 'El query est� cerrado.';
  EUIB_CASTERROR           = 'Error en conversi�n.';
  EUIB_UNEXPECTEDERROR     = 'Error inesperado.';
  EUIB_FIELDNUMNOTFOUND    = 'Campo n�mero: %d no encontrado.';
  EUIB_FIELDSTRNOTFOUND    = 'Campo "%s" no encontrado.';
  EUIB_PARAMSTRNOTFOUND    = 'Parametro "%s" no  encontrado.';
  EUIB_BLOBFIELDNOTFOUND   = 'Campo Blob n�mero: %d no  encontrado.';
  EUIB_FETCHBLOBNOTSET     = 'La propiedad FetchBlob debe habilitarse para usar est� m�todo';
  EUIB_INDEXERROR          = '�ndice fuera de l�mite (%d)';
  EUIB_SIZENAME            = 'Nombre demasiado largo (%s)';
  EUIB_MUSTBEPREPARED      = 'El query debe prepararse primero.';
  EUIB_MUSTBEOPEN          = 'El query debe ser abierto primero.';
  EUIB_EXPLICITTRANS       = 'La transaccisn debe ser iniciada explmcitamente.';
  EUIB_EXCEPTIONNOTFOUND   = 'Excepcion con nombre %s, no fue encontrada.';
  EUIB_EXPTIONREGISTERED   = 'Excepcion: %d ya esta registrada.';
  EUIB_NOAUTOSTOP          = 'La transaccisn debe ser cerrada explmcitamente.';
  EUIB_NOGENERATOR         = 'Generator %s not found.';
  EUIB_NOFIELD             = 'Field not found.';
  EUIB_TABLESTRNOTFOUND    = 'Table "%s" not found.';
  EUIB_DOMAINSTRNOTFOUND   = 'Domain %s not found.';
  EUIB_PROCSTRNOTFOUND     = 'Procedure %s not found.';
  EUIB_CACHEDFETCHNOTSET   = 'CachedFetch property not set to True.';
  EUIB_PARSESQLDIALECT     = 'Parse error: SET SQL DIALECT';
  EUIB_PARSESETNAMES       = 'Parse error: SET NAMES';
  EUIB_CHARSETNOTFOUND     = 'CharacterSet %s not found.';
  EUIB_UNEXPECTEDCASTERROR = 'Unexpected cast error.';
  EUIB_INVALIDUSERNAME     = 'Invalid user name : "%s".';
  EUIB_SERVICESPARSING     = 'Error while parsing Services API output.';
  EUIB_NOT_NULLABLE        = 'Field "%s" is not nullable.';

  {$ENDIF UIBLANG_ES}

  {$IFDEF UIBLANG_TR}
  sUIBTrue  = 'Do�ru';
  sUIBFalse = 'Yanl��';

  // UIB Errors
  EUIB_INVALIDEIBVERSION   = 'Hatal� Veritaban� Sunucusu s�r�m�, derleyici se�eneklerini kontrol ediniz.';
  EUIB_CANTLOADLIB         = 'Kitapl�k y�klenemiyor: %s.';
  EUIB_DBHANDLEALREADYSET  = 'Veritaban� tan�t�c�s� atanm�� durumda, �ncelikle veritaban� ba�lant�s�n� kesiniz.';
  EUIB_TRANSACTIONNOTDEF   = '��lem grubu atanmam��.';
  EUIB_DATABASENOTDEF      = 'Veritaban� atanmam��.';
  EUIB_QUERYNOTOPEN        = 'Sorgu a��k de�il.';
  EUIB_CASTERROR           = 'Tip atama hatas�.';
  EUIB_UNEXPECTEDERROR     = 'Beklenmeyen hata.';
  EUIB_FIELDNUMNOTFOUND    = '%d numaral� saha bulunamad�.';
  EUIB_FIELDSTRNOTFOUND    = '"%s" sahas� bulunamad�.';
  EUIB_PARAMSTRNOTFOUND    = '"%s" parametresi bulunamad�.';
  EUIB_BLOBFIELDNOTFOUND   = '%d numaral� Blob sahas� bulunamad�.';
  EUIB_FETCHBLOBNOTSET     = 'Bu metodu kullanmak i�in FetchBlob niteli�i ayarlanmal�d�r.';
  EUIB_INDEXERROR          = 'Indeks s�n�r d���nda (%d)';
  EUIB_SIZENAME            = 'Ad b�y�kl��� �ok y�ksek (%s)';
  EUIB_MUSTBEPREPARED      = 'Sorgu �ncelikle haz�rlanmal�d�r.';
  EUIB_MUSTBEOPEN          = 'Sorgu �ncelikle a��lmal�d�r.';
  EUIB_EXPLICITTRANS       = '��lem grubu elle ba�lat�lmal�d�r.';
  EUIB_EXCEPTIONNOTFOUND   = 'Kural d��� nesnesi "%s" bulunamad�.';
  EUIB_EXPTIONREGISTERED   = 'Kural d���: %d zaten kay�tl�.';
  EUIB_NOAUTOSTOP          = '��lem grubu elle kapat�lmal�d�r.';
  EUIB_NOGENERATOR         = '%s �reteci bulunamad�.';
  EUIB_NOFIELD             = 'Saha bulunamad�.';
  EUIB_TABLESTRNOTFOUND    = '"%s" tablosu bulunamad�.';
  EUIB_DOMAINSTRNOTFOUND   = '"%s" etki alan� bulunamad�.';
  EUIB_PROCSTRNOTFOUND     = '%s yordam� bulunamad�.';
  EUIB_CACHEDFETCHNOTSET   = 'CachedFetch niteli�i True olarak belirlenmemi�.';
  EUIB_PARSESQLDIALECT     = 'Ayr��t�rma hatas�: SET SQL DIALECT';
  EUIB_PARSESETNAMES       = 'Ayr��t�rma hatas�: SET NAMES';
  EUIB_CHARSETNOTFOUND     = '%s karakter seti bulunamad�.';
  EUIB_UNEXPECTEDCASTERROR = 'Beklenmeyen tip atama hatas�.';
  EUIB_INVALIDUSERNAME     = 'Ge�ersiz kullan�c� ad� : "%s".';
  EUIB_SERVICESPARSING     = 'Services API ��kt�s� ayr��t�r�l�rken hata olu�tu.';
  EUIB_NOT_NULLABLE        = 'Field "%s" is not nullable.';

  {$ENDIF UIBLANG_TR}

implementation

end.
