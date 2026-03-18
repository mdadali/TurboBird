// Common definitions for TurboBird
const
  {$IFDEF LINUX}
  Target = 'Linux';
  {$ENDIF}

  {$IFDEF WINDOWS}
  Target = 'Win';
  {$ENDIF}

  {$IFDEF BSD}
  {$IFDEF DARWIN}
  Target = 'Mac'; //Mac OSX
  {$ELSE}
  Target = 'BSD'; //FreeBSD, OpenBSD, NetBSD,...
  {$ENDIF}
  {$ENDIF}

  {$ifDEF CPU32}
  Arch = '32';
  {$ENDIF}

  {$ifDEF CPU64}
  Arch = '64';
  {$ENDIF}

const

  CompileDate = {$I %DATE%};  // z. B. '2025/07/25'
  CompileTime = {$I %TIME%};  // z. B. '17:42:33'

  // Some field types used in e.g. RDB$FIELDS
  // todo: (low priority) perhaps move to enumeration with fixed constant values
  BlobType = 261;
  CharType = 14;
  CStringType = 40; // probably null-terminated string used for UDFs
  VarCharType = 37;

  // Available character set encodings for Firebird.
  // Update this whenever Firebird supports new character sets
  DefaultFBCharacterSet=42; //Used for GUI controls etc. UTF8 in CharacterSets below.
  // Available character sets as per Firebird 2.5
  FBCharacterSets: array[0..51] of string =
    ('NONE',
    'ASCII',
    'BIG_5',
    'CP943C',
    'CYRL',
    'DOS437',
    'DOS737',
    'DOS775',
    'DOS850',
    'DOS852',
    'DOS857',
    'DOS858',
    'DOS860',
    'DOS861',
    'DOS862',
    'DOS863',
    'DOS864',
    'DOS865',
    'DOS866',
    'DOS869',
    'EUCJ_0208',
    'GB18030',
    'GBK',
    'GB_2312',
    'ISO8859_1',
    'ISO8859_13',
    'ISO8859_2',
    'ISO8859_3',
    'ISO8859_4',
    'ISO8859_5',
    'ISO8859_6',
    'ISO8859_7',
    'ISO8859_8',
    'ISO8859_9',
    'KOI8R',
    'KOI8U',
    'KSC_5601',
    'NEXT',
    'OCTETS',
    'SJIS_0208',
    'TIS620',
    'UNICODE_FSS', //obsolete
    'UTF8', //good default
    'WIN1250',
    'WIN1251',
    'WIN1252',
    'WIN1253',
    'WIN1254',
    'WIN1255',
    'WIN1256',
    'WIN1257',
    'WIN1258');
  // Available collations as per Firebird 2.5
  // Pairs of collation names and the character set name
  // that must be used to support this collation
  FBCollations: array[0..148-1,0..1] of string =
    (
    ('ASCII','ASCII'),
    ('BIG_5','BIG_5'),
    ('BS_BA','WIN1250'),
    ('CP943C','CP943C'),
    ('CP943C_UNICODE','CP943C'),
    ('CS_CZ','ISO8859_2'),
    ('CYRL','CYRL'),
    ('DA_DA','ISO8859_1'),
    ('DB_CSY','DOS852'),
    ('DB_DAN865','DOS865'),
    ('DB_DEU437','DOS437'),
    ('DB_DEU850','DOS850'),
    ('DB_ESP437','DOS437'),
    ('DB_ESP850','DOS850'),
    ('DB_FIN437','DOS437'),
    ('DB_FRA437','DOS437'),
    ('DB_FRA850','DOS850'),
    ('DB_FRC850','DOS850'),
    ('DB_FRC863','DOS863'),
    ('DB_ITA437','DOS437'),
    ('DB_ITA850','DOS850'),
    ('DB_NLD437','DOS437'),
    ('DB_NLD850','DOS850'),
    ('DB_NOR865','DOS865'),
    ('DB_PLK','DOS852'),
    ('DB_PTB850','DOS850'),
    ('DB_PTG860','DOS860'),
    ('DB_RUS','CYRL'),
    ('DB_SLO','DOS852'),
    ('DB_SVE437','DOS437'),
    ('DB_SVE850','DOS850'),
    ('DB_TRK','DOS857'),
    ('DB_UK437','DOS437'),
    ('DB_UK850','DOS850'),
    ('DB_US437','DOS437'),
    ('DB_US850','DOS850'),
    ('DE_DE','ISO8859_1'),
    ('DOS437','DOS437'),
    ('DOS737','DOS737'),
    ('DOS775','DOS775'),
    ('DOS850','DOS850'),
    ('DOS852','DOS852'),
    ('DOS857','DOS857'),
    ('DOS858','DOS858'),
    ('DOS860','DOS860'),
    ('DOS861','DOS861'),
    ('DOS862','DOS862'),
    ('DOS863','DOS863'),
    ('DOS864','DOS864'),
    ('DOS865','DOS865'),
    ('DOS866','DOS866'),
    ('DOS869','DOS869'),
    ('DU_NL','ISO8859_1'),
    ('EN_UK','ISO8859_1'),
    ('EN_US','ISO8859_1'),
    ('ES_ES','ISO8859_1'),
    ('ES_ES_CI_AI','ISO8859_1'),
    ('EUCJ_0208','EUCJ_0208'),
    ('FI_FI','ISO8859_1'),
    ('FR_CA','ISO8859_1'),
    ('FR_FR','ISO8859_1'),
    ('FR_FR_CI_AI','ISO8859_1'),
    ('GB18030','GB18030'),
    ('GB18030_UNICODE','GB18030'),
    ('GBK','GBK'),
    ('GBK_UNICODE','GBK'),
    ('GB_2312','GB_2312'),
    ('ISO8859_1','ISO8859_1'),
    ('ISO8859_13','ISO8859_13'),
    ('ISO8859_2','ISO8859_2'),
    ('ISO8859_3','ISO8859_3'),
    ('ISO8859_4','ISO8859_4'),
    ('ISO8859_5','ISO8859_5'),
    ('ISO8859_6','ISO8859_6'),
    ('ISO8859_7','ISO8859_7'),
    ('ISO8859_8','ISO8859_8'),
    ('ISO8859_9','ISO8859_9'),
    ('ISO_HUN','ISO8859_2'),
    ('ISO_PLK','ISO8859_2'),
    ('IS_IS','ISO8859_1'),
    ('IT_IT','ISO8859_1'),
    ('KOI8R','KOI8R'),
    ('KOI8R_RU','KOI8R'),
    ('KOI8U','KOI8U'),
    ('KOI8U_UA','KOI8U'),
    ('KSC_5601','KSC_5601'),
    ('KSC_DICTIONARY','KSC_5601'),
    ('LT_LT','ISO8859_13'),
    ('NEXT','NEXT'),
    ('NONE','NONE'),
    ('NO_NO','ISO8859_1'),
    ('NXT_DEU','NEXT'),
    ('NXT_ESP','NEXT'),
    ('NXT_FRA','NEXT'),
    ('NXT_ITA','NEXT'),
    ('NXT_US','NEXT'),
    ('OCTETS','OCTETS'),
    ('PDOX_ASCII','DOS437'),
    ('PDOX_CSY','DOS852'),
    ('PDOX_CYRL','CYRL'),
    ('PDOX_HUN','DOS852'),
    ('PDOX_INTL','DOS437'),
    ('PDOX_ISL','DOS861'),
    ('PDOX_NORDAN4','DOS865'),
    ('PDOX_PLK','DOS852'),
    ('PDOX_SLO','DOS852'),
    ('PDOX_SWEDFIN','DOS437'),
    ('PT_BR','ISO8859_1'),
    ('PT_PT','ISO8859_1'),
    ('PXW_CSY','WIN1250'),
    ('PXW_CYRL','WIN1251'),
    ('PXW_GREEK','WIN1253'),
    ('PXW_HUN','WIN1250'),
    ('PXW_HUNDC','WIN1250'),
    ('PXW_INTL','WIN1252'),
    ('PXW_INTL850','WIN1252'),
    ('PXW_NORDAN4','WIN1252'),
    ('PXW_PLK','WIN1250'),
    ('PXW_SLOV','WIN1250'),
    ('PXW_SPAN','WIN1252'),
    ('PXW_SWEDFIN','WIN1252'),
    ('PXW_TURK','WIN1254'),
    ('SJIS_0208','SJIS_0208'),
    ('SV_SV','ISO8859_1'),
    ('TIS620','TIS620'),
    ('TIS620_UNICODE','TIS620'),
    ('UCS_BASIC','UTF8'),
    ('UNICODE','UTF8'),
    ('UNICODE_CI','UTF8'),
    ('UNICODE_CI_AI','UTF8'),
    ('UNICODE_FSS','UNICODE_FSS'),
    ('UTF8','UTF8'),
    ('WIN1250','WIN1250'),
    ('WIN1251','WIN1251'),
    ('WIN1251_UA','WIN1251'),
    ('WIN1252','WIN1252'),
    ('WIN1253','WIN1253'),
    ('WIN1254','WIN1254'),
    ('WIN1255','WIN1255'),
    ('WIN1256','WIN1256'),
    ('WIN1257','WIN1257'),
    ('WIN1257_EE','WIN1257'),
    ('WIN1257_LT','WIN1257'),
    ('WIN1257_LV','WIN1257'),
    ('WIN1258','WIN1258'),
    ('WIN_CZ','WIN1250'),
    ('WIN_CZ_CI_AI','WIN1250'),
    ('WIN_PTBR','WIN1252')
    );
