unit uCrossRDBMSTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB,
  IB, IBDatabase, IBQuery;

type
  { ------------------------------------------------------------------ }
  { DBMS-Erkennung                                                     }
  { ------------------------------------------------------------------ }
  TCrossDBMS = (
    cdUnknown,
    cdFirebird,
    cdPostgreSQL,
    cdMySQL,
    cdSQLite,
    cdMSSQL,
    cdOracle,
    cdSybase,
    cdODBC
  );

  { ------------------------------------------------------------------ }
  { Transfer-Strategien                                                }
  { ------------------------------------------------------------------ }
  TCrossStrategy = (
    csRowByRow,          // A – parametrisiertes Row-by-Row
    csBatched,           // B – Batched Parameter Inserts
    csInsertSelect       // C – batchweises INSERT … SELECT
  );

  { ------------------------------------------------------------------ }
  { Verhalten bei existierender Zieltabelle                            }
  { ------------------------------------------------------------------ }
  TIfExistsAction = (
    ieaAsk,
    ieaRecreate,
    ieaAppend,
    ieaAbort
  );

  { ------------------------------------------------------------------ }
  { Feld-Definition                                                    }
  { ------------------------------------------------------------------ }
  TCrossField = record
    SourceName     : string;
    SourceType     : string;
    DestName       : string;
    DestType       : string;
    Formula        : string;
    Selected       : Boolean;
    IsProblemField : Boolean;   // Array oder Binary BLOB — wird abgewählt
    IsComputed     : Boolean;   // Computed/Generated Field
    ComputedExpr   : string;    // Expression (Firebird-Syntax) — leer wenn unbekannt
  end;

  TCrossFieldArray = array of TCrossField;

  { ------------------------------------------------------------------ }
  { Transfer-Optionen (werden an die Engine übergeben)                 }
  { ------------------------------------------------------------------ }
  TCrossTransferOptions = record
    // Verbindungen (SQLDB-Pfad)
    SourceConn   : TSQLConnection;
    SourceTrans  : TSQLTransaction;
    SourceQuery  : TSQLQuery;

    DestConn     : TSQLConnection;
    DestTrans    : TSQLTransaction;
    DestQuery    : TSQLQuery;

    // Verbindungen (IBX-Pfad — nur wenn IsIBX = True)
    SourceIsIBX   : Boolean;
    SourceIBXConn : TIBDatabase;
    SourceIBXTrans: TIBTransaction;
    SourceIBXQuery: TIBQuery;

    DestIsIBX     : Boolean;
    DestIBXConn   : TIBDatabase;
    DestIBXTrans  : TIBTransaction;
    DestIBXQuery  : TIBQuery;

    // Erkanntes Backend (bei ODBC wichtig)
    SourceBackend : TCrossDBMS;
    DestBackend   : TCrossDBMS;

    // Tabellen
    SourceTable  : string;
    DestTable    : string;

    // Felder + Formeln
    Fields       : TCrossFieldArray;
    UseFormulas  : Boolean;

    // Verhalten
    CopyData     : Boolean;
    IfExists     : TIfExistsAction;
    Strategy     : TCrossStrategy;

    // Bereiche
    FromRow      : Int64;
    ToRow        : Int64;              // 0 = bis Ende

    // Performance / Kontrolle
    CommitInterval    : Integer;
    BatchSize         : Integer;
    RefreshIntervalMs : Integer;
  end;

  { ------------------------------------------------------------------ }
  { Transfer-Statistik                                                 }
  { ------------------------------------------------------------------ }
  TCrossTransferStats = record
    // Quelle
    SourceDBMS           : TCrossDBMS;
    SourceConnectorType  : string;
    SourceDatabase       : string;
    SourceTable          : string;

    // Ziel
    DestDBMS             : TCrossDBMS;
    DestConnectorType    : string;
    DestDatabase         : string;
    DestTable            : string;

    // Ergebnis
    StrategyUsed         : TCrossStrategy;
    RowsProcessed        : Int64;
    RowsTotal            : Int64;
    BytesProcessed       : Int64;
    CommitsDone          : Integer;

    // Zeiten
    StartTime            : TDateTime;
    ElapsedSeconds       : Double;
    RowsPerSec           : Double;
    MBPerSec             : Double;

    // Tabellen-Status
    TableCreated         : Boolean;
    TableRecreated       : Boolean;
    TableExisted         : Boolean;

    // SQL (zur Dokumentation)
    CreateSQL            : string;
    InsertSQL            : string;     // oder Batch-SQL

    // Status
    Cancelled            : Boolean;
    Warnings             : TStringList;
  end;

  { ------------------------------------------------------------------ }
  { Events                                                             }
  { ------------------------------------------------------------------ }
  TCrossProgressEvent = procedure(ARowsDone, ARowsTotal: Int64;
    const AElapsed: string; var ACancel: Boolean) of object;

  TCrossLogEvent = procedure(const AMsg: string) of object;


implementation

end.
