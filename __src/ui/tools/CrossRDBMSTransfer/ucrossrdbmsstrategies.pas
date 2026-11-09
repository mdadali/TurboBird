unit uCrossRDBMSStrategies;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uCrossRDBMSTypes;

{ ------------------------------------------------------------------ }
{ Strategie-Auswahl                                                  }
{ ------------------------------------------------------------------ }

{ Liefert die beste verfügbare Strategie für die gegebene Kombination. }
function RecommendStrategy(ASourceDBMS, ADestDBMS: TCrossDBMS;
  ASameConnection: Boolean = False): TCrossStrategy;

{ Prüft, ob eine bestimmte Strategie für die Kombination erlaubt ist. }
function IsStrategyAvailable(AStrategy: TCrossStrategy;
  ASourceDBMS, ADestDBMS: TCrossDBMS;
  ASameConnection: Boolean = False): Boolean;

{ Lesbarer Name der Strategie }
function StrategyName(AStrategy: TCrossStrategy): string;

{ Kurze Beschreibung der Strategie }
function StrategyDescription(AStrategy: TCrossStrategy): string;

implementation

function StrategyName(AStrategy: TCrossStrategy): string;
begin
  case AStrategy of
    csRowByRow:     Result := 'Row-by-Row';
    csBatched:      Result := 'Batched Inserts';
    csInsertSelect: Result := 'INSERT … SELECT (batched)';
  else
    Result := 'Unknown';
  end;
end;

function StrategyDescription(AStrategy: TCrossStrategy): string;
begin
  case AStrategy of
    csRowByRow:
      Result := 'Parametrisiertes Row-by-Row. Beste Cancel- und Progress-Kontrolle. Immer verfügbar.';
    csBatched:
      Result := 'Mehrere Zeilen werden gesammelt und gemeinsam geschrieben. Guter Kompromiss aus Geschwindigkeit und Kontrolle.';
    csInsertSelect:
      Result := 'Serverseitiges INSERT … SELECT in Batches (FIRST/SKIP, LIMIT/OFFSET). Sehr schnell, nur bei gleicher Connection sinnvoll.';
  else
    Result := '';
  end;
end;

function IsStrategyAvailable(AStrategy: TCrossStrategy;
  ASourceDBMS, ADestDBMS: TCrossDBMS;
  ASameConnection: Boolean): Boolean;
begin
  Result := False;

  case AStrategy of
    csRowByRow:
      // Row-by-Row ist immer möglich
      Result := True;

    csBatched:
      // Batched ist bei fast allen Kombinationen möglich
      Result := True;

    csInsertSelect:
      // INSERT … SELECT nur sinnvoll und möglich, wenn Quelle und Ziel
      // dieselbe Connection nutzen (gleiche RDBMS + gleicher Server)
      Result := ASameConnection and (ASourceDBMS = ADestDBMS) and
                (ASourceDBMS <> cdUnknown);
  end;
end;

function RecommendStrategy(ASourceDBMS, ADestDBMS: TCrossDBMS;
  ASameConnection: Boolean): TCrossStrategy;
begin
  // 1. Beste Variante: INSERT … SELECT (nur bei gleicher Connection)
  if IsStrategyAvailable(csInsertSelect, ASourceDBMS, ADestDBMS, ASameConnection) then
    Exit(csInsertSelect);

  // 2. Ansonsten Batched Inserts (guter Standard)
  if IsStrategyAvailable(csBatched, ASourceDBMS, ADestDBMS, ASameConnection) then
    Exit(csBatched);

  // 3. Immer verfügbarer Fallback
  Result := csRowByRow;
end;

end.

