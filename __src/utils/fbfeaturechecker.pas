unit fbfeaturechecker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection;

type
  TCapabilities = record
    HasBoolean: Boolean;
    HasPackages: Boolean;
    HasUDR: Boolean;
    HasDecFloat: Boolean;
    HasInt128: Boolean;
    HasTimeZones: Boolean;
    HasReplication: Boolean;
    HasIdentity: Boolean;
  end;

/// Detects Firebird capabilities silently. No exceptions or popups.
function DetectCapabilitiesSilent(AConn: TIBConnection): TCapabilities;

implementation

function DetectCapabilitiesSilent(AConn: TIBConnection): TCapabilities;

function SafeExecScalar(const SQL: string): Boolean;
  var
    ds: TIBSQL;
  begin
    Result := False;
    try
      ds := TIBSQL.Create(nil);
      try
        ds.Database := AConn;
        ds.SQL.Text := SQL;
        ds.ExecQuery;
        Result := ds.Fields[0].AsInteger > 0;
      finally
        ds.Free;
      end;
    except
      Result := False;
    end;
  end;

  function FieldTypeExists(FieldType: Integer): Boolean;
  begin
    Result := SafeExecScalar(
      'SELECT COUNT(*) FROM RDB$FIELDS WHERE RDB$FIELD_TYPE = ' + IntToStr(FieldType)
    );
  end;

  function RelationExists(const Rel: string): Boolean;
  begin
    Result := SafeExecScalar(
      'SELECT COUNT(*) FROM RDB$RELATIONS WHERE RDB$RELATION_NAME = ''' + Rel + ''''
    );
  end;

  function FieldExists(const Rel, Field: string): Boolean;
  begin
    Result := SafeExecScalar(
      'SELECT COUNT(*) FROM RDB$RELATION_FIELDS ' +
      'WHERE RDB$RELATION_NAME = ''' + Rel + ''' ' +
      'AND RDB$FIELD_NAME = ''' + Field + ''''
    );
  end;

  function GeneratorLike(const Pattern: string): Boolean;
  begin
    Result := SafeExecScalar(
      'SELECT COUNT(*) FROM RDB$GENERATORS WHERE RDB$GENERATOR_NAME LIKE ''' + Pattern + ''''
    );
  end;

begin
  FillChar(Result, SizeOf(Result), 0);

  try
    // Boolean type (FB3+)
    Result.HasBoolean := FieldTypeExists(23);

    // Packages (FB3+)
    Result.HasPackages := RelationExists('RDB$PACKAGES');

    // UDR/Engine (FB3+)
    Result.HasUDR := FieldExists('RDB$FUNCTIONS', 'RDB$ENGINE_NAME');

    // DECFLOAT / INT128 (FB4+)
    Result.HasDecFloat := FieldTypeExists(26);
    Result.HasInt128 := FieldTypeExists(27);

    // Time Zone Types (FB4+)
    Result.HasTimeZones := FieldTypeExists(29) or FieldTypeExists(30);

    // Replication (FB4+)
    Result.HasReplication := RelationExists('RDB$PUBLICATIONS');

    // Identity Columns (FB3+)
    Result.HasIdentity := GeneratorLike('RDB$IDENTITY%');
  except
    // komplett still: alles bleibt False bei Fehler
  end;
end;

end.

