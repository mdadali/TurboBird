unit fClipboardExport;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,   Clipbrd, Dialogs,
  DB, IB, IBDatabase, IBCustomDataSet;



procedure _ExportDataToClipboard(ADataSet: TDataSet; MaxRows: Integer);



implementation

procedure _ExportDataToClipboard(ADataSet: TDataSet; MaxRows: Integer);
var
  i, RowCounter: Integer;
  Line: string;
  Stream: TStringStream;
begin
  if (ADataSet = nil) or (ADataSet.FieldCount = 0) then
    Exit;

  Stream := TStringStream.Create('', TEncoding.UTF8);
  try
    ADataSet.First;

    // --- Header ---
    Line := '';
    for i := 0 to ADataSet.FieldCount - 1 do
    begin
      Line := Line + '"' + ADataSet.Fields[i].FieldName + '"';
      if i + 1 < ADataSet.FieldCount then
        Line := Line + ',';
    end;
    Stream.WriteString(Line + sLineBreak);

    // --- Rows ---
    RowCounter := 0;
    while not ADataSet.EOF do
    begin
      if (MaxRows > 0) and (RowCounter >= MaxRows) then
        Break;

      Line := '';
      for i := 0 to ADataSet.FieldCount - 1 do
      begin
        Line := Line + '"' + ADataSet.Fields[i].AsString + '"';
        if i + 1 < ADataSet.FieldCount then
          Line := Line + ',';
      end;

      Stream.WriteString(Line + sLineBreak);

      Inc(RowCounter);
      ADataSet.Next;
    end;

    // --- Clipboard füllen ---
    Clipboard.AsText := Stream.DataString;

  except
    on E: Exception do
      ShowMessage('Error copying data to clipboard: ' + E.Message);
  end;

  Stream.Free;
end;



end.

