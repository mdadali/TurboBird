unit fhtmlexport;

{$mode ObjFPC}{$H+}

interface

uses
  Forms, Dialogs, Classes, SysUtils,  StdCtrls, ComCtrls,
  Variants,
  DB, sqldb, IBConnection,  IB, IBDatabase, IBCustomDataSet,

  IBArrayHelper;


procedure _ExportDataHtml(ADataSet: TDataSet);


implementation

procedure _ExportDataHTML(ADataSet: TDataSet);
var
  OutFile: TextFile;
  i, FieldCount, RecCount, ExportedCount: Integer;
  sLine, sValue: string;
  ProgressForm: TForm;
  ProgressBar: TProgressBar;
  LabelProgress: TLabel;
  SaveDlg: TSaveDialog;
begin
  if not Assigned(ADataSet) then
    raise Exception.Create('Dataset is not assigned');

  SaveDlg := TSaveDialog.Create(nil);
  try
    SaveDlg.Filter := 'HTML file|*.html;*.htm|All files|*.*';
    SaveDlg.DefaultExt := 'html';
    if not SaveDlg.Execute then Exit;

    ProgressForm := TForm.Create(nil);
    try
      with ProgressForm do
      begin
        Position := poScreenCenter;
        Width := 400;
        Height := 100;
        Caption := '';
      end;

      LabelProgress := TLabel.Create(ProgressForm);
      LabelProgress.Parent := ProgressForm;
      LabelProgress.Left := 20;
      LabelProgress.Top := 10;
      LabelProgress.Caption := '0%';

      ProgressBar := TProgressBar.Create(ProgressForm);
      ProgressBar.Parent := ProgressForm;
      ProgressBar.Left := 20;
      ProgressBar.Top := 40;
      ProgressBar.Width := 350;
      ProgressBar.Min := 0;
      ProgressBar.Max := 100;
      ProgressBar.Position := 0;

      ProgressForm.Show;
      ProgressForm.Update;

      AssignFile(OutFile, SaveDlg.FileName);
      Rewrite(OutFile);

      try
        ADataSet.First;
        FieldCount := ADataSet.FieldCount;

        // HTML Header
        Writeln(OutFile, '<!DOCTYPE html>');
        Writeln(OutFile, '<html>');
        Writeln(OutFile, '<head>');
        Writeln(OutFile, '<meta charset="UTF-8">');
        Writeln(OutFile, '<title>Dataset Export</title>');
        Writeln(OutFile, '<style>');
        Writeln(OutFile, 'table { border-collapse: collapse; width: 100%; }');
        Writeln(OutFile, 'th, td { border: 1px solid #ccc; padding: 4px; text-align: left; }');
        Writeln(OutFile, 'th { background-color: #f0f0f0; }');
        Writeln(OutFile, '</style>');
        Writeln(OutFile, '</head>');
        Writeln(OutFile, '<body>');
        Writeln(OutFile, '<table>');

        // Table Header
        sLine := '  <tr>';
        for i := 0 to FieldCount - 1 do
          sLine := sLine + '<th>' + ADataSet.Fields[i].FieldName + '</th>';
        sLine := sLine + '</tr>';
        Writeln(OutFile, sLine);

        // Count records
        RecCount := 0;
        ADataSet.First;
        while not ADataSet.EOF do
        begin
          Inc(RecCount);
          ADataSet.Next;
        end;

        // Export rows
        ExportedCount := 0;
        ADataSet.First;
        while not ADataSet.EOF do
        begin
          sLine := '  <tr>';
          for i := 0 to FieldCount - 1 do
          begin
            // Array-Felder zuerst behandeln
            if ADataSet.Fields[i] is TIBArrayField then
            begin
              //sValue := IB_Array_AsText(TIBArrayField(ADataSet.Fields[i])); or
              sValue := IB_ND_Array_AsText(TIBArrayField(ADataSet.Fields[i]));
            end
            else if ADataSet.Fields[i].IsNull then
              sValue := ''  // leeres Feld
            else if ADataSet.Fields[i].DataType in [ftFloat, ftCurrency] then
              sValue := StringReplace(FormatFloat('0.##', ADataSet.Fields[i].AsFloat), ',', '.', [rfReplaceAll])
            else if ADataSet.Fields[i].DataType in [ftDate, ftDateTime, ftTime] then
              sValue := FormatDateTime('yyyy-mm-dd', ADataSet.Fields[i].AsDateTime)
            else
            begin
              try
                sValue := VarToStr(ADataSet.Fields[i].Value);
              except
                sValue := 'Array';
              end;
            end;

            // Remove line breaks
            sValue := StringReplace(sValue, sLineBreak, ' ', [rfReplaceAll]);
            sValue := StringReplace(sValue, #10, ' ', [rfReplaceAll]);
            sValue := StringReplace(sValue, #13, ' ', [rfReplaceAll]);

            // HTML-escape
            sValue := StringReplace(sValue, '&', '&amp;', [rfReplaceAll]);
            sValue := StringReplace(sValue, '<', '&lt;', [rfReplaceAll]);
            sValue := StringReplace(sValue, '>', '&gt;', [rfReplaceAll]);

            sLine := sLine + '<td>' + sValue + '</td>';
          end;
          sLine := sLine + '</tr>';
          Writeln(OutFile, sLine);

          Inc(ExportedCount);
          ProgressBar.Position := Round(ExportedCount / RecCount * 100);
          LabelProgress.Caption := IntToStr(ProgressBar.Position) + '%';
          ProgressForm.Update;

          ADataSet.Next;
        end;

        // HTML Footer
        Writeln(OutFile, '</table>');
        Writeln(OutFile, '</body>');
        Writeln(OutFile, '</html>');

      finally
        CloseFile(OutFile);
      end;

      ProgressForm.Hide;
      ShowMessage(Format('Export completed. %d records exported.', [ExportedCount]));

    finally
      ProgressForm.Free;
    end;

  finally
    SaveDlg.Free;
  end;
end;

end.

