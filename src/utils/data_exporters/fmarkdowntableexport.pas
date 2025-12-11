unit fMarkDownTableExport;

{$mode ObjFPC}{$H+}

interface

uses
  Forms, Dialogs, Classes, SysUtils,  StdCtrls, ComCtrls,
  Variants,
  DB, IBDatabase, IBCustomDataSet,
  IBArrayHelper;


procedure _ExportDataMarkDownTable(ADataSet: TDataSet);


implementation


procedure _ExportDataMarkDownTable(ADataSet: TDataSet);
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
    SaveDlg.Filter := 'Markdown file|*.md|Text file|*.txt|All files|*.*';
    SaveDlg.DefaultExt := 'md';
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

        // Header-Zeile
        sLine := '|';
        for i := 0 to FieldCount - 1 do
          sLine := sLine + ' ' + ADataSet.Fields[i].FieldName + ' |';
        Writeln(OutFile, sLine);

        // Trennlinie
        sLine := '|';
        for i := 0 to FieldCount - 1 do
          sLine := sLine + '---|';
        Writeln(OutFile, sLine);

        // Zählen der Datensätze
        RecCount := 0;
        ADataSet.First;
        while not ADataSet.EOF do
        begin
          Inc(RecCount);
          ADataSet.Next;
        end;

        // Export
        ExportedCount := 0;
        ADataSet.First;
        while not ADataSet.EOF do
        begin
          sLine := '|';
          for i := 0 to FieldCount - 1 do
          begin
            // Array-Felder zuerst prüfen
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

            // Zeilenumbrüche entfernen
            sValue := StringReplace(sValue, sLineBreak, ' ', [rfReplaceAll]);
            sValue := StringReplace(sValue, #10, ' ', [rfReplaceAll]);
            sValue := StringReplace(sValue, #13, ' ', [rfReplaceAll]);

            sLine := sLine + ' ' + sValue + ' |';
          end;

          Writeln(OutFile, sLine);

          Inc(ExportedCount);
          ProgressBar.Position := Round(ExportedCount / RecCount * 100);
          LabelProgress.Caption := IntToStr(ProgressBar.Position) + '%';
          ProgressForm.Update;

          ADataSet.Next;
        end;

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

