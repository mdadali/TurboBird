program ai_prompt;

{$mode objfpc}{$H+}

uses
  cthreads,
  Interfaces, Forms,

  fmAIPrompt,
  uOllamaAgent;

var
  Form1: TfmAIPromptForm;
begin
  Application.Initialize;
  Application.CreateForm(TfmAIPromptForm, Form1);
  Application.Run;
end.
