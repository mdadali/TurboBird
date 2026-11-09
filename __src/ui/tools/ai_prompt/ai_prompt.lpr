program ai_prompt;

{$mode objfpc}{$H+}

uses
  cthreads,
  Interfaces, Forms, FrameViewer09, rxnew,

  fmAIPrompt,
  uOllamaAgent,
  uSystemTools,
  uWebTools;

var
  Form1: TfmAIPromptForm;
begin
  Application.Initialize;
  Application.CreateForm(TfmAIPromptForm, Form1);
  Application.Run;
end.
