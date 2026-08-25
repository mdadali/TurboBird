unit RVFreeReg;
interface

  {$I RV_Defs.inc}

uses
  Classes,
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  RichView, RVStyle, PtblRV;

procedure Register;

implementation

{--------------------------------------------------------------}
procedure Register;
begin
  RegisterComponents('RichView', [TRVStyle, TRichView,TRichEdit, TRVPrint]);
end;

initialization
{$I richview.lrs}

end.
