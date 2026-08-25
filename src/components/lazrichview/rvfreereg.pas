unit RVFreeReg;

interface

{$I RV_Defs.inc}

uses
  Classes,
  {$IFDEF FPC}
  LResources,
  {$ELSE}
    {$IFDEF RICHVIEWDEF6}
    DesignIntf, 
    {$ELSE}
    DsgnIntf,
    {$ENDIF}
    RVSEdit,
  {$ENDIF}
  RichView, RVStyle, PtblRV;

procedure Register;

implementation

{--------------------------------------------------------------}
procedure Register;
begin
  RegisterComponents('RichView', [TRVStyle, TRichView, TRVPrint]);
{$IFNDEF FPC}
  RegisterComponentEditor(TRVStyle, TRVSEditor);
  RegisterPropertyEditor(TypeInfo(TFontInfos), TRVStyle, '', TRVSProperty);
{$ENDIF}
end;

initialization

{$IFDEF FPC}
{$I richview.lrs}
{$ENDIF}

end.
