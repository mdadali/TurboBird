unit fDataStudio;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls;

type

  { TfrmDataStudio }

  TfrmDataStudio = class(TForm)
    btnMakeTable: TButton;
    btnMakeFromTable: TButton;
    btnOpenFile: TButton;
    btnSaveFileAs: TButton;
    PageControl1: TPageControl;
    Panel1: TPanel;
    StatusBar1: TStatusBar;
  private

  public

  end;

var
  frmDataStudio: TfrmDataStudio;

implementation

{$R *.lfm}

end.

