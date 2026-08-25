unit RVSEdit;

interface


uses
  {$I RV_Defs.inc}

  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  RVStyle,
  {$IFDEF RICHVIEWDEF6}
  DesignIntf, DesignEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF}
  ComCtrls, ExtCtrls, StdCtrls;

const
  StandardStylesName:array[0..LAST_DEFAULT_STYLE_NO] of String =
         ( 'Normal Text', 'Heading', 'Subheading', 'Keywords',
           'Jump1', 'Jump2');
type
  {----------------------------------------------------------}
  TRVSProperty = class(TClassProperty)
   public
    //function GetValue:String; override;
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;
  {----------------------------------------------------------}
  TRVSEditor = class(TDefaultEditor)
  protected
    {$IFDEF RICHVIEWDEF6}
    procedure EditProperty(const PropertyEditor: IProperty;
      var Continue: Boolean); override;
    {$ELSE}
    procedure EditProperty(PropertyEditor: TPropertyEditor;
      var Continue, FreeEditor: Boolean); override;
    {$ENDIF}      
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;
  {----------------------------------------------------------}
  TfrmRVSEdit = class(TForm)
    btnOk: TButton;
    btnDel: TButton;
    btnEdit: TButton;
    btnAdd: TButton;
    Bevel1: TBevel;
    Label1: TLabel;
    tv: TTreeView;
    panPreview: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnDelClick(Sender: TObject);
    procedure tvChange(Sender: TObject; Node: TTreeNode);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
    TextStyles: TFontInfos;
    procedure UpdateList();
  public
    { Public declarations }
    IsModified: Boolean;
    procedure SetTextStyles(ATextStyles: TFontInfos);
  end;

implementation

{$R *.DFM}
{--------------------------------------------------------}
procedure TfrmRVSEdit.FormCreate(Sender: TObject);
begin
  TextStyles := nil;
  IsModified := False;
end;
{--------------------------------------------------------}
procedure TfrmRVSEdit.UpdateList();
var
  i: Integer;
begin
  tv.Items.Clear();
  if not Assigned(TextStyles) then Exit;
  for i := 0 to TextStyles.Count-1 do
  begin
    if i <= LAST_DEFAULT_STYLE_NO then
      tv.Items.Add(nil, IntToStr(i) + '. ' + StandardStylesName[i])
    else
      tv.Items.Add(nil, IntToStr(i) + '. ' + 'User Style');
  end;
  tv.Selected := tv.Items[0];
end;
{--------------------------------------------------------}
procedure TfrmRVSEdit.SetTextStyles(ATextStyles: TFontInfos);
begin
  TextStyles := ATextStyles;
  UpdateList();
end;
{--------------------------------------------------------}
procedure TfrmRVSEdit.btnAddClick(Sender: TObject);
begin
  TextStyles.Add();
  UpdateList();
  IsModified := True;
end;
{--------------------------------------------------------}
procedure TfrmRVSEdit.btnEditClick(Sender: TObject);
var
  dlg: TFontDialog;
  fnt: TFontInfo;
begin
  if not Assigned(tv.Selected) then
  begin
     Application.MessageBox('Style is not selected', 'Can not Edit',
                           MB_OK or MB_ICONSTOP);
     Exit;
  end;
  dlg := TFontDialog.Create(Self);
  try
    fnt := TFontInfo(TextStyles[tv.Selected.AbsoluteIndex]);
    dlg.Font.Name  := fnt.FontName;
    dlg.Font.Size  := fnt.Size;
    dlg.Font.Style := fnt.Style;
    dlg.Font.Color := fnt.Color;
    {$IFDEF RICHVIEWDEF3}
    dlg.Font.CharSet := fnt.CharSet;
    {$ENDIF}
    if dlg.Execute() then
    begin
      IsModified := True;
      fnt.FontName  := dlg.Font.Name;
      fnt.Size  := dlg.Font.Size;
      fnt.Style := dlg.Font.Style;
      fnt.Color := dlg.Font.Color;
      {$IFDEF RICHVIEWDEF3}
      fnt.CharSet := dlg.Font.CharSet;
      {$ENDIF}
    end;
  finally
    dlg.Free();
    UpdateList();
  end;
end;
{--------------------------------------------------------}
procedure TfrmRVSEdit.btnDelClick(Sender: TObject);
begin
   if not Assigned(tv.Selected) then
   begin
     Application.MessageBox('Style is not selected', 'Can not Edit',
                            MB_OK or MB_ICONSTOP);
     Exit;
   end;
   if tv.Selected.AbsoluteIndex <= LAST_DEFAULT_STYLE_NO then
     Application.MessageBox('Selected style is not user defined', 'Can not Delete',
                            MB_OK or MB_ICONSTOP)
   else
   begin
     IsModified := True;
     TextStyles.Delete(tv.Selected.AbsoluteIndex);
   end;
   UpdateList();
end;
{--------------------------------------------------------}
procedure TfrmRVSEdit.tvChange(Sender: TObject; Node: TTreeNode);
begin
  if not Assigned(tv.Selected) then
  begin
    panPreview.Caption := '';
    panPreview.Color := clBtnFace;
  end
  else
  begin
    panPreview.Caption := 'Style Preview';
    panPreview.Font.Size := TextStyles[tv.Selected.AbsoluteIndex].Size;
    panPreview.Font.Style := TextStyles[tv.Selected.AbsoluteIndex].Style;
    panPreview.Font.Color := TextStyles[tv.Selected.AbsoluteIndex].Color;
    {$IFDEF RICHVIEWDEF3}
    panPreview.Font.CharSet := TextStyles[tv.Selected.AbsoluteIndex].CharSet;
    {$ENDIF}
    if ColorToRGB(panPreview.Font.Color) = ColorToRGB(clBtnFace) then
      panPreview.Color := clWindow
    else
      panPreview.Color := clBtnFace;
  end;
end;
{--------------------------------------------------------}
procedure TfrmRVSEdit.FormActivate(Sender: TObject);
begin
  tvChange(tv, nil);
end;
{==========================================================}
procedure TRVSProperty.Edit();
var
  frm: TfrmRVSEdit;
begin
  frm := TfrmRVSEdit.Create(Application);
  try
    frm.SetTextStyles(TFontInfos(GetOrdValue()));
    frm.ShowModal();
    if frm.IsModified then Modified();
  finally
    frm.Free();
  end;
end;
{--------------------------------------------------------}
function TRVSProperty.GetAttributes(): TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;
{==========================================================}
{$IFDEF RICHVIEWDEF6}
procedure TRVSEditor.EditProperty(const PropertyEditor: IProperty;
                                  var Continue: Boolean);
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName();
  if (CompareText(PropName, 'TextStyles') = 0) then
  begin
    PropertyEditor.Edit();
    Continue := False;
  end;
end;
{$ELSE}
procedure TRVSEditor.EditProperty(PropertyEditor: TPropertyEditor;
  var Continue, FreeEditor: Boolean);
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName();
  if (CompareText(PropName, 'TextStyles') = 0) then
  begin
    PropertyEditor.Edit();
    Continue := False;
  end;
end;
{$ENDIF}
{--------------------------------------------------------}
function TRVSEditor.GetVerbCount(): Integer;
begin
  Result := 1;
end;
{--------------------------------------------------------}
function TRVSEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := 'Edit Text Styles'
  else
    Result := '';
end;
{--------------------------------------------------------}
procedure TRVSEditor.ExecuteVerb(Index: Integer);
begin
  if Index = 0 then Edit;
end;

end.
