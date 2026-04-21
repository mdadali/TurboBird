unit uPSC_DBControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  DB, DBCtrls, DBGrids,
  Controls, StdCtrls,
  uPSCompiler, uPSRuntime;

procedure SIRegister_DBControls(CL: TPSPascalCompiler);
procedure RIRegister_DBControls(CL: TPSRuntimeClassImporter);

implementation

{ ==================== COMPILE TIME ==================== }

procedure SIRegister_TDBEdit(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TCustomEdit'), 'TDBEdit') do
  begin
    RegisterProperty('DataSource', 'TDataSource', iptrw);
    RegisterProperty('DataField', 'string', iptrw);
    RegisterProperty('ReadOnly', 'Boolean', iptrw);
    RegisterProperty('OnChange', 'TNotifyEvent', iptrw);
  end;
end;

procedure SIRegister_TDBMemo(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TCustomMemo'), 'TDBMemo') do
  begin
    RegisterProperty('DataSource', 'TDataSource', iptrw);
    RegisterProperty('DataField', 'string', iptrw);
    RegisterProperty('ReadOnly', 'Boolean', iptrw);
  end;
end;

procedure SIRegister_TDBText(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TCustomLabel'), 'TDBText') do
  begin
    RegisterProperty('DataSource', 'TDataSource', iptrw);
    RegisterProperty('DataField', 'string', iptrw);
  end;
end;

procedure SIRegister_TDBCheckBox(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TCustomCheckBox'), 'TDBCheckBox') do
  begin
    RegisterProperty('DataSource', 'TDataSource', iptrw);
    RegisterProperty('DataField', 'string', iptrw);
    RegisterProperty('ValueChecked', 'string', iptrw);
    RegisterProperty('ValueUnchecked', 'string', iptrw);
  end;
end;

procedure SIRegister_TDBImage(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TCustomControl'), 'TDBImage') do
  begin
    RegisterProperty('DataSource', 'TDataSource', iptrw);
    RegisterProperty('DataField', 'string', iptrw);
  end;
end;

procedure SIRegister_TDBNavigator(CL: TPSPascalCompiler);
begin
  CL.AddTypeS('TNavigateBtn',
    '(nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbDelete, nbEdit, nbPost, nbCancel, nbRefresh)');
  CL.AddTypeS('TNavigateBtnSet', 'set of TNavigateBtn');

  with CL.AddClassN(CL.FindClass('TCustomControl'), 'TDBNavigator') do
  begin
    RegisterProperty('DataSource', 'TDataSource', iptrw);
    RegisterProperty('VisibleButtons', 'TNavigateBtnSet', iptrw);
    RegisterProperty('OnClick', 'TNotifyEvent', iptrw);
    RegisterProperty('Color', 'integer', iptrw);
  end;
end;

procedure SIRegister_TDBGrid(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TCustomControl'), 'TDBGrid') do
  begin
    RegisterMethod('procedure Refresh');
    RegisterProperty('DataSource', 'TDataSource', iptrw);
    RegisterProperty('ReadOnly', 'Boolean', iptrw);
    RegisterProperty('OnCellClick', 'TNotifyEvent', iptrw);
    RegisterProperty('Color', 'integer', iptrw);
  end;
end;

procedure SIRegister_TDBNavButton(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TSpeedButton'), 'TDBNavButton') do
  begin
    RegisterProperty('Enabled', 'Boolean', iptrw);
    RegisterProperty('Visible', 'Boolean', iptrw);
    RegisterProperty('Left', 'Integer', iptrw);
    RegisterProperty('Top', 'Integer', iptrw);
    RegisterProperty('Width', 'Integer', iptrw);
    RegisterProperty('Height', 'Integer', iptrw);
    RegisterProperty('Tag', 'Integer', iptrw);
    RegisterProperty('OnClick', 'TNotifyEvent', iptrw);
  end;
end;

procedure SIRegister_DBControls(CL: TPSPascalCompiler);
begin
  SIRegister_TDBNavButton(CL);
  SIRegister_TDBEdit(CL);
  SIRegister_TDBMemo(CL);
  SIRegister_TDBText(CL);
  SIRegister_TDBCheckBox(CL);
  SIRegister_TDBImage(CL);
  SIRegister_TDBNavigator(CL);
  SIRegister_TDBGrid(CL);
end;

{ ==================== RUNTIME ==================== }

{ --- Helper --- }

procedure TDBEdit_DataSource_R(Self: TDBEdit; var T: TDataSource); begin T := Self.DataSource; end;
procedure TDBEdit_DataSource_W(Self: TDBEdit; T: TDataSource); begin Self.DataSource := T; end;
procedure TDBEdit_DataField_R(Self: TDBEdit; var T: string); begin T := Self.DataField; end;
procedure TDBEdit_DataField_W(Self: TDBEdit; const T: string); begin Self.DataField := T; end;

procedure TDBEdit_ReadOnly_R(Self: TDBEdit; var T: Boolean); begin T := Self.ReadOnly; end;
procedure TDBEdit_ReadOnly_W(Self: TDBEdit; T: Boolean); begin Self.ReadOnly := T; end;

{ --- DBNavigator --- }

procedure TDBNavigator_DataSource_R(Self: TDBNavigator; var T: TDataSource); begin T := Self.DataSource; end;
procedure TDBNavigator_DataSource_W(Self: TDBNavigator; T: TDataSource); begin Self.DataSource := T; end;

{ --- DBNavigatorButton --- }
procedure TDBNavButton_Enabled_R(Self: TDBNavButton; var T: Boolean); begin T := Self.Enabled; end;
procedure TDBNavButton_Enabled_W(Self: TDBNavButton; T: Boolean); begin Self.Enabled := T; end;

procedure TDBNavButton_Visible_R(Self: TDBNavButton; var T: Boolean); begin T := Self.Visible; end;
procedure TDBNavButton_Visible_W(Self: TDBNavButton; T: Boolean); begin Self.Visible := T; end;

procedure TDBNavButton_Left_R(Self: TDBNavButton; var T: Integer); begin T := Self.Left; end;
procedure TDBNavButton_Left_W(Self: TDBNavButton; T: Integer); begin Self.Left := T; end;

procedure TDBNavButton_Top_R(Self: TDBNavButton; var T: Integer); begin T := Self.Top; end;
procedure TDBNavButton_Top_W(Self: TDBNavButton; T: Integer); begin Self.Top := T; end;

procedure TDBNavButton_Width_R(Self: TDBNavButton; var T: Integer); begin T := Self.Width; end;
procedure TDBNavButton_Width_W(Self: TDBNavButton; T: Integer); begin Self.Width := T; end;

procedure TDBNavButton_Height_R(Self: TDBNavButton; var T: Integer); begin T := Self.Height; end;
procedure TDBNavButton_Height_W(Self: TDBNavButton; T: Integer); begin Self.Height := T; end;

procedure TDBNavButton_Tag_R(Self: TDBNavButton; var T: Integer); begin T := Self.Tag; end;
procedure TDBNavButton_Tag_W(Self: TDBNavButton; T: Integer); begin Self.Tag := T; end;

{ --- DBGrid --- }

procedure TDBGrid_DataSource_R(Self: TDBGrid; var T: TDataSource); begin T := Self.DataSource; end;
procedure TDBGrid_DataSource_W(Self: TDBGrid; T: TDataSource); begin Self.DataSource := T; end;

procedure TDBGrid_ReadOnly_R(Self: TDBGrid; var T: Boolean); begin T := Self.ReadOnly; end;
procedure TDBGrid_ReadOnly_W(Self: TDBGrid; T: Boolean); begin Self.ReadOnly := T; end;

{ === REGISTER === }

procedure RIRegister_TDBEdit(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TDBEdit) do
  begin
    RegisterPropertyHelper(@TDBEdit_DataSource_R, @TDBEdit_DataSource_W, 'DataSource');
    RegisterPropertyHelper(@TDBEdit_DataField_R, @TDBEdit_DataField_W, 'DataField');
    RegisterPropertyHelper(@TDBEdit_ReadOnly_R, @TDBEdit_ReadOnly_W, 'ReadOnly');
  end;
end;

procedure RIRegister_TDBNavigator(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TDBNavigator) do
  begin
    RegisterPropertyHelper(@TDBNavigator_DataSource_R, @TDBNavigator_DataSource_W, 'DataSource');
  end;
end;

procedure RIRegister_TDBNavButton(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TDBNavButton) do
  begin
    RegisterPropertyHelper(@TDBNavButton_Enabled_R, @TDBNavButton_Enabled_W, 'Enabled');
    RegisterPropertyHelper(@TDBNavButton_Visible_R, @TDBNavButton_Visible_W, 'Visible');
    RegisterPropertyHelper(@TDBNavButton_Left_R, @TDBNavButton_Left_W, 'Left');
    RegisterPropertyHelper(@TDBNavButton_Top_R, @TDBNavButton_Top_W, 'Top');
    RegisterPropertyHelper(@TDBNavButton_Width_R, @TDBNavButton_Width_W, 'Width');
    RegisterPropertyHelper(@TDBNavButton_Height_R, @TDBNavButton_Height_W, 'Height');
    RegisterPropertyHelper(@TDBNavButton_Tag_R, @TDBNavButton_Tag_W, 'Tag');
  end;
end;

procedure RIRegister_TDBGrid(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TDBGrid) do
  begin
    RegisterMethod(@TDBGrid.Refresh, 'Refresh');
    RegisterPropertyHelper(@TDBGrid_DataSource_R, @TDBGrid_DataSource_W, 'DataSource');
    RegisterPropertyHelper(@TDBGrid_ReadOnly_R, @TDBGrid_ReadOnly_W, 'ReadOnly');
  end;
end;

procedure RIRegister_DBControls(CL: TPSRuntimeClassImporter);
begin
  RIRegister_TDBNavButton(CL);
  RIRegister_TDBEdit(CL);
  RIRegister_TDBNavigator(CL);
  RIRegister_TDBGrid(CL);
end;

end.
