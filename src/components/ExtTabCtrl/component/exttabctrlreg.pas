unit ExtTabCtrlReg;

{$mode objfpc}{$H+}

// Design-time registration unit for TExtTabCtrl
interface

uses
  Classes, SysUtils, ComponentEditors, PropEdits, GraphPropEdits, ImgList,
  ExtTabCtrl;

type
  TExtTabCtrlEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): String; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  // Property editor for TabIndex that also notifies the control to repaint
  // when the value is changed from the Object Inspector/component
  // tree, so the selected tab is visible at design time
  TTabIndexPropertyEditor = class(TIntegerPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: String; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const NewValue: String); override;
  end;

  TExtTabCtrlImageIndexProperty = class(TImageIndexPropertyEditor)
  protected
    function GetImageList: TCustomImageList; override;
  end;

procedure Register;

implementation

{ TExtTabCtrlEditor }
procedure TExtTabCtrlEditor.ExecuteVerb(Index: Integer);
var
  TabControl: TExtTabCtrl;
  TargetIndex: Integer;
  CurrentTab, NewTab: TExtTab;

procedure RebuildDesignerTabTree(Ctrl: TExtTabCtrl; OldIdx, NewIdx: Integer);
  begin
    // Move the tab in the collection
    Ctrl.Tabs[OldIdx].Index := NewIdx;
    Ctrl.TabIndex := NewIdx;

    // Notify the designer. The component tree does not reorder existing
    // nodes in Lazarus, but the control renders correctly and the Object
    // Inspector reflects the right state. Attempting to delete+readd nodes
    // causes "list index out of bounds" because DeletePersistent frees the
    // collection items while the loop is still running.
    Designer.Modified;
    if Assigned(GlobalDesignHook) then
    begin
      GlobalDesignHook.RefreshPropertyValues;
      GlobalDesignHook.SelectOnlyThis(Ctrl.Tabs[NewIdx]);
    end;
  end;
begin
  TabControl := TExtTabCtrl(Component);
  TargetIndex := TabControl.TabIndex;

  case Index of
    0: begin // Add Tab
      NewTab := TabControl.AddTab('New Tab ' + IntToStr(TabControl.Tabs.Count + 1));
      Designer.Modified;
      if Assigned(NewTab) and Assigned(GlobalDesignHook) then
      begin
        GlobalDesignHook.PersistentAdded(NewTab, True);
        Exit;
      end;
    end;

    1: begin // Delete Tab
      if (TargetIndex >= 0) and (TargetIndex < TabControl.Tabs.Count) then
      begin
        CurrentTab := TabControl.Tabs[TargetIndex];
        // DeletePersistent removes the node; DeleteTab is not needed
        if Assigned(GlobalDesignHook) then
          GlobalDesignHook.DeletePersistent(TPersistent(CurrentTab));
        //TabControl.DeleteTab(TargetIndex);
      end;
    end;

    2: begin // Move Left / Move Up
      if TargetIndex > 0 then
      begin
        RebuildDesignerTabTree(TabControl, TargetIndex, TargetIndex - 1);
        Exit;
      end;
    end;

    3: begin // Move Right / Move Down
      if (TargetIndex >= 0) and (TargetIndex < TabControl.Tabs.Count - 1) then
      begin
        RebuildDesignerTabTree(TabControl, TargetIndex, TargetIndex + 1);
        Exit;
      end;
    end;
  end;

  Designer.Modified;
  Designer.SelectOnlyThisComponent(TabControl);
end;

function TExtTabCtrlEditor.GetVerb(Index: Integer): String;
var
  TabControl: TExtTabCtrl;
begin
  TabControl := TExtTabCtrl(Component);

  case Index of
    0: Result := 'Add Tab';
    1: Result := 'Delete Tab';
    2: if TabControl.IsHorizontal then
         Result := 'Move Left'
       else
         Result := 'Move Up';
    3: if TabControl.IsHorizontal then
         Result := 'Move Right'
       else
         Result := 'Move Down';
    else
      Result := '';
  end;
end;

function TExtTabCtrlEditor.GetVerbCount: Integer;
begin
  Result := 4;
end;

procedure Register;
begin
  RegisterComponents('Common Controls', [TExtTabCtrl]);
  RegisterComponentEditor(TExtTabCtrl, TExtTabCtrlEditor);

  // Register the custom property editor so that changing TabIndex in the
  // Object Inspector immediately updates the visible tab at design time
  RegisterPropertyEditor(TypeInfo(Integer), TExtTabCtrl, 'TabIndex', TTabIndexPropertyEditor);

  // Register visual dropdowns for the ImageIndex properties on Tabs and Buttons
  RegisterPropertyEditor(TypeInfo(TImageIndex), TExtTab, 'ImageIndex', TExtTabCtrlImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TButtonImages, '', TExtTabCtrlImageIndexProperty);
end;

{ TTabIndexPropertyEditor }
procedure TTabIndexPropertyEditor.GetValues(Proc: TGetStrProc);
var
  Ctrl: TExtTabCtrl;
  i: Integer;
begin
  Ctrl := GetComponent(0) as TExtTabCtrl;
  if not Assigned(Ctrl) then Exit;
  for i := 0 to Ctrl.Tabs.Count - 1 do
    Proc(IntToStr(i) + ' - ' + Ctrl.Tabs[i].Caption);
end;

function TTabIndexPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paRevertable];
end;

function TTabIndexPropertyEditor.GetValue: String;
var
  Ctrl: TExtTabCtrl;
begin
  Ctrl := GetComponent(0) as TExtTabCtrl;
  if Assigned(Ctrl) and (Ctrl.TabIndex >= 0) and (Ctrl.TabIndex < Ctrl.Tabs.Count) then
    Result := IntToStr(Ctrl.TabIndex) + ' - ' + Ctrl.Tabs[Ctrl.TabIndex].Caption
  else
    Result := IntToStr(GetOrdValue);
end;

procedure TTabIndexPropertyEditor.SetValue(const NewValue: String);
var
  Ctrl: TExtTabCtrl;
  Idx: Integer;
  S: String;
begin
  Ctrl := GetComponent(0) as TExtTabCtrl;
  if not Assigned(Ctrl) then Exit;
  // Accept either a plain integer ("2") or the "2 - Caption" format
  S := Trim(NewValue);
  if Pos(' ', S) > 0 then
    S := Copy(S, 1, Pos(' ', S) - 1);
  Idx := StrToIntDef(S, -1);
  Ctrl.SetDesignTabIndex(Idx);
end;

{ TExtTabCtrlImageIndexProperty }
function TExtTabCtrlImageIndexProperty.GetImageList: TCustomImageList;
var
  P: TPersistent;
  TC: TExtTabCtrl;
begin
  Result := nil;
  P := GetComponent(0);

  if P is TExtTab then
    TC := TExtTab(P).GetOwner
  else if P is TButtonImages then
    TC := TButtonImages(P).GetOwner
  else
    Exit;

  if Assigned(TC) then
    Result := TC.Images;
end;


end.
