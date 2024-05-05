{*******************************************************************************

  ABF Visual Components Library, OneTouch(tm) Designer

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfDesignerStatusBar;

{$I abf.inc}

interface

uses
{$IfDef D6}
  DesignIntf, DesignEditors,
{$Else D6}
  DsgnIntf,
{$EndIf D6}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Buttons,
  Menus, ExtCtrls, ComCtrls, StdCtrls, abfStatusBars;

type

{$IFDEF D4}
  TDesigner = IDesigner;
  {$IfDef D6}
  TFormDesigner = IDesigner;
  {$Else D6}
  TFormDesigner = IFormDesigner;
  {$EndIf D6}
{$ENDIF}

{******************************************************************************}
{ TabfStatusPanelsEditor }

  TabfStatusPanelsEditor = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    btnApply: TButton;
    gbxPanels: TGroupBox;
    plToolbar: TPanel;
    sbtnNewItem: TSpeedButton;
    sbtnDelItem: TSpeedButton;
    sbtnUpItem: TSpeedButton;
    sbtnDownItem: TSpeedButton;
    plBorder: TPanel;
    gbxPanelProperties: TGroupBox;
    lbBevel: TLabel;
    lbBrushColor: TLabel;
    lbBrushStyle: TLabel;
    lbPanelStyle: TLabel;
    lbName: TLabel;
    lbWidth: TLabel;
    lbParentColor: TLabel;
    lbParentFont: TLabel;
    cbxBevel: TComboBox;
    cbxBrushColor: TComboBox;
    cbxBrushStyle: TComboBox;
    cbxPanelStyle: TComboBox;
    pcStyleProperties: TPageControl;
    tsTextStyle: TTabSheet;
    lbSpacing: TLabel;
    Label6: TLabel;
    lbGlyphIndex: TLabel;
    lbLayout: TLabel;
    lbScrollEffect: TLabel;
    lbText: TLabel;
    edSpacing: TEdit;
    cbxAlignment: TComboBox;
    edGlyphIndex: TEdit;
    cbxLayout: TComboBox;
    cbxScrollEffect: TComboBox;
    edText: TEdit;
    tsProgressBarStyle: TTabSheet;
    lbShowCaption: TLabel;
    Label1: TLabel;
    lbMinValue: TLabel;
    Label3: TLabel;
    cbxShowCaption: TComboBox;
    edProgress: TEdit;
    edMinValue: TEdit;
    edMaxValue: TEdit;
    edName: TEdit;
    edWidth: TEdit;
    btnFont: TButton;
    cbxParentColor: TComboBox;
    cbxParentFont: TComboBox;
    lbxPanels: TListBox;
    cbxShowText: TComboBox;
    lbShowText: TLabel;
    lbPopupMenu: TLabel;
    cbxPopupMenu: TComboBox;
    edHint: TEdit;
    lbHint: TLabel;
    tsLockKeyStyle: TTabSheet;
    lbLockKeyCaption: TLabel;
    edLockKeyCaption: TEdit;
    lbLockKeyType: TLabel;
    cbxLockKeyType: TComboBox;
    lbProgressCaption: TLabel;
    edProgressCaption: TEdit;
    lbProgressType: TLabel;
    cbxProgressType: TComboBox;
    lbLockKeyColorOn: TLabel;
    cbxLockKeyColorOn: TComboBox;
    lbLockKeyColorOff: TLabel;
    cbxLockKeyColorOff: TComboBox;
    procedure AddClick(Sender: TObject);
    procedure DeleteClick(Sender: TObject);
    procedure MoveUpClick(Sender: TObject);
    procedure MoveDownClick(Sender: TObject);
    procedure lbxPanelsClick(Sender: TObject);
    procedure lbxPanelsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lbxPanelsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lbxPanelsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PropertiesModified(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure cbxBrushColorChange(Sender: TObject);
    procedure cbxParentColorChange(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
    procedure cbxParentFontChange(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure cbxPopupMenuKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cbxLockKeyTypeChange(Sender: TObject);
    procedure cbxProgressTypeChange(Sender: TObject);
  private
    FEditingPanels: TabfStatusPanels;
    FDesigner: TFormDesigner;
    FLoadingProperties: Boolean;
    FabfStatusBar: TabfStatusBar;
    FCurrFont: TFont;
    FCurrPanel: TabfStatusPanel;
    procedure GetColorName(const St: String);
  protected
    function GetItemName(ItemIndex: Integer): string; virtual;
    procedure EnableControls; virtual;
    procedure DisableControls; virtual;
    procedure GetProperties(Item: TabfStatusPanel); virtual;
    function SetProperties(Item: TabfStatusPanel): Boolean; virtual;
    procedure UpdateListbox(Item: TabfStatusPanel); virtual;
  end;

{******************************************************************************}
{ TabfStatusPanelsEditor }

  TabfStatusPanelsEditorClass = class of TabfStatusPanelsEditor;

  TabfStatusPanelsPropertyEditor = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

{******************************************************************************}
{ Designing }

procedure ShowAbfStatusPanelsEditor(ADesigner: TDesigner; AStatusBar: TabfStatusBar);
function ShowAbfStatusPanelsEditorClass(ADesigner: TDesigner;
  AStatusBar: TabfStatusBar): TabfStatusPanelsEditor;

{******************************************************************************}
implementation
{******************************************************************************}
{$R *.DFM}
uses
  Dialogs, TypInfo, abfSysUtils, abfGraphics, abfVclUtils;

type
  THackStatusPanels = class(TabfStatusPanels);

{******************************************************************************}
{ Designing }

function ShowAbfStatusPanelsEditorClass(ADesigner: TDesigner;
  AStatusBar: TabfStatusBar): TabfStatusPanelsEditor;
begin
  Result := TabfStatusPanelsEditorClass.Create(nil);
  with Result do
  try
    FDesigner := ADesigner as TFormDesigner;
    FabfStatusBar := AStatusBar;
    FEditingPanels := TabfStatusPanels.Create(FabfStatusBar);
    FCurrFont := TFont.Create;
    Caption := Format('Editing %s.Panels', [FabfStatusBar.Name]);
    abfFormFitToScreen(Result);
    ShowModal;
  finally
    FCurrFont.Free;
    FEditingPanels.Free;
    Free;
  end;
end;

{--------------------------------------}

procedure ShowAbfStatusPanelsEditor(ADesigner: TDesigner; AStatusBar: TabfStatusBar);
begin
  ShowAbfStatusPanelsEditorClass(ADesigner, AStatusBar);
end;


{******************************************************************************}
{ TabfStatusPanelsPropertyEditor }

procedure TabfStatusPanelsPropertyEditor.Edit;
var
  Obj: TabfStatusBar;
begin
  Obj := GetComponent(0) as TabfStatusBar;
  ShowAbfStatusPanelsEditorClass(Designer, Obj);
end;

{--------------------------------------}

function TabfStatusPanelsPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

{--------------------------------------}

function TabfStatusPanelsEditor.GetItemName(ItemIndex: Integer): string;
begin
  if Trim(TabfStatusPanel(FEditingPanels.Items[ItemIndex]).Name) = '' then
    THackStatusPanels(FEditingPanels).SetItemName(FEditingPanels.Items[ItemIndex]);
  Result := TabfStatusPanel(FEditingPanels.Items[ItemIndex]).Name;
  Result := Format('%d - %s',[ItemIndex, Result]);
end;

{--------------------------------------}

procedure TabfStatusPanelsEditor.EnableControls;
var i, j: Integer;
begin
  sbtnDelItem.Enabled := True;
  with gbxPanelProperties do
  for i := 0 to gbxPanelProperties.ControlCount - 1 do
  if Controls[i] is TEdit then
    TEdit(Controls[i]).Enabled := True
  else if Controls[i] is TComboBox then
    TComboBox(Controls[i]).Enabled := True
  else if Controls[i] is TButton then
    TButton(Controls[i]).Enabled := True;
  for i := 0 to pcStyleProperties.ControlCount - 1 do
  with TTabSheet(pcStyleProperties.Controls[i]) do
  for j := 0 to ControlCount - 1 do
  if Controls[j] is TEdit then
    TEdit(Controls[j]).Enabled := True
  else if Controls[j] is TComboBox then
    TComboBox(Controls[j]).Enabled := True
  else if Controls[j] is TButton then
    TButton(Controls[j]).Enabled := True;
end;

{--------------------------------------}

procedure TabfStatusPanelsEditor.DisableControls;
var i, j: Integer;
begin
  sbtnDelItem.Enabled := False;
  sbtnUpItem.Enabled := False;
  sbtnDownItem.Enabled := False;
  with gbxPanelProperties do
  for i := 0 to ControlCount - 1 do
  if Controls[i] is TEdit then
  begin
    TEdit(Controls[i]).Text := '';
    TEdit(Controls[i]).Enabled := False;
  end
  else if Controls[i] is TComboBox then
  begin
    TComboBox(Controls[i]).Enabled := False;
    TComboBox(Controls[i]).ItemIndex := -1;
  end
  else if Controls[i] is TButton then
    TButton(Controls[i]).Enabled := False;
  for i := 0 to pcStyleProperties.ControlCount - 1 do
  with TTabSheet(pcStyleProperties.Controls[i]) do
  for j := 0 to ControlCount - 1 do
  if Controls[j] is TEdit then
  begin
    TEdit(Controls[j]).Text := '';
    TEdit(Controls[j]).Enabled := False;
  end
  else if Controls[j] is TComboBox then
  begin
    TComboBox(Controls[j]).Enabled := False;
    TComboBox(Controls[j]).ItemIndex := -1;
  end
  else if Controls[j] is TButton then
    TButton(Controls[j]).Enabled := False;
end;

{--------------------------------------}

procedure TabfStatusPanelsEditor.UpdateListbox(Item: TabfStatusPanel);
var i: Integer;
begin
  if FEditingPanels = nil then Exit;
  lbxPanels.Items.BeginUpdate;
  try
    lbxPanels.Items.Clear;
    for i := 0 to FEditingPanels.Count - 1 do
    lbxPanels.Items.AddObject(GetItemName(i), TabfStatusPanel(FEditingPanels.Items[i]));
    i := -1;
    if Assigned(Item) then i := Item.Index
    else if (lbxPanels.Items.Count > 0) then i := 0;
    lbxPanels.ItemIndex := i;
  finally
    lbxPanels.Items.EndUpdate;
  end;
  if i >= 0 then
    FCurrPanel := TabfStatusPanel(FEditingPanels.Items[i])
  else
    FCurrPanel := nil;
  if Assigned(FCurrPanel) then FCurrFont.Assign(FCurrPanel.Font)
  else if Assigned(FabfStatusBar) then FCurrFont.Assign(FabfStatusBar.Font);
  GetProperties(FCurrPanel);
end;

{--------------------------------------}

procedure TabfStatusPanelsEditor.AddClick(Sender: TObject);
begin
  FEditingPanels.BeginUpdate;
  try
    if (FEditingPanels.Count > 0) and (not SetProperties(FCurrPanel)) then Abort;
    FCurrPanel := TabfStatusPanel(FEditingPanels.Add);
    PropertiesModified(Sender);
  finally
    FEditingPanels.EndUpdate;
    UpdateListbox(FCurrPanel);
  end;
end;

{--------------------------------------}

procedure TabfStatusPanelsEditor.DeleteClick(Sender: TObject);
var i: Integer;
begin
  if not Assigned(FCurrPanel) then Exit;
  try
    i := FCurrPanel.Index;
    TabfStatusPanel(FEditingPanels.Items[FCurrPanel.Index]).Free;
    if FEditingPanels.Count > 0 then
    begin
      if i >= FEditingPanels.Count then i := FEditingPanels.Count - 1;
      FCurrPanel := TabfStatusPanel(FEditingPanels.Items[i]);
    end else FCurrPanel := nil;
    PropertiesModified(Sender);
  finally
    UpdateListbox(FCurrPanel);
  end;
end;

{--------------------------------------}

procedure TabfStatusPanelsEditor.MoveUpClick(Sender: TObject);
var i: Integer;
begin
  if not Assigned(FCurrPanel) then Exit;
  try
    if (FEditingPanels.Count > 0) and (not SetProperties(FCurrPanel)) then Abort;
    i := FCurrPanel.Index;
    Dec(i);
    if i >= 0 then FCurrPanel.Index := i;
    PropertiesModified(Sender);
  finally
    UpdateListbox(FCurrPanel);
  end;
end;

{--------------------------------------}

procedure TabfStatusPanelsEditor.MoveDownClick(Sender: TObject);
var i: Integer;
begin
  if not Assigned(FCurrPanel) then Exit;
  try
    if (FEditingPanels.Count > 0) and (not SetProperties(FCurrPanel)) then Abort;
    i := FCurrPanel.Index;
    Inc(i);
    if i < FEditingPanels.Count then FCurrPanel.Index := i;
    PropertiesModified(Sender);
  finally
    UpdateListbox(FCurrPanel);
  end;
end;

{--------------------------------------}

procedure TabfStatusPanelsEditor.lbxPanelsClick(Sender: TObject);
var SavedIndex: Integer;
begin
  if lbxPanels.Items.Count = 0 then Exit;
  try
    if Assigned(FCurrPanel) then
    begin
      if SetProperties(FCurrPanel) then
      with lbxPanels.Items do
      begin
        BeginUpdate;
        try
          SavedIndex := lbxPanels.ItemIndex;
          lbxPanels.ItemIndex := FCurrPanel.Index;
          lbxPanels.Items[FCurrPanel.Index] := Format('%d - %s',[FCurrPanel.Index, FCurrPanel.Name]);
          lbxPanels.ItemIndex := SavedIndex;
        finally
          EndUpdate;
        end;
      end
      else Exit;
    end;
    FCurrPanel := TabfStatusPanel(lbxPanels.Items.Objects[lbxPanels.ItemIndex]);
    if Assigned(FCurrPanel) then FCurrFont.Assign(FCurrPanel.Font)
    else if Assigned(FabfStatusBar) then FCurrFont.Assign(FabfStatusBar.Font);
    GetProperties(FCurrPanel);
  except
    if Assigned(FCurrPanel) then lbxPanels.ItemIndex := FCurrPanel.Index;
    raise;
  end;
end;

{--------------------------------------}

procedure TabfStatusPanelsEditor.lbxPanelsKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  lbxPanelsClick(Sender);
end;

{--------------------------------------}

procedure TabfStatusPanelsEditor.lbxPanelsDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var Index: Integer;
begin
  Index := lbxPanels.ItemAtPos(Point(X, Y), True);
  Accept := (Index >= 0) and (Source = lbxPanels) and (lbxPanels.ItemIndex <> Index);
end;

{--------------------------------------}

procedure TabfStatusPanelsEditor.lbxPanelsDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var Index: Integer;
begin
  Index := lbxPanels.ItemAtPos(Point(X, Y), True);
  if (Index < 0) or (Source <> lbxPanels) then Exit;
  try
    FCurrPanel.Index := TabfStatusPanel(lbxPanels.Items.Objects[Index]).Index;
  finally
    UpdateListbox(FCurrPanel);
  end;
end;

{--------------------------------------}

procedure TabfStatusPanelsEditor.GetColorName(const St: String);
begin
  cbxBrushColor.Items.Add(St);
  cbxLockKeyColorOff.Items.Add(St);
  cbxLockKeyColorOn.Items.Add(St);
end;

{--------------------------------------}

procedure TabfStatusPanelsEditor.GetProperties(Item: TabfStatusPanel);
begin
  FLoadingProperties := True;
  try
    if Assigned(FEditingPanels) and Assigned(Item) then
    with Item do
    begin
      EnableControls;
      sbtnUpItem.Enabled := Index <> 0;
      sbtnDownItem.Enabled := not (Index = FEditingPanels.Count - 1);
      edName.Text := Name;
      edHint.Text := Hint;
      edWidth.Text := IntToStr(Width);
      cbxBevel.ItemIndex := Integer(Bevel);
      cbxPanelStyle.ItemIndex := Integer(PanelStyle);
      cbxParentFont.ItemIndex := Integer(ParentFont);
      cbxBrushColor.OnChange := nil;
      try
        cbxBrushColor.ItemIndex := cbxBrushColor.Items.IndexOf(abfColorToString(BrushColor));
      finally
        cbxBrushColor.OnChange := cbxBrushColorChange;
      end;
      cbxBrushStyle.ItemIndex := Integer(BrushStyle);
      cbxParentColor.OnChange := nil;
      try
        cbxParentColor.ItemIndex := Integer(ParentColor);
      finally
        cbxParentColor.OnChange := cbxParentColorChange;
      end;
      if Assigned(PopupMenu) then
        cbxPopupMenu.ItemIndex := cbxPopupMenu.Items.IndexOf(PopupMenu.Name)
      else
        cbxPopupMenu.ItemIndex := -1;
      with TextStyle do
      begin
        edText.Text := Text;
        cbxScrollEffect.ItemIndex := Integer(ScrollEffect);
        cbxAlignment.ItemIndex := Integer(Alignment);
        cbxLayout.ItemIndex := Integer(Layout);
        edGlyphIndex.Text := IntToStr(GlyphIndex);
        edSpacing.Text := IntToStr(Spacing);
        cbxShowText.ItemIndex := Integer(ShowText);
      end;
      with ProgressBarStyle do
      begin
        edProgressCaption.Text := Caption;
        cbxProgressType.ItemIndex := Integer(ProgressType);
        cbxShowCaption.ItemIndex := Integer(ShowCaption);
        edMinValue.Text := IntToStr(MinValue);
        edMaxValue.Text := IntToStr(MaxValue);
        edProgress.Text := IntToStr(Progress);
      end;
      with LockKeyStyle do
      begin
        edLockKeyCaption.Text := Caption;
        cbxLockKeyColorOff.OnChange := nil;
        try
          cbxLockKeyColorOff.ItemIndex := cbxLockKeyColorOff.Items.IndexOf(abfColorToString(ColorOff));
        finally
          cbxLockKeyColorOff.OnChange := PropertiesModified;
        end;
        cbxLockKeyColorOn.OnChange := nil;
        try
          cbxLockKeyColorOn.ItemIndex := cbxLockKeyColorOn.Items.IndexOf(abfColorToString(ColorOn));
        finally
          cbxLockKeyColorOn.OnChange := PropertiesModified;
        end;
        cbxLockKeyType.ItemIndex := Integer(LockKeyType);
      end;
    end
    else DisableControls;
  finally
    FLoadingProperties := False;
  end;
end;

{--------------------------------------}

function TabfStatusPanelsEditor.SetProperties(Item: TabfStatusPanel): Boolean;
begin
  Result := False;
  if Item = nil then
  begin
    GetProperties(FCurrPanel);
    Exit;
  end;
  try StrToInt(edWidth.Text)
  except
    if edWidth.CanFocus then edWidth.SetFocus;
    edWidth.SelectAll;
    raise;
  end;
  try StrToInt(edGlyphIndex.Text)
  except
    pcStyleProperties.ActivePage := tsTextStyle;
    if edGlyphIndex.CanFocus then edGlyphIndex.SetFocus;
    edGlyphIndex.SelectAll;
    raise;
  end;
  try StrToInt(edSpacing.Text)
  except
    pcStyleProperties.ActivePage := tsTextStyle;
    if edSpacing.CanFocus then edSpacing.SetFocus;
    edSpacing.SelectAll;
    raise;
  end;
  try StrToInt(edMinValue.Text)
  except
    pcStyleProperties.ActivePage := tsProgressBarStyle;
    if edMinValue.CanFocus then edMinValue.SetFocus;
    edMinValue.SelectAll;
    raise;
  end;
  try StrToInt(edMaxValue.Text)
  except
    pcStyleProperties.ActivePage := tsProgressBarStyle;
    if edMaxValue.CanFocus then edMaxValue.SetFocus;
    edMaxValue.SelectAll;
    raise;
  end;
  try StrToInt(edProgress.Text)
  except
    pcStyleProperties.ActivePage := tsProgressBarStyle;
    if edProgress.CanFocus then edProgress.SetFocus;
    edProgress.SelectAll;
    raise;
  end;
  FEditingPanels.BeginUpdate;
  with FCurrPanel do
  try
    Name := edName.Text;
    Hint := edHint.Text;
    Width := StrToInt(edWidth.Text);
    Bevel := TabfStatusPanelBevel(cbxBevel.ItemIndex);
    PanelStyle := TabfStatusPanelStyle(cbxPanelStyle.ItemIndex);
    if not Boolean(cbxParentFont.ItemIndex) then
      Font.Assign(FCurrFont);
    ParentFont := Boolean(cbxParentFont.ItemIndex);
    if not Boolean(cbxParentColor.ItemIndex) then
      BrushColor := abfStringToColor(cbxBrushColor.Text);
    BrushStyle := TBrushStyle(cbxBrushStyle.ItemIndex);
    ParentColor := Boolean(cbxParentColor.ItemIndex);
    PopupMenu := TPopupMenu(FDesigner.GetComponent(cbxPopupMenu.Text));
    with TextStyle do
    begin
      Text := edText.Text;
      ScrollEffect := TabfScrollEffect(cbxScrollEffect.ItemIndex);
      Alignment := TAlignment(cbxAlignment.ItemIndex);
      Layout := TabfTextLayout(cbxLayout.ItemIndex);
      GlyphIndex := StrToInt(edGlyphIndex.Text);
      Spacing := StrToInt(edSpacing.Text);
      ShowText := Boolean(cbxShowText.ItemIndex);
    end;
    with ProgressBarStyle do
    begin
      Caption := edProgressCaption.Text;
      ProgressType := TabfProgressType(cbxProgressType.ItemIndex);
      ShowCaption := Boolean(cbxShowCaption.ItemIndex);
      MinValue := StrToInt(edMinValue.Text);
      MaxValue := StrToInt(edMaxValue.Text);
      Progress := StrToInt(edProgress.Text);
    end;
    with LockKeyStyle do
    begin
      Caption := edLockKeyCaption.Text;
      ColorOff := abfStringToColor(cbxLockKeyColorOff.Text);
      ColorOn := abfStringToColor(cbxLockKeyColorOn.Text);
      LockKeyType := TabfLockKeyType(cbxLockKeyType.ItemIndex);
    end;
    Result := True;
  finally
    FEditingPanels.EndUpdate;
  end;
end;

{--------------------------------------}

procedure TabfStatusPanelsEditor.FormShow(Sender: TObject);

    procedure _GetEnumItems(EnumType: PTypeInfo; List: TStrings);
    var i: Integer;
    begin
      with GetTypeData(EnumType)^ do
      for i := MinValue to MaxValue do
      List.Add(GetEnumName(EnumType, i));
    end;

    procedure _GetComponentList(AComponentClass: TComponentClass; List: TStrings);
    var i: Integer;
    begin
      if Assigned(FabfStatusBar) and Assigned(FabfStatusBar.Owner) then
      with FabfStatusBar.Owner do
      for i := 0 to ComponentCount - 1 do
      if (Components[i] is AComponentClass) then
        List.Add((Components[i] as AComponentClass).Name);
    end;

begin
  _GetEnumItems(TypeInfo(TabfStatusPanelBevel), cbxBevel.Items);
  _GetEnumItems(TypeInfo(TabfStatusPanelStyle), cbxPanelStyle.Items);
  abfGetColorValues(GetColorName);
  _GetEnumItems(TypeInfo(TBrushStyle), cbxBrushStyle.Items);
  _GetEnumItems(TypeInfo(TAlignment), cbxAlignment.Items);
  _GetEnumItems(TypeInfo(TabfScrollEffect), cbxScrollEffect.Items);
  _GetEnumItems(TypeInfo(TabfTextLayout), cbxLayout.Items);
  _GetEnumItems(TypeInfo(TabfProgressType), cbxProgressType.Items);
  _GetEnumItems(TypeInfo(TabfLockKeyType), cbxLockKeyType.Items);
  _GetComponentList(TPopupMenu, cbxPopupMenu.Items);
  pcStyleProperties.ActivePage := tsTextStyle;
  FEditingPanels.Assign(FabfStatusBar.Panels);
  UpdateListbox(nil);
end;

{--------------------------------------}

procedure TabfStatusPanelsEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if (ModalResult = mrOk) and btnApply.Enabled then
    FabfStatusBar.Panels.Assign(FEditingPanels);
  Action := caFree;
end;

{--------------------------------------}

procedure TabfStatusPanelsEditor.PropertiesModified(Sender: TObject);
begin
  if not FLoadingProperties then btnApply.Enabled := True;
end;

{--------------------------------------}

procedure TabfStatusPanelsEditor.btnApplyClick(Sender: TObject);
begin
  if SetProperties(FCurrPanel) then
  begin
    btnApply.Enabled := False;
    UpdateListbox(FCurrPanel);
    with FabfStatusBar.Panels do
    begin
      BeginUpdate;
      try
        Assign(FEditingPanels);
      finally
        EndUpdate;
      end;
    end;
  end;
end;

{--------------------------------------}

procedure TabfStatusPanelsEditor.cbxBrushColorChange(Sender: TObject);
begin
  cbxParentColor.OnChange := nil;
  try
    cbxParentColor.ItemIndex := 0;
    PropertiesModified(Sender);
  finally
    cbxParentColor.OnChange := cbxParentColorChange;
  end;
end;

{--------------------------------------}

procedure TabfStatusPanelsEditor.cbxParentColorChange(Sender: TObject);
begin
  if cbxParentColor.ItemIndex = 1 then
  begin
    cbxBrushColor.OnChange := nil;
    try
      cbxBrushColor.ItemIndex := cbxBrushColor.Items.IndexOf(abfColorToString(FabfStatusBar.Color));
      PropertiesModified(Sender);
    finally
      cbxBrushColor.OnChange := cbxBrushColorChange;
    end;
  end;
end;

{--------------------------------------}

procedure TabfStatusPanelsEditor.btnFontClick(Sender: TObject);
var FontDialog: TFontDialog;
begin
  FontDialog := TFontDialog.Create(nil);
  try
    FontDialog.Font.Assign(FCurrFont);
    if FontDialog.Execute then
    begin
      FCurrFont.Assign(FontDialog.Font);
      cbxParentFont.OnChange := nil;
      try cbxParentFont.ItemIndex := 0;
      finally
        cbxParentFont.OnChange := cbxParentFontChange;
      end;
      PropertiesModified(Sender);
    end;
  finally
    FontDialog.Free;
  end;
end;

{--------------------------------------}

procedure TabfStatusPanelsEditor.cbxParentFontChange(Sender: TObject);
begin
  if cbxParentFont.ItemIndex = 1 then
  begin
    FCurrFont.Assign(FabfStatusBar.Font);
    PropertiesModified(Sender);
  end;
end;

{--------------------------------------}

procedure TabfStatusPanelsEditor.btnOkClick(Sender: TObject);
begin
  if btnApply.Enabled then SetProperties(FCurrPanel);
  FDesigner.Modified;
end;

{--------------------------------------}

procedure TabfStatusPanelsEditor.cbxPopupMenuKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
  begin
    cbxPopupMenu.ItemIndex := -1;
    PropertiesModified(Sender);
  end;
end;

{--------------------------------------}

procedure TabfStatusPanelsEditor.cbxLockKeyTypeChange(Sender: TObject);
begin
  edLockKeyCaption.OnChange := nil;
  try
    if Assigned(FCurrPanel) then
      edLockKeyCaption.Text := FCurrPanel.LockKeyStyle.Caption;
    PropertiesModified(Sender);
  finally
    edLockKeyCaption.OnChange := PropertiesModified;
  end;
end;

{--------------------------------------}

procedure TabfStatusPanelsEditor.cbxProgressTypeChange(Sender: TObject);
begin
  edProgressCaption.OnChange := nil;
  try
    if Assigned(FCurrPanel) then
      edProgressCaption.Text := FCurrPanel.ProgressBarStyle.Caption;
    PropertiesModified(Sender);
  finally
    edProgressCaption.OnChange := PropertiesModified;
  end;
end;

{--------------------------------------}

end{unit abfDesignerStatusBar}.

