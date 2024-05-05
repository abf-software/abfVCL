{*******************************************************************************

  ABF Visual Components Library, Design-time part

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfDesignerWAB;

{$I abf.inc}

interface

uses
{$IFDEF D4}
  ImgList,
{$ENDIF D4}
{$IfDef D6}
  DesignIntf, DesignEditors, DesignWindows,
{$Else D6}
  DsgnIntf, DsgnWnds,
{$EndIf D6}
  Windows, SysUtils, Controls, ComCtrls, ToolWin, ExtCtrls, Classes, Forms,
  Dialogs, Messages, Menus, abfWAB, WABDefs;

const
  AM_DeferUpdate = WM_USER + 100;  // avoids break-before-make listview ugliness

type
{$IFNDEF D4}
  IPersistent = TComponent;
  IDesigner = TDesigner;
  IFormDesigner = TFormDesigner;
{$ENDIF D4}
{$IFNDEF D5}
  TDesignerSelectionList = TComponentList;
{$ENDIF D5}

//==============================================================================
// TabfWABItemsEditor
//==============================================================================

  TabfWABItemsEditor = class(TDesignWindow)
    sbMain: TStatusBar;
    plBorder: TPanel;
    lvItems: TListView;
    tbrToolbar: TToolBar;
    tbtnNew: TToolButton;
    tbtnProp: TToolButton;
    ilstNormalImages: TImageList;
    ilstHotImages: TImageList;
    tbtnDelete: TToolButton;
    imlstIcons: TImageList;
    ilstDisableImages: TImageList;
    pmMain: TPopupMenu;
    miNew: TMenuItem;
    miProp: TMenuItem;
    miDelete: TMenuItem;
    N1: TMenuItem;
    miSelectAll: TMenuItem;
    miRefresh: TMenuItem;
    pmNew: TPopupMenu;
    miNewContact: TMenuItem;
    miNewGroup: TMenuItem;
    miNewContact2: TMenuItem;
    miNewGroup2: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tbtnPropClick(Sender: TObject);
    procedure tbtnDeleteClick(Sender: TObject);
    procedure miNewContactClick(Sender: TObject);
    procedure miNewGroupClick(Sender: TObject);
    procedure tbtnNewClick(Sender: TObject);
    procedure miSelectAllClick(Sender: TObject);
    procedure miRefreshClick(Sender: TObject);
    procedure pmMainPopup(Sender: TObject);
    procedure lvItemsDblClick(Sender: TObject);
    procedure lvItemsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure Activated; override;
    procedure LockState;
    procedure UnlockState;
    function UniqueName(Component: TComponent): string; override;
  private
    FCollection: TCollection;
    FCollectionPropertyName: string;
    FEventFired: Boolean;
    FExternalCount: Integer;
    FItemIDList: TList;
    FSelectionError: Boolean;
    FStateLock: Integer;
    FWAB: TabfWAB;
    procedure AMDeferUpdate(var Msg: TMessage); message AM_DeferUpdate;
    procedure DoExternalEvent;
    procedure ExternalEvent(Sender: TObject);
    procedure LockExternalEvent;
    procedure UnlockExternalEvent;
    procedure RefreshListView(SelectIndex: Integer);
    procedure ClearSelection;
//    procedure GetSelection;
    procedure SetSelection;
    function GetWABItemIndex(PSB: PSBinary;
      var ItemType: Integer; var ItemIdx: Integer): Integer;
    procedure RefreshButtonsState;
    procedure SetCollectionPropertyName(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
{$IFDEF D6}
    procedure ItemDeleted(const ADesigner: IDesigner;
      AItem: TPersistent); override;
    procedure DesignerClosed(const ADesigner: IDesigner;
      AGoingDormant: Boolean); override;
    procedure ItemsModified(const ADesigner: IDesigner); override;
    procedure SelectionChanged(const ADesigner: IDesigner;
      const ASelection: IDesignerSelections); override;
{$ELSE D6}
    procedure ComponentDeleted(Component: IPersistent); override;
    procedure FormClosed(AForm: TCustomForm); override;
    procedure FormModified; override;
    procedure SelectionChanged(ASelection: TDesignerSelectionList); override;
{$ENDIF D6}
    property CollectionPropertyName: string read FCollectionPropertyName
      write SetCollectionPropertyName;
  end;

//==============================================================================
// TabfWABContactsPropertyEditor
//==============================================================================

  TabfWABItemsEditorClass = class of TabfWABItemsEditor;

  TabfWABItemsPropertyEditor = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

{******************************************************************************}
{ ContactsEditorsList }

var
  ItemsEditorsList: TList = nil;

{******************************************************************************}
{ Designing subroutines }

procedure ShowAbfWABItemsEditor(ADesigner: IDesigner; AWAB: TabfWAB;
  ACollection: TCollection; const PropertyName: string);
function ShowAbfWABItemsEditorClass(ADesigner: IDesigner;
  AWAB: TabfWAB; ACollection: TCollection;
  const PropertyName: string): TabfWABItemsEditor;
{******************************************************************************}
implementation
{******************************************************************************}
{$R *.DFM}

uses
{$IfDef D6}
  Variants, ComponentDesigner,
{$Else D6}
  LibIntf,
{$EndIf D6}
  Graphics, abfSysUtils, abfVclUtils;


//==============================================================================
// TabfWABContactsEditor
//==============================================================================
// Author: LinX
// Date: 04/20/2001

type
  THackPersistent = class(TPersistent);
  THackWAB = class(TabfWAB);
  THackWABItem = class(TabfWABItem);

//------------------------------------------------------------------------------

constructor TabfWABItemsEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEventFired := False;
  FItemIdList := TList.Create;
  FStateLock := 0;
  FExternalCount := 0;
  FSelectionError := False;
end;

//------------------------------------------------------------------------------

destructor TabfWABItemsEditor.Destroy;
begin
  LockState;
  if FWAB <> nil then
  try
    if FCollection is TabfWABContactsCollection then
      THackWAB(FWAB).OnInternalContactsChange := nil
    else if FCollection is TabfWABGroupsCollection then
      THackWAB(FWAB).OnInternalGroupsChange := nil
    else if FCollection is TabfWABItemsCollection then
      THackWAB(FWAB).OnInternalItemsChange := nil;
    FWAB := nil;
  except
    FWAB := nil;
  end;
  if FItemIdList <> nil then FItemIdList.Free;
  FItemIdList := nil;
  ItemsEditorsList.Remove(Self);
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TabfWABItemsEditor.FormShow(Sender: TObject);
begin
  if FWAB <> nil then
  begin
    if FCollection is TabfWABContactsCollection then
      THackWAB(FWAB).OnInternalContactsChange := ExternalEvent
    else if FCollection is TabfWABGroupsCollection then
      THackWAB(FWAB).OnInternalGroupsChange := ExternalEvent
    else if FCollection is TabfWABItemsCollection then
      THackWAB(FWAB).OnInternalItemsChange := ExternalEvent;
  end;
  RefreshListView(0);
end;

//------------------------------------------------------------------------------

procedure TabfWABItemsEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
  LockState;
end;

//------------------------------------------------------------------------------

procedure TabfWABItemsEditor.tbtnPropClick(Sender: TObject);
var
  SaveHandle, i: Integer;
begin
  if lvItems.Selected <> nil then
  begin
    LockExternalEvent;
    try
      i := lvItems.Selected.Index;
      SaveHandle := FWAB.ParentHandle;
      try
        FWAB.ParentHandle := Self.Handle;
        if FCollection = FWAB.Items then
        begin
          if FWAB.ItemDetails(i) then RefreshListView(i);
        end
        else if FCollection = FWAB.Groups then
        begin
          if FWAB.GroupDetails(i) then RefreshListView(i);
        end
        else if FCollection = FWAB.Contacts then
        begin
          if FWAB.ContactDetails(i) then RefreshListView(i);
        end;
      finally
        FWAB.ParentHandle := SaveHandle;
      end;
    finally
      UnlockExternalEvent;
      PostMessage(Handle, AM_DeferUpdate, 0, 0);
      DoExternalEvent;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABItemsEditor.tbtnDeleteClick(Sender: TObject);
var
  IndexesArr: Variant;
  Len, i, a, SavePos: Integer;
  St1, St2: string;
begin
  if FCollection = FWAB.Items then
  begin
    St1 := 'Delete these items?';
    St2 := 'Delete "%s" item?'
  end
  else if FCollection = FWAB.Groups then
  begin
    St1 := 'Delete these groups?';
    St2 := 'Delete "%s" group?'
  end
  else if FCollection = FWAB.Contacts then
  begin
    St1 := 'Delete these contacts?';
    St2 := 'Delete "%s" contact?'
  end;
  ClearSelection;
  if lvItems.SelCount > 0 then
  begin
    SavePos := lvItems.Selected.Index;
    if lvItems.SelCount > 1 then
    begin
      if MessageDlg(St1, mtConfirmation,
        [mbYes, mbNo], 0) = mrYes then
      begin
        LockExternalEvent;
        try
          if lvItems.Items.Count <> lvItems.SelCount then
          begin
            Len := lvItems.SelCount;
            a := 0;
            IndexesArr := VarArrayCreate([0, Len - 1], varInteger);
            for i := 0 to lvItems.Items.Count - 1 do
            if lvItems.Items[i].Selected then
            begin
              IndexesArr[a] := FWAB.GetItemIndex(lvItems.Items[i].Data);
              Inc(a);
            end;
            FWAB.DeleteItems(IndexesArr);
          end
          else
          begin
            if FCollection = FWAB.Items then
              FWAB.DeleteAllItems
            else if FCollection = FWAB.Groups then
              FWAB.DeleteAllGroups
            else if FCollection = FWAB.Contacts then
              FWAB.DeleteAllContacts;
          end;
          if SavePos >= FCollection.Count then
            SavePos := FCollection.Count - 1;
          RefreshListView(SavePos);
        finally
          UnlockExternalEvent;
          PostMessage(Handle, AM_DeferUpdate, 0, 0);
          DoExternalEvent;
        end;
      end;
    end
    else
    begin
      i := lvItems.Selected.Index;
      if MessageDlg(Format(St2, [FCollection.Items[i].DisplayName]),
        mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      begin
        LockExternalEvent;
        try
          if FCollection = FWAB.Items then
            FWAB.DeleteItems(i)
          else if FCollection = FWAB.Groups then
            FWAB.DeleteGroups(i)
          else if FCollection = FWAB.Contacts then
            FWAB.DeleteContacts(i);
          if SavePos >= FCollection.Count then
            SavePos := FCollection.Count - 1;
          RefreshListView(SavePos);
        finally
          UnlockExternalEvent;
          PostMessage(Handle, AM_DeferUpdate, 0, 0);
          DoExternalEvent;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABItemsEditor.miNewContactClick(Sender: TObject);
var
  SaveHandle, i: Integer;
begin
  i := -1;
  LockExternalEvent;
  try
    SaveHandle := FWAB.ParentHandle;
    try
      FWAB.ParentHandle := Self.Handle;
      i := FWAB.NewContact;
    finally
      FWAB.ParentHandle := SaveHandle;
    end;

    if i < 0 then Exit;
    if (FCollection <> FWAB.Contacts) and (FCollection <> FWAB.Items) then Exit;

    if FCollection = FWAB.Items then
      i := FWAB.GetItemIndex(FWAB.Contacts[i].EntryID);

    if i >= 0 then RefreshListView(i);
  finally
    UnlockExternalEvent;
    if i >= 0 then
    begin
      PostMessage(Handle, AM_DeferUpdate, 0, 0);
      DoExternalEvent;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABItemsEditor.miNewGroupClick(Sender: TObject);
var
  SaveHandle, i: Integer;
begin
  i := -1;
  LockExternalEvent;
  try
    SaveHandle := FWAB.ParentHandle;
    try
      FWAB.ParentHandle := Self.Handle;
      i := FWAB.NewGroup;
    finally
      FWAB.ParentHandle := SaveHandle;
    end;

    if i < 0 then Exit;
    if (FCollection <> FWAB.Groups) and (FCollection <> FWAB.Items) then Exit;

    if FCollection = FWAB.Items then
      i := FWAB.GetItemIndex(FWAB.Groups[i].EntryID);

    if i >= 0 then RefreshListView(i);
  finally
    UnlockExternalEvent;
    if i >= 0 then
    begin
      PostMessage(Handle, AM_DeferUpdate, 0, 0);
      DoExternalEvent;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABItemsEditor.tbtnNewClick(Sender: TObject);
var
  APoint: TPoint;
begin
  if FCollection = FWAB.Contacts then
    miNewContact.Click
  else if FCollection = FWAB.Groups then
    miNewGroup.Click
  else if FCollection = FWAB.Items then
  begin
    SendCancelMode(nil);
    APoint := tbtnNew.ClientToScreen(Point(0, tbtnNew.ClientHeight));
{$IFDEF D4}
    if pmNew.IsRightToLeft then Inc(APoint.X, tbtnNew.Width);
{$ENDIF D4}
    pmNew.Popup(APoint.X, APoint.Y);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABItemsEditor.miSelectAllClick(Sender: TObject);
var
  i: Integer;
begin
  with lvItems.Items do
  begin
    LockState;
    BeginUpdate;
    try
      for i := 0 to lvItems.Items.Count - 1 do
      if not lvItems.Items[i].Selected then
        lvItems.Items[i].Selected := True;
    finally
      EndUpdate;
      UnlockState;
    end;
    SetSelection;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABItemsEditor.miRefreshClick(Sender: TObject);
var
  SavePos: Integer;
begin
  if lvItems.Selected <> nil then
    SavePos := lvItems.Selected.Index
  else
    SavePos := -1;
  if SavePos >= FCollection.Count then
  SavePos := FCollection.Count - 1;
  FWAB.Refresh;
  RefreshListView(SavePos);
  PostMessage(Handle, AM_DeferUpdate, 0, 0);
end;

//------------------------------------------------------------------------------

procedure TabfWABItemsEditor.pmMainPopup(Sender: TObject);
begin
  RefreshButtonsState;
end;

//------------------------------------------------------------------------------

procedure TabfWABItemsEditor.lvItemsDblClick(Sender: TObject);
begin
  tbtnProp.Click;
end;

//------------------------------------------------------------------------------

procedure TabfWABItemsEditor.lvItemsChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
var
  Msg: TMsg;
begin
  if (Change = ctState) and (FStateLock = 0) then
    if not PeekMessage(Msg, Handle, AM_DeferUpdate, AM_DeferUpdate,
    PM_NOREMOVE) then PostMessage(Handle, AM_DeferUpdate, 0, 0);
end;

//------------------------------------------------------------------------------

procedure TabfWABItemsEditor.AMDeferUpdate(var Msg: TMessage);
var
  lpMsg: TMsg;
begin
  if FStateLock = 0 then
  begin
    SetSelection;
    PeekMessage(lpMsg, Handle, AM_DeferUpdate, AM_DeferUpdate, PM_REMOVE);
  end
  else
    if not PeekMessage(lpMsg, Handle, AM_DeferUpdate, AM_DeferUpdate,
    PM_NOREMOVE) then PostMessage(Handle, AM_DeferUpdate, 0, 0);
end;

//------------------------------------------------------------------------------

procedure TabfWABItemsEditor.DoExternalEvent;
var
  Msg: TMsg;
  i: Integer;
begin
  if not Assigned(FWAB) or (not Assigned(FCollection)) then Exit;
  if FExternalCount = 0 then
  begin
    if FEventFired then FEventFired := False else Exit;
    if lvItems.Selected <> nil then
      i := lvItems.Selected.Index
    else
      i := -1;
    if i >= FCollection.Count then i := FCollection.Count - 1;
    RefreshListView(i);
    if not PeekMessage(Msg, Handle, AM_DeferUpdate, AM_DeferUpdate,
    PM_NOREMOVE) then PostMessage(Handle, AM_DeferUpdate, 0, 0);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABItemsEditor.ExternalEvent(Sender: TObject);
begin
  FEventFired := True;
  DoExternalEvent;
end;

//------------------------------------------------------------------------------

procedure TabfWABItemsEditor.LockExternalEvent;
begin
  Inc(FExternalCount);
end;

//------------------------------------------------------------------------------

procedure TabfWABItemsEditor.UnlockExternalEvent;
begin
  if FExternalCount > 0 then Dec(FExternalCount);
end;

//------------------------------------------------------------------------------

procedure TabfWABItemsEditor.RefreshListView(SelectIndex: Integer);
var
  i, a, MeIndex, ObjType: Integer;
begin
  if FStateLock > 0 then Exit;
  with lvItems.Items do
  begin
    LockState;
    BeginUpdate;
    try
      Clear;
      if (not Assigned(FWAB)) or (not FWAB.Active)
        or (not Assigned(FCollection)) then Exit;
      MeIndex := FWAB.GetMeContactIndex(False);
      for i := 0 to FCollection.Count - 1 do
      begin
        with Add do
        begin
          Caption := FCollection.Items[i].DisplayName;
          if FCollection is TabfWABContactsCollection then
            Data := TabfWABContactItem(FCollection.Items[i]).EntryID
          else if FCollection is TabfWABGroupsCollection then
            Data := TabfWABGroupItem(FCollection.Items[i]).EntryID
          else if FCollection is TabfWABItemsCollection then
            Data := TabfWABItem(FCollection.Items[i]).EntryID;
          GetWABItemIndex(PSBinary(Data), ObjType, a);
          if ObjType = 1 then
          begin
            ImageIndex := 1;
            //Font.Style := Font.Style + [fsBold];
            SubItems.Add('');
            SubItems.Add(FWAB.Groups[a].TelephoneNumber);
            SubItems.Add('');
          end
          else if ObjType = 2 then
          begin
            if a = MeIndex then
              ImageIndex := 3;
            SubItems.Add(FWAB.Contacts[a].EmailAddress);
            SubItems.Add(FWAB.Contacts[a].HomeTelephoneNumber);
            SubItems.Add(FWAB.Contacts[a].BusinessTelephoneNumber);
          end;
          Selected := SelectIndex = i;
          Focused := Selected;
        end;
      end;
    finally
      if Assigned(FCollection) and (FCollection.Count > 0) then
        sbMain.SimpleText := IntToStr(FCollection.Count) + ' items'
      else
        sbMain.SimpleText := '';
      UnlockState;
      EndUpdate;
      RefreshButtonsState;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABItemsEditor.ClearSelection;
var
{$IFDEF D6}
  List: IDesignerSelections;
{$ELSE}
  List: TDesignerSelectionList;
{$ENDIF D6}
begin
{$IFDEF D6}
  List := CreateSelectionList;
{$ELSE}
  List := TDesignerSelectionList.Create;
{$ENDIF D6}
  try
    FItemIDList.Clear;
    if not (csDestroying in FWAB.ComponentState) then
    begin
      List.Add(FWAB);
      FItemIDList.Add(FWAB);
    end;
    Designer.SetSelections(List);
  finally
  {$IFNDEF D6}
    List.Free;
  {$ENDIF D6}
  end;
end;

//------------------------------------------------------------------------------
(*
procedure TabfWABItemsEditor.GetSelection;
var
  I: Integer;
  Item: TCollectionItem;
{$IFDEF D6}
  List: IDesignerSelections;
{$ELSE}
  List: TDesignerSelectionList;
{$ENDIF D6}
begin
  if (FWAB = nil) or (FCollection = nil) then Exit;
{$IFDEF D6}
  List := CreateSelectionList;
{$ELSE}
  List := TDesignerSelectionList.Create;
{$ENDIF D6}
  try
    Designer.GetSelections(List);
    if (List.Count = 0) or (List.Count > FCollection.Count) then Exit;
    if not ((List[0] = FWAB) or (List[0] = FCollection)
      or (TabfWABItemsEditor(List[0]).GetOwner = FCollection)) then Exit;
  finally
  {$IFNDEF D6}
    List.Free;
  {$ENDIF D6}
  end;
  LockState;
  lvItems.Items.BeginUpdate;
  try
    for I := FItemIDList.Count - 1 downto 0 do
    begin
      Item := FCollection.FindItemID(Integer(FItemIDList[i]));
      if Item <> nil then
        lvItems.Items[Item.Index].Selected := True
      else FItemIDList.Delete(I);
    end;
    RefreshButtonsState;
  finally
    lvItems.Items.EndUpdate;
    UnlockState;
  end;
end;
(**)
//------------------------------------------------------------------------------

procedure TabfWABItemsEditor.SetSelection;
var
  i, a, ObjType: Integer;
{$IFDEF D6}
  List: IDesignerSelections;
{$ELSE}
  List: TDesignerSelectionList;
{$ENDIF D6}
begin
  if (FStateLock > 0) or (FExternalCount > 0) then  Exit;
  if FSelectionError then Exit;
  try
  {$IFDEF D6}
    List := CreateSelectionList;
  {$ELSE}
    List := TDesignerSelectionList.Create;
  {$ENDIF D6}
    try
      FItemIDList.Clear;
      if Assigned(FCollection) then
      begin
        if (FCollection.Count > 0) and (lvItems.SelCount = 1) then
        begin
          begin
            i := GetWABItemIndex(lvItems.Selected.Data, ObjType, a);
            if ObjType = 1 then
              List.Add(FWAB.Groups[a])
            else if ObjType = 2 then
              List.Add(FWAB.Contacts[a]);
            FItemIDList.Add(Pointer(FCollection.Items[i].ID));
          end
        end
        else
        begin
          List.Add(FWAB);
          FItemIDList.Add(FWAB);
        end;
      end;  
      Designer.SetSelections(List);
    finally
    {$IFNDEF D6}
      List.Free;
    {$ENDIF D6}
    end;
    RefreshButtonsState;
  except
    FSelectionError := True;
    Application.HandleException(ExceptObject);
    Close;
  end;
end;

//------------------------------------------------------------------------------

function TabfWABItemsEditor.GetWABItemIndex(PSB: PSBinary;
  var ItemType: Integer; var ItemIdx: Integer): Integer;
begin
  ItemType := 0;
  Result := -1;
  if FCollection = FWAB.Groups then
  begin
    Result := FWAB.GetGroupIndex(PSB);
    if Result >= 0 then ItemType := 1;
    ItemIdx := Result;
  end
  else if FCollection = FWAB.Contacts then
  begin
    Result := FWAB.GetContactIndex(PSB);
    if Result >= 0 then ItemType := 2;
    ItemIdx := Result;
  end
  else if FCollection = FWAB.Items then
  begin
    Result := FWAB.GetItemIndex(PSB);
    if Result >= 0 then
    begin
      if THackWABItem(FWAB.Items[Result]).ItemType = 8 then
      begin
        ItemType := 1;
        ItemIdx := FWAB.GetGroupIndex(THackWABItem(FWAB.Items[Result]).EntryID);
      end
      else
      begin
        ItemType := 2;
        ItemIdx := FWAB.GetContactIndex(THackWABItem(FWAB.Items[Result]).EntryID);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABItemsEditor.RefreshButtonsState;
begin
  if FWAB = nil then Exit;
  tbtnNew.Enabled := FWAB.Active and (not FWAB.ReadOnly);
  miNew.Enabled := tbtnNew.Enabled;
  miRefresh.Enabled := tbtnNew.Enabled;
  miSelectAll.Enabled := (lvItems.SelCount < lvItems.Items.Count)
    and (lvItems.Items.Count > 0);
  if (lvItems.SelCount > 0) and (not FWAB.ReadOnly) then
  begin
    tbtnProp.Enabled := not (lvItems.SelCount > 1);
    miProp.Enabled := tbtnProp.Enabled;
    tbtnDelete.Enabled := True;
    miDelete.Enabled := True;
  end else
  begin
    tbtnProp.Enabled := False;
    miProp.Enabled := False;
    tbtnDelete.Enabled := False;
    miDelete.Enabled := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABItemsEditor.SetCollectionPropertyName(const Value: string);
begin
  if Value <> FCollectionPropertyName then
  begin
    FCollectionPropertyName := Value;
    Caption := Format('Browsing %s.%s', [FWAB.Name, Value]);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABItemsEditor.Activated;
{$IFNDEF D6}
var
  Msg: TMessage;
{$ENDIF D6}
begin
{$IFNDEF D6}
  Msg.Msg := WM_ACTIVATE;
  Msg.WParam := 1;
  Designer.IsDesignMsg(Designer.Form, Msg);
{$ELSE}
  Designer.Activate;
{$ENDIF D6}
  SetSelection;
end;

//------------------------------------------------------------------------------

procedure TabfWABItemsEditor.LockState;
begin
  Inc(FStateLock);
end;

//------------------------------------------------------------------------------

procedure TabfWABItemsEditor.UnlockState;
begin
  Dec(FStateLock);
end;

//------------------------------------------------------------------------------

function TabfWABItemsEditor.UniqueName(Component: TComponent): string;
begin
  // Do nothing
end;

//------------------------------------------------------------------------------

{$IFDEF D6}

procedure TabfWABItemsEditor.ItemDeleted(const ADesigner: IDesigner;
  AItem: TPersistent);
var
  Msg: TMsg;

  function _IsOwnedBy(Owner, Child: TPersistent): Boolean;
  begin
    Result := False;
    if Owner = nil then Exit;
    while (Child <> nil) and (Child <> Owner) and not (Child is TComponent) do
      Child := THackPersistent(Child).GetOwner;
    Result := Child = Owner;
  end;{Internal function _IsOwnedBy}

begin
  if FStateLock > 0 then Exit;
  if AItem = nil then Exit;
  if (csDestroying in FWAB.ComponentState) or (AItem = FWAB)
    or (FCollection = nil) then Close
  else
    if _IsOwnedBy(FCollection, AItem) then
    begin
      if not PeekMessage(Msg, Handle, AM_DeferUpdate, AM_DeferUpdate,
        PM_NOREMOVE) then PostMessage(Handle, AM_DeferUpdate, 0, 0);
    end;
end;

//------------------------------------------------------------------------------

procedure TabfWABItemsEditor.DesignerClosed(const ADesigner: IDesigner;
  AGoingDormant: Boolean);
begin
  if Designer = ADesigner then Close;
end;

//------------------------------------------------------------------------------

procedure TabfWABItemsEditor.ItemsModified(const ADesigner: IDesigner);
begin
  // Do nothing
end;

//------------------------------------------------------------------------------

procedure TabfWABItemsEditor.SelectionChanged(const ADesigner: IDesigner;
  const ASelection: IDesignerSelections);
begin
  // Do nothing
end;

//------------------------------------------------------------------------------
{$ELSE D6}

procedure TabfWABItemsEditor.ComponentDeleted(Component: IPersistent);
var
  Temp: TPersistent;
  Msg: TMsg;

  function _IsOwnedBy(Owner, Child: TPersistent): Boolean;
  begin
    Result := False;
    if Owner = nil then Exit;
    while (Child <> nil) and (Child <> Owner) and not (Child is TComponent) do
      Child := THackPersistent(Child).GetOwner;
    Result := Child = Owner;
  end;{Internal function _IsOwnedBy}

begin
  if FStateLock > 0 then Exit;
{$IFNDEF D4}
  Temp := Component;
{$ELSE D4}
  Temp := TryExtractPersistent(Component);
{$ENDIF D4}
  if Temp = nil then Exit;
  if (csDestroying in FWAB.ComponentState) or (Temp = FWAB)
    or (FCollection = nil) then Close
  else
    if _IsOwnedBy(FCollection, Temp) then
    begin
      if not PeekMessage(Msg, Handle, AM_DeferUpdate, AM_DeferUpdate,
        PM_NOREMOVE) then PostMessage(Handle, AM_DeferUpdate, 0, 0);
    end;
end;

//------------------------------------------------------------------------------

procedure TabfWABItemsEditor.FormClosed(AForm: TCustomForm);
begin
  if Designer.Form = AForm then Close;
end;

//------------------------------------------------------------------------------

procedure TabfWABItemsEditor.FormModified;
begin
  // Do nothing
end;

//------------------------------------------------------------------------------

procedure TabfWABItemsEditor.SelectionChanged(ASelection: TDesignerSelectionList);
begin
  // Do nothing
end;

{$ENDIF D6}

{******************************************************************************}
{ Designing subroutines }

function ShowAbfWABItemsEditorClass(ADesigner: IDesigner;
  AWAB: TabfWAB; ACollection: TCollection;
  const PropertyName: string): TabfWABItemsEditor;
var
  i: Integer;
begin
  if ItemsEditorsList = nil then ItemsEditorsList := TList.Create;
  for i := 0 to ItemsEditorsList.Count - 1 do
  if ItemsEditorsList[i] <> nil then
  begin
    Result := TabfWABItemsEditor(ItemsEditorsList[I]);
    with Result do
      if (Designer = ADesigner) and (FWAB = AWAB)
        and (FCollection = ACollection)
        and (CompareText(CollectionPropertyName, PropertyName) = 0) then
      begin
        Show;
        BringToFront;
        Exit;
      end;
  end;
  Result := TabfWABItemsEditorClass.Create(nil);
  if Result = nil then Exit;
  ItemsEditorsList.Add(Result);
  with Result do
  try
  {$IFDEF D6}
    Designer := ADesigner;
  {$ELSE D6}
    Designer := ADesigner as IFormDesigner;
  {$ENDIF D6}
    FWAB := AWAB;
    FCollection := ACollection;
    CollectionPropertyName := PropertyName;
    miNewContact2.Enabled := (FCollection = FWAB.Items) or (FCollection = FWAB.Contacts);
    miNewGroup2.Enabled := (FCollection = FWAB.Items) or (FCollection = FWAB.Groups);
    Show;
  except
    Free;
  end;
end;

//------------------------------------------------------------------------------

procedure ShowAbfWABItemsEditor(ADesigner: IDesigner; AWAB: TabfWAB;
  ACollection: TCollection; const PropertyName: string);
begin
  ShowAbfWABItemsEditorClass(ADesigner, AWAB, ACollection, PropertyName);
end;


//==============================================================================
// TabfWABContactsPropertyEditor
//==============================================================================
// Author: LinX
// Date: 07/01/2000

procedure TabfWABItemsPropertyEditor.Edit;
var
  Obj: TabfWAB;
begin
  Obj := GetComponent(0) as TabfWAB;
  ShowAbfWABItemsEditorClass(Designer, Obj, TCollection(GetOrdValue), GetName);
end;

//------------------------------------------------------------------------------

function TabfWABItemsPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

{******************************************************************************}
initialization
{******************************************************************************}

{******************************************************************************}
finalization
{******************************************************************************}

  if ItemsEditorsList <> nil then
  begin
    while ItemsEditorsList.Count > 0 do
      TabfWABItemsEditor(ItemsEditorsList.Items[0]).Free;
    ItemsEditorsList.Free;
  end;
  ItemsEditorsList := nil;

end{unit abfDesignerWAB}.


