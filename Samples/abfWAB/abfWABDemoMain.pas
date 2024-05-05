{*******************************************************************************

  abfWAB Demo. Main form unit.

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfWABDemoMain;

{$I abf.inc}

interface

{$IFDEF C3_ONLY}
uses
  Windows, SysUtils, Controls, ComCtrls, ToolWin, ExtCtrls, Classes, Forms,
  abfComponents, Menus, Dialogs, abfWAB, WabDefs, abfWABDialogs, ImgList;
{$ELSE}
uses
  Windows, SysUtils, Controls, ComCtrls, ToolWin, ExtCtrls, Classes, Forms,
  abfComponents, Menus, Dialogs, abfWAB, WabDefs, abfWABDialogs
  {$IFDEF D6}, Variants {$ENDIF D6}
  {$IFDEF D4} , ImgList {$ENDIF D4}
;
{$ENDIF C3_ONLY}

const
  cFlatButtons = False; // Determines are speed buttons flat or not
  cWidth  = 640;
  cHeight = 375;

type

//==============================================================================
// TfrmMain
//==============================================================================
{ TfrmMain }

  TfrmWabDemoMain = class(TForm)
    abfWAB: TabfWAB;
    tbToolbar: TToolBar;
      tbtnNew: TToolButton;
      tbtnProperties: TToolButton;
      tbtnDelete: TToolButton;
      tbtnFindPeople: TToolButton;
    pnBorder: TPanel;
    lvItems: TListView;
    sbMain: TStatusBar;
  { Non-visual }
    dlgOpenWABFile: TOpenDialog;
    ilstNormalImages: TImageList;
    ilstHotImages: TImageList;
    ilstIcons: TImageList;
    ilstDisableImages: TImageList;
    mmMain: TMainMenu;
      miFile: TMenuItem;
        miNewContact: TMenuItem;
        miNewGroup: TMenuItem;
          mi1: TMenuItem;
        miProperties: TMenuItem;
        miDelete: TMenuItem;
          mi2: TMenuItem;
        miWABFile: TMenuItem;
        miCurrWAB: TMenuItem;
          mi3: TMenuItem;
        miExit: TMenuItem;
      miEdit: TMenuItem;
        miSelectAll: TMenuItem;
          mi4: TMenuItem;
        miProfile: TMenuItem;
          mi5: TMenuItem;
        miFindPeople: TMenuItem;
      miView: TMenuItem;
        miAddToFolder: TMenuItem;
          miProfileFolder: TMenuItem;
          miSharedFolder: TMenuItem;
        miFilter: TMenuItem;
          miAllContents: TMenuItem;
          miProfileContacts: TMenuItem;
          miSharedContacts: TMenuItem;
        mi6: TMenuItem;
        miRefresh: TMenuItem;
      miOptions: TMenuItem;
        miUseProfiles: TMenuItem;
      miAbout: TMenuItem;
    pmMain: TPopupMenu;
      miM_New: TMenuItem;
        miM_NewContact: TMenuItem;
        miM_NewGroup: TMenuItem;
      miM1: TMenuItem;
      miM_SelectAll: TMenuItem;
      miM_Refresh: TMenuItem;
        miM2: TMenuItem;
      miM_Properties: TMenuItem;
      miM_Delete: TMenuItem;
        miM3: TMenuItem;
      miM_FindPeople: TMenuItem;
    pmNew: TPopupMenu;
      miN_NewContact: TMenuItem;
      miN_NewGroup: TMenuItem;
    rmUseOutlook: TabfRegistryMonitor;
    tmRegistryTimer: TTimer;
    tbtnSelectAddresses: TToolButton;
    abfWABAddrDlg: TabfWABAddrDlg;
  { Events }
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure FormShow(Sender: TObject);
    procedure abfWABOpenAddressBook(Sender: TObject);
    procedure abfWABCloseAddressBook(Sender: TObject);
    procedure abfWABChangeAddressBook(Sender: TObject);
    procedure abfWABDeletingItem(Sender: TObject;
      var AllowDelete: Boolean);
    procedure lvItemsDblClick(Sender: TObject);
    procedure lvItemsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure tbtnNewClick(Sender: TObject);
    procedure tbtnPropertiesClick(Sender: TObject);
    procedure tbtnDeleteClick(Sender: TObject);
    procedure tbtnFindPeopleClick(Sender: TObject);
    procedure tbtnSelectAddressesClick(Sender: TObject);
    procedure pmMainPopup(Sender: TObject);
    procedure miNewContactClick(Sender: TObject);
    procedure miNewGroupClick(Sender: TObject);
    procedure miSelectAllClick(Sender: TObject);
    procedure miRefreshClick(Sender: TObject);
    procedure miFileClick(Sender: TObject);
    procedure miCurrWABClick(Sender: TObject);
    procedure miWABFileClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miProfileClick(Sender: TObject);
    procedure miProfileFolderClick(Sender: TObject);
    procedure miSharedFolderClick(Sender: TObject);
    procedure miAllContentsClick(Sender: TObject);
    procedure miProfileContactsClick(Sender: TObject);
    procedure miSharedContactsClick(Sender: TObject);
    procedure miOptionsClick(Sender: TObject);
    procedure miUseProfilesClick(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
    procedure rmUseOutlookChange;
    procedure tmRegistryTimerTimer(Sender: TObject);
  private
    FRegChanged: Boolean;
    FUseOutlook: Boolean;
    function GetWABItemIndex(PSB: PSBinary; var ItemType: Integer): Integer;
    procedure RefreshListView(SelectItemIndex: Integer);
    procedure RefreshButtons;
    procedure SwitchWABMode;
  end;

var
  frmWabDemoMain: TfrmWabDemoMain;

//==============================================================================
// Entry point
//==============================================================================
procedure ShowDemo;

{******************************************************************************}
implementation
{******************************************************************************}
{$R *.DFM}
uses
  Registry, abfAboutApplication, abfWABUtils;

//==============================================================================
// Entry point
//==============================================================================

procedure ShowDemo;
begin
  if not Assigned(frmWabDemoMain) then
    Application.CreateForm(TfrmWabDemoMain, frmWabDemoMain);
  frmWabDemoMain.Show;
  frmWabDemoMain.BringToFront;
end;


//==============================================================================
// TfrmMain
//==============================================================================
{ TfrmMain }

function TfrmWabDemoMain.GetWABItemIndex(PSB: PSBinary; var ItemType: Integer): Integer;
begin
  ItemType := 0;
  Result := abfWAB.GetGroupIndex(PSB);
  if Result >= 0 then ItemType := 1 else
  begin
    Result := abfWAB.GetContactIndex(PSB);
    if Result >= 0 then ItemType := 2
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmWabDemoMain.RefreshListView(SelectItemIndex: Integer);
var
  i, a, MeIndex, ObjType: Integer;
begin
  lvItems.Items.BeginUpdate;
  try
    lvItems.Items.Clear;
    MeIndex := abfWAB.GetMeContactIndex(False);
    for i := 0 to abfWAB.Items.Count - 1 do
      with lvItems.Items.Add, abfWAB.Items[i] do
      begin
        Caption := DisplayName;
        Data := EntryID;
        a := GetWABItemIndex(Data, ObjType);
        if ObjType = 1 then
        begin
          ImageIndex := 1;
          SubItems.Add('');
          SubItems.Add(abfWAB.Groups[a].TelephoneNumber);
          SubItems.Add('');
        end
        else if ObjType = 2 then
        begin
          if MeIndex = a then ImageIndex := 3;
          SubItems.Add(abfWAB.Contacts[a].EmailAddress);
          SubItems.Add(abfWAB.Contacts[a].HomeTelephoneNumber);
          SubItems.Add(abfWAB.Contacts[a].BusinessTelephoneNumber);
        end;
        Selected := SelectItemIndex = i;
        Focused := Selected;
      end;
    sbMain.Panels[0].Text := IntToStr(abfWAB.Items.Count) + ' items';
  finally
    lvItems.Items.EndUpdate;
  end;
  RefreshButtons;
end;

//------------------------------------------------------------------------------

procedure TfrmWabDemoMain.RefreshButtons;
begin
  tbtnNew.Enabled := abfWAB.Active;
  tbtnFindPeople.Enabled := abfWAB.Active;
  tbtnSelectAddresses.Enabled := abfWAB.Active;

  if lvItems.SelCount > 0 then
  begin
    tbtnDelete.Enabled := True;
    miDelete  .Enabled := True;
    miM_Delete.Enabled := True;
    if lvItems.SelCount > 1 then
    begin
      tbtnProperties.Enabled := False;
      miProperties  .Enabled := False;
      miM_Properties.Enabled := False;
    end else
    begin
      tbtnProperties.Enabled := True;
      miProperties  .Enabled := True;
      miM_Properties.Enabled := True;
    end;
  end else
  begin
    tbtnProperties.Enabled := False;
    miProperties  .Enabled := False;
    miM_Properties.Enabled := False;
    tbtnDelete.Enabled := False;
    miDelete  .Enabled := False;
    miM_Delete.Enabled := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmWabDemoMain.SwitchWABMode;
begin
  miCurrWAB.Checked := abfWAB.WABMode = wmCurrentWAB;
  miWABFile.Checked := abfWAB.WABMode = wmWABFile;
  miUseProfiles.Checked := abfWAB.EnableProfiles;
  if abfWAB.Active then
  begin
    if miCurrWAB.Checked then sbMain.Panels[2].Text := 'Use current WAB'
    else
    if miWABFile.Checked then sbMain.Panels[2].Text := abfWAB.FileName
    else sbMain.Panels[2].Text := '';
  end else
    sbMain.Panels[2].Text := '';
end;

//==============================================================================
// Events

procedure TfrmWabDemoMain.FormCreate(Sender: TObject);
begin
  Width  := cWidth;
  Height := cHeight;
{$IfDef D4}
  OnCanResize := FormCanResize;
{$EndIf D4}
end;

//------------------------------------------------------------------------------

procedure TfrmWabDemoMain.FormResize(Sender: TObject);
begin
{$IfNDef D4}
  if Width  < cWidth  then Width  := cWidth;
  if Height < cHeight then Height := cHeight;
{$EndIf D4}
end;

//------------------------------------------------------------------------------

procedure TfrmWabDemoMain.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  if NewWidth  < cWidth  then NewWidth  := cWidth;
  if NewHeight < cHeight then NewHeight := cHeight;
end;

//------------------------------------------------------------------------------

procedure TfrmWabDemoMain.FormShow(Sender: TObject);
begin
  FRegChanged := False;
  FUseOutlook := abfIsOutlookSharedMode;

  abfWAB.Open;

  sbMain.Panels[1].Text := 'WAB version: ' + abfWAB.WABVersion;
end;

//------------------------------------------------------------------------------

procedure TfrmWabDemoMain.abfWABOpenAddressBook(Sender: TObject);
begin
  rmUseOutlook.Active := (abfWAB.WABMode = wmCurrentWAB);
  RefreshListView(0);
  SwitchWABMode;
end;

//------------------------------------------------------------------------------

procedure TfrmWabDemoMain.abfWABCloseAddressBook(Sender: TObject);
begin
  if csDestroying in ComponentState then Exit;
  rmUseOutlook.Active := False;
end;

//------------------------------------------------------------------------------

procedure TfrmWabDemoMain.abfWABChangeAddressBook(Sender: TObject);
var
  i: Integer;
begin
  if Assigned(lvItems.Selected) then
  begin
    i := lvItems.Selected.Index;
    if i >= abfWAB.Items.Count then i := abfWAB.Items.Count - 1;
    RefreshListView(i);
  end else
    RefreshListView(-1);
end;

//------------------------------------------------------------------------------

procedure TfrmWabDemoMain.abfWABDeletingItem(Sender: TObject;
  var AllowDelete: Boolean);
begin
  AllowDelete := MessageDlg('Delete "' +
    abfWAB.Items[lvItems.Selected.Index].DisplayName +
    '" item?', mtConfirmation, [mbYes, mbNo], 0) = mrYes;
end;

//------------------------------------------------------------------------------

procedure TfrmWabDemoMain.lvItemsDblClick(Sender: TObject);
begin
  tbtnProperties.Click;
end;

//------------------------------------------------------------------------------

procedure TfrmWabDemoMain.lvItemsChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if Change = ctState then RefreshButtons;
end;

//------------------------------------------------------------------------------
// Shows drop-down popup menu

procedure TfrmWabDemoMain.tbtnNewClick(Sender: TObject);
var
  APoint: TPoint;
begin
  SendCancelMode(nil);
  APoint := tbtnNew.ClientToScreen(Point(0, tbtnNew.ClientHeight));
{$IfDef D4}
  if pmNew.IsRightToLeft then Inc(APoint.X, tbtnNew.Width);
{$EndIf D4}
  pmNew.Popup(APoint.X, APoint.Y);
end;

//------------------------------------------------------------------------------

procedure TfrmWabDemoMain.tbtnPropertiesClick(Sender: TObject);
var
  i: Integer;
begin
  if Assigned(lvItems.Selected) then
  begin
    abfWAB.OnChangeAddressBook := nil;
    try
      i := lvItems.Selected.Index;
      if abfWAB.ItemDetails(i) then RefreshListView(i);
    finally
      abfWAB.OnChangeAddressBook := abfWABChangeAddressBook;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmWabDemoMain.tbtnDeleteClick(Sender: TObject);
var
  IndexesArr: Variant;
  Len, i, a: Integer;
  SavePos: Integer;
begin
  if lvItems.SelCount > 0 then
  begin
    SavePos := lvItems.Selected.Index;
    if lvItems.SelCount > 1 then
    begin
      if MessageDlg('Delete these items?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      begin
        abfWAB.OnChangeAddressBook := nil;
        abfWAB.OnDeletingItem := nil;
        try
          if lvItems.Items.Count <> lvItems.SelCount then
          begin
            Len := lvItems.SelCount;
            a := 0;
            IndexesArr := VarArrayCreate([0, Len - 1], varInteger);
            for i := 0 to lvItems.Items.Count - 1 do
            if lvItems.Items[i].Selected then
            begin
              IndexesArr[a] := lvItems.Items[i].Index;
              Inc(a);
            end;
            abfWAB.DeleteItems(IndexesArr);
          end
          else
            abfWAB.DeleteAllItems;
        finally
          abfWAB.OnDeletingItem := abfWABDeletingItem;
          abfWAB.OnChangeAddressBook := abfWABChangeAddressBook;
        end;
        if SavePos >= abfWAB.Items.Count then SavePos := abfWAB.Items.Count - 1;
        RefreshListView(SavePos);
      end
    end else
    begin
      abfWAB.OnChangeAddressBook := nil;
      try
        abfWAB.DeleteItems(SavePos);
      finally
        abfWAB.OnChangeAddressBook := abfWABChangeAddressBook;
      end;
      if SavePos >= abfWAB.Items.Count then
      SavePos := abfWAB.Items.Count - 1;
      RefreshListView(SavePos);
    end;
  end;
end;{procedure TfrmMain.tbtnDeleteClick}

//------------------------------------------------------------------------------

procedure TfrmWabDemoMain.tbtnFindPeopleClick(Sender: TObject);
begin
  abfWAB.Find;
end;

//------------------------------------------------------------------------------

procedure TfrmWabDemoMain.tbtnSelectAddressesClick(Sender: TObject);
begin
  abfWABAddrDlg.Execute;
end;

//------------------------------------------------------------------------------

procedure TfrmWabDemoMain.pmMainPopup(Sender: TObject);
begin
  RefreshButtons;
end;

//------------------------------------------------------------------------------

procedure TfrmWabDemoMain.miNewContactClick(Sender: TObject);
var
  i: Integer;
begin
  abfWAB.OnChangeAddressBook := nil;
  try
    i := abfWAB.NewContact;
    if i < 0 then Exit;
    i := abfWAB.GetItemIndex(abfWAB.Contacts[i].EntryID);
    if i >= 0 then RefreshListView(i);
  finally
    abfWAB.OnChangeAddressBook := abfWABChangeAddressBook;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmWabDemoMain.miNewGroupClick(Sender: TObject);
var
  i: Integer;
begin
  abfWAB.OnChangeAddressBook := nil;
  try
    i := abfWAB.NewGroup;
    if i < 0 then Exit;
    i := abfWAB.GetItemIndex(abfWAB.Groups[i].EntryID);
    if i >= 0 then RefreshListView(i);
  finally
    abfWAB.OnChangeAddressBook := abfWABChangeAddressBook;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmWabDemoMain.miSelectAllClick(Sender: TObject);
var
  i: Integer;
begin
  with lvItems.Items do
  begin
    BeginUpdate;
    try
      for i := 0 to lvItems.Items.Count - 1 do
        if not lvItems.Items[i].Selected then
          lvItems.Items[i].Selected := True;
    finally
      EndUpdate;
    end;
  end;
  RefreshButtons;
end;

//------------------------------------------------------------------------------

procedure TfrmWabDemoMain.miRefreshClick(Sender: TObject);
var
  SavePos: Integer;
begin
  if lvItems.Selected <> nil then
    SavePos := lvItems.Selected.Index
  else
    SavePos := -1;
  abfWAB.Refresh;
  RefreshListView(SavePos);
end;

//------------------------------------------------------------------------------

procedure TfrmWabDemoMain.miFileClick(Sender: TObject);
begin
  SwitchWABMode;
end;

//------------------------------------------------------------------------------

procedure TfrmWabDemoMain.miCurrWABClick(Sender: TObject);
begin
  abfWAB.WABMode := wmCurrentWAB;
  abfWAB.Open;
end;

//------------------------------------------------------------------------------

procedure TfrmWabDemoMain.miWABFileClick(Sender: TObject);
begin
  dlgOpenWABFile.FileName := abfWAB.FileName;
  if dlgOpenWABFile.Execute then
  begin
    abfWAB.WABMode := wmWABFile;
    abfWAB.FileName := dlgOpenWABFile.FileName;
    abfWAB.Open;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmWabDemoMain.miExitClick(Sender: TObject);
begin
  Close;
end;

//------------------------------------------------------------------------------

procedure TfrmWabDemoMain.miProfileClick(Sender: TObject);
begin
  abfWAB.SetMeContact(-1, True);
end;

//------------------------------------------------------------------------------

procedure TfrmWabDemoMain.miProfileFolderClick(Sender: TObject);
begin
  abfWAB.AddToFolder := wafProfileFolder;
  miProfileFolder.Checked := True;
end;

//------------------------------------------------------------------------------

procedure TfrmWabDemoMain.miSharedFolderClick(Sender: TObject);
begin
  abfWAB.AddToFolder := wafSharedFolder;
  miSharedFolder.Checked := True;
end;

//------------------------------------------------------------------------------

procedure TfrmWabDemoMain.miAllContentsClick(Sender: TObject);
var
  SaveState: Boolean;
begin
  SaveState := abfWAB.Active;
  abfWAB.ContentsFilter := wcfAllContents;
  miAllContents.Checked := True;
  abfWAB.Active := SaveState;
end;

//------------------------------------------------------------------------------

procedure TfrmWabDemoMain.miProfileContactsClick(Sender: TObject);
var
  SaveState: Boolean;
begin
  SaveState := abfWAB.Active;
  abfWAB.ContentsFilter := wcfProfileFolderOnly;
  miProfileContacts.Checked := True;
  abfWAB.Active := SaveState;
end;

//------------------------------------------------------------------------------

procedure TfrmWabDemoMain.miSharedContactsClick(Sender: TObject);
var
  SaveState: Boolean;
begin
  SaveState := abfWAB.Active;
  abfWAB.ContentsFilter := wcfSharedFolderOnly;
  miSharedContacts.Checked := True;
  abfWAB.Active := SaveState;
end;

//------------------------------------------------------------------------------

procedure TfrmWabDemoMain.miOptionsClick(Sender: TObject);
begin
  SwitchWABMode;
end;

//------------------------------------------------------------------------------

procedure TfrmWabDemoMain.miUseProfilesClick(Sender: TObject);
begin
  abfWAB.EnableProfiles := not miUseProfiles.Checked;
  abfWAB.Open;
  miUseProfiles.Checked := abfWAB.EnableProfiles;
end;

//------------------------------------------------------------------------------

procedure TfrmWabDemoMain.miAboutClick(Sender: TObject);
begin
  abfApplicationAbout(Application.Title);
end;

//------------------------------------------------------------------------------

procedure TfrmWabDemoMain.rmUseOutlookChange;
begin
  tmRegistryTimer.Enabled := True;
end;

//------------------------------------------------------------------------------

procedure TfrmWabDemoMain.tmRegistryTimerTimer(Sender: TObject);
var
  TempBool: Boolean;
begin
  if IsWindowEnabled(Self.Handle) then
  begin
    tmRegistryTimer.Enabled := False;

    TempBool := abfIsOutlookSharedMode;
    if FUseOutlook <> TempBool then
    begin
      FUseOutlook := TempBool;
      abfWAB.Open;
    end;
  end;
end;

//------------------------------------------------------------------------------

end{unit abfWABForm}.


 