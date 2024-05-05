{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfWABDialogs;

{$I abf.inc}

interface

uses
  Windows, Classes, Controls, Forms,
  WabDefs,
  abfComponents,
  abfWAB;

type

//==============================================================================
// TabfWABAddrDlg
//==============================================================================
// Addresses/Contacts dialog.

  TabfWABAddrDestField = (wdfTo, wdfCC, wdfBCC);
  TabfWABAddrDestFields = set of TabfWABAddrDestField;

  TabfWABAddrDlg = class(TabfComponent)
  private
    FAddrBookPtr: Pointer;  
    FAddrList: PAdrList;
    FAddrParams: PAdrParam;
    FBrowseOnly: Boolean;
    FBCCItems: TabfWABItemsCollection;
    FCCItems: TabfWABItemsCollection;
    FCaption: string;
    FHelpContext: THelpContext;
    FDestFieldsTitle: string;
    FDestFields: TabfWABAddrDestFields;
    FDestFieldFocused: TabfWABAddrDestField;
    FToItems: TabfWABItemsCollection;
    FTitleBCC: string;
    FTitleCC: string;
    FTitleTo: string;
    FWAB: TabfWAB;
    FOnClose: TNotifyEvent;
    FOnShow: TNotifyEvent;
    procedure SetDestFields(Value: TabfWABAddrDestFields);
    procedure SetDestFieldFocused(Value: TabfWABAddrDestField);
    procedure SetItemsCollection(Index: Integer;
      Value: TabfWABItemsCollection);
    procedure SetWAB(Value: TabfWAB);
  protected
    procedure CreateAddrList; virtual;
    procedure DeflateItems; virtual;
    procedure DoClose; dynamic;
    procedure DoShow; dynamic;
    procedure Notification(AComponent: TComponent;
      AOperation: TOperation); override;
    procedure UpdateItemsCollection; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearAllFields; virtual;
    function Execute: Boolean; virtual;
    property BCCItems: TabfWABItemsCollection index 0 read FBCCItems
      write SetItemsCollection stored False;
    property CCItems: TabfWABItemsCollection index 1 read FCCItems
      write SetItemsCollection stored False;
    property ToItems: TabfWABItemsCollection index 2 read FToItems
      write SetItemsCollection stored False;
  published
    property About;
    property BrowseOnly: Boolean read FBrowseOnly write FBrowseOnly default False;
    property Caption: string read FCaption write FCaption;
    property DestFieldsTitle: string read FDestFieldsTitle write FDestFieldsTitle;
    property DestFields: TabfWABAddrDestFields read FDestFields
      write SetDestFields default [wdfTo, wdfCC, wdfBCC];
    property DestFieldFocused: TabfWABAddrDestField read FDestFieldFocused
      write SetDestFieldFocused default wdfTo;
    property HelpContext: THelpContext read FHelpContext write
      FHelpContext default 0;
    property TitleBCC: string read FTitleBCC write FTitleBCC;
    property TitleCC: string read FTitleCC write FTitleCC;
    property TitleTo: string read FTitleTo write FTitleTo;
    property WAB: TabfWAB read FWAB write SetWAB;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
  end;

{******************************************************************************}
implementation
{******************************************************************************}

uses
  SysUtils,
  WabTags,
  abfSysUtils,
  abfWABConsts, abfWABUtils;


{$I abf_init.inc}


//==============================================================================
// Private types
//==============================================================================

type
  PFakePCharArr = ^TFakePCharArr;
  TFakePCharArr = array[0..0] of PChar;

  PFakeULONGArr = ^TFakeULONGArr;
  TFakeULONGArr = array[0..0] of ULONG;

//==============================================================================
// TabfWABAddrDlg
//==============================================================================
// Addresses/Contacts dialog.

constructor TabfWABAddrDlg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if not (csDesigning in ComponentState) then _GlobalInit;
  FAddrBookPtr := nil;
  FAddrParams := nil;
  FAddrList := nil;
  FBrowseOnly := False;
  FBCCItems := TabfWABItemsCollection.Create(TabfWABItem, nil);
  FCCItems := TabfWABItemsCollection.Create(TabfWABItem, nil);
  FDestFields := [wdfTo, wdfCC, wdfBCC];
  FDestFieldFocused := wdfTo;
  FHelpContext := 0;
  FToItems := TabfWABItemsCollection.Create(TabfWABItem, nil);
end;

//------------------------------------------------------------------------------

destructor TabfWABAddrDlg.Destroy;
begin
  FreeAndNil(FToItems);
  FreeAndNil(FCCItems);
  FreeAndNil(FBCCItems);
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TabfWABAddrDlg.SetDestFields(Value: TabfWABAddrDestFields);
begin
  try
    if not (wdfBCC in Value) and (wdfBCC in FDestFields) then
      FDestFields := FDestFields - [wdfBCC] else
    if not (wdfBCC in FDestFields) and (wdfBCC in Value) then
    begin
      FDestFields := [wdfTo, wdfCC, wdfBCC];
      Exit;
    end;

    if not (wdfCC in Value) and (wdfCC in FDestFields) then
      FDestFields := FDestFields - [wdfCC, wdfBCC] else
    if not (wdfCC in FDestFields) and (wdfCC in Value) then
    begin
      FDestFields := FDestFields + [wdfTo, wdfCC];
      Exit;
    end;

    if not (wdfTo in Value) and (wdfTo in FDestFields) then
    begin
      FDestFields := [];
      Exit;
    end else
    if not (wdfTo in FDestFields) and (wdfTo in Value) then
      FDestFields := [wdfTo];

  finally
    if not (FDestFieldFocused in FDestFields) then
      FDestFieldFocused := wdfTo;
  end;
end;{procedure TabfWABAddrDlg.SetDestFields}

//------------------------------------------------------------------------------

procedure TabfWABAddrDlg.SetDestFieldFocused(Value: TabfWABAddrDestField);
begin
  if Value in FDestFields then FDestFieldFocused := Value;
end;

//------------------------------------------------------------------------------

procedure TabfWABAddrDlg.SetItemsCollection(Index: Integer;
  Value: TabfWABItemsCollection);
begin
  case Index of
    0: FBCCItems.Assign(Value);
    1: FCCItems.Assign(Value);
    2: FToItems.Assign(Value);
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABAddrDlg.SetWAB(Value: TabfWAB);
begin
  if FWAB <> Value then
  begin
    ClearAllFields;
    FWAB := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABAddrDlg.CreateAddrList;

  //-------------------------------------

  function _AddAddrItem(const MAPIType: Cardinal;
    const AItem: TabfWABItem): Boolean;
  var
    PropValue: PSPropValue;
  begin
    Result := False;

    if not Assigned(AItem) or abfIsEmptyEntryID(AItem.EntryID) then Exit;

    with FAddrList^.aEntries[FAddrList^.cEntries] do
    begin
      cValues := 4;
      FWAB.WABObject.AllocateMore(cValues * SizeOf(rgPropVals^), FAddrList,
        Pointer(rgPropVals));
      FillChar(rgPropVals^, cValues * SizeOf(rgPropVals^), 0);
      PropValue := Pointer(rgPropVals);

      PropValue^.ulPropTag := PR_ENTRYID;
      PropValue^.Value.bin.cb := AItem.EntryID.cb;
      FWAB.WABObject.AllocateMore(PropValue^.Value.bin.cb, rgPropVals,
        Pointer(PropValue^.Value.bin.lpb));
      CopyMemory(PropValue^.Value.bin.lpb, AItem.EntryID.lpb,
        PropValue^.Value.bin.cb);

      PropValue := Pointer(Cardinal(PropValue) + Cardinal(SizeOf(PropValue^)));

      PropValue^.ulPropTag := PR_RECIPIENT_TYPE;
      PropValue^.Value.ul := MAPIType;

      PropValue := Pointer(Cardinal(PropValue) + Cardinal(SizeOf(PropValue^)));

      PropValue^.ulPropTag := PR_DISPLAY_NAME;
      if Assigned(FAddrParams) and (FAddrParams^.cDestFields = 0) then
      begin
        FWAB.WABObject.AllocateMore(Length(AItem.DisplayName) + 2, rgPropVals,
          Pointer(PropValue^.Value.lpszA));
        StrPCopy(PropValue^.Value.lpszA, AItem.DisplayName + ' ');
      end else
      begin
        FWAB.WABObject.AllocateMore(Length(AItem.DisplayName) + 1, rgPropVals,
          Pointer(PropValue^.Value.lpszA));
        StrPCopy(PropValue^.Value.lpszA, AItem.DisplayName);
      end;

      PropValue := Pointer(Cardinal(PropValue) + Cardinal(SizeOf(PropValue^)));

      PropValue^.ulPropTag := PR_OBJECT_TYPE;
      PropValue^.Value.ul := AItem.ItemType;
    end;

    Result := True;
  end;

  //-------------------------------------

var
  i, ListSize: Integer;
begin
  if not (Assigned(FWAB) and FWAB.Active) then Exit;

  abfFreePAdrList(FWAB.WABObject, FAddrList);

  if Assigned(FAddrBookPtr) and (FAddrBookPtr <> Pointer(FWAB.AddrBook)) then
  begin
    ClearAllFields;
    FAddrBookPtr := nil;
    Exit;
  end else
    FAddrBookPtr := Pointer(FWAB.AddrBook);

  DeflateItems;

  if (FToItems.Count + FCCItems.Count + FBCCItems.Count) = 0 then Exit;

  ListSize := SizeOf(ULONG) +
    SizeOf(TAdrEntry) * (FToItems.Count + FCCItems.Count + FBCCItems.Count);

  FWAB.WABObject.AllocateBuffer(ListSize, Pointer(FAddrList));

  FillChar(FAddrList^, ListSize, 0);

  for i := 0 to FToItems.Count - 1 do
  begin
    if _AddAddrItem(MAPI_TO, FToItems[i]) then
      Inc(FAddrList^.cEntries);
  end;

  for i := 0 to FCCItems.Count - 1 do
  begin
    if _AddAddrItem(MAPI_CC, FCCItems[i]) then
      Inc(FAddrList^.cEntries);
  end;

  for i := 0 to FBCCItems.Count - 1 do
  begin
    if _AddAddrItem(MAPI_BCC, FBCCItems[i]) then
      Inc(FAddrList^.cEntries);
  end;

  if FAddrList^.cEntries = 0 then
    abfFreePAdrList(FWAB.WABObject, FAddrList);
end;

//------------------------------------------------------------------------------

procedure TabfWABAddrDlg.DeflateItems;
var
  i: Integer;
begin
  for i := FToItems.Count - 1 downto 0 do
    if abfIsEmptyEntryID(FToItems[i].EntryID)
      or (FToItems[i].ItemType = 0) then
      FToItems[i].Free;

  for i := FCCItems.Count - 1 downto 0 do
    if abfIsEmptyEntryID(FCCItems[i].EntryID)
      or (FCCItems[i].ItemType = 0) then
      FCCItems[i].Free;

  for i := FBCCItems.Count - 1 downto 0 do
    if abfIsEmptyEntryID(FBCCItems[i].EntryID)
      or (FBCCItems[i].ItemType = 0) then
      FBCCItems[i].Free;
end;

//------------------------------------------------------------------------------

procedure TabfWABAddrDlg.DoClose;
begin
  try
    if Assigned(FOnClose) then FOnClose(Self);
  except
  end;
end;

//------------------------------------------------------------------------------

procedure TabfWABAddrDlg.DoShow;
begin
  try
    if Assigned(FOnShow) then FOnShow(Self);
  except
  end;  
end;

//------------------------------------------------------------------------------

procedure TabfWABAddrDlg.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);
  if AOperation = opRemove then
  begin
    if AComponent = FWAB then
    begin
      if FWAB.Active then abfFreePAdrList(FWAB.WABObject, FAddrList);
      FWAB := nil;
    end;
  end
end;

//------------------------------------------------------------------------------

procedure TabfWABAddrDlg.UpdateItemsCollection;
var
  i, a, TempIndex: Integer;
  RecipType: ULONG;
  RecipID: PSBinary;
  PropValue: PSPropValue;
begin
  ClearAllFields;

  if not Assigned(FAddrList) then Exit;

  if not (Assigned(FWAB) and FWAB.Active) then Exit;

  for i := 0 to FAddrList^.cEntries - 1 do
    with FAddrList^.aEntries[i] do
    begin
      RecipType := 0;
      RecipID := nil;
      try
        for a := 0 to cValues - 1 do
        if @(FAddrList^.aEntries[i]) <> nil then
        begin
          PropValue := Pointer(Cardinal(FAddrList^.aEntries[i].rgPropVals) +
            Cardinal(a*SizeOf(PropValue^)));
          if PropValue^.ulPropTag = PR_RECIPIENT_TYPE then
            RecipType := PropValue^.Value.ul else
          if PropValue^.ulPropTag = PR_ENTRYID then
            abfMoveSBinary(@(PropValue^.Value.bin), RecipID);
        end;

        if not Assigned(RecipID) then Exit;

        TempIndex := FWAB.GetItemIndex(RecipID);

        if TempIndex < 0 then Exit;

        case RecipType of
          MAPI_TO:  FToItems .Add.Assign(FWAB.Items[TempIndex]);
          MAPI_CC:  FCCItems .Add.Assign(FWAB.Items[TempIndex]);
          MAPI_BCC: FBCCItems.Add.Assign(FWAB.Items[TempIndex]);
        else
          FToItems.Add.Assign(FWAB.Items[TempIndex]);
        end;

      finally
        abfFreeSBinary(RecipID);
      end;
    end;{with FAddrList^.aEntries[i] do}
end;{procedure TabfWABAddrDlg.UpdateItemsCollection}

//------------------------------------------------------------------------------

procedure TabfWABAddrDlg.ClearAllFields;
begin
  FToItems.Clear;
  FCCItems.Clear;
  FBCCItems.Clear;
end;

//------------------------------------------------------------------------------

function TabfWABAddrDlg.Execute: Boolean;
var
  TempHandle: ULONG;
  TempTitles: PFakePCharArr;
  TempTypes: PFakeULONGArr;
  i: Integer;
begin
  Result := False;

  if not Assigned(FWAB) then Exit;

  if not FWAB.Active then
    raise EabfWABException.Create(SabfWAB_NotOpenedError);

  TempHandle := ParentHandle;

  FWAB.WabObject.AllocateBuffer(SizeOf(FAddrParams^), Pointer(FAddrParams));
  try
    FillChar(FAddrParams^, SizeOf(FAddrParams^), 0);

    try
      DoShow;

      with FAddrParams^ do
      begin
        ulHelpContext := FHelpContext;

        FWAB.WABObject.AllocateMore(Length(FCaption) + 1, FAddrParams,
          Pointer(lpszCaption));
        StrPCopy(lpszCaption, FCaption);

        FWAB.WABObject.AllocateMore(Length(FDestFieldsTitle) + 1, FAddrParams,
          Pointer(lpszDestWellsTitle));
        StrPCopy(lpszDestWellsTitle, FDestFieldsTitle);

        if wdfBCC in FDestFields then cDestFields := 3 else
        if wdfCC  in FDestFields then cDestFields := 2 else
        if wdfTo  in FDestFields then cDestFields := 1;

        case FDestFieldFocused of
          wdfTo:  nDestFieldFocus := 0;
          wdfCC:  nDestFieldFocus := 1;
          wdfBCC: nDestFieldFocus := 2;
        end;

        ulFlags := DIALOG_MODAL or AB_SELECTONLY or AB_RESOLVE;

        if FBrowseOnly then cDestFields := 0 else
        if (cDestFields = 0) then ulFlags := ulFlags or ADDRESS_ONE;

        if not FBrowseOnly and (cDestFields > 0) then
        begin
          if Length(FTitleTo + FTitleCC + FTitleBCC) > 0 then
            FWAB.WABObject.AllocateMore(cDestFields * SizeOf(PChar), FAddrParams,
              Pointer(lppszDestTitles));

          TempTitles := Pointer(lppszDestTitles);

          if Assigned(TempTitles) then
            for i := 0 to cDestFields - 1 do
              case i of
                0:
                  begin
                    FWAB.WABObject.AllocateMore(Length(FTitleTo) + 1,
                      FAddrParams, Pointer(TempTitles[i]));
                     StrPCopy(TempTitles[i], FTitleTo);
                  end;
                1:
                  begin
                    FWAB.WABObject.AllocateMore(Length(FTitleCC) + 1,
                      FAddrParams, Pointer(TempTitles[i]));
                     StrPCopy(TempTitles[i], FTitleCC);
                  end;
                2:
                  begin
                    FWAB.WABObject.AllocateMore(Length(FTitleBCC) + 1,
                      FAddrParams, Pointer(TempTitles[i]));
                     StrPCopy(TempTitles[i], FTitleBCC);
                  end;
              end;{case i of}

          if Assigned(TempTitles) then
            FWAB.WABObject.AllocateMore(cDestFields * SizeOf(ULONG), FAddrParams,
              Pointer(lpulDestComps));

          TempTypes := Pointer(lpulDestComps);

          if Assigned(TempTitles) then
            for i := 0 to cDestFields - 1 do
              case i of
                0: TempTypes[i] := MAPI_TO;
                1: TempTypes[i] := MAPI_CC;
                2: TempTypes[i] := MAPI_BCC;
              end;
        end;{if not FBrowseOnly and (cDestFields > 0) then}

        CreateAddrList;

        FWAB.AddrBook.GetPAB(cbABContEntryID, lpABContEntryID);
        try
          Result := FWAB.AddrBook.Address(TempHandle,
            FAddrParams, FAddrList) = S_OK;

          if Result then
            UpdateItemsCollection;

        finally
          FWAB.WabObject.FreeBuffer(lpABContEntryID);
          lpABContEntryID := nil;
          cbABContEntryID := 0;
        end;
      end;{with FAddrParams^ do}
    finally
      abfFreePAdrList(FWAB.WABObject, FAddrList);
      DoClose;
    end;
  finally
    FWAB.WabObject.FreeBuffer(FAddrParams);
    FAddrParams := nil;
  end;
end;{function TabfWABAddrDlg.Execute}


{******************************************************************************}
initialization
{******************************************************************************}
{$IfDef abfVCLTrial}
  abfCheckTrialVersion;
{$EndIf abfVCLTrial}

//------------------------------------------------------------------------------
// Register classes to allow use RTI and create descendant components.
  abfRegisterClasses([TabfWABAddrDlg]);

end{unit abfWABDialogs}.
