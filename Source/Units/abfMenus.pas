{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfMenus;

{$I abf.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Menus,
  abfComponents;

type

//==============================================================================
// TabfCustomSystemMenuHook
//==============================================================================
// A prototype of all system menu hooks
// Note: ImageList of menu should not be specified for properly submenu drawing.

  TabfCustomSystemMenuHook = class(TabfCustomWndProcHook)
  protected
    FInserted: Boolean;
    FInsertToAppMenu: Boolean;
    FPosition: Integer;
    procedure Loaded; override;
    procedure MessageProc(Sender: TObject; var Msg: TMessage;
      var Handled: Boolean); virtual;
  // Properties Get/Set
    procedure SetHandle(A: THandle); override;
    function  GetMenuHandle: THandle; virtual;
    procedure SetInserted(A: Boolean); virtual;
    procedure SetInsertToAppMenu(A: Boolean); virtual;
    procedure SetPosition(A: Integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Insert; virtual;
    procedure Remove; virtual;
  // Properties 
    property MenuHandle: THandle read GetMenuHandle;
    property Inserted: Boolean read FInserted write SetInserted default True;
    property InsertToAppMenu: Boolean read FInsertToAppMenu
      write SetInsertToAppMenu default False;
    property Position: Integer read FPosition write SetPosition default 0;
  end;{TabfCustomSystemMenuHook = class(TabfCustomWndProcHook)}


//==============================================================================
// TabfSystemMenuItem
//==============================================================================
// TabfSystemMenuItem is a non-visual component that adds a menu item into the
// system menu of any window, form or application. The menu item can be taken
// from any TMainManu or TPopupMenu components are present on the form.
// The component adds item to the form's system menu by default. Set the
// InsertToAppMenu property True to add a menu item to the system menu of the
// application. Use public Handle property to specify any other window by its
// handle.
// Note: ImageList of menu should not be specified for properly submenu drawing.

  TabfSystemMenuItem = class(TabfCustomSystemMenuHook)
  protected
    FCommand: Integer;
    FMenuItem: TMenuItem;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure MessageProc(Sender: TObject; var Msg: TMessage;
      var Handled: Boolean); override;
  // Properties Get/Set
    function GetCommand: Integer; virtual;
    function GetSubMenuHandle: THandle; virtual;
    procedure SetMenuItem(const A: TMenuItem); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Insert; override;
    procedure Remove; override;
  // Properties
    property Command: Integer read GetCommand;
    property MenuHandle;
    property SubMenuHandle: THandle read GetSubMenuHandle;
  published
    property About;
    property Inserted;
    property InsertToAppMenu;
    property MenuItem: TMenuItem read FMenuItem write SetMenuItem;
    property Position;
  end;{TabfSystemMenuItem = class(TabfCustomSystemMenuHook)}


//==============================================================================
// TabfSystemMenuInserter
//==============================================================================
// TabfSystemMenuInserter is a non-visual component that adds all items of some
// TPopupMenu component into the system menu of any window, form or application.
// The component adds items to the form's system menu by default. Set the
// InsertToAppMenu property True to add menu items to the system menu of the
// application. Use public Handle property to specify any other window by its
// handle.
// Note: ImageList of menu should not be specified for properly submenu drawing.

  TabfSystemMenuInserter = class(TabfCustomSystemMenuHook)
  protected
    FPopupMenu: TPopupMenu;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure MessageProc(Sender: TObject; var Msg: TMessage;
      var Handled: Boolean); override;
  // Properties Get/Set
    procedure SetPopupMenu(const A: TPopupMenu); virtual;
  public
    procedure Insert; override;
    procedure Remove; override;
  // Properties
    property MenuHandle;
  published
    property About;
    property Inserted;
    property InsertToAppMenu;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property Position;
  end;{TabfSystemMenuInserter = class(TabfCustomSystemMenuHook)}


//==============================================================================
// TabfPopupMenu
//==============================================================================
// Enchanced popup menu component. Has Visible property and OnClose event

  TabfPopupMenu = class(TPopupMenu)
  private
    FAbout: string;
    FVisible: Boolean;
    FOnClose: TNotifyEvent;
   public
     procedure Popup(X, Y: Integer); override;
     property Visible: Boolean read FVisible;
   published
     property About: string read FAbout write FAbout stored False;
     property OnClose: TNotifyEvent read FOnClose write FOnClose;
   end;


//==============================================================================
// Menu utilities
//==============================================================================

//------------------------------------------------------------------------------
// Inserts delphi styled menu item into windows menu.

function abfInsertMenuItem(Menu: HMENU; MenuItem: TMenuItem; Position: Integer;
  Command: Word; ARightToLeft: Boolean): Boolean;

{******************************************************************************}
implementation
{******************************************************************************}

uses
  abfSysUtils, abfStrUtils;

{$I abf_init.inc}

//==============================================================================
// Menu utilities
//==============================================================================

const
  RightToLeftMenuFlag = MFT_RIGHTORDER or MFT_RIGHTJUSTIFY;
  Checks: array[Boolean] of DWORD = (MF_UNCHECKED, MF_CHECKED);
  Enables: array[Boolean] of DWORD = (MF_DISABLED or MF_GRAYED, MF_ENABLED);
  Breaks: array[TMenuBreak] of DWORD = (0, MF_MENUBREAK, MF_MENUBARBREAK);
  Separators: array[Boolean] of DWORD = (MF_STRING, MF_SEPARATOR);

//------------------------------------------------------------------------------
// Inserts delphi styled menu item into the windows menu.

function abfInsertMenuItem(Menu: HMENU; MenuItem: TMenuItem; Position: Integer;
  Command: Word; ARightToLeft: Boolean): Boolean;
const
  IBreaks: array[TMenuBreak] of DWORD = (MFT_STRING, MFT_MENUBREAK, MFT_MENUBARBREAK);
  IChecks: array[Boolean] of DWORD = (MFS_UNCHECKED, MFS_CHECKED);
  IDefaults: array[Boolean] of DWORD = (0, MFS_DEFAULT);
  IEnables: array[Boolean] of DWORD = (MFS_DISABLED or MFS_GRAYED, MFS_ENABLED);
  IRadios: array[Boolean] of DWORD = (MFT_STRING, MFT_RADIOCHECK);
  ISeparators: array[Boolean] of DWORD = (MFT_STRING, MFT_SEPARATOR);
  IRTL: array[Boolean] of DWORD = (0, RightToLeftMenuFlag);
var
  MenuItemInfo: TMenuItemInfo;
  Caption: string;
  NewFlags: Integer;

  //-------------------------------------

  procedure _InsertNew;
  begin
    if IsWin95 or IsWin95OSR2 then MenuItemInfo.cbSize := 44
    else MenuItemInfo.cbSize := SizeOf(MenuItemInfo);
    with MenuItemInfo do
    begin
      fMask := MIIM_CHECKMARKS or MIIM_DATA or MIIM_ID or MIIM_STATE or
        MIIM_TYPE;
      fType := MFT_STRING or IRadios[MenuItem.RadioItem] or
        IBreaks[MenuItem.Break] or ISeparators[MenuItem.Caption = '-'] or
        IRTL[ARightToLeft];
      fState := IChecks[MenuItem.Checked] or IEnables[MenuItem.Enabled] or
        IDefaults[MenuItem.Default];
      wID := Command;
      hSubMenu := 0;
      hbmpChecked := 0;
      hbmpUnchecked := 0;
      dwTypeData := PChar(Caption);
      cch := Length(Caption);
      if MenuItem.Count > 0 then
      begin
        hSubMenu := MenuItem.Handle;
        fMask := fMask or MIIM_SUBMENU;
      end;
    {$IfDef D4}
{ TODO -oKARPOLAN : Implement Bitmap and ImageList support. }
{      if not MenuItem.Bitmap.Empty then
      begin
        hbmpItem := MenuItem.Bitmap.Handle;
        fMask := fMask or MIIM_BITMAP and not (MFT_SEPARATOR or MFT_STRING);
      end;{}
    {$EndIf D4}
    end;{with MenuItemInfo do}
    Result := InsertMenuItem(Menu, Position, True, MenuItemInfo);
  end;{Internal procedure _InsertNew}

  //-------------------------------------

  procedure _InsertOld;
  begin
    NewFlags := Breaks[MenuItem.Break] or Checks[MenuItem.Checked] or
      Enables[MenuItem.Enabled] or Separators[MenuItem.Caption = ''] or
      MF_BYPOSITION;
    if MenuItem.Count > 0 then
      Result := InsertMenu(Menu, Position, MF_POPUP or NewFlags,
        MenuItem.Handle, PChar(MenuItem.Caption))
    else
      Result := InsertMenu(Menu, Position, NewFlags, Command, PChar(Caption));
  end;{Internal procedure _InsertOld}

begin
  if not MenuItem.Visible then Exit;
  Caption := MenuItem.Caption;
  if MenuItem.Count > 0 then MenuItemInfo.hSubMenu := MenuItem.Handle else
  if (MenuItem.ShortCut <> scNone) then Caption := Caption + #9 +
    ShortCutToText(MenuItem.ShortCut);
  if Lo(GetVersion) >= 4 then _InsertNew else _InsertOld;
end;{procedure abfInsertMenuItem}


//==============================================================================
// TabfCustomSystemMenuHook
//==============================================================================
// A prototype of all system menu hooks.
// Note: ImageList of menu should not be specified for properly submenu drawing.
// Date: 03/16/2001
{ TabfCustomSystemMenuHook }

constructor TabfCustomSystemMenuHook.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if not (csDesigning in ComponentState) then _GlobalInit;
  FInserted := True;
  FInsertToAppMenu := False;
  FPosition := 0;
  OnMessageBefore := MessageProc;
end;

//------------------------------------------------------------------------------

destructor TabfCustomSystemMenuHook.Destroy;
begin
  Remove;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TabfCustomSystemMenuHook.Insert;
begin
  FInserted := True;
  Active := True;
end;

//------------------------------------------------------------------------------

procedure TabfCustomSystemMenuHook.Remove;
begin
  FInserted := False;
  Active := False;
end;

//------------------------------------------------------------------------------

procedure TabfCustomSystemMenuHook.Loaded;
begin
  inherited Loaded;
  if Inserted then
  begin
    FInserted := False;
    Inserted := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfCustomSystemMenuHook.MessageProc(Sender: TObject;
  var Msg: TMessage; var Handled: Boolean);
begin
  Handled := False;
end;

//------------------------------------------------------------------------------

procedure TabfCustomSystemMenuHook.SetHandle(A: THandle);
var
  OldValue: Boolean;
begin
  if A = FHandle then Exit;
  OldValue := Inserted;
  try
    Inserted := False;
    inherited SetHandle(A);
  finally
    Inserted := OldValue;
  end;
end;

//------------------------------------------------------------------------------

function TabfCustomSystemMenuHook.GetMenuHandle: THandle;
begin
  Result := GetSystemMenu(Handle, False);
end;

//------------------------------------------------------------------------------

procedure TabfCustomSystemMenuHook.SetInserted(A: Boolean);
begin
  if FInserted = A then Exit;
  if ([csDesigning, csLoading, csDestroying] * ComponentState <> []) then
  begin
    FInserted := A;
    Exit;
  end;
  if A then Insert else Remove;
end;

//------------------------------------------------------------------------------

procedure TabfCustomSystemMenuHook.SetInsertToAppMenu(A: Boolean);
begin
  if InsertToAppMenu = A then Exit;
  FInsertToAppMenu := A;
  if InsertToAppMenu then Handle := Application.Handle else Handle := 0; 
end;

//------------------------------------------------------------------------------

procedure TabfCustomSystemMenuHook.SetPosition(A: Integer);
var
  OldValue: Boolean;
begin
  if Position = A then Exit;
  OldValue := Inserted;
  try
    Inserted := False;
    FPosition := A;
  finally
    Inserted := OldValue;
  end;
end;


//==============================================================================
// TabfSystemMenuItem
//==============================================================================
// TabfSystemMenuItem is a non-visual component that adds a menu item into the
// system menu of any window, form or application. The menu item can be taken
// from any TMainManu or TPopupMenu components are present on the form.
// The component adds item to the form's system menu by default. Set the
// InsertToAppMenu property True to add a menu item to the system menu of the
// application. Use public Handle property to specify any other window by its
// handle.
// Note: ImageList of menu should not be specified for properly submenu drawing.
// Date: 04/15/2001
{ TabfSystemMenuItem }

var
  _SystemMenuInserterCommand: Word = 0; // Iterator

//------------------------------------------------------------------------------

function _ProcessMenuItems(const Item: TMenuItem; ACommand: Word): Boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to Item.Count - 1 do
    if Item.Items[i].Command = ACommand then
    begin
      Item.Items[i].Click;
      Result := True;
      Exit;
    end else
      _ProcessMenuItems(Item.Items[i], ACommand);
end;

//------------------------------------------------------------------------------

constructor TabfSystemMenuItem.Create(AOwner: TComponent);
begin
// Make unique command 
  Inc(_SystemMenuInserterCommand, 16);
  FCommand := _SystemMenuInserterCommand;
  inherited Create(AOwner);
end;

//------------------------------------------------------------------------------

procedure TabfSystemMenuItem.Insert;
begin
  if not Assigned(MenuItem) then Exit;
  FInserted := abfInsertMenuItem(MenuHandle, MenuItem, Position, Command,
    {$IfNDef D4}False{$Else}MenuItem.GetParentMenu.IsRightToLeft{$EndIf});
  inherited Insert;
end;

//------------------------------------------------------------------------------

procedure TabfSystemMenuItem.Remove;
var
  Menu: HMENU;
  Buff: array[1..255] of Char;
  S: widestring;
begin
  inherited Remove;
  if not Assigned(MenuItem) then Exit;
  Menu := MenuHandle;
  GetMenuString(Menu, Command, @Buff, SizeOf(Buff), MF_BYCOMMAND);
{$IFDEF WIN32}
  S := StrPas(@Buff);
  abfDeleteAfterCharW(S, #9);
{$ELSE}
  S := Buff;
  abfDeleteAfterCharW(S, #9);
{$ENDIF}
  if (AnsiCompareText(MenuItem.Caption, S) = 0) or (MenuItem.Caption = '-') then
    RemoveMenu(Menu, Command, MF_BYCOMMAND);
end;

//------------------------------------------------------------------------------

procedure TabfSystemMenuItem.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = MenuItem then MenuItem := nil else
    if (AComponent = Self) and Inserted then Remove;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfSystemMenuItem.MessageProc(Sender: TObject; var Msg: TMessage;
  var Handled: Boolean);
begin
  Handled := False;
  if not Assigned(MenuItem) then Exit;
  if not Inserted then Exit;
  if Msg.Msg <> WM_SysCommand then Exit;
  with TWMSysCommand(Msg) do
    if CmdType = Command then
    begin
      MenuItem.Click;
      if not (csDesigning in ComponentState) then if not _ASK then _TADA;
    end else
      _ProcessMenuItems(MenuItem, CmdType);
end;

//------------------------------------------------------------------------------

function TabfSystemMenuItem.GetCommand: Integer;
begin
  Result := FCommand;
  if Assigned(MenuItem) then Result := Result + MenuItem.Command;
end;

//------------------------------------------------------------------------------

function TabfSystemMenuItem.GetSubMenuHandle: THandle;
begin
  Result := 0;
  if Assigned(MenuItem) then
    if MenuItem.Count > 0 then Result := MenuItem.Handle;
end;

//------------------------------------------------------------------------------

procedure TabfSystemMenuItem.SetMenuItem(const A: TMenuItem);
var
  OldValue: Boolean;
begin
  if FMenuItem = A then Exit;
  OldValue := Inserted;
  try
    Inserted := False;
    FMenuItem := A;
    if Assigned(FMenuItem) then FMenuItem.FreeNotification(Self);
  finally
    Inserted := OldValue;
  end;
end;


//==============================================================================
// TabfSystemMenuInserter
//==============================================================================
// TabfSystemMenuInserter is a non-visual component that adds all items of some
// TPopupMenu component into the system menu of any window, form or application.
// The component adds items to the form's system menu by default. Set the
// InsertToAppMenu property True to add menu items to the system menu of the
// application. Use public Handle property to specify any other window by its
// handle.
// Note: ImageList of menu should not be specified for properly submenu drawing.
// Date: 04/15/2001
{ TabfSystemMenuInserter }

procedure TabfSystemMenuInserter.Insert;
var
  Menu: HMENU;
  i, p: Integer;
begin
  if not Assigned(PopupMenu) then Exit;
  Menu := MenuHandle;
  with PopupMenu do
    for i := 0 to Items.Count - 1 do
    begin
      if Position < 0 then p := -1 else p := Position + i;
      abfInsertMenuItem(Menu, Items[i], DWORD(p), Items[i].Command,
       {$IfNDef D4}False{$Else}PopupMenu.IsRightToLeft{$EndIf});
    end;
  inherited Insert;
end;

//------------------------------------------------------------------------------

procedure TabfSystemMenuInserter.Remove;
var
  Menu: HMENU;
  Buff: array[1..255] of Char;
  S: widestring;
  i: Integer;
begin
  inherited Remove;
  if not Assigned(PopupMenu) then Exit;
  Menu := MenuHandle;
  with PopupMenu do
    for i := 0 to Items.Count - 1 do
    begin
      GetMenuString(Menu, Items[i].Command, @Buff, SizeOf(Buff), MF_BYCOMMAND);
{$IFDEF D9}
      S := Buff;
      abfDeleteAfterCharW(S, #9);
{$ELSE}
      S := StrPas(@Buff);
      abfDeleteAfterChar(S, #9);
{$ENDIF}
      if (AnsiCompareText(Items[i].Caption, S) = 0) or
        (Items[i].Caption = '-') then
        RemoveMenu(Menu, Items[i].Command, MF_BYCOMMAND);
    end;
end;

//------------------------------------------------------------------------------

procedure TabfSystemMenuInserter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = PopupMenu then PopupMenu := nil else
    if (AComponent = Self) and Inserted then Remove;
  end;
end;

//------------------------------------------------------------------------------

procedure TabfSystemMenuInserter.MessageProc(Sender: TObject; var Msg: TMessage;
  var Handled: Boolean);
begin
  Handled := False;
  if not Assigned(PopupMenu) then Exit;
  if not Inserted then Exit;
  if Msg.Msg <> WM_SysCommand then Exit;
  with TWMSysCommand(Msg) do
    _ProcessMenuItems(PopupMenu.Items, CmdType);
  if not (csDesigning in ComponentState) then if not _ASK then _TADA;
end;

//------------------------------------------------------------------------------

procedure TabfSystemMenuInserter.SetPopupMenu(const A: TPopupMenu);
var
  OldValue: Boolean;
begin
  if FPopupMenu = A then Exit;
  OldValue := Inserted;
  try
    Inserted := False;
    FPopupMenu := A;
    if Assigned(FPopupMenu) then FPopupMenu.FreeNotification(Self);
  finally
    Inserted := OldValue;
  end;
end;


//==============================================================================
// TabfPopupMenu
//==============================================================================
// Enchanced popup menu component. Has Visible property and OnClose event
{ TabfPopupMenu }

procedure TabfPopupMenu.Popup(X, Y: Integer);
begin
  FVisible := True;
  try
    inherited Popup(X, Y);

    if Assigned(OnClose) then OnClose(Self);
  finally
    FVisible := False;
  end;
end;


{******************************************************************************}
initialization
{******************************************************************************}
{$IfDef abfVCLTrial}
  abfCheckTrialVersion;
{$EndIf abfVCLTrial}

//------------------------------------------------------------------------------
// Register classes to allow use RTI and create descendant components.
  abfRegisterClasses([TabfCustomSystemMenuHook, TabfSystemMenuItem,
    TabfSystemMenuInserter, TabfPopupMenu]);{}

end{unit abfMenus}.
