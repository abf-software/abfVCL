{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfVclUtils;

{$I abf.inc}

interface

uses
  Windows, Forms, Classes, Controls, Graphics,
// ABF VCL
  abfClasses;


//==============================================================================
// Misc utilities
//==============================================================================

//------------------------------------------------------------------------------
// Provides a waiting (delay) for a given count of MSec (Sec*10^-3). The message
// processing will continue.
procedure abfDelay(MSec: Cardinal);


//==============================================================================
// Files/folders utilities
//==============================================================================

//------------------------------------------------------------------------------
// Copies files specified in the given FileList string list. Formats of the
// FileList item are 'SrcFile=DstFile', 'SrcFile', 'SrcFile=*.dll',
// 'SrcFile=App.*'
function abfCopyFiles(const AFileList: TabfWideStrings): Boolean;

//------------------------------------------------------------------------------
// Copies files specified in the given AFileList string list to ADestDir
// directory.
function abfCopyFilesToDir(const AFileList: TabfWideStrings;
  const ADestDir: TabfWideString; const ACreateDir: Boolean = False): Boolean;

//------------------------------------------------------------------------------
// Copies SrcFile to DstFile with events. Any event can be nil.
function abfCopyFileWithEvents(const SrcFile, DstFile: string;
  OnError: TabfCopyThreadErrorEvent; OnProgress: TabfCopyThreadProgressEvent;
  OnFinish: TNotifyEvent): Boolean;

//------------------------------------------------------------------------------
// Moves files specified in the given AFileList string list. Formats of the
// FileList item are 'SrcFile=DstFile', 'SrcFile', 'SrcFile=*.dll',
// 'SrcFile=App.*'
function abfMoveFiles(const AFileList: TabfWideStrings): Boolean;

//------------------------------------------------------------------------------
// Moves files specified in the given AFileList string list to ADestDir 
// directory.
function abfMoveFilesToDir(const AFileList: TabfWideStrings;
  const ADestDir: TabfWideString; const ACreateDir: Boolean = False): Boolean;

//------------------------------------------------------------------------------
// Removes specified directory even if it is not empty.
// Date: 07/09/2001
procedure abfRemoveDir(const DirName: TabfWideString);

//------------------------------------------------------------------------------
// Empties specified directory.
// Date: 07/09/2001
procedure abfClearDir(const DirName: TabfWideString);


//------------------------------------------------------------------------------
// Runs Create Shortcut wizard for creating shortcut in the specified directory
function abfRunShortcutWizard(const DirName: string): Boolean;

//------------------------------------------------------------------------------
// Creates shortcut/link with given data. HotKeyText format is a text
// interpretation of the HotKey "Ctrl+Shift+Z" for example. If HotKeyText = ''
// then HotKey value will be taken from HotKey parameter. ShowFlag can be
// SW_SHOWNORMAL, SW_SHOWMAXIMIZED or SW_SHOWMINNOACTIVE.
function abfCreateShellLink(const FileName, CommandLine, Arguments,
  WorkingDirectory, Description, IconLocation: string; IconIndex: Integer;
  HotKey: Word; const HotKeyText: string; ShowFlag: Integer): Boolean;

//------------------------------------------------------------------------------
// Reads shortcut/link file data of file specified by FileName parameter.
// HotKeyText format is a text interpretation of the HotKey "Ctrl+Shift+Z" for
// example.
function abfReadShellLink(const FileName: string; var CommandLine, Arguments,
  WorkingDirectory, Description, IconLocation: string; var IconIndex: Integer;
  var HotKey: Word; var HotKeyText: string; var ShowFlag: Integer): Boolean;


//==============================================================================
// Components/Controls utilities
//==============================================================================

//------------------------------------------------------------------------------
// Components enumerator. Calls Proc for each child components of Container that
// match goven ChildClass. Enumerates all components if ChildClass = nil

procedure abfForEachComponent(const Container: TComponent;
  ChildClass: TComponentClass; Proc: TNotifyEvent);

//------------------------------------------------------------------------------
// Controls enumerator. Calls Proc for each child controls of Container that
// match goven ChildClass. Enumerates all controls if ChildClass = nil
procedure abfForEachControl(const Container: TWinControl;
  ChildClass: TControlClass; Proc: TNotifyEvent);

//==============================================================================
// Application, forms
//==============================================================================

//------------------------------------------------------------------------------
// Restores application's button at the taskbar.
procedure abfApplicationTaskBarButtonShow;

//------------------------------------------------------------------------------
// Removes application's button from the taskbar.
procedure abfApplicationTaskBarButtonHide;

//------------------------------------------------------------------------------
// Shows main form of the application and restores application's button at the
// taskbar.
procedure abfMainFormShow;

//------------------------------------------------------------------------------
// Hides main form of the application and removes application's button from the
// taskbar.
procedure abfMainFormHide;

//------------------------------------------------------------------------------
// Returns coordinates of screen where the specified form is placed.
// Date: 01/16/2003
function abfGetWorkAreaRectByForm(const Form: TCustomForm): TRect;

//------------------------------------------------------------------------------
// Returns coordinates of screen where the specified point is placed.
// Date: 10/22/2003
function abfGetWorkAreaRectByPoint(X, Y: Integer): TRect;

//------------------------------------------------------------------------------
// Returns coordinates of screen where the specified window is placed.
function abfGetWorkAreaRectByWindow(const AWindowHandle: THandle): TRect;

//------------------------------------------------------------------------------
// Centeres the SlaveForm form upon the MasterForm form and alligned to screen
procedure abfFormCenterForm(const SlaveForm, MasterForm: TCustomForm);

//------------------------------------------------------------------------------
// Centres the AForm form upon the window and aligned to screen
procedure abfFormCenterWindow(const AForm: TCustomForm;
  const AWindowHandle: THandle);

//------------------------------------------------------------------------------
// Sets a position of the given form to be fully fit the nearest monitor
procedure abfFormFitToScreen(const Form: TCustomForm);

//------------------------------------------------------------------------------
// Sets a position of the given form to be centerd to the nearest monitor
// Date: 01/16/2003
procedure abfFormCenterScreen(const Form: TCustomForm);

//------------------------------------------------------------------------------
// Searches for the previous instance of the running application. Returns handle
// of the prev instance or the current one.
function abfFindPrevInstance: HWnd;

//------------------------------------------------------------------------------
// Activates the previous instance of the running application. Returns True if
// prev instance successfully activated.
function abfActivatePrevInstance: Boolean;

//------------------------------------------------------------------------------
// Opens/runs specified file.
procedure abfShellExecuteA(const AFileName: AnsiString);
procedure abfShellExecute(const AFileName: string);
procedure abfShellExecuteW(const AFileName: WideString);

//------------------------------------------------------------------------------
// Loads icons from shell32.dll by index
procedure abfGetShell32Icon(IconIndex: Integer; const LargeIcon,
  SmallIcon: TIcon);


//==============================================================================
// Internet utilities
//==============================================================================

//------------------------------------------------------------------------------
// Opens specified link in the default browser. A new browser window will be
// created if the NewWindow parameter is True.
procedure abfGotoUrlEx(const URL: string; NewWindow: Boolean);

//------------------------------------------------------------------------------
// Opens specified link in the default browser
procedure abfGotoUrl(const URL: string);

//------------------------------------------------------------------------------
// Creates e-mail message in the default mail browser
// Note: Under constraction!
procedure abfSendEmailEx(const Address, Subject, Body: string);

//------------------------------------------------------------------------------
// Creates e-mail message in the default mail browser
procedure abfSendEmail(const Address, Subject: string);

//------------------------------------------------------------------------------
// Opens specified link in the default browser or creates e-mail to specified address
procedure abfSmartOpenUrl(const URL: string); overload;
procedure abfSmartOpenUrl(const URL, Subject: string); overload;
procedure abfSmartOpenUrl(const URL: string; NewWindow: Boolean); overload;
procedure abfSmartOpenUrl(const URL, Subject: string; NewWindow: Boolean); overload;

{******************************************************************************}
implementation
{******************************************************************************}

uses
  MultiMon,
  SysUtils, Menus, ShlObj, ComObj, ActiveX, ShellAPI,
// ABF VCL
  abfConsts, abfVclConsts, abfComponents,
  abfSysUtils, abfStrUtils, abfRegistry;

//==============================================================================
// Misc utilities
//==============================================================================

//------------------------------------------------------------------------------
// Provides a waiting (delay) for a given count of MSec (Sec*10^-3). The message
// processing will continue.
// Date: 04/01/2000

procedure abfDelay(MSec: Cardinal);
var
  BeginTime: Cardinal;
begin
  BeginTime := GetTickCount;
  repeat
    Application.ProcessMessages;
  until Cardinal(Abs(GetTickCount - BeginTime)) >= MSec;
end;


//==============================================================================
// Files/folders utilities
//==============================================================================

//------------------------------------------------------------------------------
// Copies files specified in the given AFileList string list. Formats of the
// FileList item are 'SrcFile=DstFile', 'SrcFile', 'SrcFile=*.dll',
// 'SrcFile=App.*'
// Date: 04/01/2000

function abfCopyFiles(const AFileList: TabfWideStrings): Boolean;
begin
  with TabfFileOperation.Create(nil) do
  try
    Operation := fotCopy;
    FileList := AFileList;
    Options := Options +
      [fofSilent, fofNoConfirmation] -
      [fofRenameCollision];
    Result := Execute;
  finally
    Free;
  end;
end;

//------------------------------------------------------------------------------
// Copies files specified in the given AFileList string list to ADestDir
// directory.
// Date: 07/09/2007

function abfCopyFilesToDir(const AFileList: TabfWideStrings;
  const ADestDir: TabfWideString; const ACreateDir: Boolean = False): Boolean;
begin
  if ACreateDir then
    abfForceDirectoriesW(ADestDir);

  with TabfFileOperation.Create(nil) do
  try
    Operation := fotCopy;
    FileList := AFileList;
    DestFolder := ADestDir;
    Options := Options +
      [fofSilent, fofNoConfirmation] -
      [fofRenameCollision];
    Result := Execute;

    if Result then
      SHChangeNotify(SHCNE_UPDATEDIR, SHCNF_PATH, Pointer(ADestDir), nil);
  finally
    Free;
  end;
end;

//------------------------------------------------------------------------------
// Copies SrcFile to DstFile with events. Any event can be nil.

function abfCopyFileWithEvents(const SrcFile, DstFile: string;
  OnError: TabfCopyThreadErrorEvent; OnProgress: TabfCopyThreadProgressEvent;
  OnFinish: TNotifyEvent): Boolean;
var
  Thread: TabfCustomCopyThread;
begin
  Result := False;

// Check file existing
  if not FileExists(SrcFile) then
  begin
    if Assigned(OnError) then
      OnError(nil, ERROR_FILE_NOT_FOUND, Format(SabfNoFile, [SrcFile]));
    Exit;
  end;

// Copy using thread
  Thread := TabfLocalCopyThread.Create(SrcFile, DstFile, False, False, OnError,
    OnProgress, OnFinish, nil);
  try
    repeat
      if Application.Terminated then Exit;
      Application.ProcessMessages;
    until Thread.Terminated or Thread.Finished;
    Result := True;
  finally
    FreeAndNil(Thread);
  end;
end;

//------------------------------------------------------------------------------
// Moves files specified in the given AFileList string list. Formats of the
// FileList item are 'SrcFile=DstFile', 'SrcFile', 'SrcFile=*.dll',
// 'SrcFile=App.*'
// Date: 04/01/2000

function abfMoveFiles(const AFileList: TabfWideStrings): Boolean;
begin
  with TabfFileOperation.Create(nil) do
  try
    Operation := fotMove;
    FileList := AFileList;
    Options := Options +
      [fofSilent, fofNoConfirmation] -
      [fofRenameCollision];
    Result := Execute;
  finally
    Free;
  end;
end;

//------------------------------------------------------------------------------
// Moves files specified in the given AFileList string list to ADestDir
// directory.
// Date: 07/09/2007

function abfMoveFilesToDir(const AFileList: TabfWideStrings;
  const ADestDir: TabfWideString; const ACreateDir: Boolean = False): Boolean;
begin
  if ACreateDir then
    abfForceDirectoriesW(ADestDir);

  with TabfFileOperation.Create(nil) do
  try
    Operation := fotMove;
    FileList := AFileList;
    DestFolder := ADestDir;
    Options := Options +
      [fofSilent, fofNoConfirmation] -
      [fofRenameCollision];
    Result := Execute;

    if Result then
      SHChangeNotify(SHCNE_UPDATEDIR, SHCNF_PATH, Pointer(ADestDir), nil);
  finally
    Free;
  end;
end;


//------------------------------------------------------------------------------
// Removes specified directory even if it is not empty.
// Date: 07/09/2001

procedure abfRemoveDir(const DirName: TabfWideString);
var
  S: TabfWideString;
begin
  S := abfRemoveSlashW(DirName);
  if not abfDirectoryExistsW(S) then Exit;

// Delete directory using Shell's File Operation
  with TabfFileOperation.Create(nil) do
  try
    Operation := fotDelete;
    FileList.Text := S;
    Options := Options + [fofNoConfirmation, fofSilent] -
      [fofFilesOnly, fofAllowUndo];
    Execute;
  finally
    Free;
  end;
end;

//------------------------------------------------------------------------------
// Empties specified directory by deleting and recreating
// Date: 07/09/2001

procedure abfClearDir(const DirName: TabfWideString);
begin
  abfRemoveDir(DirName);
  abfDelay(100);
  abfCreateDirectoryW(abfRemoveSlashW(DirName));  // Variant 1
//  abfForceDirectoriesW(DirName);      // Variant 2
end;

//------------------------------------------------------------------------------
// Runs Create Shortcut wizard for creating shortcut in the specified directory.

function abfRunShortcutWizard(const DirName: string): Boolean;
var
  S: string;
begin
  Result := False;
  S := abfRemoveSlash(DirName);
  if not abfDirectoryExists(S) then Exit;
  Result := WinExec(PChar('rundll32 appwiz.cpl,NewLinkHere ' + S + '\'),
    SW_SHOWNORMAL) > 31;
end;

//------------------------------------------------------------------------------
// Creates shortcut/link with given data. HotKeyText format is a text
// interpretation of the HotKey "Ctrl+Shift+Z" for example. If HotKeyText = ''
// then HotKey value will be taken from HotKey parameter. ShowFlag can be
// SW_SHOWNORMAL, SW_SHOWMAXIMIZED or SW_SHOWMINNOACTIVE.

function abfCreateShellLink(const FileName, CommandLine, Arguments,
  WorkingDirectory, Description, IconLocation: string; IconIndex: Integer;
  HotKey: Word; const HotKeyText: string; ShowFlag: Integer): Boolean;
var
  WS: WideString;
  ShellObject: IUnknown;
  ShellLink: IShellLink;
  PersistFile: IPersistFile;
begin
  Result := False;

// Creating link file
  ShellObject := CreateComObject(CLSID_ShellLink);
  ShellLink := ShellObject as IShellLink;
  PersistFile := ShellObject as IPersistFile;

// Fill data fields
  with ShellLink do
  begin
    if SetPath(PChar(CommandLine)) <> NOERROR then Exit;
    if SetArguments(PChar(Arguments)) <> NOERROR then Exit;
    if SetWorkingDirectory(PChar(WorkingDirectory)) <> NOERROR then Exit;
    if SetShowCmd(ShowFlag) <> NOERROR then Exit;
    if HotKeyText<> '' then
       HotKey := TextToShortCut(HotKeyText);
    if SetHotKey(HotKey) <> NOERROR then Exit;
    if SetDescription(PChar(Description)) <> NOERROR then Exit;
    if SetIconLocation(PChar(IconLocation), IconIndex) <> NOERROR then Exit;
  end;

// Saving the link file
  WS := FileName;
  Result := (PersistFile.Save(PWChar(WS), False) = S_OK)
end;{function abfCreateShellLink}

//------------------------------------------------------------------------------
// Reads shortcut/link file data of file specified by FileName parameter.
// HotKeyText format is a text interpretation of the HotKey "Ctrl+Shift+Z" for
// example.

function abfReadShellLink(const FileName: string; var CommandLine, Arguments,
  WorkingDirectory, Description, IconLocation: string; var IconIndex: Integer;
  var HotKey: Word; var HotKeyText: string; var ShowFlag: Integer): Boolean;
var
  WS: WideString;
  FD: TWin32FindData;
  ShellObject: IUnknown;
  ShellLink: IShellLink;
  PersistFile: IPersistFile;
  Buf: array [0..MAX_PATH + 2] of Char;
begin
  Result := False;
  if not FileExists(FileName) then Exit;

// Creating link file
  ShellObject := CreateComObject(CLSID_ShellLink);
  ShellLink := ShellObject as IShellLink;
  PersistFile := ShellObject as IPersistFile;

// Read the link file
  WS := FileName;
  if (PersistFile.Load(PWChar(WS), STGM_READ) <> S_OK) then Exit;

// Fill data fields
  with ShellLink do
  begin
    if GetPath(@Buf, Length(Buf), FD, SLGP_UNCPRIORITY) <> NOERROR then Exit;
    CommandLine := Buf;

    if GetArguments(@Buf, Length(Buf)) <> NOERROR then Exit;
    Arguments := Buf;

    if GetWorkingDirectory(@Buf, Length(Buf)) <> NOERROR then Exit;
    WorkingDirectory := Buf;

    if GetShowCmd(ShowFlag) <> NOERROR then Exit;

    if GetHotKey(HotKey) <> NOERROR then Exit;
    HotKeyText := ShortCutToText(HotKey);

    if GetDescription(@Buf, Length(Buf)) <> NOERROR then Exit;
    Description := Buf;

    if GetIconLocation(@Buf, Length(Buf), IconIndex) <> NOERROR then Exit;
    IconLocation := Buf;
  end;

  Result := True;
end;{function abfReadShellLink}


//==============================================================================
// Components/Controls utilities
//==============================================================================

//------------------------------------------------------------------------------
// Components enumerator. Calls Proc for each child components of Container that
// match goven ChildClass. Enumerates all components if ChildClass = nil

procedure abfForEachComponent(const Container: TComponent;
  ChildClass: TComponentClass; Proc: TNotifyEvent);
var
  i: Integer;
  C: TComponent;
begin
  if not (Assigned(Container) and Assigned(Proc)) then Exit;

  if ChildClass <> nil then
    for i := 0 to Container.ComponentCount - 1 do
    begin
      C := Container.Components[i];
      if (C is ChildClass) then Proc(C);
    end
  else
    for i := 0 to Container.ComponentCount - 1 do
      Proc(Container.Components[i]);
end;

//------------------------------------------------------------------------------
// Controls enumerator. Calls Proc for each child controls of Container that
// match goven ChildClass. Enumerates all controls if ChildClass = nil

procedure abfForEachControl(const Container: TWinControl;
  ChildClass: TControlClass; Proc: TNotifyEvent);
var
  i: Integer;
  C: TControl;
begin
  if not (Assigned(Container) and Assigned(Proc)) then Exit;

  if ChildClass <> nil then
    for i := 0 to Container.ControlCount - 1 do
    begin
      C := Container.Controls[i];
      if (C is ChildClass) then Proc(C);
    end
  else
    for i := 0 to Container.ControlCount - 1 do
      Proc(Container.Controls[i]);
end;


//==============================================================================
// Application, forms
//==============================================================================

//------------------------------------------------------------------------------
// Restores application's button at the taskbar.
// Date: 09/01/2000

procedure abfApplicationTaskBarButtonShow;
begin
  with Application do
    if IsWindow(Handle) then ShowWindow(Handle, SW_SHOWNORMAL{SW_RESTORE});
end;

//------------------------------------------------------------------------------
// Removes application's button from the taskbar.
// Date: 09/01/2000

procedure abfApplicationTaskBarButtonHide;
begin
  with Application do
    if IsWindow(Handle) and IsWindowVisible(Handle) then
      ShowWindow(Handle, SW_HIDE);
end;

//------------------------------------------------------------------------------
// Shows main form of the application and restores application's button at the
// taskbar.
// Date: 09/01/2000

procedure abfMainFormShow;
begin
  abfApplicationTaskBarButtonShow;
  if Assigned(Application.MainForm) then Application.MainForm.Visible := True;
end;

//------------------------------------------------------------------------------
// Hides main form of the application and removes application's button from the
// taskbar.
// Date: 09/01/2000

procedure abfMainFormHide;
begin
  abfApplicationTaskBarButtonHide;
  if Assigned(Application.MainForm) then Application.MainForm.Visible := False;
end;

//------------------------------------------------------------------------------
// Returns coordinates of screen where the specified form is placed.
// Date: 01/16/2003

function abfGetWorkAreaRectByForm(const Form: TCustomForm): TRect;
var
  R: TRect;
{$IfDef D6}
  Mon: TMonitor;
{$Else}
  i: Integer;
  HM: HMonitor;
{$EndIf D6}
begin
  Result := abfGetWorkAreaRect;

  if not Assigned(Form) then Exit;

  with Form do
    R := Rect(Left, Top, Left + Width, Top + Height);

{$IfDef D6}
  Mon := Screen.MonitorFromRect(R, mdNearest);
  if Assigned(Mon) then Result := Mon.WorkareaRect;
{$Else D6}
  HM := MonitorFromRect(@R, MONITOR_DEFAULTTONEAREST);
  for i := 0 to Screen.MonitorCount - 1 do
    if Screen.Monitors[i].Handle = HM then
      with Screen.Monitors[i] do
        Result := Rect(Left, Top, Left + Width, Top + Height);
{$EndIf D6}
end;

//------------------------------------------------------------------------------
// Returns coordinates of screen where the specified point is placed.
// Date: 10/22/2003

function abfGetWorkAreaRectByPoint(X, Y: Integer): TRect;
var
{$IfDef D6}
  Mon: TMonitor;
{$Else}
  R: TRect;
  i: Integer;
  HM: HMonitor;
{$EndIf D6}
begin
  Result := abfGetWorkAreaRect;

{$IfDef D6}
  Mon := Screen.MonitorFromPoint(Point(X, Y), mdNearest);
  if Assigned(Mon) then Result := Mon.WorkareaRect;
{$Else D6}
  R := Rect(X, Y, X + 1, Y + 1);
  HM := MonitorFromRect(@R, MONITOR_DEFAULTTONEAREST);
  for i := 0 to Screen.MonitorCount - 1 do
    if Screen.Monitors[i].Handle = HM then
      with Screen.Monitors[i] do
        Result := Rect(Left, Top, Left + Width, Top + Height);
{$EndIf D6}
end;

//------------------------------------------------------------------------------
// Returns coordinates of screen where the specified window is placed.
// Date: 12/05/2007

function abfGetWorkAreaRectByWindow(const AWindowHandle: THandle): TRect;
var
  R: TRect;
{$IfDef D6}
  Mon: TMonitor;
{$Else}
  i: Integer;
  HM: HMonitor;
{$EndIf D6}
begin
  Result := abfGetWorkAreaRect;

  if not IsWindow(AWindowHandle) then Exit;

  GetWindowRect(AWindowHandle, R);

{$IfDef D6}
  Mon := Screen.MonitorFromRect(R, mdNearest);
  if Assigned(Mon) then Result := Mon.WorkareaRect;
{$Else D6}
  HM := MonitorFromRect(@R, MONITOR_DEFAULTTONEAREST);
  for i := 0 to Screen.MonitorCount - 1 do
    if Screen.Monitors[i].Handle = HM then
      with Screen.Monitors[i] do
        Result := Rect(Left, Top, Left + Width, Top + Height);
{$EndIf D6}
end;

//------------------------------------------------------------------------------
// Centres the SlaveForm form upon the MasterForm form and aligned to screen
// Date: 01/16/2003

procedure abfFormCenterForm(const SlaveForm, MasterForm: TCustomForm);
var
  X, Y: Integer;
  R: TRect;
begin
  if (not Assigned(MasterForm)) or (not MasterForm.Visible) then
  begin
    abfFormCenterScreen(SlaveForm);
    Exit;
  end;

  R := abfGetWorkAreaRectByForm(MasterForm);
  with SlaveForm do
  begin
    X := MasterForm.Left + ((MasterForm.Width  - Width ) div 2);
    Y := MasterForm.Top  + ((MasterForm.Height - Height) div 2);
    if X < R.Left then X := R.Left;
    if Y < R.Top  then Y := R.Top;
    if (X + Width ) > R.Right  then X := R.Right - Width;
    if (Y + Height) > R.Bottom then Y := R.Bottom - Height;
  // Make second check to make TopLeft angle always visible.
    if X < R.Left then X := R.Left;
    if Y < R.Top  then Y := R.Top;
    SetBounds(X, Y, Width, Height);
  end;
end;

//------------------------------------------------------------------------------
// Centres the AForm form upon the window and aligned to screen
// Date: 12/05/2007

procedure abfFormCenterWindow(const AForm: TCustomForm;
  const AWindowHandle: THandle);
var
  X, Y: Integer;
  MasterRect: TRect;
  MasterHeight, MasterWidth: Integer;
  AreaRect: TRect;
begin
  if not (IsWindow(AWindowHandle) and IsWindowVisible(AWindowHandle)) then
  begin
    abfFormCenterScreen(AForm);
    Exit;
  end;

  AreaRect := abfGetWorkAreaRectByWindow(AWindowHandle);

  GetWindowRect(AWindowHandle, MasterRect);
  MasterHeight := MasterRect.Bottom - MasterRect.Top;
  MasterWidth := MasterRect.Right - MasterRect.Left;

  with AForm do
  begin
    X := MasterRect.Left + ((MasterWidth  - Width ) div 2);
    Y := MasterRect.Top  + ((MasterHeight - Height) div 2);
    if X < AreaRect.Left then X := AreaRect.Left;
    if Y < AreaRect.Top  then Y := AreaRect.Top;
    if (X + Width ) > AreaRect.Right  then X := AreaRect.Right - Width;
    if (Y + Height) > AreaRect.Bottom then Y := AreaRect.Bottom - Height;
  // Make second check to make TopLeft angle always visible.
    if X < AreaRect.Left then X := AreaRect.Left;
    if Y < AreaRect.Top  then Y := AreaRect.Top;
    SetBounds(X, Y, Width, Height);
  end;
end;

//------------------------------------------------------------------------------
// Sets a position of the given form to be aligned the nearest monitor
// Date: 01/16/2003

procedure abfFormFitToScreen(const Form: TCustomForm);
var
  X, Y: Integer;
  R: TRect;
begin
  if not Assigned(Form) then Exit;

// Align form to the work area
  R := abfGetWorkAreaRectByForm(Form);
  with Form do
  begin
    X := Left;
    Y := Top;
    if X < R.Left then X := R.Left;
    if Y < R.Top  then Y := R.Top;
    if (X + Width ) > R.Right  then X := R.Right  - Width;
    if (Y + Height) > R.Bottom then Y := R.Bottom - Height;
  // Make second check to make TopLeft angle always visible.
    if X < R.Left then X := R.Left;
    if Y < R.Top  then Y := R.Top;
    SetBounds(X, Y, Width, Height);
  end;
end;

//------------------------------------------------------------------------------
// Sets a position of the given form to be centerd to the nearest monitor
// Date: 01/16/2003

procedure abfFormCenterScreen(const Form: TCustomForm);
var
  X, Y: Integer;
  R: TRect;
begin
  if not Assigned(Form) then Exit;

// Move form to the center of work area
  R := abfGetWorkAreaRectByForm(Form);
  with Form do
  begin
    X := R.Left + ((R.Right - R.Left) - Width) div 2;
    Y := R.Top + ((R.Bottom - R.Top) - Height) div 2;
    SetBounds(X, Y, Width, Height);
  end;
end;

//------------------------------------------------------------------------------
// Searches for the previous instance of the running application. Returns handle
// of the prev instance or the current one.
// Date: 05/15/2000

function abfFindPrevInstance: HWnd;
var
  S: string;
  ClassName, Title: PChar;
begin
  Result := 0;
  S := Application.ClassName;
  ClassName := PChar(S);
  Title     := PChar(Application.Title);
  repeat
    Result := FindWindowEx(0, Result, ClassName, Title);
  until Result <> Application.Handle;
end;

//------------------------------------------------------------------------------
// Activates the previous instance of the running application. Returns True if
// prev instance successfully activated.
// Date: 05/15/2000

function abfActivatePrevInstance: Boolean;
var
  Wnd: HWnd;
begin
  Result := False;
  Wnd := abfFindPrevInstance;
  if not IsWindow(Wnd) then Exit;
  if IsIconic(Wnd) then OpenIcon(Wnd)
  else SetForegroundWindow(Wnd);
  Result := True;
end;

//------------------------------------------------------------------------------
// Opens/runs specified file.
// Date: 05/17/2001
// Unicode version added 12/18/2007

procedure abfShellExecuteA(const AFileName: AnsiString);
begin
  ShellExecuteA(Application.Handle, nil, PAnsiChar(AFileName), nil, nil,
    SW_SHOW);
end;

//------------------------------------------------------------------------------

procedure abfShellExecute(const AFileName: string);
begin
  abfShellExecuteA(AFileName);
end;

//------------------------------------------------------------------------------

procedure abfShellExecuteW(const AFileName: WideString);
begin
  if not IsWinNT then
  begin
    abfShellExecuteA(AFileName);
    Exit;
  end;

  ShellExecuteW(Application.Handle, nil, PWideChar(AFileName), nil, nil,
    SW_SHOW);
end;

//------------------------------------------------------------------------------
// Loads icons from shell32.dll by index
// Date: 05/20/2002
// Modified: 12/18/2007

procedure abfGetShell32Icon(IconIndex: Integer; const LargeIcon,
  SmallIcon: TIcon);
var
  LH, SH: hIcon;
begin
  if IsWinNT then
  try
    ExtractIconExW(PWideChar(abfAddSlashW(abfGetSystemDirW) + 'shell32.dll'),
      IconIndex, LH, SH, 1);
  except
    LH := 0;
    SH := 0;
  end else
  try
    ExtractIconEx(PChar(abfAddSlash(abfGetSystemDir) + 'shell32.dll'),
      IconIndex, LH, SH, 1);
  except
    LH := 0;
    SH := 0;
  end;

  if Assigned(LargeIcon) then LargeIcon.Handle := LH;
  if Assigned(SmallIcon) then SmallIcon.Handle := SH;
end;


//==============================================================================
// Internet utilities
//==============================================================================

//------------------------------------------------------------------------------
// Opens specified link in the default browser. A new browser window will be
// created if the NewWindow parameter is True.
// Date: 12/19/2001

procedure abfGotoUrlEx(const URL: string; NewWindow: Boolean);
var
  P: Integer;
  S, DefaultBrowser, Params: string;
begin
  if Pos('://', URL) > 0 then S := URL else S := 'http://' + URL;

// Open in new window
  if NewWindow then
  begin
  // Get default browser command line.
    DefaultBrowser := abfRegReadStringDef(HKEY_CLASSES_ROOT,
      'http\shell\open\command', '', '');

  // Remove additional parameters and fill "%1" ones
  // Variants:
  // "C:\Program Files\Internet Explorer\iexplore.exe" -nohome
  // E:\PROGRA~1\MOZILLA.ORG\MOZILLA\MOZILLA.EXE -url "%1"
  // C:\PROGRA~1\Netscape\COMMUN~1\Program\Netscape.exe -h "%1"

  // Remove %1 or "%1" parameter
    P := Pos('%1', DefaultBrowser);
    if P > 0 then
    begin
      Delete(DefaultBrowser, P, 2);
      abfReplaceSubStrings(DefaultBrowser, '""', '');
    end;

  // Detect other parameters
    Params := ExtractFileExt(DefaultBrowser); // ".exe with all params"
    if Length(Params) > 4 then
    begin
    // Remove params from command line and remove "
      P := Pos(Copy(Params, 1, 4), DefaultBrowser);
      Delete(DefaultBrowser, P + 4, MaxInt);
      if DefaultBrowser[1] = '"' then
        Delete(DefaultBrowser, 1, 1);
    // Delete extension with 1 char after it from params
      Delete(Params, 1, 5);
      Params := Params + ' ' + S;
    end else
      Params := S;

  // Open new browser window if command line is valid
    if (DefaultBrowser <> '') and FileExists(DefaultBrowser) then
    begin
      ShellExecute(Application.Handle, 'open', PChar(DefaultBrowser),
        PChar(Params), nil, SW_SHOW);
      Exit;
    end;
  end;

// Try to open link in default window any way.
  ShellExecute(Application.Handle, 'open', PChar(S), nil, nil, SW_SHOW);
end;

//------------------------------------------------------------------------------
// Opens specified link in the default browser
// Date: 10/21/2000

procedure abfGotoUrl(const URL: string);
begin
  abfGotoUrlEx(URL, False);
end;

//------------------------------------------------------------------------------
// Creates e-mail message in the default mail browser
// Note: Under constraction!
// Date: 12/14/2001

procedure abfSendEmailEx(const Address, Subject, Body: string);
var
  S: string;
begin
  if abfPosIgnoreCase('mailto:', Address) > 0 then S := Address
  else S := 'mailto:' + Address;

  S := Address + '?subject=' + abfEscapeURL(Subject) + '&body=' +
    abfEscapeURL(Body);
  ShellExecute(Application.Handle, 'open', PChar(S), nil, nil, SW_SHOW);
end;

//------------------------------------------------------------------------------
// Creates e-mail message in the default mail browser
// Note: Under constraction!
// Date: 12/14/2001

procedure abfSendEmail(const Address, Subject: string);
var
  S: string;
begin
  if abfPosIgnoreCase('mailto:', Address) > 0 then S := Address
  else S := 'mailto:' + Address;

  if Subject <> '' then S := S + '?subject=' + abfEscapeURL(Subject);
  ShellExecute(Application.Handle, 'open', PChar(S), nil, nil, SW_SHOW);
end;

//------------------------------------------------------------------------------
// Opens specified link in the default browser or creates e-mail to specified address
// Date: 06/16/2006

procedure abfSmartOpenUrl(const URL: string);
begin
  if Pos('@', URL) > 0 then
    abfSendEmail(URL, '')
  else
    abfGotoUrlEx(URL, False);
end;

//------------------------------------------------------------------------------

procedure abfSmartOpenUrl(const URL, Subject: string);
begin
  if Pos('@', URL) > 0 then
    abfSendEmail(URL, Subject)
  else
    abfGotoUrlEx(URL, False);
end;

//------------------------------------------------------------------------------

procedure abfSmartOpenUrl(const URL: string; NewWindow: Boolean); 
begin
  if Pos('@', URL) > 0 then
    abfSendEmail(URL, '')
  else
    abfGotoUrlEx(URL, NewWindow);
end;

//------------------------------------------------------------------------------

procedure abfSmartOpenUrl(const URL, Subject: string; NewWindow: Boolean); overload;
begin
  if Pos('@', URL) > 0 then
    abfSendEmail(URL, Subject)
  else
    abfGotoUrlEx(URL, NewWindow);
end;

//==============================================================================
// Routines for initialization/finalization
//==============================================================================

procedure _DoInitialization;
begin
end;

//------------------------------------------------------------------------------

procedure _DoFinalization;
begin
end;

{******************************************************************************}
initialization
{******************************************************************************}
  _DoInitialization;

{******************************************************************************}
finalization
{******************************************************************************}
  _DoFinalization;

end{unit abfVclUtils}.
