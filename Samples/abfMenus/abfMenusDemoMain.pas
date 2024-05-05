{*******************************************************************************

  abfMenus Demo. Main form unit.

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfMenusDemoMain;

{$I abf.inc}

interface

uses
{$IfDef D4}
  ImgList,
{$EndIf D4}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls,
  StdCtrls, Buttons, Menus, Dialogs, abfComponents, abfMenus;

const
  cFlatButtons = False; // Determines are speed buttons flat or not
  cWidth  = 640;
  cHeight = 400;

type

//==============================================================================
// TfrmMain
//==============================================================================

  TfrmMenusDemoMain = class(TForm)
  { TabfSystemMenuItem }
    abfSystemMenuItem1: TabfSystemMenuItem;
    abfSystemMenuItem2: TabfSystemMenuItem;
    grpItem: TGroupBox;
      imgItem: TImage;
      lbItemInfo: TLabel;
      grpItemTarget: TRadioGroup;
        cmbInserterTarget: TComboBox;
      chbItemInserted: TCheckBox;
  { TabfSystemMenuInserter }
    abfSystemMenuInserter: TabfSystemMenuInserter;
    grpInserter: TGroupBox;
      imgInserter: TImage;
      lbInserterInfo: TLabel;
      grpInserterTarget: TRadioGroup;
        cmbItemTarget: TComboBox;
      chbInserterInserted: TCheckBox;
  { Non-visual }
    pmMain: TPopupMenu;
      miAboutApp: TMenuItem;
      mi1: TMenuItem;
      miExit: TMenuItem;
    pmItem: TPopupMenu;
      miIt_NewItem: TMenuItem;
        miIt_1: TMenuItem;
    pmInserter: TPopupMenu;
        miIn_1: TMenuItem;
      miIn_About: TMenuItem;
      miIn_Web: TMenuItem;
        miIn_2: TMenuItem;
      miIn_Sub: TMenuItem;
        miIn_Item1: TMenuItem;
        miIn_Item2: TMenuItem;
        miIn_Item3: TMenuItem;
  { Events }
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure ComponentIconClick(Sender: TObject);
    procedure AboutClick(Sender: TObject);
    procedure CloseClick(Sender: TObject);
    procedure WebClick(Sender: TObject);
    procedure chbInsertedClick(Sender: TObject);
    procedure MenuItemClick(Sender: TObject);
    procedure grpTargetClick(Sender: TObject);
    procedure cmbTargetEnter(Sender: TObject);
    procedure cmbTargetClick(Sender: TObject);
  private
  end;

var
  frmMenusDemoMain: TfrmMenusDemoMain;

//==============================================================================
// Entry point
//==============================================================================
procedure ShowDemo;

{******************************************************************************}
implementation
{******************************************************************************}
{$R *.DFM}
uses
  abfConsts, abfVclUtils, abfAboutApplication, abfAboutComponent;

//==============================================================================
// Entry point
//==============================================================================

procedure ShowDemo;
begin
  if not Assigned(frmMenusDemoMain) then
    Application.CreateForm(TfrmMenusDemoMain, frmMenusDemoMain);
  frmMenusDemoMain.Show;
  frmMenusDemoMain.BringToFront;
end;


//==============================================================================
// TfrmMain
//==============================================================================
{ TfrmMain }

procedure TfrmMenusDemoMain.FormCreate(Sender: TObject);
begin
  cmbTargetEnter(cmbItemTarget);
  cmbTargetEnter(cmbInserterTarget);
  Width  := cWidth;
  Height := cHeight;
{$IfDef D4}
  OnCanResize := FormCanResize;
{$EndIf D4}
end;

//------------------------------------------------------------------------------

procedure TfrmMenusDemoMain.FormResize(Sender: TObject);
begin
{$IfNDef D4}
  if Width  < cWidth  then Width  := cWidth;
  if Height < cHeight then Height := cHeight;
{$EndIf D4}
end;

//------------------------------------------------------------------------------

procedure TfrmMenusDemoMain.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  if NewWidth  < cWidth  then NewWidth  := cWidth;
  if NewHeight < cHeight then NewHeight := cHeight;
end;

//------------------------------------------------------------------------------

procedure TfrmMenusDemoMain.ComponentIconClick(Sender: TObject);
begin
  case (Sender as TComponent).Tag of
    1: abfComponentAboutEx(abfSystemMenuItem1, '', '', True);
    2: abfComponentAboutEx(abfSystemMenuInserter, '', '', True);
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmMenusDemoMain.AboutClick(Sender: TObject);
begin
  abfApplicationAboutEx(Application.Title, ''{SabfVCLVersion}, '', '',
    True, nil);
end;

//------------------------------------------------------------------------------

procedure TfrmMenusDemoMain.CloseClick(Sender: TObject);
begin
  Close;
end;

//------------------------------------------------------------------------------

procedure TfrmMenusDemoMain.WebClick(Sender: TObject);
begin
  abfGotoUrl(SabfWeb);
end;

//------------------------------------------------------------------------------

procedure TfrmMenusDemoMain.grpTargetClick(Sender: TObject);

  //-------------------------------------

  procedure _SetTarget(const MenuHook: TabfCustomSystemMenuHook);
  begin
    MenuHook.Handle := 0;
    with TRadioGroup(Sender) do
      case ItemIndex of
        0: MenuHook.InsertToAppMenu := False;
        1: MenuHook.InsertToAppMenu := True;
        2:
          if Tag = 1 then
            MenuHook.Handle := THandle(cmbItemTarget.Items.Objects[ItemIndex]);
          else
            MenuHook.Handle := THandle(cmbInserterTarget.Items.Objects[ItemIndex]);
      end;
  end;{Internal procedure _SetTarget}

  //-------------------------------------

begin
  with TRadioGroup(Sender) do
    if Tag = 1 then
    begin
      _SetTarget(abfSystemMenuItem1);
      _SetTarget(abfSystemMenuItem2);
    end else
      _SetTarget(abfSystemMenuInserter);
end;

//------------------------------------------------------------------------------

procedure TfrmMenusDemoMain.chbInsertedClick(Sender: TObject);
begin
  with TCheckBox(Sender) do
    if Tag = 1 then
    begin
      abfSystemMenuItem1.Inserted := Checked;
      abfSystemMenuItem2.Inserted := Checked;
    end else
      abfSystemMenuInserter.Inserted := Checked;
end;

//------------------------------------------------------------------------------

procedure TfrmMenusDemoMain.MenuItemClick(Sender: TObject);
const
  SMsg = '%s clicked';
begin
  Beep;
  ShowMessage(Format(SMsg, [TMenuItem(Sender).Caption]));
end;

//------------------------------------------------------------------------------

function _EnumWindowsProc(hWnd: THandle; lParam: LongInt): BOOL; stdcall;
var
  TextLength: Integer;
  Text: string;
begin
  Result := True;
  TextLength := GetWindowTextLength(hWnd);
  if TextLength < 1 then Exit;
  SetLength(Text, TextLength + 1);
  GetWindowText(hWnd, PChar(Text), TextLength + 1);
  TComboBox(lParam).Items.AddObject(Text, TObject(hWnd));
end;

procedure TfrmMenusDemoMain.cmbTargetEnter(Sender: TObject);
begin
  TComboBox(Sender).Items.Clear;
  EnumWindows(@_EnumWindowsProc, LongInt(Sender));
end;

//------------------------------------------------------------------------------

procedure TfrmMenusDemoMain.cmbTargetClick(Sender: TObject);
begin
  with TComboBox(Sender) do
    if Tag = 1 then
    begin
      if grpItemTarget.ItemIndex <> 2 then Exit;
      abfSystemMenuItem1.Handle := THandle(Items.Objects[ItemIndex]);
      abfSystemMenuItem2.Handle := THandle(Items.Objects[ItemIndex]);
    end else
    begin
      if grpInserterTarget.ItemIndex <> 2 then Exit;
      abfSystemMenuInserter.Handle := THandle(Items.Objects[ItemIndex]);
    end;
end;

//------------------------------------------------------------------------------

end{unit abfMenusDemoMain}.
