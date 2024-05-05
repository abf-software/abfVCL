{*******************************************************************************

  abfDialogs Demo. Main form unit.

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfDialogsDemoMain;

{$I abf.inc}

interface

uses
{$IfDef D4}
  ImgList,
{$EndIf D4}
{$IfDef D3}
  ExtDlgs, TypInfo, CheckLst,
{$EndIf D3}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, Menus, ComCtrls, abfComponents, abfDialogs;

const
  cFlatButtons = False; // Determines are speed buttons flat or not
  cWidth  = 640;
  cHeight = 400;

type

//==============================================================================
// TfrmDialogsDemoMain
//==============================================================================
{ TfrmDialogsDemoMain }

  TfrmDialogsDemoMain = class(TForm)
  // TabfWinAboutDlg
    abfWinAboutDlg: TabfWinAboutDlg;
    grpWinAbout: TGroupBox;
      imgWinAbout: TImage;
      lbWinAboutInfo: TLabel;
      lbWinAboutAppTitle: TLabel;
        edWinAboutAppTitle: TEdit;
      lbWinAboutCaption: TLabel;
        edWinAboutCaption: TEdit;
      lbWinAboutText: TLabel;
        memWinAboutText: TMemo;
      pnWinAboutIcon: TPanel;
        imgWinAboutIcon: TImage;
        lbWinAboutIcon: TLabel;
        btnWinAboutIconLoad: TSpeedButton;
        btnWinAboutIconClear: TSpeedButton;
      btnWinAboutExecute: TButton;
  // TabfSelectDirDlg
    abfSelectDirDlg: TabfSelectDirDlg;
    grpSelectDir: TGroupBox;
      imgSelectDir: TImage;
      lbSelectDirInfo: TLabel;
    btnSelectDirExecute: TButton;
  // abfBrowseFolderDlg
    abfBrowseFolderDlg: TabfBrowseFolderDlg;
    grpBF: TGroupBox;
      imgBF: TImage;
      lbBFInfo: TLabel;
      lbBFOptions: TLabel;
        memBFOptions: TMemo;
      lbBFRootDir: TLabel;
        cmbBFRootDir: TComboBox;
      lbBFText: TLabel;
        edBFText: TEdit;
      lbBFStatusText: TLabel;
        edBFStatusText: TEdit;
      chbBFPath: TCheckBox;
      btnBFExecute: TButton;
  // TabfCplDlg
    abfCplDlg: TabfCplDlg;
    dlgCplLoad: TOpenDialog;
    grpCpl: TGroupBox;
      imgCplDlg: TImage;
      lbCplInfo: TLabel;
      lbCplCplApplet: TLabel;
        edCplCplApplet: TEdit;
        udCplCplApplet: TUpDown;
      lbCplCplName: TLabel;
        edCplCplName: TEdit;
      btnCplLoad: TSpeedButton;
      lbCplCplType: TLabel;
        cmbCplCplType: TComboBox;
      lbCplPageNumber: TLabel;
        edCplPageNumber: TEdit;
        udCplPageNumber: TUpDown;
      btnCplDlgExecute: TButton;
  // Non-visual }
    pmMain: TPopupMenu;
      miAboutApp: TMenuItem;
        mi1: TMenuItem;
      miExit: TMenuItem;
  // Events
    procedure FormCreate(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure FormResize(Sender: TObject);
    procedure ComponentIconClick(Sender: TObject);
    procedure AboutClick(Sender: TObject);
    procedure CloseClick(Sender: TObject);
    procedure ExecuteClick(Sender: TObject);
  // TabfWinAboutDlg
    procedure btnWinAboutIconLoadClick(Sender: TObject);
    procedure btnWinAboutIconClearClick(Sender: TObject);
  // TabfCplDlg
    procedure btnCplLoadClick(Sender: TObject);
  private
    procedure ApplyWinAbout;
    procedure UpdateWinAbout;
    procedure ApplyBrowseFolder;
    procedure UpdateBrowseFolder;
    procedure ApplyCpl;
    procedure UpdateCpl;
  public
  {$IfDef D3}
    lbxBFOptions: TCheckListBox;
  {$EndIf D3}
  end;{TfrmDialogsDemoMain = class(TForm)}

var
  frmDialogsDemoMain: TfrmDialogsDemoMain;

//==============================================================================
// Entry point
//==============================================================================
procedure ShowDemo;

{******************************************************************************}
implementation
{******************************************************************************}
{$R *.DFM}
uses
  abfTypInfo, abfAboutApplication, abfAboutComponent;

//==============================================================================
// Entry point
//==============================================================================

procedure ShowDemo;
begin
  if not Assigned(frmDialogsDemoMain) then
    Application.CreateForm(TfrmDialogsDemoMain, frmDialogsDemoMain);
  frmDialogsDemoMain.Show;
  frmDialogsDemoMain.BringToFront;
end;


//==============================================================================
// TfrmDialogsDemoMain
//==============================================================================
{ TfrmDialogsDemoMain }

type
  THackControl = class(TControl);
{$IfNDef D3}
  TOpenPictureDialog = TOpenDialog;
{$EndIf D3}

//------------------------------------------------------------------------------

procedure TfrmDialogsDemoMain.ApplyWinAbout;
begin
  if Assigned(imgWinAboutIcon.Picture.Icon) then
    abfWinAboutDlg.Icon := imgWinAboutIcon.Picture.Icon
  else
    abfWinAboutDlg.Icon := nil;
  abfWinAboutDlg.AppTitle := edWinAboutAppTitle.Text;
  abfWinAboutDlg.Caption  := edWinAboutCaption.Text;
  abfWinAboutDlg.Text     := memWinAboutText.Text;
end;

//------------------------------------------------------------------------------

procedure TfrmDialogsDemoMain.UpdateWinAbout;
begin
  lbWinAboutIcon.Visible  := imgWinAboutIcon.Picture.Icon.Empty;
end;

//------------------------------------------------------------------------------

procedure TfrmDialogsDemoMain.ApplyBrowseFolder;
{$IfDef D3}
var
  i: Integer;
  Options: TabfBrowseFolderDlgOptions;
{$EndIf D3}
begin
{$IfDef D3}
// Options
  Options := [];
  for i := 0 to lbxBFOptions.Items.Count - 1 do
    if lbxBFOptions.Checked[i] then
      Include(Options, TabfBrowseFolderDlgOption(i));
  abfBrowseFolderDlg.Options := Options;
{$EndIf D3}
// RootDir
  abfBrowseFolderDlg.RootDir :=
    TabfBrowseFolderDlgRootDir(cmbBFRootDir.ItemIndex);
// Text
  abfBrowseFolderDlg.Text := edBFText.Text;
// StatusText
  abfBrowseFolderDlg.StatusText := edBFStatusText.Text;
// StatusTextAsPath
  abfBrowseFolderDlg.StatusTextAsPath := chbBFPath.Checked;
end;

//------------------------------------------------------------------------------

procedure TfrmDialogsDemoMain.UpdateBrowseFolder;
{$IfDef D3}
var
  i: Integer;
{$EndIf D3}
begin
{$IfDef D3}
// Options
  abfGetTypeInfoValueNames(
    abfGetTypeInfo(
      GetPropInfo(abfBrowseFolderDlg.ClassInfo, 'Options')),
    lbxBFOptions.Items);
  for i := 0 to lbxBFOptions.Items.Count - 1 do
    lbxBFOptions.Checked[i] :=
      TabfBrowseFolderDlgOption(i) in abfBrowseFolderDlg.Options;
{$EndIf D3}
// RootDir
  abfGetTypeInfoValueNames(
    abfGetTypeInfo(
      abfGetPropInfo(abfBrowseFolderDlg.ClassInfo, 'RootDir')),
    cmbBFRootDir.Items);
  cmbBFRootDir.ItemIndex := Integer(abfBrowseFolderDlg.RootDir);
// StatusTextAsPath
  chbBFPath.Checked := abfBrowseFolderDlg.StatusTextAsPath;
end;

//------------------------------------------------------------------------------

procedure TfrmDialogsDemoMain.ApplyCpl;
begin
  abfCplDlg.CplApplet  := udCplCplApplet.Position;
  abfCplDlg.CplName    := edCplCplName.Text;
  abfCplDlg.CplType    := TabfCplType(cmbCplCplType.ItemIndex);
  abfCplDlg.PageNumber := udCplPageNumber.Position;
end;

//------------------------------------------------------------------------------

procedure TfrmDialogsDemoMain.UpdateCpl;
var
  SysDir: string;
begin
  abfGetTypeInfoValueNames(
    abfGetTypeInfo(
      abfGetPropInfo(abfCplDlg.ClassInfo, 'CplType')),
    cmbCplCplType.Items);
  cmbCplCplType.ItemIndex := Integer(abfCplDlg.CplType);
// InitDir for dialog
  SetLength(SysDir, MAX_PATH + 1);
  GetSystemDirectory(PChar(SysDir), MAX_PATH);
  dlgCplLoad.InitialDir := SysDir;
end;


//==============================================================================
// Events

procedure TfrmDialogsDemoMain.FormCreate(Sender: TObject);

{$IfDef D3}
  procedure _BrowseFolderOn;
  begin
    memBFOptions.Visible := False;
    lbxBFOptions := TCheckListBox.Create(Self);
    lbxBFOptions.Parent := grpBF;
    lbxBFOptions.Font := memBFOptions.Font;
    with memBFOptions do
      lbxBFOptions.SetBounds(Left, Top, Width, Height);
    lbxBFOptions.TabOrder := memBFOptions.TabOrder;
  end;{Internal procedure _BrowseFolderOn}
{$EndIf D3}

begin
{$IfDef D3}
  btnWinAboutIconLoad .Flat := cFlatButtons;
  btnWinAboutIconClear.Flat := cFlatButtons;
  btnCplLoad.Flat := cFlatButtons;
  _BrowseFolderOn;
{$EndIf D3}
  UpdateBrowseFolder;
  UpdateWinAbout;
  UpdateCpl;
// Fix form width and height
  Width  := cWidth;
  Height := cHeight;
{$IfDef D4}
  OnCanResize := FormCanResize;
{$EndIf D4}
end;

//------------------------------------------------------------------------------

procedure TfrmDialogsDemoMain.FormResize(Sender: TObject);
begin
{$IfNDef D4}
  if Width  < cWidth  then Width  := cWidth;
  if Height < cHeight then Height := cHeight;
{$EndIf D4}
end;

//------------------------------------------------------------------------------

procedure TfrmDialogsDemoMain.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  if NewWidth  < cWidth  then NewWidth  := cWidth;
  if NewHeight < cHeight then NewHeight := cHeight;
end;

//------------------------------------------------------------------------------

procedure TfrmDialogsDemoMain.ComponentIconClick(Sender: TObject);
begin
  case (Sender as TComponent).Tag of
    1: abfComponentAboutEx(abfWinAboutDlg, '', '', True);
    2: abfComponentAboutEx(abfSelectDirDlg, '', '', True);
    3: abfComponentAboutEx(abfBrowseFolderDlg, '', '', True);
    4: abfComponentAboutEx(abfCplDlg, '', '', True);
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmDialogsDemoMain.AboutClick(Sender: TObject);
begin
  abfApplicationAboutEx(Application.Title, ''{SabfVCLVersion}, '', '',
    True, nil);
end;

//------------------------------------------------------------------------------

procedure TfrmDialogsDemoMain.CloseClick(Sender: TObject);
begin
  Close;
end;

//------------------------------------------------------------------------------

procedure TfrmDialogsDemoMain.ExecuteClick(Sender: TObject);
begin
  with Sender as TControl do
    case Tag of
      1: begin
        ApplyWinAbout;
        abfWinAboutDlg.Execute;
      end;
      2: abfSelectDirDlg.Execute;
      3: begin
        ApplyBrowseFolder;
        abfBrowseFolderDlg.Execute;
      end;
      4: begin
        ApplyCpl;
        abfCplDlg.Execute;
      end;
    end;
end;

//==============================================================================
// TabfWinAboutDlg

procedure TfrmDialogsDemoMain.btnWinAboutIconLoadClick(Sender: TObject);
var
  Dialog: TOpenPictureDialog;
begin
  Dialog := TOpenPictureDialog.Create(nil);
  try
    Dialog.Filter := GraphicFilter(TIcon);
    if Dialog.Execute then
      imgWinAboutIcon.Picture.LoadFromFile(Dialog.FileName);
    UpdateWinAbout;
  finally
    Dialog.Free;
  end
end;

//------------------------------------------------------------------------------

procedure TfrmDialogsDemoMain.btnWinAboutIconClearClick(Sender: TObject);
begin
  imgWinAboutIcon.Picture := nil;
  UpdateWinAbout;
end;

//==============================================================================
// TabfCplDlg

procedure TfrmDialogsDemoMain.btnCplLoadClick(Sender: TObject);
begin
  if dlgCplLoad.Execute then edCplCplName.Text := dlgCplLoad.FileName;
end;

//------------------------------------------------------------------------------

end{unit abfDialogsDemoMain}.

