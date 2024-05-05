{*******************************************************************************

  abfDialogs Demo. Second form unit.

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfDialogsDemoSecond;

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
// TfrmDialogsDemoSecond
//==============================================================================
{ TfrmDialogsDemoSecond }

  TfrmDialogsDemoSecond = class(TForm)
  // TabfRunDlg
    abfRunDlg: TabfRunDlg;
    grpRun: TGroupBox;
      imgRun: TImage;
      lbRunInfo: TLabel;
      lbRunCaption: TLabel;
        edRunCaption: TEdit;
      lbRunText: TLabel;
        memRunText: TMemo;
      pnRunIcon: TPanel;
        imgRunIcon: TImage;
        lbRunIcon: TLabel;
        btnRunIconLoad: TSpeedButton;
        btnRunIconClear: TSpeedButton;
      btnRun: TButton;
  // TabfPickIconDlg
    abfPickIconDlg: TabfPickIconDlg;
    grpPI: TGroupBox;
      imgPI: TImage;
      lbPIInfo: TLabel;
      lbPIFileName: TLabel;
        edPIFileName: TEdit;
        btnPIFileName: TSpeedButton;
      grpPIIconLarge: TGroupBox;
        imgPIIconLarge: TImage;
        lbPIIconLarge: TLabel;
      grpPIIconSmall: TGroupBox;
        imgPIIconSmall: TImage;
        lbPIIconSmall: TLabel;
      btnPI: TButton;
  // TabfOpenWithDlg
    abfOpenWithDlg: TabfOpenWithDlg;
    grpOW: TGroupBox;
      imgOW: TImage;
      lbOWInfo: TLabel;
      lbOWFileName: TLabel;
        edOWFileName: TEdit;
        btnOWFileName: TSpeedButton;
      btnOW: TButton;
  // TabfObjectPropertiesDlg
    abfObjectPropertiesDlg: TabfObjectPropertiesDlg;
    grpOP: TGroupBox;
      imgOP: TImage;
      lbOPInfo: TLabel;
      lbOPFileName: TLabel;
        edOPFileName: TEdit;
        btnOPFileName: TSpeedButton;
      btnOP: TButton;
  // TabfFindDlg
    abfFindDlg: TabfFindDlg;
    grpFind: TGroupBox;
      imgFind: TImage;
      lbFindInfo: TLabel;
      lbFindDirectory: TLabel;
        edFindDirectory: TEdit;
        btnFindDirectory: TSpeedButton;
      lbFindKind: TLabel;
        cmbFindKind: TComboBox;
      btnFind: TButton;
  // TabfCreateShortcutDlg
    abfCreateShortcutDlg: TabfCreateShortcutDlg;
    grpCS: TGroupBox;
      imgCS: TImage;
      lbCSInfo: TLabel;
      lbCSDirectory: TLabel;
        edCSDirectory: TEdit;
        btnCSDirectory: TSpeedButton;
      btnCSExecute: TButton;
  // Non-visual
    dlgOpenFile: TOpenDialog;
    dlgSelectDir: TabfBrowseFolderDlg;
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
    procedure btnRunIconLoadClick(Sender: TObject);
    procedure btnRunIconClearClick(Sender: TObject);
    procedure btnFileNameClick(Sender: TObject);
    procedure btnDirectoryClick(Sender: TObject);
  private
    procedure ApplyRun;
    procedure UpdateRun;
    procedure ApplyPickIcon;
    procedure UpdatePickIcon;
  public
  {$IfDef D3}
    lbxBFOptions: TCheckListBox;
  {$EndIf D3}
  end;{TfrmDialogsDemoSecond = class(TForm)}

var
  frmDialogsDemoSecond: TfrmDialogsDemoSecond;

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
  if not Assigned(frmDialogsDemoSecond) then
    Application.CreateForm(TfrmDialogsDemoSecond, frmDialogsDemoSecond);
  frmDialogsDemoSecond.Show;
  frmDialogsDemoSecond.BringToFront;
end;


//==============================================================================
// TfrmMain
//==============================================================================
{ TfrmMain }

type
  THackControl = class(TControl);
{$IfNDef D3}
  TOpenPictureDialog = TOpenDialog;
{$EndIf D3}

//------------------------------------------------------------------------------

procedure TfrmDialogsDemoSecond.ApplyRun;
begin
  if Assigned(imgRunIcon.Picture.Icon) then
    abfRunDlg.Icon := imgRunIcon.Picture.Icon
  else
    abfRunDlg.Icon := nil;
  abfRunDlg.Caption  := edRunCaption.Text;
  abfRunDlg.Text     := memRunText.Text;
end;

//------------------------------------------------------------------------------

procedure TfrmDialogsDemoSecond.UpdateRun;
begin
  lbRunIcon.Visible  := imgRunIcon.Picture.Icon.Empty;
end;

//------------------------------------------------------------------------------

procedure TfrmDialogsDemoSecond.ApplyPickIcon;
begin
  if Assigned(imgPIIconLarge.Picture.Icon) then
    abfPickIconDlg.LargeIcon := imgPIIconLarge.Picture.Icon
  else
    abfPickIconDlg.LargeIcon := nil;
  if Assigned(imgPIIconSmall.Picture.Icon) then
    abfPickIconDlg.SmallIcon := imgPIIconSmall.Picture.Icon
  else
    abfPickIconDlg.SmallIcon := nil;
  abfPickIconDlg.FileName := edPIFileName.Text;
end;

//------------------------------------------------------------------------------

procedure TfrmDialogsDemoSecond.UpdatePickIcon;
begin
  imgPIIconLarge.Picture.Assign(abfPickIconDlg.LargeIcon);
  imgPIIconSmall.Picture.Assign(abfPickIconDlg.SmallIcon);
  edPIFileName.Text := abfPickIconDlg.FileName;
  lbPIIconLarge.Visible  := imgPIIconLarge.Picture.Icon.Empty;
  lbPIIconSmall.Visible  := imgPIIconSmall.Picture.Icon.Empty;
end;


//==============================================================================
// Events

procedure TfrmDialogsDemoSecond.FormCreate(Sender: TObject);
begin
{$IfDef D3}
  btnRunIconLoad .Flat := cFlatButtons;
  btnRunIconClear.Flat := cFlatButtons;
  btnOWFileName.Flat := cFlatButtons;
{$EndIf D3}
  UpdateRun;
  abfGetTypeInfoValueNames(
    abfGetTypeInfo(
      abfGetPropInfo(abfFindDlg.ClassInfo, 'Kind')),
    cmbFindKind.Items);
  cmbFindKind.ItemIndex := Integer(abfFindDlg.Kind);
// Fix form width and height
  Width  := cWidth;
  Height := cHeight;
{$IfDef D4}
  OnCanResize := FormCanResize;
{$EndIf D4}
end;

//------------------------------------------------------------------------------

procedure TfrmDialogsDemoSecond.FormResize(Sender: TObject);
begin
{$IfNDef D4}
  if Width  < cWidth  then Width  := cWidth;
  if Height < cHeight then Height := cHeight;
{$EndIf D4}
end;

//------------------------------------------------------------------------------

procedure TfrmDialogsDemoSecond.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  if NewWidth  < cWidth  then NewWidth  := cWidth;
  if NewHeight < cHeight then NewHeight := cHeight;
end;

//------------------------------------------------------------------------------

procedure TfrmDialogsDemoSecond.ComponentIconClick(Sender: TObject);
begin
  case (Sender as TComponent).Tag of
    1: abfComponentAboutEx(abfRunDlg, '', '', True);
    2: abfComponentAboutEx(abfPickIconDlg, '', '', True);
    3: abfComponentAboutEx(abfOpenWithDlg, '', '', True);
    4: abfComponentAboutEx(abfObjectPropertiesDlg, '', '', True);
    5: abfComponentAboutEx(abfFindDlg, '', '', True);
    6: abfComponentAboutEx(abfCreateShortcutDlg, '', '', True);
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmDialogsDemoSecond.AboutClick(Sender: TObject);
begin
  abfApplicationAboutEx(Application.Title, ''{SabfVCLVersion}, '', '',
    True, nil);
end;

//------------------------------------------------------------------------------

procedure TfrmDialogsDemoSecond.CloseClick(Sender: TObject);
begin
  Close;
end;

//------------------------------------------------------------------------------

procedure TfrmDialogsDemoSecond.ExecuteClick(Sender: TObject);
begin
  with Sender as TControl do
    case Tag of
      1: begin
        ApplyRun;
        abfRunDlg.Execute;
      end;
      2: begin
        ApplyPickIcon;
        abfPickIconDlg.Execute;
        UpdatePickIcon;
      end;
      3: begin
        abfOpenWithDlg.FileName := edOWFileName.Text;
        abfOpenWithDlg.Execute;
      end;
      4: begin
        abfObjectPropertiesDlg.FileName := edOPFileName.Text;
        abfObjectPropertiesDlg.Execute;
      end;
      5: begin
        abfFindDlg.Kind := TabfFindDlgKind(cmbFindKind.ItemIndex);
        abfFindDlg.Directory := edFindDirectory.Text;
        abfFindDlg.Execute;
      end;
      6: begin
        abfCreateShortcutDlg.Directory := edCSDirectory.Text;
        abfCreateShortcutDlg.Execute;
      end;
    end;
end;

//------------------------------------------------------------------------------

procedure TfrmDialogsDemoSecond.btnRunIconLoadClick(Sender: TObject);
var
  Dialog: TOpenPictureDialog;
begin
  Dialog := TOpenPictureDialog.Create(nil);
  try
    Dialog.Filter := GraphicFilter(TIcon);
    if Dialog.Execute then
      imgRunIcon.Picture.LoadFromFile(Dialog.FileName);
    UpdateRun;
  finally
    Dialog.Free;
  end
end;

//------------------------------------------------------------------------------

procedure TfrmDialogsDemoSecond.btnRunIconClearClick(Sender: TObject);
begin
  imgRunIcon.Picture := nil;
  UpdateRun;
end;

//------------------------------------------------------------------------------

procedure TfrmDialogsDemoSecond.btnFileNameClick(Sender: TObject);
begin
  with TControl(Sender) do
    case Tag of
      2: dlgOpenFile.FileName := edPIFileName.Text;
      3: dlgOpenFile.FileName := edOWFileName.Text;
      4: dlgOpenFile.FileName := edOPFileName.Text;
    end;
  if not dlgOpenFile.Execute then Exit;
  with TControl(Sender) do
    case Tag of
      2: edPIFileName.Text := dlgOpenFile.FileName;
      3: edOWFileName.Text := dlgOpenFile.FileName;
      4: edOPFileName.Text := dlgOpenFile.FileName;
    end;
end;

//------------------------------------------------------------------------------

procedure TfrmDialogsDemoSecond.btnDirectoryClick(Sender: TObject);
begin
  with TControl(Sender) do
    case Tag of
      4: dlgSelectDir.Folder := edFindDirectory.Text;
      6: dlgSelectDir.Folder := edCSDirectory.Text;
    end;
  if not dlgSelectDir.Execute then Exit;
  with TControl(Sender) do
    case Tag of
      4: edFindDirectory.Text := dlgSelectDir.Folder;
      6: edCSDirectory.Text := dlgSelectDir.Folder;
    end;
end;

//------------------------------------------------------------------------------

end{unit abfDialogsDemoMain}.

