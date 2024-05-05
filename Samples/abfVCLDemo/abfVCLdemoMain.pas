{*******************************************************************************

  ABF VCL Demo. Main form unit.

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfVCLDemoMain;

{$I abf.inc}

{$R abfComponentsReg.res}
{$R abfControlsReg.res}
{$R abfAPMReg.res}
{$R abfComboBoxesReg.res}
{$R abfComControlsReg.res}
{$R abfDialogsReg.res}
{$R abfEditsReg.res}
{$R abfEffectsReg.res}
{$R abfLabelsReg.res}
{$R abfMenusReg.res}
{$R abfStatusBarsReg.res}
{$R abfWABReg.res}

interface

uses
{$IfDef D5}
  AppEvnts,
{$EndIf D5}
{$IfDef D4}
  ImgList,
{$EndIf D4}
  abfAppProps,
  abfComponents , abfComponentsDemoMain, abfComponentsDemoSecond,
  abfControls   , abfControlsDemoMain,
  abfComboBoxes , abfComboBoxesDemoMain,
  abfComControls, abfComControlsDemoMain,
  abfDialogs    , abfDialogsDemoMain, abfDialogsDemoSecond, abfDialogsDemoThird,
  abfEdits      , abfEditsDemoMain,
  abfLabels     , abfLabelsDemoMain,
  abfMenus      , abfMenusDemoMain,
  abfStatusBars , abfStatusBarsDemoMain,
{$IfDef D3}
  abfAPM        , abfAPMDemoMain,
  abfEffects    , abfEffectsDemoMain,
  abfWAB        , abfWABDemoMain, abfWABDialogs,
{$EndIf D3}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, Menus, Buttons, ExtCtrls;

const
  cWidth  = 620;
  cHeight = 356;
  cComponentIconWidth  = 24;
  cComponentIconHeight = 24;

  cProdComponents  = 0;
  cProdControls    = 10;
  cProdAPM         = 20;
  cProdComboBoxes  = 30;
  cProdComControls = 40;
  cProdDialogs     = 50;
  cProdEdits       = 60;
  cProdEffects     = 70;
  cProdLabels      = 80;
  cProdMenus       = 90;
  cProdStatusBars  = 100;
  cProdWAB         = 110;

type

//==============================================================================
// TfrmABFVCLDemoMain
//==============================================================================

  TComponentClass = class of TComponent;

  TfrmABFVCLDemoMain = class(TForm)
    imglstCompIcons: TImageList;
    imglstCompButtons: TImageList;
    StatusBar: TabfStatusBar;
    smiForm: TabfSystemMenuInserter;
    pmSystem: TPopupMenu;
    miS1: TMenuItem;
    miS_About: TMenuItem;
    miS_Web: TMenuItem;
    smiApplication: TabfSystemMenuInserter;
    btnAbout: TBitBtn;
    sbiAbout: TabfStatusBarInserter;
    pnInfo: TPanel;
    lbInfo: TabfActiveLabel;
    lbWeb: TabfActiveLabel;
    pnComponents: TPanel;
    lvComponents: TabfListView;
    abfOneInstance1: TabfOneInstance;
  // Events
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure AboutClick(Sender: TObject);
    procedure CloseClick(Sender: TObject);
    procedure WebClick(Sender: TObject);
    procedure lvComponentsDblClick(Sender: TObject);
    procedure lvComponentsColumnClick(Sender: TObject;
      Column: TListColumn);
  private
    ComponentIndex: Integer; // Index of current component
    ProductName: string;     // Name of product that contains current component
    ProductID: Integer;      // ID of product that contains current component
    procedure InitComponentsList;
    procedure AddComponent(ComponentClass: TComponentClass);
    procedure CreateCompButtons;
  end;

var
  frmABFVCLDemoMain: TfrmABFVCLDemoMain;

{******************************************************************************}
implementation
{******************************************************************************}
{$R *.DFM}

uses
  abfConsts, abfVclUtils, abfAboutApplication;

//==============================================================================
// TfrmABFVCLDemoMain
//==============================================================================
{ TfrmABFVCLDemoMain }

//------------------------------------------------------------------------------
// Inits components list view.

procedure TfrmABFVCLDemoMain.InitComponentsList;
begin
  ComponentIndex := 0;
  imglstCompIcons.Width  := cComponentIconWidth;
  imglstCompIcons.Height := cComponentIconHeight;
  lvComponents.Items.BeginUpdate;
  try
    lvComponents.Items.Clear;
  // abfComponents
    ProductName := 'abfComponents';
    ProductID := cProdComponents;
    AddComponent(TabfApplicationProperties);
    AddComponent(TabfAutoRun);
    AddComponent(TabfOneInstance);
    ProductID := cProdComponents + 1;
    AddComponent(TabfShutdown);
    ProductID := cProdComponents;
    AddComponent(TabfTrayIcon);
    AddComponent(TabfWndProcHook);
    ProductID := cProdComponents + 1;
    AddComponent(TabfFileStorage);
    ProductID := cProdComponents;
    AddComponent(TabfWav);
    ProductID := cProdComponents + 1;
    AddComponent(TabfThreadComponent);
    AddComponent(TabfThreadTimer);
    AddComponent(TabfFileOperation);
    AddComponent(TabfFolderMonitor);
    AddComponent(TabfRegistryMonitor);
    AddComponent(TabfStartButtonProperties);
    AddComponent(TabfColorPicker);
    AddComponent(TabfFileAssociation);
  // abfControls
    ProductName := 'abfControls';
    ProductID := cProdControls;
    AddComponent(TabfEdit);
    AddComponent(TabfComboBox);
    AddComponent(TabfImage);
    AddComponent(TabfGroupBox);
    AddComponent(TabfScrollBar);
    AddComponent(TabfDatePanel);
  // abfAPM
    ProductName := 'abfAPM';
    ProductID := cProdAPM;
{$IfDef D3}
    AddComponent(TabfAPMManager);
    AddComponent(TabfAPMScheduler);
{$EndIf D3}
  // abfComboBoxes
    ProductName := 'abfComboBoxes';
    ProductID := cProdComboBoxes;
    AddComponent(TabfColorComboBox);
    AddComponent(TabfFontNameComboBox);
    AddComponent(TabfFontSizeComboBox);
  // abfComControls
    ProductName := 'abfComControls';
    ProductID := cProdComControls;
    AddComponent(TabfRichEdit);
    AddComponent(TabfListView);
    AddComponent(TabfProgressBar);
    AddComponent(TabfTrackBar);
  // abfDialogs
    ProductName := 'abfDialogs';
    ProductID := cProdDialogs;
    AddComponent(TabfWinAboutDlg);
    AddComponent(TabfSelectDirDlg);
    AddComponent(TabfBrowseFolderDlg);
    AddComponent(TabfCplDlg);
    ProductID := cProdDialogs + 1;
    AddComponent(TabfRunDlg);
    AddComponent(TabfOpenWithDlg);
    AddComponent(TabfObjectPropertiesDlg);
    AddComponent(TabfPickIconDlg);
    AddComponent(TabfFindDlg);
    AddComponent(TabfCreateShortcutDlg);
    ProductID := cProdDialogs + 2;
    AddComponent(TabfAddToFavoritesDlg);
    AddComponent(TabfOrganizeFavoritesDlg);
  // abfEdits
    ProductName := 'abfEdits';
    ProductID := cProdEdits;
    AddComponent(TabfIntegerEdit);
    AddComponent(TabfAdvancedEdit);
    AddComponent(TabfButtonEdit);
    AddComponent(TabfUpDownEdit);
    AddComponent(TabfFileNameEdit);
    AddComponent(TabfDirectoryNameEdit);
  // abfEffects
    ProductName := 'abfEffects';
    ProductID := cProdEffects;
{$IfDef D3}
    AddComponent(TabfFormEffect);
    AddComponent(TabfBackGround);
    AddComponent(TabfCredits);
    AddComponent(TabfMovablePanel);
    AddComponent(TabfMagnifier);
{$EndIf D3}
  // abfLabels
    ProductID := cProdLabels;
    ProductName := 'abfLabels';
    AddComponent(TabfLabel);
    AddComponent(TabfActiveLabel);
  // abfMenus
    ProductID := cProdMenus;
    ProductName := 'abfMenus';
    AddComponent(TabfSystemMenuItem);
    AddComponent(TabfSystemMenuInserter);
    AddComponent(TabfPopupMenu);
  // abfStatusBars
    ProductID := cProdStatusBars;
    ProductName := 'abfStatusBars';
    AddComponent(TabfStatusBar);
    AddComponent(TabfStatusBarInserter);
  // abfWAB
    ProductName := 'abfWAB';
    ProductID := cProdWAB;
{$IfDef D3}
    AddComponent(TabfWAB);
    AddComponent(TabfWABAddrDlg);
{$EndIf D3}
    CreateCompButtons;
  finally
    lvComponents.Items.EndUpdate;
  end;
end;

//------------------------------------------------------------------------------
// Adds component info item to the system list.

procedure TfrmABFVCLDemoMain.AddComponent(ComponentClass: TComponentClass);
var
  ImageName: string;
  Bitmap: TBitmap;
begin
// Add item to ListView, ProductID is stored in SubItems.Objects[0]
  with lvComponents.Items.Add do
  begin
    Caption := ComponentClass.ClassName;
    SubItems.AddObject(ProductName, TObject(ProductID));
    ImageIndex := ComponentIndex;
  end;
// Add component icon to ImageList
  Bitmap := TBitmap.Create;
  with Bitmap do
  try
    Width  := cComponentIconWidth;
    Height := cComponentIconHeight;
    ImageName := ComponentClass.ClassName;
  // Search for class bitmap, if not found - search for parent class bitmap.
    if FindResource(hInstance, PChar(ImageName), RT_BITMAP) = 0 then
    begin
      ImageName := ComponentClass.ClassParent.ClassName;
      if FindResource(hInstance, PChar(ImageName), RT_BITMAP) <> 0 then
        LoadFromResourceName(hInstance, ImageName);
    end else
      LoadFromResourceName(hInstance, ImageName);
{$IfDef D3}
    Handle := CreateGrayMappedBmp(Handle);
{$EndIf D3}
    imglstCompIcons.AddMasked(Bitmap, Canvas.Pixels[0, Height - 1]);
  finally
    Bitmap.Free;
  end;
// Prepare for next component
  Inc(ComponentIndex);
end;

//------------------------------------------------------------------------------

procedure TfrmABFVCLDemoMain.CreateCompButtons;
var
  i: Integer;
  Bitmap: TBitmap;
  R: TRect;
begin
  R := Rect(0, 0, 32, 32);
  Bitmap := TBitmap.Create;
  with Bitmap, Bitmap.Canvas do
  try
    Width  := R.Right;
    Height := R.Bottom;
    for i := 0 to imglstCompIcons.Count - 1 do
    begin
      R := Rect(0, 0, 32, 32);
      Brush.Color := clWindow;
      FillRect(R);
      InflateRect(R, -1, -1);
      DrawEdge(Canvas.Handle, R, BDR_RAISEDINNER, BF_RECT);
      InflateRect(R, -1, -1);
      Brush.Color := clBtnFace;
      FillRect(R);
      imglstCompIcons.Draw(Canvas, 4, 4, i);
      imglstCompButtons.AddMasked(Bitmap, clWindow);
    end;
  finally
    Bitmap.Free;
  end;
end;


//==============================================================================
// Events

procedure TfrmABFVCLDemoMain.FormCreate(Sender: TObject);
begin
  InitComponentsList;
  StatusBar.PanelByName('Total').Text := 'Components count: ' +
    IntToStr(lvComponents.Items.Count);
{$IfDef D3}
  lvComponents.RowSelect := True;
{$EndIf D3}
// Create a TabfActiveLabel with default cursors settings, and apply these settings
  with TabfActiveLabel.Create(nil) do
  try
    lbInfo.Cursors := Cursors;
    lbWeb.Cursors  := Cursors;
  finally
    Free;
  end;
  Width  := cWidth;
  Height := cHeight;
{$IfDef D4}
  OnCanResize := FormCanResize;
{$EndIf D4}
end;

//------------------------------------------------------------------------------

procedure TfrmABFVCLDemoMain.FormResize(Sender: TObject);
begin
{$IfNDef D4}
  if Width  < cWidth  then Width  := cWidth;
  if Height < cHeight then Height := cHeight;
{$EndIf D4}
end;

//------------------------------------------------------------------------------

procedure TfrmABFVCLDemoMain.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  if NewWidth  < cWidth  then NewWidth  := cWidth;
  if NewHeight < cHeight then NewHeight := cHeight;
end;

//------------------------------------------------------------------------------

procedure TfrmABFVCLDemoMain.AboutClick(Sender: TObject);
begin
  abfApplicationAboutEx(Application.Title, ''{SabfVCLVersion}, '', '',
    True, nil);
end;

//------------------------------------------------------------------------------

procedure TfrmABFVCLDemoMain.CloseClick(Sender: TObject);
begin
  Close;
end;

//------------------------------------------------------------------------------

procedure TfrmABFVCLDemoMain.WebClick(Sender: TObject);
begin
  abfGotoUrl(SabfWeb);
end;

//------------------------------------------------------------------------------

procedure TfrmABFVCLDemoMain.lvComponentsDblClick(Sender: TObject);
begin
  if not Assigned(lvComponents.Selected) then Exit;
  case Integer(lvComponents.Selected.SubItems.Objects[0]) of
    cProdComponents    : abfComponentsDemoMain.ShowDemo;
    cProdComponents + 1: abfComponentsDemoSecond.ShowDemo;
    cProdControls      : abfControlsDemoMain.ShowDemo;
{$IfDef D3}
    cProdAPM           : abfAPMDemoMain.ShowDemo;
{$EndIf D3}
    cProdComboBoxes    : abfComboBoxesDemoMain.ShowDemo;
    cProdComControls   : abfComControlsDemoMain.ShowDemo;
    cProdDialogs       : abfDialogsDemoMain.ShowDemo;
    cProdDialogs + 1   : abfDialogsDemoSecond.ShowDemo;
    cProdDialogs + 2   : abfDialogsDemoThird.ShowDemo;
    cProdEdits         : abfEditsDemoMain.ShowDemo;
{$IfDef D3}
    cProdEffects       : abfEffectsDemoMain.ShowDemo;
{$EndIf D3}
    cProdLabels        : abfLabelsDemoMain.ShowDemo;
    cProdMenus         : abfMenusDemoMain.ShowDemo;
    cProdStatusBars    : abfStatusBarsDemoMain.ShowDemo;
{$IfDef D3}
    cProdWAB           : abfWabDemoMain.ShowDemo;
{$EndIf D3}
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmABFVCLDemoMain.lvComponentsColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  lvComponents.SortType := stText; 
end;

//------------------------------------------------------------------------------

end{unit abfVCLDemoMain}.
