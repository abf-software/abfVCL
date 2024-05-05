{*******************************************************************************

  abfComponents Demo. Main form unit.

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfComponentsDemoMain;

{$I abf.inc}

interface

uses
  Windows, Forms, Messages, Classes, Controls, StdCtrls, ExtCtrls, Graphics,
  ImgList, ComCtrls, Dialogs, Buttons, Menus, AppEvnts,
  abfComponents, abfAppProps;

const
  cFlatButtons = False; // Determines are speed buttons flat or not
  cWidth  = 640;
  cHeight = 400;

type

//==============================================================================
// TfrmComponentsDemoMain
//==============================================================================
// Main form of application.

  TfrmComponentsDemoMain = class(TForm)
    pmMain: TPopupMenu;
      miAboutApp: TMenuItem;
        mi1: TMenuItem;
      miExit: TMenuItem;
  { TbfApplicationProperties }
    abfAppProp: TabfApplicationProperties;
    grpApplicationProperties: TGroupBox;
    imgApplicationProperties: TImage;
    lbAppPropInfo: TLabel;
    grpAppPropBeep: TGroupBox;
      chbAppPropMinimize: TCheckBox;
      chbAppPropOnActivate: TCheckBox;
      chbAppPropOnDeactivate: TCheckBox;
      chbAppPropOnRestore: TCheckBox;
      chbAppPropOnHint: TCheckBox;
    grpAppPropHint: TGroupBox;
      lbAppPropHintPause: TLabel;
        edAppPropHintPause: TEdit;
        udAppPropHintPause: TUpDown;
      lbAppPropHintHidePause: TLabel;
        edAppPropHintHidePause: TEdit;
        udAppPropHintHidePause: TUpDown;
      lbAppPropHintShortPause: TLabel;
        edAppPropHintShortPause: TEdit;
        udAppPropHintShortPause: TUpDown;
      btnAppPropHintColor: TSpeedButton;
    dlgColor: TColorDialog;
  { TabfAutoRun }
    abfAutoRun: TabfAutoRun;
    grpAutoRun: TGroupBox;
      imgAutoRun: TImage;
      lbAutoRunInfo: TLabel;
      chbAutoRun: TCheckBox;
  { TabfOneInstance }
    abfOneInstance: TabfOneInstance;
    grpOneInstance: TGroupBox;
      imgOneInstance: TImage;
      lbOneInstanceInfo: TLabel;
      btnOneInstance: TButton;
  { TabfTrayIcon }
    abfTrayIcon: TabfTrayIcon;
    grpTrayIcon: TGroupBox;
      imgIcon: TImage;
      lbTrayIconInfo: TLabel;
      lbIconHint: TLabel;
        edIconHint: TEdit;
      btnIconLoad: TSpeedButton;
      btnIconShow: TSpeedButton;
      btnIconHide: TSpeedButton;
      chbIconEnabled: TCheckBox;
      chbIconCycleIcons: TCheckBox;
      chbIconMinimizeToTray: TCheckBox;
      chbIconPopupByLeft: TCheckBox;
    pmIcon: TPopupMenu;
      miIconHide: TMenuItem;
      mi5: TMenuItem;
      miIconShowApp: TMenuItem;
      miIconHideApp: TMenuItem;
      mi6: TMenuItem;
      miIconAbout: TMenuItem;
      miIconExit: TMenuItem;
    dlgIconLoad: TOpenDialog;
    imgsIcon: TImageList;
  { TabfWndProcHook }
    abfWndProcHook: TabfWndProcHook;
    grpWPH: TGroupBox;
      imgWPH: TImage;
      lbWPHInfo: TLabel;
      edHookedControl: TEdit;
      grpWPHBeep: TGroupBox;
        chbWPHMouseDown: TCheckBox;
        chbWPHKeyDown: TCheckBox;
        chbWPHKeyUp: TCheckBox;
      chbWPHActive: TCheckBox;
      chbWPHAutoDrag: TCheckBox;
  { TabfWav }
    abfWav: TabfWav;
    grpWav: TGroupBox;
      imgWav: TImage;
      lbWavInfo: TLabel;
      chbWavLopped: TCheckBox;
      chbWavAsync: TCheckBox;
      btnWav: TBitBtn;
  { Events }
    procedure FormCreate(Sender: TObject);
    procedure ComponentIconClick(Sender: TObject);
    procedure AboutClick(Sender: TObject);
    procedure CloseClick(Sender: TObject);
  { TbfApplicationProperties }
    procedure chbAppPropBeepClick(Sender: TObject);
    procedure btnAppPropHintColorClick(Sender: TObject);
    procedure edAppPropHintChange(Sender: TObject);
  { TabfAutoRun }
    procedure chbAutoRunClick(Sender: TObject);
  { TabfOneInstance }
    procedure btnOneInstanceClick(Sender: TObject);
  { TabfTrayIcon }
    procedure edIconHintChange(Sender: TObject);
    procedure btnIconLoadClick(Sender: TObject);
    procedure chbIconEnabledClick(Sender: TObject);
    procedure chbIconCycleIconsClick(Sender: TObject);
    procedure chbIconMinimizeToTrayClick(Sender: TObject);
    procedure chbIconPopupByLeftClick(Sender: TObject);
    procedure btnIconShowClick(Sender: TObject);
    procedure btnIconHideClick(Sender: TObject);
    procedure miIconHideAppClick(Sender: TObject);
    procedure miIconShowAppClick(Sender: TObject);
  { TabfWndProcHook }
    procedure abfWndProcHookMessageBefore(Sender: TObject;
      var Msg: TMessage; var Handled: Boolean);
    procedure chbWPHActiveClick(Sender: TObject);
    procedure chbWPHAutoDragClick(Sender: TObject);
  { TabfWav }
    procedure abfWavPlayAfter(Sender: TObject);
    procedure abfWavPlayBefore(Sender: TObject);
    procedure btnWavClick(Sender: TObject);
    procedure chbWavAsyncClick(Sender: TObject);
    procedure chbWavLoppedClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
  private
    FSoundPlayed: Boolean;
    procedure DoBeep(Sender: TObject);
    procedure ChangeAppPropGlyph;
    procedure UpdateAppProp;
    procedure UpdateIconProp;
    procedure UpdateWav;
    procedure UpdateWavButton;
    procedure StopSound;
  end;{TfrmMain = class(TForm)}

var
  frmComponentsDemoMain: TfrmComponentsDemoMain;

//==============================================================================
// Entry point
//==============================================================================
procedure ShowDemo;

{******************************************************************************}
implementation
{******************************************************************************}
{$R *.DFM}
uses
  SysUtils,
  abfAboutApplication, abfAboutComponent;

//==============================================================================
// Entry point
//==============================================================================

procedure ShowDemo;
begin
  if not Assigned(frmComponentsDemoMain) then
    Application.CreateForm(TfrmComponentsDemoMain, frmComponentsDemoMain);
  frmComponentsDemoMain.Show;
  frmComponentsDemoMain.BringToFront;
end;


//==============================================================================
// TfrmComponentsDemoMain
//==============================================================================
// Main form of application.
{ TfrmComponentsDemoMain }

procedure TfrmComponentsDemoMain.DoBeep(Sender: TObject);
begin
  Beep;
end;

//------------------------------------------------------------------------------

procedure TfrmComponentsDemoMain.ChangeAppPropGlyph;
var
  Glyph: TBitmap;
  R: TRect;
begin
  Glyph := TBitmap.Create;
  with Glyph, Glyph.Canvas do
  try
    Width  := btnAppPropHintColor.Width  - 8;
    Height := btnAppPropHintColor.Height - 8;
    R := Rect(0, 0, Width, Height);
    Brush.Style := bsSolid;
  { Create transparent part becouse LeftBotom point specifies a transparent
    color of the glyph. Select color for it other then Application.HintColor }
    Brush.Color := clBtnFace;
    FrameRect(R);
  { Draw complex border }
    InflateRect(R, -1, -1);
    Brush.Color := clWhite;
    FrameRect(R);
    InflateRect(R, -1, -1);
    Brush.Color := clBtnShadow;
    FrameRect(R);
  { Draw color rect }
    InflateRect(R, -1, -1);
    Brush.Color := Application.HintColor;
    FillRect(R);
  { Assign new glyph to button }
    btnAppPropHintColor.Glyph := Glyph;
  finally
    Glyph.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmComponentsDemoMain.UpdateAppProp;
begin
  udAppPropHintPause.Position := Application.HintPause;
  udAppPropHintHidePause.Position := Application.HintHidePause;
  udAppPropHintShortPause.Position := Application.HintShortPause;
  edAppPropHintPause.Text := IntToStr(udAppPropHintPause.Position);
  edAppPropHintHidePause.Text := IntToStr(udAppPropHintHidePause.Position);
  edAppPropHintShortPause.Text := IntToStr(udAppPropHintShortPause.Position);
end;

//------------------------------------------------------------------------------

procedure TfrmComponentsDemoMain.UpdateIconProp;
begin
  with abfTrayIcon do
  begin
    edIconHint.Text := Hint;
    chbIconEnabled.Checked := Enabled;
    chbIconCycleIcons.Checked := CycleIcons;
    chbIconMinimizeToTray.Checked := MinimizeToTray;
    chbIconPopupByLeft.Checked := PopupByLeft;
    UpDate;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmComponentsDemoMain.UpdateWav;
begin
  with abfWav do
  begin
    chbWavAsync.Checked := AsyncPlay;
    chbWavLopped.Checked := Looped;
  end;
  UpdateWavButton
end;

//------------------------------------------------------------------------------

procedure TfrmComponentsDemoMain.UpdateWavButton;
begin
  if FSoundPlayed then btnWav.Caption := 'Stop sound'
  else btnWav.Caption := 'Play sound';
end;

//------------------------------------------------------------------------------

procedure TfrmComponentsDemoMain.StopSound;
begin
  abfWav.Stop;
  FSoundPlayed := False;
end;


//==============================================================================
// Published by Delphi

procedure TfrmComponentsDemoMain.FormCreate(Sender: TObject);
begin
{$IfDef D3}
  btnAppPropHintColor.Flat := cFlatButtons;
  btnIconLoad.Flat := cFlatButtons;
  btnIconShow.Flat := cFlatButtons;
  btnIconHide.Flat := cFlatButtons;
{$EndIf D3}
  ChangeAppPropGlyph;
  UpdateAppProp;
  UpdateIconProp;
  chbWPHActive.Checked := abfWndProcHook.Active;
  chbWPHAutoDrag.Checked := abfWndProcHook.AutoDrag;
  UpdateWav;
{ Fix form width and height }
  Width  := cWidth;
  Height := cHeight;
{$IfDef D4}
  OnCanResize := FormCanResize;
{$EndIf D4}
end;

//------------------------------------------------------------------------------

procedure TfrmComponentsDemoMain.FormResize(Sender: TObject);
begin
{$IfNDef D4}
  if Width  < cWidth  then Width  := cWidth;
  if Height < cHeight then Height := cHeight;
{$EndIf D4}
end;

//------------------------------------------------------------------------------

procedure TfrmComponentsDemoMain.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  if NewWidth  < cWidth  then NewWidth  := cWidth;
  if NewHeight < cHeight then NewHeight := cHeight;
end;

//------------------------------------------------------------------------------

procedure TfrmComponentsDemoMain.ComponentIconClick(Sender: TObject);
begin
  case (Sender as TComponent).Tag of
    1: abfComponentAboutEx(abfAppProp, '', '', True);
    2: abfComponentAboutEx(abfAutoRun, '', '', True);
    3: abfComponentAboutEx(abfOneInstance, '', '', True);
    4: abfComponentAboutEx(abfTrayIcon, '', '', True);
    5: abfComponentAboutEx(abfWndProcHook, '', '', True);
    6: abfComponentAboutEx(abfWav, '', '', True);
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmComponentsDemoMain.AboutClick(Sender: TObject);
begin
  abfApplicationAboutEx(Application.Title, ''{SabfVCLVersion}, '', '',
    True, nil);
end;

//------------------------------------------------------------------------------

procedure TfrmComponentsDemoMain.CloseClick(Sender: TObject);
begin
  Close;
end;


//==============================================================================
// TbfApplicationProperties

procedure TfrmComponentsDemoMain.chbAppPropBeepClick(Sender: TObject);
var
  Event: TNotifyEvent;
begin
  with (Sender as TCheckBox) do
  begin
    if Checked then Event := DoBeep else Event := nil;
    case Tag of
      1: abfAppProp.OnMinimize := Event;
      2: abfAppProp.OnRestore := Event;
      3: abfAppProp.OnActivate := Event;
      4: abfAppProp.OnDeactivate := Event;
      5: abfAppProp.OnHint := Event;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmComponentsDemoMain.btnAppPropHintColorClick(Sender: TObject);
begin
  dlgColor.Color := abfAppProp.HintColor;
  if not dlgColor.Execute then Exit;
  abfAppProp.HintColor := dlgColor.Color;
  ChangeAppPropGlyph;
end;

//------------------------------------------------------------------------------

procedure TfrmComponentsDemoMain.edAppPropHintChange(Sender: TObject);
var
  UD: TUpDown;
  Value: Integer;
begin
  if (csLoading in ComponentState) then Exit;
  with (Sender as TEdit) do
  begin
    UD := TUpDown(Self.FindComponent('ud' + Copy(Name, 3, MaxInt)));
    Value := StrToIntDef(Text, UD.Position);
    Text := IntToStr(Value);
    case Tag of
      1: abfAppProp.HintPause := Value;
      2: abfAppProp.HintHidePause := Value;
      3: abfAppProp.HintShortPause := Value;
    end;
  end;
end;


//==============================================================================
// TabfAutoRun

procedure TfrmComponentsDemoMain.chbAutoRunClick(Sender: TObject);
begin
  abfAutoRun.AutoRun := (Sender as TCheckBox).Checked;
end;


//==============================================================================
// TabfOneInstance

//------------------------------------------------------------------------------

procedure TfrmComponentsDemoMain.btnOneInstanceClick(Sender: TObject);
var
  CmdLine: string;
begin
  CmdLine := Application.ExeName;
  WinExec(PChar(CmdLine), 0);
end;


//==============================================================================
// TabfTrayIcon

procedure TfrmComponentsDemoMain.edIconHintChange(Sender: TObject);
begin
  abfTrayIcon.Hint := TEdit(Sender).Text;
end;

//------------------------------------------------------------------------------

procedure TfrmComponentsDemoMain.btnIconLoadClick(Sender: TObject);
begin
  if not dlgIconLoad.Execute then Exit;
  abfTrayIcon.CycleIcons := False; //Stop the icons cycling
  abfTrayIcon.Icon.LoadFromFile(dlgIconLoad.FileName);
  UpdateIconProp;
end;

//------------------------------------------------------------------------------

procedure TfrmComponentsDemoMain.chbIconEnabledClick(Sender: TObject);
begin
  abfTrayIcon.Enabled := TCheckBox(Sender).Checked;
end;

//------------------------------------------------------------------------------

procedure TfrmComponentsDemoMain.chbIconCycleIconsClick(Sender: TObject);
begin
  abfTrayIcon.CycleIcons := TCheckBox(Sender).Checked;
end;

//------------------------------------------------------------------------------

procedure TfrmComponentsDemoMain.chbIconMinimizeToTrayClick(Sender: TObject);
begin
  abfTrayIcon.MinimizeToTray := TCheckBox(Sender).Checked;
{ Prevent minimize to tray when the icon is disabled }
  if abfTrayIcon.MinimizeToTray then
  begin
    abfTrayIcon.Enabled := True;
    UpdateIconProp;
    chbIconEnabled.Enabled := False;
  end else
    chbIconEnabled.Enabled := True;
end;

//------------------------------------------------------------------------------

procedure TfrmComponentsDemoMain.chbIconPopupByLeftClick(Sender: TObject);
begin
  abfTrayIcon.PopupByLeft := TCheckBox(Sender).Checked;
end;

//------------------------------------------------------------------------------

procedure TfrmComponentsDemoMain.btnIconShowClick(Sender: TObject);
begin
  abfTrayIcon.Show;
end;

//------------------------------------------------------------------------------

procedure TfrmComponentsDemoMain.btnIconHideClick(Sender: TObject);
begin
  abfTrayIcon.Hide;
end;

//------------------------------------------------------------------------------

procedure TfrmComponentsDemoMain.miIconHideAppClick(Sender: TObject);
begin
  abfTrayIcon.HideMainForm;
end;

//------------------------------------------------------------------------------

procedure TfrmComponentsDemoMain.miIconShowAppClick(Sender: TObject);
begin
  abfTrayIcon.ShowMainForm;
end;


//==============================================================================
// TabfWndProcHook

procedure TfrmComponentsDemoMain.abfWndProcHookMessageBefore(Sender: TObject;
  var Msg: TMessage; var Handled: Boolean);
begin
  Handled := False;
  case Msg.Msg of
    WM_LButtonDown, WM_MButtonDown, WM_RButtonDown:
      if chbWPHMouseDown.Checked then Beep;
{    WM_LButtonUp, WM_MButtonUp, WM_RButtonUp:
      if chbWPHMouseUp.Checked then Beep;}
    WM_KeyDown: if chbWPHKeyDown.Checked then Beep;
    WM_KeyUp: if chbWPHKeyUp.Checked then Beep;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmComponentsDemoMain.chbWPHActiveClick(Sender: TObject);
begin
  abfWndProcHook.Active := (Sender as TCheckBox).Checked;
end;

//------------------------------------------------------------------------------

procedure TfrmComponentsDemoMain.chbWPHAutoDragClick(Sender: TObject);
begin
  abfWndProcHook.AutoDrag := (Sender as TCheckBox).Checked;
end;


//==============================================================================
// TabfWave

procedure TfrmComponentsDemoMain.abfWavPlayAfter(Sender: TObject);
begin
  if abfWav.Looped then Exit;
  FSoundPlayed := False;
  UpdateWavButton;
end;

//------------------------------------------------------------------------------

procedure TfrmComponentsDemoMain.abfWavPlayBefore(Sender: TObject);
begin
  FSoundPlayed := True;
  UpdateWavButton;
end;

//------------------------------------------------------------------------------

procedure TfrmComponentsDemoMain.btnWavClick(Sender: TObject);
begin
  if FSoundPlayed then StopSound else abfWav.Play;
  UpdateWavButton;
end;

//------------------------------------------------------------------------------

procedure TfrmComponentsDemoMain.chbWavAsyncClick(Sender: TObject);
begin
  StopSound;
  abfWav.AsyncPlay := (Sender as TCheckBox).Checked;
{ Prevent freez be synchronous playing of looped sound }
  if not abfWav.AsyncPlay then
  begin
    abfWav.Looped := False;
    UpdateWav;
    chbWavLopped.Enabled := False;
  end else
    chbWavLopped.Enabled := True;
  UpdateWavButton;
end;

//------------------------------------------------------------------------------

procedure TfrmComponentsDemoMain.chbWavLoppedClick(Sender: TObject);
begin
  StopSound;
  abfWav.Looped := (Sender as TCheckBox).Checked;
  UpdateWavButton;
end;

//------------------------------------------------------------------------------

end{unit abfComponentsDemoMain}.
