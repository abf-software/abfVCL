{*******************************************************************************

  abfTrayIcon component DEMO. Main form unit.

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL
  
*******************************************************************************}
unit abfTrayIconDemoMain;

{$I abf.inc}

interface

uses
{$IfDef D4}
  ImgList,
{$EndIf D4}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, Menus, ComCtrls,
  abfComponents;

type

{******************************************************************************}
{ TfrmMain }

  TfrmMain = class(TForm)
    lbBigTitle: TLabel;
    lbSmallTitle: TLabel;

    lbIconHint: TLabel;
      edIconHint: TEdit;
    btnLoadIcon: TSpeedButton;

    chbEnabled: TCheckBox;
    chbCycleIcons: TCheckBox;
    lbInteval: TLabel;
      edInterval: TEdit;
        udInterval: TUpDown;
    chbMinimizeToTray: TCheckBox;
    chbPopupByLeft: TCheckBox;
    chbShowHint: TCheckBox;

    btnShowIcon: TBitBtn;
    btnHideIcon: TBitBtn;
    btnAbout: TBitBtn;

    abfTrayIcon: TabfTrayIcon;
    pmIcon: TPopupMenu;
      miHideIcon: TMenuItem;
        N1: TMenuItem;
      miHideApp: TMenuItem;
      miShowApp: TMenuItem;
        N2: TMenuItem;
      miAbout: TMenuItem;
      miExit: TMenuItem;
    dlgLoadIcon: TOpenDialog;
    imgsIcons: TImageList;
    
    procedure FormCreate(Sender: TObject);
    procedure btnShowIconClick(Sender: TObject);
    procedure btnHideIconClick(Sender: TObject);
    procedure btnAboutClick(Sender: TObject);
    procedure btnLoadIconClick(Sender: TObject);
    procedure edIconHintChange(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miHideAppClick(Sender: TObject);
    procedure miShowAppClick(Sender: TObject);
    procedure chbEnabledClick(Sender: TObject);
    procedure chbCycleIconsClick(Sender: TObject);
    procedure edIntervalChange(Sender: TObject);
    procedure chbMinimizeToTrayClick(Sender: TObject);
    procedure chbPopupByLeftClick(Sender: TObject);
    procedure chbShowHintClick(Sender: TObject);
  private
    procedure UpdateProperties;
  end;

var
  frmMain: TfrmMain;

{******************************************************************************}
implementation
{******************************************************************************}
{$R *.DFM}
uses
  abfAboutComponent;

{******************************************************************************}
{ TfrmMain }

procedure TfrmMain.UpdateProperties;
begin
  with abfTrayIcon do
  begin
    edIconHint.Text := Hint;
    chbEnabled.Checked := Enabled;
    chbCycleIcons.Checked := CycleIcons;
    udInterval.Position := CycleInterval;
    chbMinimizeToTray.Checked := MinimizeToTray;
    chbPopupByLeft.Checked := PopupByLeft;
    chbShowHint.Checked := ShowHint;
    UpDate;
  end;
end;

{--------------------------------------}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  UpdateProperties;
  abfTrayIcon.Show;
end;

{--------------------------------------}

procedure TfrmMain.btnShowIconClick(Sender: TObject);
begin
  abfTrayIcon.Show;
end;

{--------------------------------------}

procedure TfrmMain.btnHideIconClick(Sender: TObject);
begin
  abfTrayIcon.ShowMainForm;
  abfTrayIcon.Hide;
end;

{--------------------------------------}

procedure TfrmMain.btnAboutClick(Sender: TObject);
begin
  abfComponentAbout(abfTrayIcon);
end;

{--------------------------------------}

procedure TfrmMain.btnLoadIconClick(Sender: TObject);
begin
  if not dlgLoadIcon.Execute then Exit;
  abfTrayIcon.CycleIcons := False; //Stop the icons cycling
  abfTrayIcon.Icon.LoadFromFile(dlgLoadIcon.FileName);
  UpdateProperties;
end;

{--------------------------------------}

procedure TfrmMain.edIconHintChange(Sender: TObject);
begin
  abfTrayIcon.Hint := TEdit(Sender).Text;
end;

{--------------------------------------}

procedure TfrmMain.miHideAppClick(Sender: TObject);
begin
  abfTrayIcon.HideMainForm;
end;

{--------------------------------------}

procedure TfrmMain.miShowAppClick(Sender: TObject);
begin
  abfTrayIcon.ShowMainForm;
end;

{--------------------------------------}

procedure TfrmMain.miExitClick(Sender: TObject);
begin
  Close;
end;

{--------------------------------------}

procedure TfrmMain.chbEnabledClick(Sender: TObject);
begin
  abfTrayIcon.Enabled := TCheckBox(Sender).Checked;
end;

{--------------------------------------}

procedure TfrmMain.chbCycleIconsClick(Sender: TObject);
begin
  abfTrayIcon.CycleIcons := TCheckBox(Sender).Checked;
end;

{--------------------------------------}

procedure TfrmMain.edIntervalChange(Sender: TObject);
begin
  abfTrayIcon.CycleInterval := StrToIntDef(TEdit(Sender).Text, 200);
  UpdateProperties;
end;

{--------------------------------------}

procedure TfrmMain.chbMinimizeToTrayClick(Sender: TObject);
begin
  abfTrayIcon.MinimizeToTray := TCheckBox(Sender).Checked;
end;

{--------------------------------------}

procedure TfrmMain.chbPopupByLeftClick(Sender: TObject);
begin
  abfTrayIcon.PopupByLeft := TCheckBox(Sender).Checked;
end;

{--------------------------------------}

procedure TfrmMain.chbShowHintClick(Sender: TObject);
begin
  abfTrayIcon.ShowHint := TCheckBox(Sender).Checked;
end;

{--------------------------------------}

end{unit abfTrayIconDemoMain}.
