{*******************************************************************************

  abfDialogs Demo. Third form unit.

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfDialogsDemoThird;

{$I abf.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, Menus, ComCtrls,
// ABF VCL  
  abfComponents, abfDialogs;

const
  cFlatButtons = False; // Are speed buttons flat or not
  cWidth  = 640;
  cHeight = 400;

type

//==============================================================================
// TfrmDialogsDemoThird
//==============================================================================
{ TfrmDialogsDemoThird }

  TfrmDialogsDemoThird = class(TForm)
    pmMain: TPopupMenu;
      miAboutApp: TMenuItem;
      mi1: TMenuItem;
      miExit: TMenuItem;
  // TabfOrganizeFavorites
    abfOrganizeFavoritesDlg: TabfOrganizeFavoritesDlg;
    grpOrganizeFavorite: TGroupBox;
      imgOrganizeFavorite: TImage;
      lbOrganizeFavoriteInfo: TLabel;
      btnOrganizeFavoriteExecute: TButton;
  // TabfAddToFavorites
    abfAddToFavoritesDlg: TabfAddToFavoritesDlg;
    grpAddToFavorites: TGroupBox;
      imgAddToFavorites: TImage;
      lbAddToFavoritesInfo: TLabel;
      lbAddToFavoritesTitle: TLabel;
        edAddToFavoritesTitle: TEdit;
      lbAddToFavoritesURL: TLabel;
        edAddToFavoritesURL: TEdit;
      btnAddToFavoritesExecute: TButton;
    lbReserved: TLabel;
  // Events
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure FormResize(Sender: TObject);
    procedure ComponentIconClick(Sender: TObject);
    procedure AboutClick(Sender: TObject);
    procedure CloseClick(Sender: TObject);
    procedure ExecuteClick(Sender: TObject);
  private
    procedure ApplyAddToFavorites;
  end;

var
  frmDialogsDemoThird: TfrmDialogsDemoThird;

//==============================================================================
// Entry point
//==============================================================================

procedure ShowDemo;

{******************************************************************************}
implementation
{******************************************************************************}
{$R *.DFM}
uses
 abfAboutApplication, abfAboutComponent;

//==============================================================================
// Entry point
//==============================================================================

procedure ShowDemo;
begin
  if not Assigned(frmDialogsDemoThird) then
    Application.CreateForm(TfrmDialogsDemoThird, frmDialogsDemoThird);
  frmDialogsDemoThird.Show;
  frmDialogsDemoThird.BringToFront;
end;


//==============================================================================
// TfrmDialogsDemoThird
//==============================================================================
{ TfrmDialogsDemoThird }

procedure TfrmDialogsDemoThird.ApplyAddToFavorites;
begin
  with abfAddToFavoritesDlg do
  begin
    Title := edAddToFavoritesTitle.Text;
    URL := edAddToFavoritesURL.Text;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmDialogsDemoThird.FormResize(Sender: TObject);
begin
{$IfNDef D4}
  if Width  < cWidth  then Width  := cWidth;
  if Height < cHeight then Height := cHeight;
{$EndIf D4}
end;

//------------------------------------------------------------------------------

procedure TfrmDialogsDemoThird.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  if NewWidth  < cWidth  then NewWidth  := cWidth;
  if NewHeight < cHeight then NewHeight := cHeight;
end;

//------------------------------------------------------------------------------

procedure TfrmDialogsDemoThird.ComponentIconClick(Sender: TObject);
begin
  case (Sender as TComponent).Tag of
    1: abfComponentAboutEx(abfOrganizeFavoritesDlg, '', '', True);
    2: abfComponentAboutEx(abfAddToFavoritesDlg, '', '', True);
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmDialogsDemoThird.AboutClick(Sender: TObject);
begin
  abfApplicationAboutEx(Application.Title, ''{SabfVCLVersion}, '', '',
    True, nil);
end;

//------------------------------------------------------------------------------

procedure TfrmDialogsDemoThird.CloseClick(Sender: TObject);
begin
  Close;
end;

//------------------------------------------------------------------------------

procedure TfrmDialogsDemoThird.ExecuteClick(Sender: TObject);
begin
  with Sender as TControl do
    case Tag of
      1: abfOrganizeFavoritesDlg.Execute;
      2: begin
        ApplyAddToFavorites;
        abfAddToFavoritesDlg.Execute;
      end;
    end;
end;

//------------------------------------------------------------------------------

end{unit abfDialogsDemoThree}.
