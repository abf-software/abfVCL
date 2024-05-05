{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfAboutApplication;

{$I abf.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons;

{$IfNDef D3}
const
  crHandPoint = crUpArrow;
{$EndIf D3}

type

//==============================================================================
// TfrmAboutApplication
//==============================================================================
// Application about form.

  TfrmAboutApplication = class(TForm)
    pnLogo: TPanel;
      imgLogo: TImage;
    lbName1: TLabel;
    lbName2: TLabel;
      lbBy: TLabel;
    lbABF1: TLabel;
    lbABF2: TLabel;
    lbSupportCaption: TLabel;
    lbSupport: TLabel;
    lbWebCaption: TLabel;
      lbWeb: TLabel;
    lbCopyright: TLabel;
    bvBottom: TBevel;
    pnBottom: TPanel;
    btnBuy: TButton;
      btnOk: TButton;
    lbVersion: TLabel;
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure URLMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure URLMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure URLMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lbWebClick(Sender: TObject);
    procedure lbSupportClick(Sender: TObject);
    procedure btnBuyClick(Sender: TObject);
  private
    FWebSuffix: string;
    FEmailPrefix: string;
  // Properties Get/Set
    function  GetAppName: string;
    procedure SetAppName(const A: string);
    function  GetEmailSubject: string;
    procedure SetEmailPrefix(const A: string);
    procedure SetWebSuffix(const A: string);
  public
    constructor Create(AOwner: TComponent); override;
    property AppName: string read GetAppName write SetAppName;
    property EmailSubject: string read GetEmailSubject;
    property EmailPrefix: string read FEmailPrefix write SetEmailPrefix;
    property WebSuffix: string read FWebSuffix write SetWebSuffix;
  end;

//------------------------------------------------------------------------------
// Do not remark a varible declaration! It is needed for C++Builder 4 and lower!

var
  frmAboutApplication: TfrmAboutApplication;

//==============================================================================
// Application about routines
//==============================================================================

procedure abfApplicationAboutEx(const AName, AVersion, AWebSuffix,
  AEmailPrefix: string; ABuyButton: Boolean; const ALogo: TGraphic);
procedure abfApplicationAbout(const AName: string);

{******************************************************************************}
implementation
{******************************************************************************}
{$R *.DFM}

uses
  ShellApi, abfConsts, abfVclConsts, abfSysUtils, abfVclUtils;

//==============================================================================
// Application about routines
//==============================================================================

const
  cByTop = 74;
  cABFTop = 104;

procedure abfApplicationAboutEx(const AName, AVersion, AWebSuffix,
  AEmailPrefix: string; ABuyButton: Boolean; const ALogo: TGraphic);
var
  Form, MainForm: TForm;
  Build: TDateTime;
begin
  Form := TfrmAboutApplication.Create(nil);
  with TfrmAboutApplication(Form) do
  try
    AppName := AName;
  // Check is version present
    if AVersion = '' then
    begin
      lbBy.Top   := cByTop;
      lbABF1.Top := cABFTop;
      lbABF2.Top := cABFTop + 2;
    end else
    begin
      abfGetFileBuildDateTime(ParamStr(0), Build);
      lbVersion.Caption := Format(SabfAboutApplicationVersionFmt,
        [AVersion, DateToStr(Build)]);
      lbVersion.Visible := True;
    end;
    WebSuffix := AWebSuffix;
    EmailPrefix := AEmailPrefix;
  // Check new logo
    if Assigned(ALogo) and (not ALogo.Empty) then
      imgLogo.Picture.Assign(ALogo);
  // Show/Hide "Buy" button
    btnBuy.Visible := ABuyButton;
    btnBuy.Enabled := AbuyButton;
  // Centering and show the form
    MainForm := Screen.ActiveForm;
    abfFormCenterForm(Form, MainForm);
    ShowModal;
  finally
    Free;
  end
end;

//------------------------------------------------------------------------------

procedure abfApplicationAbout(const AName: string);
begin
  abfApplicationAboutEx(AName, '', '', '',
    {$IfDef abfVCLTrial}True{$Else}False{$EndIf}, nil);
end;


//==============================================================================
// TfrmAboutApplication
//==============================================================================
// Application about form.
{ TfrmAboutApplication }

constructor TfrmAboutApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := SabfAboutApplicationCaption;
  lbCopyright.Caption := SabfCopyright;
  lbBy.Caption := SabfAboutApplicationBy;
  btnBuy.Caption := SabfBuy;
  lbSupport.Cursor := crHandPoint;
  lbWeb.Cursor := crHandPoint;
  lbSupport.Font.Color := clABFLink;
  lbWeb.Font.Color := clABFLink;
//  lbName1.Font.Color := clABFLink;
end;


//==============================================================================
// Properties Get/Set

function TfrmAboutApplication.GetAppName: string;
begin
  Result := lbName1.Caption;
end;

//------------------------------------------------------------------------------

procedure TfrmAboutApplication.SetAppName(const A: string);
begin
  lbName1.Caption := A;
  lbName2.Caption := A;
end;

//------------------------------------------------------------------------------

function TfrmAboutApplication.GetEmailSubject: string;
const
  SSubject  = 'From the %s application about...';
  SSubject2 = 'From the %s application (%s) about...';
begin
  if lbVersion.Visible then
    Result := Format(SSubject2, [AppName, lbVersion.Caption])
  else
    Result := Format(SSubject, [AppName]);
end;

//------------------------------------------------------------------------------

procedure TfrmAboutApplication.SetEmailPrefix(const A: string);
begin
  FEmailPrefix := A;
  if FEmailPrefix = '' then
    lbSupport.Caption := SabfSupportURL
  else
  if Pos('@', FEmailPrefix) > 0 then
    lbSupport.Caption := FEmailPrefix
  else
    lbSupport.Caption := FEmailPrefix + SabfEmail;
end;

//------------------------------------------------------------------------------

procedure TfrmAboutApplication.SetWebSuffix(const A: string);
begin
  FWebSuffix := A;
  if FWebSuffix = '' then
    lbWeb.Caption := SabfWebURL
  else
    lbWeb.Caption := SabfWebURL + '/' + FWebSuffix;
end;


//==============================================================================
// Event handlers

procedure TfrmAboutApplication.FormMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
// Reset labels underline
  if (fsUnderline in lbSupport.Font.Style) then
    lbSupport.Font.Style := lbSupport.Font.Style - [fsUnderline];
  if (fsUnderline in lbWeb.Font.Style) then
    lbWeb.Font.Style := lbWeb.Font.Style - [fsUnderline];
end;

//------------------------------------------------------------------------------

procedure TfrmAboutApplication.URLMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
// Set label underline
  with (Sender as TLabel) do
    if not (fsUnderline in Font.Style) then
      Font.Style := Font.Style + [fsUnderline];
end;

//------------------------------------------------------------------------------

procedure TfrmAboutApplication.URLMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbLeft then Exit;
  (Sender as TLabel).Font.Color := clYellow;
end;

//------------------------------------------------------------------------------

procedure TfrmAboutApplication.URLMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  (Sender as TLabel).Font.Color := clABFLink;
end;

//------------------------------------------------------------------------------

procedure TfrmAboutApplication.lbWebClick(Sender: TObject);
begin
  abfGotoUrlEx(TLabel(Sender).Caption, True);
end;

//------------------------------------------------------------------------------

procedure TfrmAboutApplication.lbSupportClick(Sender: TObject);
begin
  abfSendEmail(TLabel(Sender).Caption, EmailSubject);
end;

//------------------------------------------------------------------------------

procedure TfrmAboutApplication.btnBuyClick(Sender: TObject);
begin
  abfGotoUrlEx(SabfWeb_BuyURL, True);
end;

//------------------------------------------------------------------------------

end{unit abfAboutApplication}.
