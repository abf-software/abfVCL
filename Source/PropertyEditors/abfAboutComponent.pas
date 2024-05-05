{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfAboutComponent;

{$I abf.inc}

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms,
  ExtCtrls, StdCtrls, Buttons;

{$IfNDef D3}
const
  crHandPoint = crUpArrow;
{$EndIf D3}

type

//==============================================================================
// TfrmAboutComponent
//==============================================================================
// Component about form.

  TfrmAboutComponent = class(TForm)
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
    function  GetComponentName: string;
    procedure SetComponentName(const A: string);
    function  GetEmailSubject: string;
    procedure SetEmailPrefix(const A: string);
    procedure SetWebSuffix(const A: string);
  public
    constructor Create(AOwner: TComponent); override;
    property ComponentName: string read GetComponentName write SetComponentName;
    property EmailSubject: string read GetEmailSubject;
    property EmailPrefix: string read FEmailPrefix write SetEmailPrefix;
    property WebSuffix: string read FWebSuffix write SetWebSuffix;
  end;

//------------------------------------------------------------------------------
// Do not remark a varible declaration! It is needed for C++Builder 4 and lower!

var
  frmAboutComponent: TfrmAboutComponent;

//==============================================================================
// Component about routines
//==============================================================================

procedure abfComponentAboutEx(const AComponent: TComponent; const AWebSuffix,
  AEmailPrefix: string; ABuyButton: Boolean);
procedure abfComponentAbout(const AComponent: TComponent);


{******************************************************************************}
implementation
{******************************************************************************}
{$R *.DFM}

uses
  SysUtils, 
  abfConsts, abfVclConsts, abfSysUtils, abfVclUtils;

//==============================================================================
// Component about routines
//==============================================================================

procedure abfComponentAboutEx(const AComponent: TComponent; const AWebSuffix,
  AEmailPrefix: string; ABuyButton: Boolean);
var
  Form: TfrmAboutComponent;
{$IfNDef D9}
  MainForm: TForm;
{$EndIf D9}  
begin
  if not Assigned(AComponent) then Exit;

  Form := TfrmAboutComponent.Create(nil);
  with Form do
  try
    ComponentName := AComponent.ClassName;
    WebSuffix := AWebSuffix;
    EmailPrefix := AEmailPrefix;
  // Show/Hide "Buy" button
    btnBuy.Visible := ABuyButton;
    btnBuy.Enabled := ABuyButton;

  // Centering the form
{$IfDef D9}
    Position := poOwnerFormCenter;
{$Else}

    if (csDesigning in AComponent.ComponentState) then
      MainForm := nil
    else
      MainForm := Screen.ActiveForm;

    abfFormCenterForm(Form, MainForm);
{$EndIf D9}

    ShowModal;
  finally
    Free;
  end
end;

//------------------------------------------------------------------------------

procedure abfComponentAbout(const AComponent: TComponent);
begin
  abfComponentAboutEx(AComponent, '', '',
    {$IfDef abfVCLTrial}True{$Else}False{$EndIf});
end;


//==============================================================================
// TfrmAboutComponent
//==============================================================================
// Component about form.
{ TfrmAboutComponent }

constructor TfrmAboutComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := SabfAboutComponentCaption;
  lbCopyright.Caption := SabfCopyright;
  lbVersion.Caption := Format(SabfAboutComponentVersionFmt, [SabfVCLVersion]);
  lbBy.Caption := SabfAboutComponentBy;
  btnBuy.Caption := SabfBuy;
  lbSupport.Cursor := crHandPoint;
  lbWeb  .Cursor := crHandPoint;
  lbSupport.Font.Color := clABFLink;
  lbWeb.Font.Color := clABFLink;
//  lbName1.Font.Color := clABFLink;
end;


//==============================================================================
// Properties Get/Set

function TfrmAboutComponent.GetComponentName: string;
begin
  Result := lbName1.Caption;
end;

//------------------------------------------------------------------------------

procedure TfrmAboutComponent.SetComponentName(const A: string);
begin
  lbName1.Caption := A;
  lbName2.Caption := A;
end;

//------------------------------------------------------------------------------

function TfrmAboutComponent.GetEmailSubject: string;
const
{$IfDef Trial}
  SSubject = 'From the %s component (Ver %s Trial, under %s) about...';
{$Else Trial}
  SSubject = 'From the %s component (Ver %s, under %s) about...';
{$EndIf Trial}
{$IfDef D2_ONLY}SDelphi = 'Delphi 2';    {$EndIf}
{$IfDef D3_ONLY}SDelphi = 'Delphi 3';    {$EndIf}
{$IfDef D4_ONLY}SDelphi = 'Delphi 4';    {$EndIf}
{$IfDef D5_ONLY}SDelphi = 'Delphi 5';    {$EndIf}
{$IfDef D6_ONLY}SDelphi = 'Delphi 6';    {$EndIf}
{$IfDef D7_ONLY}SDelphi = 'Delphi 7';    {$EndIf}
{$IfDef D8_ONLY}SDelphi = 'Delphi 8';    {$EndIf}
{$IfDef D9_ONLY}SDelphi = 'Delphi 2005'; {$EndIf}
{$IfDef D10_ONLY}SDelphi = 'Delphi 2006'; {$EndIf}
{$IfDef D11_ONLY}SDelphi = 'Delphi 2007'; {$EndIf}
{$IfDef C1_ONLY}SDelphi = 'C++Builder 1';{$EndIf}
{$IfDef C3_ONLY}SDelphi = 'C++Builder 3';{$EndIf}
{$IfDef C4_ONLY}SDelphi = 'C++Builder 4';{$EndIf}
{$IfDef C5_ONLY}SDelphi = 'C++Builder 5';{$EndIf}
{$IfDef C6_ONLY}SDelphi = 'C++Builder 6';{$EndIf}
{$IfDef C10_ONLY}SDelphi = 'C++Builder 2006';{$EndIf}
{$IfDef C11_ONLY}SDelphi = 'C++Builder 2007';{$EndIf}
begin
  Result := Format(SSubject, [ComponentName, SabfVCLVersion, SDelphi]);
end;

//------------------------------------------------------------------------------

procedure TfrmAboutComponent.SetEmailPrefix(const A: string);
begin
  FEmailPrefix := A;
  if FEmailPrefix = '' then
    lbSupport.Caption := SabfSupportURL
  else
    lbSupport.Caption := FEmailPrefix + SabfEmail;
end;

//------------------------------------------------------------------------------

procedure TfrmAboutComponent.SetWebSuffix(const A: string);
begin
  FWebSuffix := A;
  if FWebSuffix = '' then
    lbWeb.Caption := SabfWebURL
  else
    lbWeb.Caption := SabfWebURL + '/' + FWebSuffix;
end;


//==============================================================================
// Event handlers

procedure TfrmAboutComponent.FormMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
// Reset labels underline
  if (fsUnderline in lbSupport.Font.Style) then
    lbSupport.Font.Style := lbSupport.Font.Style - [fsUnderline];
  if (fsUnderline in lbWeb.Font.Style) then
    lbWeb.Font.Style := lbWeb.Font.Style - [fsUnderline];
end;

//------------------------------------------------------------------------------

procedure TfrmAboutComponent.URLMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
// Set label underline
  with (Sender as TLabel) do
    if not (fsUnderline in Font.Style) then
      Font.Style := Font.Style + [fsUnderline];
end;

//------------------------------------------------------------------------------

procedure TfrmAboutComponent.URLMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbLeft then Exit;
  (Sender as TLabel).Font.Color := clYellow;
end;

//------------------------------------------------------------------------------

procedure TfrmAboutComponent.URLMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  (Sender as TLabel).Font.Color := clABFLink;
end;

//------------------------------------------------------------------------------

procedure TfrmAboutComponent.lbWebClick(Sender: TObject);
begin
  abfGotoUrlEx(TLabel(Sender).Caption, True);
end;

//------------------------------------------------------------------------------

procedure TfrmAboutComponent.lbSupportClick(Sender: TObject);
begin
  abfSendEmail(TLabel(Sender).Caption, EmailSubject);
end;

//------------------------------------------------------------------------------

procedure TfrmAboutComponent.btnBuyClick(Sender: TObject);
begin
  abfGotoUrlEx(SabfWeb_BuyURL, True);
end;

//------------------------------------------------------------------------------

end{unit abfAboutComponent}.
