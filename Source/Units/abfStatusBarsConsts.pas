{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfStatusBarsConsts; { English version }

{$I abf.inc}

interface

{$IfNDef D3}
const
{$Else D3}
resourcestring
{$EndIf D3}
  SabfStatusBars_OutOfRange             = 'Value must be between %d and %d';
  SabfStatusBars_NotValidPanelName      = '''''%s'''' is not a valid status panel name';
  SabfStatusBars_PanelNameAlreadyExists = 'A status panel named ''''%s'''' already exists';
  SabfStatusBars_PanelNotFound          = 'Status panel ''%s'' not found';
  SabfStatusBars_PercentCaption         = '%d%%';
  SabfStatusBars_CountCaption           = '%d of %d';
  SabfStatusBars_NumLockCaption         = 'NUM';
  SabfStatusBars_CapsLockCaption        = 'CAPS';
  SabfStatusBars_ScrollLockCaption      = 'SCROLL';
  SabfStatusBars_DesignerMenuItem       = '&Panels Editor...';

{******************************************************************************}
implementation
{******************************************************************************}

end{unit abfStatusBarsConsts}.
