{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfEditsConsts; { English version }

{$I abf.inc}

interface

{$IfNDef D3}
const
{$Else D3}
resourcestring
{$EndIf D3}
  SabfButtonEdit_ControlIsAlreadyUsed = 'Control "%s" is already used by "%s" edit';
  SabfButtonEdit_DesignerMenuItem1 = 'Click button';


{******************************************************************************}
implementation
{******************************************************************************}

end{unit abfEditsConsts}.
