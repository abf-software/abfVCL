{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfEffectsConsts; { English version }

{$I abf.inc}

interface

{$IfNDef D3}
const
{$Else D3}
resourcestring
{$EndIf D3}
  SabfEffects_WrongOwner = 'Wrong owner';
  SabfFormEffect_AlreadyExists = 'The %:0s component or its descendant is already present on the form. Only one %:0s component or its descendant is allowed on the form at the same time';
  SabfFormEffect_WrongOwner    = 'The owner of the %s component is not a TForm descendant';

{******************************************************************************}
implementation
{******************************************************************************}

end{unit abfEffectsConsts}.
