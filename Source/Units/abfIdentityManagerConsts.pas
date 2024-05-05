{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfIdentityManagerConsts; { English version }

interface

{$IfNDef D3}
const
{$Else D3}
resourcestring
{$EndIf D3}
  SabfIdMan_ErrorLoadingAPI = '%s: Error loading the Identity Manager API!';
  SabfIdMan_NotOpenedError = '%s component is not active!';

  

{******************************************************************************}
implementation
{******************************************************************************}

end{unit abfIdentityManagerConsts}.
