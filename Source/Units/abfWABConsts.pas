{*******************************************************************************

  ABF Visual Components Library

  Copyright (c) 2000-2020 ABF software, Inc. All Rights Reserved.
  Copyright (c) 2020 Dmytro Golovenko. MIT License.

  Source code: https://github.com/abf-software/abfVCL

*******************************************************************************}
unit abfWABConsts; { English version }

interface

{$IfNDef D3}
const
{$Else D3}
resourcestring
{$EndIf D3}
  SabfWAB_ErrorLoadingWABAPI = 'Error loading WAB API!';
  SabfWAB_DefaultItemName = 'Unknown';
  SabfWAB_DefaultAddrType = 'SMTP';
  SabfWAB_NotCompatibleCollection = 'This Collection is not compatible with %s';
  SabfWAB_NotOpenedError = 'The Windows Address Book is not opened';
  SabfWAB_InvalidWABConnection = 'Component is not connected to an opened Windows Address Book';
  SabfWAB_ReadOnlyError = 'The Windows Address book is opened in read-only mode';
  SabfWAB_NotImplementedError = 'Currently not implemented in this version';
  SabfWAB_ItemsPropertyName = 'Items';
  SabfWAB_ContactsPropertyName = 'Contacts';
  SabfWAB_GroupsPropertyName = 'Groups';
  SabfWAB_DesignerMenuItem1 = '&Browsing items...';
  SabfWAB_DesignerMenuItem2 = '&Browsing contacts...';
  SabfWAB_DesignerMenuItem3 = '&Browsing groups...';
  SabfWAB_UnknownVersion = 'Unknown version';

  SabfWABAddrDlg_DesignerMenuItem = 'Test dialog...';

{******************************************************************************}
implementation
{******************************************************************************}

end{unit abfWABConsts}.
