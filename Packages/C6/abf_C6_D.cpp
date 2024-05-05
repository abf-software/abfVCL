//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("..\..\Source\PropertyEditors\abfAboutComponent.pas", Abfaboutcomponent, frmAboutComponent);
USEFORMNS("..\..\Source\PropertyEditors\abfAboutApplication.pas", Abfaboutapplication, frmAboutApplication);
USEFORMNS("..\..\Source\PropertyEditors\abfDesignerWAB.pas", Abfdesignerwab, abfWABItemsEditor);
USEFORMNS("..\..\Source\PropertyEditors\abfDesignerColorComboBox.pas", Abfdesignercolorcombobox, frmDesignerColorComboBox);
USEFORMNS("..\..\Source\PropertyEditors\abfDesignerComboBox.pas", Abfdesignercombobox, frmDesignerComboBox);
USEFORMNS("..\..\Source\PropertyEditors\abfDesignerFontNameComboBox.pas", Abfdesignerfontnamecombobox, frmDesignerFontNameComboBox);
USEFORMNS("..\..\Source\PropertyEditors\abfDesignerFontSizeComboBox.pas", Abfdesignerfontsizecombobox, frmDesignerFontSizeComboBox);
USEFORMNS("..\..\Source\PropertyEditors\abfDesignerStatusBar.pas", Abfdesignerstatusbar, abfStatusPanelsEditor);
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
