//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("abf_C5_D.res");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("Vclx50.bpi");
USEPACKAGE("dclstd50.bpi");
USEPACKAGE("abf_C5_R.bpi");
USEFORMNS("..\..\Source\PropertyEditors\abfAboutApplication.pas", Abfaboutapplication, frmAboutApplication);
USEFORMNS("..\..\Source\PropertyEditors\abfAboutComponent.pas", Abfaboutcomponent, frmAboutComponent);
USEFORMNS("..\..\Source\PropertyEditors\abfDesignerColorComboBox.pas", Abfdesignercolorcombobox, frmDesignerColorComboBox);
USEFORMNS("..\..\Source\PropertyEditors\abfDesignerComboBox.pas", Abfdesignercombobox, frmDesignerComboBox);
USEFORMNS("..\..\Source\PropertyEditors\abfDesignerFontNameComboBox.pas", Abfdesignerfontnamecombobox, frmDesignerFontNameComboBox);
USEFORMNS("..\..\Source\PropertyEditors\abfDesignerFontSizeComboBox.pas", Abfdesignerfontsizecombobox, frmDesignerFontSizeComboBox);
USEFORMNS("..\..\Source\PropertyEditors\abfDesignerStatusBar.pas", Abfdesignerstatusbar, abfStatusPanelsEditor);
USEFORMNS("..\..\Source\PropertyEditors\abfDesignerWAB.pas", Abfdesignerwab, abfWABItemsEditor);
USEUNIT("..\..\Source\Units\abfWABReg.pas");
USEUNIT("..\..\Source\Units\abfAPMReg.pas");
USEUNIT("..\..\Source\Units\abfComboBoxesReg.pas");
USEUNIT("..\..\Source\Units\abfComControlsReg.pas");
USEUNIT("..\..\Source\Units\abfControlsReg.pas");
USEUNIT("..\..\Source\Units\abfDialogsReg.pas");
USEUNIT("..\..\Source\Units\abfEditsReg.pas");
USEUNIT("..\..\Source\Units\abfEffectsReg.pas");
USEUNIT("..\..\Source\Units\abfGraphicsReg.pas");
USEUNIT("..\..\Source\Units\abfLabelsReg.pas");
USEUNIT("..\..\Source\Units\abfMenusReg.pas");
USEUNIT("..\..\Source\Units\abfReg.pas");
USEUNIT("..\..\Source\Units\abfStatusBarsReg.pas");
USEUNIT("..\..\Source\Units\abfComponentsReg.pas");
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
