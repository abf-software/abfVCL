//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("abfComponents_C5_D.res");
USEPACKAGE("Vcl50.bpi");
USEPACKAGE("dclstd50.bpi");
USEPACKAGE("abfComponents_C5_R.bpi");
USEUNIT("..\..\Source\Units\abfGraphicsReg.pas");
USEUNIT("..\..\Source\Units\abfComponentsReg.pas");
USEUNIT("..\..\Source\Units\abfControlsReg.pas");
USEFORMNS("..\..\Source\PropertyEditors\abfAboutApplication.pas", Abfaboutapplication, frmAboutApplication);
USEFORMNS("..\..\Source\PropertyEditors\abfAboutComponent.pas", Abfaboutcomponent, frmAboutComponent);
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
