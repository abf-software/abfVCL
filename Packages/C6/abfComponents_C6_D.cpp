//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("..\..\Source\PropertyEditors\abfAboutComponent.pas", Abfaboutcomponent, frmAboutComponent);
USEFORMNS("..\..\Source\PropertyEditors\abfAboutApplication.pas", Abfaboutapplication, frmAboutApplication);
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
