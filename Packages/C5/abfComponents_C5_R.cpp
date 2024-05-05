//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("abfComponents_C5_R.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("..\..\Source\Units\abfAppProps.pas");
USEUNIT("..\..\Source\Units\abfClasses.pas");
USEUNIT("..\..\Source\Units\abfComponents.pas");
USEUNIT("..\..\Source\Units\abfConsts.pas");
USEUNIT("..\..\Source\Units\abfControls.pas");
USEUNIT("..\..\Source\Units\abfGraphics.pas");
USEUNIT("..\..\Source\Units\abfIDEUtils.pas");
USEUNIT("..\..\Source\Units\abfPropertyDesc.pas");
USEUNIT("..\..\Source\Units\abfRegistry.pas");
USEUNIT("..\..\Source\Units\abfShlObj.pas");
USEUNIT("..\..\Source\Units\abfShlUtils.pas");
USEUNIT("..\..\Source\Units\abfStrUtils.pas");
USEUNIT("..\..\Source\Units\abfSysUtils.pas");
USEUNIT("..\..\Source\Units\abfTypInfo.pas");
USEUNIT("..\..\Source\Units\abfVclConsts.pas");
USEUNIT("..\..\Source\Units\abfVclUtils.pas");
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
