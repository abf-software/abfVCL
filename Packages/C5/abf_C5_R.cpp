//---------------------------------------------------------------------------

#include <vcl.h>

#pragma link "wininet.lib"

#pragma hdrstop
USERES("abf_C5_R.res");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("Vclx50.bpi");
USEUNIT("..\..\Source\Units\abfAPM.pas");
USEUNIT("..\..\Source\Units\abfAPMUtils.pas");
USEUNIT("..\..\Source\Units\abfAppProps.pas");
USEUNIT("..\..\Source\Units\abfClasses.pas");
USEUNIT("..\..\Source\Units\abfComboBoxes.pas");
USEUNIT("..\..\Source\Units\abfComControls.pas");
USEUNIT("..\..\Source\Units\abfComControlsConsts.pas");
USEUNIT("..\..\Source\Units\abfComponents.pas");
USEUNIT("..\..\Source\Units\abfConsts.pas");
USEUNIT("..\..\Source\Units\abfControls.pas");
USEUNIT("..\..\Source\Units\abfConvUtils.pas");
USEUNIT("..\..\Source\Units\abfDialogs.pas");
USEUNIT("..\..\Source\Units\abfDialogsConsts.pas");
USEUNIT("..\..\Source\Units\abfEdits.pas");
USEUNIT("..\..\Source\Units\abfEditsConsts.pas");
USEUNIT("..\..\Source\Units\abfEffects.pas");
USEUNIT("..\..\Source\Units\abfEffectsConsts.pas");
USEUNIT("..\..\Source\Units\abfGraphics.pas");
USEUNIT("..\..\Source\Units\abfIdentityManager.pas");
USEUNIT("..\..\Source\Units\abfIdentityManagerConsts.pas");
USEUNIT("..\..\Source\Units\abfIDEUtils.pas");
USEUNIT("..\..\Source\Units\abfIniFiles.pas");
USEUNIT("..\..\Source\Units\abfInternet.pas");
USEUNIT("..\..\Source\Units\abfLabels.pas");
USEUNIT("..\..\Source\Units\abfMenus.pas");
USEUNIT("..\..\Source\Units\abfPropertyDesc.pas");
USEUNIT("..\..\Source\Units\abfRegistry.pas");
USEUNIT("..\..\Source\Units\abfShlObj.pas");
USEUNIT("..\..\Source\Units\abfShlUtils.pas");
USEUNIT("..\..\Source\Units\abfStatusBars.pas");
USEUNIT("..\..\Source\Units\abfStatusBarsConsts.pas");
USEUNIT("..\..\Source\Units\abfStdConvs.pas");
USEUNIT("..\..\Source\Units\abfStdConvsConsts.pas");
USEUNIT("..\..\Source\Units\abfStrUtils.pas");
USEUNIT("..\..\Source\Units\abfSysUtils.pas");
USEUNIT("..\..\Source\Units\abfTypInfo.pas");
USEUNIT("..\..\Source\Units\abfUSF.pas");
USEUNIT("..\..\Source\Units\abfVclConsts.pas");
USEUNIT("..\..\Source\Units\abfVclUtils.pas");
USEUNIT("..\..\Source\Units\abfWAB.pas");
USEUNIT("..\..\Source\Units\abfWABConsts.pas");
USEUNIT("..\..\Source\Units\abfWABDialogs.pas");
USEUNIT("..\..\Source\Units\abfWABUtils.pas");
USEUNIT("..\..\Source\ThirdParty\WabTags.pas");
USEUNIT("..\..\Source\ThirdParty\msident.pas");
USEUNIT("..\..\Source\ThirdParty\WabApi.pas");
USEUNIT("..\..\Source\ThirdParty\WabCode.pas");
USEUNIT("..\..\Source\ThirdParty\WabDefs.pas");
USEUNIT("..\..\Source\ThirdParty\WabIab.pas");
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
