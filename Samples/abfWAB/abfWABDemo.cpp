//-----------------------------------------------------------------------------
//
//  abfWAB Demo.
//
//  Copyright (c) 2000, 2001 ABF software, Inc.
//  All Rights Reserved.
//
//  e-mail: info@ABFsoftware.com
//  web:    http://www.ABFsoftware.com
//
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("abfWABDemo.res");
USEFORMNS("abfWABDemoMain.pas", Abfwabdemomain, frmWabDemoMain);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
    try
    {
        Application->Initialize();
        Application->Title = "abfWAB Demo";
        Application->CreateForm(__classid(TfrmWabDemoMain), &frmWabDemoMain);
        Application->Run();
    }
    catch (Exception &exception)
    {
        Application->ShowException(&exception);
    }
    return 0;
}
//-----------------------------------------------------------------------------
