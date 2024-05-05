@ECHO OFF
:**********************************************************
: %1 - Base file name, abfComponents for example
: %2 - $Ver$ value
: %3 - $VerS$ value
: %4 - $VerC$ value
:**********************************************************

:==========================================================
: Delete old files
:==========================================================
attrib -R ..\%1_*.res /S
del ..\%1_*.res /S
attrib -R %1_X_D.rc
del %1_X_D.rc
attrib -R %1_X_R.rc
del %1_X_R.rc

:==========================================================
: Create new *.rc file
:==========================================================
copy %1_X_D._rc %1_X_D.rc
Replacer %1_X_D.rc $Ver$  %2
Replacer %1_X_D.rc $VerS$ %3
Replacer %1_X_D.rc $VerC$ %4

copy %1_X_R._rc %1_X_R.rc
Replacer %1_X_R.rc $Ver$  %2
Replacer %1_X_R.rc $VerS$ %3
Replacer %1_X_R.rc $VerC$ %4

:==========================================================
: Create new *.res file and save it under different names
:==========================================================
brcc32 -m %1_X_D.rc
:copy %1_X_D.res ..\D4\%1_D4_D.res
copy %1_X_D.res ..\D5\%1_D5_D.res
copy %1_X_D.res ..\D6\%1_D6_D.res
copy %1_X_D.res ..\D7\%1_D7_D.res
:copy %1_X_D.res ..\D8\%1_D8_D.res
copy %1_X_D.res ..\D9\%1_D9_D.res
copy %1_X_D.res ..\D10\%1_D10_D.res
copy %1_X_D.res ..\D11\%1_D11_D.res

:copy %1_X_D.res ..\C4\%1_C4_D.res
copy %1_X_D.res ..\C5\%1_C5_D.res
copy %1_X_D.res ..\C6\%1_C6_D.res
copy %1_X_D.res ..\C10\%1_C10_D.res
copy %1_X_D.res ..\C11\%1_C11_D.res


brcc32 -m %1_X_R.rc
:copy %1_X_R.res ..\D4\%1_D4_R.res
copy %1_X_R.res ..\D5\%1_D5_R.res
copy %1_X_R.res ..\D6\%1_D6_R.res
copy %1_X_R.res ..\D7\%1_D7_R.res
:copy %1_X_R.res ..\D8\%1_D8_R.res
copy %1_X_R.res ..\D9\%1_D9_R.res
copy %1_X_R.res ..\D10\%1_D10_R.res
copy %1_X_R.res ..\D11\%1_D11_R.res

:copy %1_X_R.res ..\C4\%1_C4_R.res
copy %1_X_R.res ..\C5\%1_C5_R.res
copy %1_X_R.res ..\C6\%1_C6_R.res
copy %1_X_R.res ..\C10\%1_C10_R.res
copy %1_X_R.res ..\C11\%1_C11_R.res

:==========================================================
: Delete temp files
:==========================================================
del %1_X_D.rc 
del %1_X_D.res

del %1_X_R.rc 
del %1_X_R.res

:==========================================================
: Set ReadOnly attributes
:==========================================================
attrib +R ..\%1_*.res /S
