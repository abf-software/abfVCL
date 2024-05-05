@ECHO OFF
SET Ver=5.0.0.5
SET VerS=5.0
SET VerC="5,0,0,5"

call _MakeOneRes.bat abfComponents %Ver% %VerS% %VerC%
call _MakeOneRes.bat abf %Ver% %VerS% %VerC%

