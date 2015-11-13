@ECHO OFF

IF "%1" == "dcc" GOTO MAKE_DCC
IF "%1" == "clean" GOTO MAKE_CLEAN
ECHO.
ECHO Usage: MAKEWIN (option)
ECHO.
ECHO Available options are:
ECHO   MAKEWIN dcc   ( To make the Delphi bindings & component. )
ECHO   MAKEWIN clean ( To clean up temp files. )
ECHO.
GOTO END


:MAKE_DCC
brcc32 -fo.\curlobj.res -r .\curlobj.rc
brcc32 -fo.\curlpkg.res -r .\curlpkg.rc
dcc32 .\curl_h.pas
dcc32 .\curlobj.pas
GOTO END

:MAKE_CLEAN
@ECHO ON
IF EXIST *.o    DEL *.o
IF EXIST *.ppu  DEL *.ppu
IF EXIST *.dcu  DEL *.dcu
IF EXIST *.bpl  DEL *.bpl
IF EXIST *.dcp  DEL *.dcp
@ECHO OFF
GOTO END

:END
